from __future__ import annotations

from dataclasses import dataclass, field
from pathlib import Path
from typing import Optional, Tuple, Union, Any, Dict

import numpy as np


# =========================
# Data containers
# =========================

@dataclass
class SourcePos:
    x: Optional[np.ndarray] = None  # meters
    y: Optional[np.ndarray] = None  # meters
    z: Optional[np.ndarray] = None  # meters


@dataclass
class ReceiverPos:
    r: Optional[np.ndarray] = None  # meters (range)
    z: Optional[np.ndarray] = None  # meters (depth)


@dataclass
class Pos:
    theta: Optional[np.ndarray] = None
    s: SourcePos = field(default_factory=SourcePos)
    r: ReceiverPos = field(default_factory=ReceiverPos)
    dims: Dict[str, int] = field(default_factory=dict)

@dataclass
class Bty:
    r: Optional[np.ndarray] = None  # meters (range)
    z: Optional[np.ndarray] = None  # meters (depth)
    dims: Dict[str, int] = field(default_factory=dict)

# =========================
# Helpers
# =========================

def _clean_str(b: bytes) -> str:
    return b.decode("ascii", errors="ignore").rstrip("\x00").rstrip()


def _record_offset_bytes(rec1_based: int, rec_bytes: int) -> int:
    return (rec1_based - 1) * rec_bytes


def _read_record_raw(f, rec1_based: int, rec_bytes: int) -> bytes:
    f.seek(_record_offset_bytes(rec1_based, rec_bytes), 0)
    b = f.read(rec_bytes)
    if len(b) != rec_bytes:
        raise EOFError(f"Truncated file: could not read record {rec1_based}")
    return b


def _is_printable_ascii(b: bytes, min_ratio: float = 0.85) -> bool:
    if len(b) == 0:
        return False
    printable = sum((32 <= x <= 126) or (x in (9, 10, 13, 0)) for x in b)
    return (printable / len(b)) >= min_ratio


def _detect_endian_and_recbytes(filename: Union[str, Path]) -> Tuple[str, int, int]:
    """
    Detect endian and record length for files written by RWSHDFile.f90:

      - Record 1: LRecl (int32) + Title (80 chars)
      - Access is DIRECT, UNFORMATTED with RECL = 4*LRecl (bytes)
      - Record 2 begins with PlotType (10 chars)

    We try endian '>' then '<' and validate Title/PlotType sanity.
    """
    fn = Path(filename)
    with fn.open("rb") as f:
        first4 = f.read(4)
        if len(first4) != 4:
            raise EOFError("File too small to read LRecl.")

    for endian in (">", "<"):
        i4 = np.dtype(endian + "i4")
        LRecl = int(np.frombuffer(first4, dtype=i4)[0])
        if not (1 <= LRecl <= 50_000_000):
            continue

        rec_bytes = 4 * LRecl
        try:
            with fn.open("rb") as f:
                rec1 = _read_record_raw(f, 1, rec_bytes)
                rec2 = _read_record_raw(f, 2, rec_bytes)
        except Exception:
            continue

        title_bytes = rec1[4:4 + 80]
        plot_bytes = rec2[:10]

        title_ok = _is_printable_ascii(title_bytes)
        plot_ok = _is_printable_ascii(plot_bytes) and (
            b"rectilin" in plot_bytes or b"irregular" in plot_bytes or plot_bytes.startswith(b"TL")
        )

        if title_ok and plot_ok:
            return endian, rec_bytes, LRecl

    raise ValueError("Could not detect endian/record length for this SHD file.")


def _plausible_depth(z: np.ndarray) -> bool:
    # accepts depth vectors like [60, ...] or monotonic receiver depths
    if z.size == 0:
        return False
    if not np.all(np.isfinite(z)):
        return False
    # typical ocean depths; allow a bit beyond
    if np.nanmin(z) < -5.0:
        return False
    if np.nanmax(z) > 20000.0:
        return False
    return True

def _plausible_range(x: np.ndarray) -> bool:
    x = np.asarray(x, dtype=float)
    if x.size < 2:
        return True
    # allow 0 at start; should be nondecreasing and within sane bounds
    return np.all(np.isfinite(x)) and np.min(x) >= 0.0 and np.max(x) < 1e9 and np.all(np.diff(x) >= 0.0)


def _plausible_theta(th: np.ndarray) -> bool:
    if th.size == 0 or not np.all(np.isfinite(th)):
        return False
    # degrees or radians; accept wide but reject nonsense
    if np.nanmin(th) < -720.0:
        return False
    if np.nanmax(th) > 720.0:
        return False
    return True


def _plausible_xy(x: np.ndarray) -> bool:
    if x.size == 0 or not np.all(np.isfinite(x)):
        return False
    # meters; allow huge but reject truly insane
    if np.nanmax(np.abs(x)) > 1e9:
        return False
    return True


def _read_real_vector_from_record(
    rec: bytes,
    count: int,
    endian: str,
    prefer: str,
    plausibility_fn,
) -> Tuple[np.ndarray, np.dtype]:
    """
    Attempt to read a vector of REALs from a fixed record as float32 or float64.

    prefer: "f8" or "f4" (try that first)
    """
    if count <= 0:
        return np.array([], dtype=float), np.dtype(endian + "f8")

    dtypes = [np.dtype(endian + "f8"), np.dtype(endian + "f4")]
    if prefer == "f4":
        dtypes = [np.dtype(endian + "f4"), np.dtype(endian + "f8")]

    for dt in dtypes:
        nbytes = count * dt.itemsize
        if nbytes > len(rec):
            continue
        arr = np.frombuffer(rec[:nbytes], dtype=dt, count=count).astype(float)
        if plausibility_fn(arr):
            return arr, dt

    # fallback: take prefer if possible
    dt = np.dtype(endian + ("f8" if prefer == "f8" else "f4"))
    nbytes = count * dt.itemsize
    if nbytes <= len(rec):
        arr = np.frombuffer(rec[:nbytes], dtype=dt, count=count).astype(float)
        return arr, dt

    raise ValueError("Could not read REAL vector: not enough bytes in record.")


def _read_two_reals_from_rec3(rec3: bytes, endian: str, freqVec: np.ndarray) -> Tuple[float, float, np.dtype]:
    """
    Record 3 layout (per writer):
      WRITE(...,REC=3) Nfreq, Ntheta, NSx, NSy, NSz, NRz, NRr, freq0, atten

    freq0/atten are REAL, but REAL may be 4 or 8 bytes depending on build.
    We detect by plausibility: freq0 should be positive and near freqVec.
    """
    off = 7 * 4  # 7 int32
    # try f8 first because your symptom shows REAL*8 in at least Sz
    for dt in (np.dtype(endian + "f8"), np.dtype(endian + "f4")):
        need = 2 * dt.itemsize
        if off + need > len(rec3):
            continue
        freq0 = float(np.frombuffer(rec3[off:off + dt.itemsize], dtype=dt, count=1)[0])
        atten = float(np.frombuffer(rec3[off + dt.itemsize:off + 2 * dt.itemsize], dtype=dt, count=1)[0])

        if np.isfinite(freq0) and freq0 > 0:
            # if freqVec exists, check closeness
            if freqVec.size > 0:
                if np.min(np.abs(freqVec - freq0)) < 1e6:  # generous
                    return freq0, atten, dt
            else:
                return freq0, atten, dt

    # last resort: default f8 if possible
    dt = np.dtype(endian + "f8")
    freq0 = float(np.frombuffer(rec3[off:off + 8], dtype=dt, count=1)[0])
    atten = float(np.frombuffer(rec3[off + 8:off + 16], dtype=dt, count=1)[0])
    return freq0, atten, dt


# =========================
# Reader
# =========================

def read_shd(
    filename: Union[str, Path],
    *,
    freq: Optional[float] = None,
    xs_km: Optional[float] = None,
    ys_km: Optional[float] = None,
) -> Tuple[str, str, np.ndarray, float, float, Pos, np.ndarray]:
    """
    Read SHD/GRN file written by RWSHDFile.f90 (direct-access fixed records).

    Returns:
      title, PlotType, freqVec, freq0, atten, Pos, pressure

    pressure shape:
      (Ntheta, Nsz, Nrz, Nrr) complex for rectilinear
      (Ntheta, Nsz, 1,   Nrr) complex for irregular
    """
    fn = Path(filename)
    endian, rec_bytes, LRecl = _detect_endian_and_recbytes(fn)

    i4 = np.dtype(endian + "i4")
    f8 = np.dtype(endian + "f8")

    with fn.open("rb") as f:
        rec1 = _read_record_raw(f, 1, rec_bytes)
        title = _clean_str(rec1[4:4 + 80])

        rec2 = _read_record_raw(f, 2, rec_bytes)
        PlotType = _clean_str(rec2[:10])

        # Record 3: ints + freq0/atten (REAL kind unknown)
        rec3 = _read_record_raw(f, 3, rec_bytes)
        ints = np.frombuffer(rec3[:7 * 4], dtype=i4, count=7)
        Nfreq, Ntheta, Nsx, Nsy, Nsz, Nrz, Nrr = map(int, ints.tolist())

        # Record 4: freqVec is written as freqVec(1:Nfreq). In Bellhop it is effectively REAL*8.
        rec4 = _read_record_raw(f, 4, rec_bytes)
        freqVec = np.frombuffer(rec4, dtype=f8, count=Nfreq).astype(float)

        # Now decode freq0/atten using freqVec for plausibility
        freq0, atten, real_dt_rec3 = _read_two_reals_from_rec3(rec3, endian, freqVec)

        # Record 5: theta (REAL kind unknown)
        rec5 = _read_record_raw(f, 5, rec_bytes)
        theta, real_dt_theta = _read_real_vector_from_record(rec5, Ntheta, endian, prefer="f8", plausibility_fn=_plausible_theta)

        # Record 6/7: Sx/Sy (REAL kind unknown; TL uses 2 endpoints)
        rec6 = _read_record_raw(f, 6, rec_bytes)
        rec7 = _read_record_raw(f, 7, rec_bytes)

        if PlotType.startswith("TL"):
            sx2, dt_sx = _read_real_vector_from_record(rec6, 2, endian, prefer="f8", plausibility_fn=_plausible_xy)
            sy2, dt_sy = _read_real_vector_from_record(rec7, 2, endian, prefer="f8", plausibility_fn=_plausible_xy)
            sx = np.linspace(sx2[0], sx2[1], Nsx) if Nsx > 1 else np.array([sx2[0]], dtype=float)
            sy = np.linspace(sy2[0], sy2[1], Nsy) if Nsy > 1 else np.array([sy2[0]], dtype=float)
            real_dt_xy = dt_sx
        else:
            sx, dt_sx = _read_real_vector_from_record(rec6, Nsx, endian, prefer="f8", plausibility_fn=_plausible_xy)
            sy, dt_sy = _read_real_vector_from_record(rec7, Nsy, endian, prefer="f8", plausibility_fn=_plausible_xy)
            real_dt_xy = dt_sx

        # Record 8/9: Sz / Rz (REAL kind unknown)  <-- THIS is where your 60 -> 3.21875 happened
        rec8 = _read_record_raw(f, 8, rec_bytes)
        rec9 = _read_record_raw(f, 9, rec_bytes)
        Sz, real_dt_z = _read_real_vector_from_record(rec8, Nsz, endian, prefer="f8", plausibility_fn=_plausible_depth)
        Rz, _ = _read_real_vector_from_record(rec9, Nrz, endian, prefer="f8", plausibility_fn=_plausible_depth)

        # Record 10: Rr (receiver ranges). Many SHD writers store this as REAL*4.
        rec10 = _read_record_raw(f, 10, rec_bytes)
        Rr, _ = _read_real_vector_from_record(
            rec10, Nrr, endian, prefer="f4", plausibility_fn=_plausible_range
        )
        # # Record 10: Rr (effectively REAL*8 by design)
        # rec10 = _read_record_raw(f, 10, rec_bytes)
        # Rr = np.frombuffer(rec10, dtype=f8, count=Nrr).astype(float)

        # Pressure allocation
        if PlotType.strip() == "irregular":
            Nrcvrs_per_range = 1
            pressure = np.zeros((Ntheta, Nsz, 1, Nrr), dtype=np.complex128)
        else:
            Nrcvrs_per_range = Nrz
            pressure = np.zeros((Ntheta, Nsz, Nrz, Nrr), dtype=np.complex128)

        # Frequency selection
        ifreq = 0
        if freq is not None:
            ifreq = int(np.argmin(np.abs(freqVec - float(freq))))

        # Source selection (default first)
        if xs_km is not None and ys_km is not None:
            xs_m = float(xs_km) * 1000.0
            ys_m = float(ys_km) * 1000.0
            ix = int(np.argmin(np.abs(sx - xs_m)))
            iy = int(np.argmin(np.abs(sy - ys_m)))
        else:
            ix, iy = 0, 0

        # Field record numbering:
        # header: records 1..10; first field row is record 11
        base_rec = 11

        rows_per_sz = Nrcvrs_per_range
        rows_per_theta = Nsz * rows_per_sz
        rows_per_freq = Ntheta * rows_per_theta
        rows_per_sy = rows_per_freq
        rows_per_sx = Nsy * rows_per_sy

        # Determine REAL kind used for COMPLEX storage:
        # COMPLEX is "two REAL". If REAL is 8 bytes, record must hold 2*Nrr float64.
        # If REAL is 4 bytes, record holds 2*Nrr float32.
        # We pick based on what fits in the record.
        if 2 * Nrr * np.dtype(endian + "f8").itemsize <= rec_bytes:
            p_real_dt = np.dtype(endian + "f8")
        else:
            p_real_dt = np.dtype(endian + "f4")

        def read_complex_row(rec1_based: int) -> np.ndarray:
            rec = _read_record_raw(f, rec1_based, rec_bytes)
            need = 2 * Nrr * p_real_dt.itemsize
            tmp = np.frombuffer(rec[:need], dtype=p_real_dt, count=2 * Nrr).astype(np.float64)
            return tmp[0::2] + 1j * tmp[1::2]

        for itheta in range(Ntheta):
            for isz in range(Nsz):
                for irzrow in range(Nrcvrs_per_range):
                    offset_rows = (
                        ifreq * rows_per_freq
                        + itheta * rows_per_theta
                        + isz * rows_per_sz
                        + irzrow
                        + ix * rows_per_sx
                        + iy * rows_per_sy
                    )
                    recnum = base_rec + offset_rows
                    pressure[itheta, isz, irzrow, :] = read_complex_row(recnum)

    pos = Pos(
        theta=theta,
        s=SourcePos(x=sx, y=sy, z=Sz),
        r=ReceiverPos(r=Rr, z=Rz),
        dims=dict(Nfreq=Nfreq, Ntheta=Ntheta, Nsx=Nsx, Nsy=Nsy, Nsz=Nsz, Nrz=Nrz, Nrr=Nrr),
    )

    return title, PlotType, freqVec, float(freq0), float(atten), pos, pressure

def read_bty(
        fname: Union[str, Path], 
        r_units: str='km'):
    """
    Read BTY file

    Returns:
      bty : ranges, depths
    """
    try:
        content = np.genfromtxt(fname, skip_header=2)
    except FileNotFoundError:
        print(f"Error: The file '{fname}' was not found.")
    except UnicodeDecodeError:
        print(f"Error: The file '{fname}' is not a valid ASCII file. Try a different encoding like 'utf-8'.")

    ranges=content[:,0]
    if r_units=='m':
        ranges = ranges * 1000.

    bty = Bty(
            r=ranges, 
            z=content[:,1],
            dims=dict(Npts=content.shape[0]),
            )
    return bty 


def plotshd(
    filename: Union[str, Path],
    *,
    freq: Optional[float] = None,
    units: str = "m",
    itheta: int = 0,
    isz: int = 0,
    ax=None,
    cmap: str = "turbo",
    vmin=None,
    vmax=0,
    add_bty=True,
    add_title=False,
) -> Any:
    import matplotlib.pyplot as plt
    from sys import float_info as fi
    from sandbox import utearth, utorange, utyellow

    title, PlotType, freqVec, freq0, atten, Pos, pressure = read_shd(filename, freq=freq)

    if ax is None:
        _, ax = plt.subplots(figsize=(10, 5))

    field = pressure[itheta, isz, :, :]  # (Nrz or 1, Nrr)
    amp = np.abs(field).astype(np.float64)
#    amp[~np.isfinite(amp)] = 1e-300
#    amp = np.maximum(amp, 1e-300)
    TL = 20.0 * np.log10(fi.epsilon + amp)

    r = Pos.r.r.copy()
    z = Pos.r.z.copy()

    rb = np.asarray(Pos.r.r)
    zb = np.asarray(Pos.r.z)[:TL.shape[0]]
    print("r monotonic increasing?", np.all(np.diff(rb) > 0))
    print("r monotonic decreasing?", np.all(np.diff(rb) < 0))
    print("r unique?", np.unique(rb).size == rb.size)
    
    print("z monotonic increasing?", np.all(np.diff(zb) > 0))
    print("z monotonic decreasing?", np.all(np.diff(zb) < 0))
    print("z unique?", np.unique(zb).size == zb.size)
    
    # show first few "bad" diffs if needed
    bad = np.where(np.diff(rb) <= 0)[0]
    print("first bad r indices:", bad[:10])

    xlab = "range [m]"
    if units == "km":
        r = r / 1000.0
        xlab = "range [km]"

    R, Z = np.meshgrid(r, z[:TL.shape[0]])
    pcm = ax.pcolormesh(R, Z, TL, shading="auto", cmap=cmap, 
                        vmax=vmax,vmin=vmin)

    if add_bty:
        bty = read_bty(filename[:-3]+'bty', r_units=units)
        ax.fill_between(
                bty.r, bty.z,
                y2=np.nanmax(Z),
                color=utearth,
        )

        # --- sourcec and receiver dots
        ax.scatter(0,73, label='source = 73 m',
            s=100,
            color=utyellow,
            edgecolor='k',
            zorder=1000,
           )
        ax.scatter(2442, 300, label='receiver = 300 m',
            s=100,
            color=utorange,
            edgecolor='k',
            zorder=1000
           )

    ax.set_xlim(np.nanmin(R), np.nanmax(R))
    ax.set_ylim(np.nanmax(Z), np.nanmin(Z))
    ax.set_xlabel(xlab)
    ax.set_ylabel("depth [m]")
    if add_title:
        ax.set_title(f"{title}\nPlotType={PlotType.strip()}  freq0={freq0:g} Hz  z_src={Pos.s.z[isz]:g} m")

    cb = plt.colorbar(pcm, ax=ax, orientation='horizontal', location='top')
    cb.ax.tick_params(labelsize=12)
    cb.set_label("Transmission Loss [dB]")
    return ax
