from __future__ import annotations
from dataclasses import dataclass
from typing import Optional, Tuple, Dict, Any
import os
import numpy as np


def _decode_fixed(b: bytes) -> str:
    return b.decode("ascii", errors="ignore").rstrip("\x00").rstrip()


@dataclass
class ShdGeometry:
    title: str
    plot_type: str
    atten: float
    freq0: float
    freq: np.ndarray
    theta: np.ndarray
    sx: np.ndarray
    sy: np.ndarray
    sz: np.ndarray
    rz: np.ndarray
    rr: np.ndarray
    dims: Dict[str, int]


def _infer_layout(filename: str) -> tuple[np.dtype, int]:
    """
    Infer (int32 endian dtype, rec_bytes) for a BELLHOP .shd direct-access file.

    Tries:
      endian: little, big
      units : LRecl is bytes OR LRecl is 4-byte words (so rec_bytes = 4*LRecl)
    Picks a candidate where record 2 looks like ASCII PlotType and rec_bytes <= file size.
    """
    file_size = os.path.getsize(filename)
    if file_size < 16:
        raise EOFError(f"File too small to be a .shd (size={file_size} bytes).")

    with open(filename, "rb") as f:
        first4 = f.read(4)
        if len(first4) != 4:
            raise EOFError("Could not read first 4 bytes.")

    candidates = []
    for i4 in (np.dtype("<i4"), np.dtype(">i4")):
        LRecl = int(np.frombuffer(first4, dtype=i4, count=1)[0])
        for rec_bytes in (LRecl, 4 * LRecl):
            # Basic plausibility
            if rec_bytes <= 0:
                continue
            if rec_bytes > file_size:
                continue
            if rec_bytes < 84:  # must at least hold int32+80-char title
                continue

            # Check if record 2 contains a plausible PlotType string
            with open(filename, "rb") as f:
                # record 2 starts at offset rec_bytes (0-based file)
                f.seek(rec_bytes, 0)
                b2 = f.read(10)
                plot = _decode_fixed(b2)

            # Heuristic: PlotType tends to be ascii-ish and non-empty
            if plot and all(31 < ord(c) < 127 for c in plot):
                candidates.append((i4, rec_bytes, plot))

    if not candidates:
        raise ValueError(
            "Could not infer SHD layout. Likely not a BELLHOP direct-access .shd "
            "or the file is corrupted."
        )

    # Prefer candidates with known-ish PlotType prefixes
    known_prefixes = ("rectilin", "irregular", "TL", "Shd", "Coh", "Inc")
    candidates.sort(
        key=lambda t: (
            0 if any(t[2].startswith(p) for p in known_prefixes) else 1,
            t[1],  # smaller record size tends to be right
        )
    )
    i4, rec_bytes, plot = candidates[0]
    return i4, rec_bytes


def read_shd_bellhop(filename: str, xs_km: Optional[float] = None, ys_km: Optional[float] = None
                     ) -> Tuple[np.ndarray, ShdGeometry]:
    """
    Robust reader for BELLHOP .shd written as DIRECT, UNFORMATTED.
    Auto-detects endian + RECL units.
    """
    i4, rec_bytes = _infer_layout(filename)

    f8 = np.dtype("<f8")  # we'll interpret float64 little by default; we’ll override if needed
    f4 = np.dtype("<f4")

    # For safety: float endian should match int endian in these files.
    # If int is big-endian, make floats big-endian too.
    if i4.byteorder == ">":
        f8 = np.dtype(">f8")
        f4 = np.dtype(">f4")

    def read_rec(f, recno_1based: int) -> bytes:
        off = (recno_1based - 1) * rec_bytes
        f.seek(off, 0)
        b = f.read(rec_bytes)
        if len(b) != rec_bytes:
            raise EOFError(f"Truncated file: could not read record {recno_1based}.")
        return b

    with open(filename, "rb") as f:
        b1 = read_rec(f, 1)
        LRecl_check = int(np.frombuffer(b1[0:4], dtype=i4, count=1)[0])
        title = _decode_fixed(b1[4:4+80])

        b2 = read_rec(f, 2)
        plot_type = _decode_fixed(b2[0:10])

        b3 = read_rec(f, 3)
        ints = np.frombuffer(b3[0:28], dtype=i4, count=7).astype(int)
        Nfreq, Ntheta, NSx, NSy, NSz, NRz, NRr = ints.tolist()
        freq0 = float(np.frombuffer(b3[28:36], dtype=f8, count=1)[0])
        atten = float(np.frombuffer(b3[36:44], dtype=f8, count=1)[0])

        # Now the arrays.
        # In most AT/BELLHOP builds: freq/theta/sx/sy/rr are float64; sz/rz are float32.
        b4 = read_rec(f, 4)
        freq = np.frombuffer(b4, dtype=f8, count=Nfreq)

        b5 = read_rec(f, 5)
        theta = np.frombuffer(b5, dtype=f8, count=Ntheta)

        b6 = read_rec(f, 6)
        b7 = read_rec(f, 7)
        if not plot_type.startswith("TL"):
            sx = np.frombuffer(b6, dtype=f8, count=NSx)
            sy = np.frombuffer(b7, dtype=f8, count=NSy)
        else:
            sx_ends = np.frombuffer(b6, dtype=f8, count=2)
            sy_ends = np.frombuffer(b7, dtype=f8, count=2)
            sx = np.linspace(sx_ends[0], sx_ends[1], NSx, dtype=f8)
            sy = np.linspace(sy_ends[0], sy_ends[1], NSy, dtype=f8)

        b8 = read_rec(f, 8)
        sz = np.frombuffer(b8, dtype=f4, count=NSz)

        b9 = read_rec(f, 9)
        rz = np.frombuffer(b9, dtype=f4, count=NRz)

        b10 = read_rec(f, 10)
        rr = np.frombuffer(b10, dtype=f8, count=NRr)

        # Receiver layout
        if plot_type.strip() == "rectilin":
            Nrcvrs_per_range = NRz
        elif plot_type.strip() == "irregular":
            Nrcvrs_per_range = 1
        else:
            Nrcvrs_per_range = NRz

        # Select nearest source location
        if xs_km is None or ys_km is None:
            idxX, idxY = 0, 0
        else:
            idxX = int(np.argmin(np.abs(sx - xs_km * 1000.0)))
            idxY = int(np.argmin(np.abs(sy - ys_km * 1000.0)))

        # Pressure records are complex stored as 2*float32 (real, imag)
        pressure = np.zeros((Ntheta, NSz, Nrcvrs_per_range, NRr), dtype=np.complex64)

        def read_pressure_record(recno_1based: int) -> np.ndarray:
            b = read_rec(f, recno_1based)
            tmp = np.frombuffer(b, dtype=f4, count=2 * NRr)
            if tmp.size != 2 * NRr:
                raise EOFError(f"Unexpected EOF inside pressure record {recno_1based}")
            return (tmp[0::2] + 1j * tmp[1::2]).astype(np.complex64, copy=False)

        for itheta in range(Ntheta):
            for isz in range(NSz):
                for irz in range(Nrcvrs_per_range):
                    recno = (
                        11  # pressure starts at record 11
                        + idxX * NSy * Ntheta * NSz * Nrcvrs_per_range
                        + idxY * Ntheta * NSz * Nrcvrs_per_range
                        + itheta * NSz * Nrcvrs_per_range
                        + isz * Nrcvrs_per_range
                        + irz
                    )
                    pressure[itheta, isz, irz, :] = read_pressure_record(recno)

    geom = ShdGeometry(
        title=title,
        plot_type=plot_type,
        atten=atten,
        freq0=freq0,
        freq=freq.copy(),
        theta=theta.copy(),
        sx=sx.copy(),
        sy=sy.copy(),
        sz=sz.copy(),
        rz=rz.copy(),
        rr=rr.copy(),
        dims=dict(Nfreq=Nfreq, Ntheta=Ntheta, NSx=NSx, NSy=NSy, NSz=NSz, NRz=NRz, NRr=NRr),
    )
    return pressure, geom


## Example:
#p, g = read_shd_bellhop("tmpuxwak67x.shd")
#print(g.title, g.plot_type, g.dims)
