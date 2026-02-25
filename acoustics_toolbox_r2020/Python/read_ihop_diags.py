from __future__ import annotations

import re
from pathlib import Path
from typing import Iterable, Union, Sequence

import numpy as np
import xarray as xr


_PID_PREFIX = re.compile(r"^\(PID\.TID[^)]*\)\s*")


def _strip_pid_prefix(line: str) -> str:
    """Remove '(PID.TID ....) ' prefix that iHOP writes on every line."""
    return _PID_PREFIX.sub("", line).strip()


def _parse_float(s: str) -> float:
    return float(s.strip().split()[0])


def _parse_int(s: str) -> int:
    return int(s.strip().split()[0])
    

def _iter_from_filename(path: Union[str, Path]) -> int:
    """
    Extract iteration integer from filename like:
        'baroc.0002592000.prt'
        'baroc.0002592000.delay'
    -> 2592000
    Falls back to last integer group in the name.
    """
    name = Path(path).name
    m = re.search(r"\.(\d+)\.(?:prt|delay)$", name)
    if m:
        return int(m.group(1))
    m = re.findall(r"\d+", name)
    if not m:
        raise ValueError(f"Could not find an iteration integer in filename: {name}")
    return int(m[-1])
    

def _read_alpha_from_delay_header(path: Union[str, Path], header_lines: int = 7) -> float:
    """
    .delay format:
      - header of 7 lines
      - next line: launch angle alpha (float)
      - next line: length / shape info (ignored here)
    """
    path = Path(path)
    with path.open("r", errors="replace") as f:
        for _ in range(header_lines):
            f.readline()
        alpha_line = f.readline()
    if not alpha_line:
        raise ValueError(f"{path}: file ended before alpha line.")
    return float(alpha_line.strip().split()[0])


def _read_last_numeric_pair(path: Union[str, Path], chunk_size: int = 1_000_000) -> tuple[float, float]:
    """
    Efficiently find the last numeric (delay, depth) line in a potentially huge ASCII file
    by reading from the end in chunks.
    """
    path = Path(path)

    def parse_pair(line: str) -> tuple[float, float] | None:
        toks = line.strip().split()
        if len(toks) < 2:
            return None
        try:
            a = float(toks[0])
            b = float(toks[1])
            return a, b
        except ValueError:
            return None

    with path.open("rb") as f:
        f.seek(0, 2)
        file_size = f.tell()
        if file_size == 0:
            raise ValueError(f"{path}: empty file.")

        # Pull chunks from end until we can parse a numeric pair.
        offset = 0
        carry = b""
        while offset < file_size:
            read_size = min(chunk_size, file_size - offset)
            offset += read_size
            f.seek(file_size - offset)
            buf = f.read(read_size) + carry

            # Split into lines; keep the first partial line as carry for next iteration
            lines = buf.splitlines()
            if not lines:
                carry = buf
                continue

            carry = lines[0]  # possible partial at the beginning
            # scan backwards for last parseable numeric pair
            for raw in reversed(lines[1:]):  # skip potential partial
                try:
                    line = raw.decode("utf-8", errors="replace")
                except Exception:
                    continue
                pair = parse_pair(line)
                if pair is not None:
                    return pair

        raise ValueError(f"{path}: could not find a final numeric (delay, depth) pair.")


def _parse_prt_sound_speed(path: Union[str, Path], range_units: str = "m") -> xr.Dataset:
    """
    Parse a single .prt file and return Dataset with one time entry.

    Parameters
    ----------
    path : str | Path
        Path to a .prt file.
    range_units : {"m","km"}
        Unit to store the range coordinate in. The .prt prints ranges in km.

    Returns
    -------
    xr.Dataset
        Dataset with variable 'c' and dims ('time','depth','range').
    """
    path = Path(path)
    lines_raw = path.read_text(errors="replace").splitlines()
    lines = [_strip_pid_prefix(L) for L in lines_raw]

    # --- Find SSP counts
    nR = nZ = None
    for L in lines:
        if "Number of SSP ranges" in L:
            nR = int(L.split("=")[-1].strip())
        if "Number of SSP depths" in L:
            nZ = int(L.split("=")[-1].strip())
        if nR is not None and nZ is not None:
            break
    if nR is None or nZ is None:
        raise ValueError(f"{path}: Could not find 'Number of SSP ranges/depths'.")

    # --- Find profile ranges block
    idx_ranges_hdr = None
    for i, L in enumerate(lines):
        if L.startswith("Profile ranges"):
            idx_ranges_hdr = i
            break
    if idx_ranges_hdr is None:
        raise ValueError(f"{path}: Could not find 'Profile ranges [km]:' block.")

    ranges_km: list[float] = []
    i = idx_ranges_hdr + 1
    while i < len(lines):
        L = lines[i].strip()
        if L == "" or "Sound speed matrix" in L or L.startswith("____"):
            break
        # collect floats from this line
        toks = L.split()
        for t in toks:
            try:
                ranges_km.append(float(t))
            except ValueError:
                pass
        i += 1

    if len(ranges_km) < nR:
        raise ValueError(
            f"{path}: Parsed {len(ranges_km)} ranges but expected nR={nR}. "
            f"Ranges parsed: {ranges_km}"
        )
    ranges_km = ranges_km[:nR]

    # --- Find the sound speed matrix block
    idx_mat_hdr = None
    for i, L in enumerate(lines):
        if L.startswith("Sound speed matrix"):
            idx_mat_hdr = i
            break
    if idx_mat_hdr is None:
        raise ValueError(f"{path}: Could not find 'Sound speed matrix:' header.")

    # The next line is usually "Depth [m ]  Soundspeed [m/s]"
    # Data starts after that.
    data_start = idx_mat_hdr + 2

    depths = []
    C = []
    i = data_start
    while i < len(lines) and len(depths) < nZ:
        L = lines[i].strip()
        if L == "" or L.startswith("____"):
            break
        toks = L.split()
        # expect: depth + nR speed values
        if len(toks) >= 1 + nR:
            try:
                z = float(toks[0])
                row = [float(x) for x in toks[1 : 1 + nR]]
            except ValueError:
                i += 1
                continue
            depths.append(z)
            C.append(row)
        i += 1

    if len(depths) != nZ or len(C) != nZ:
        raise ValueError(
            f"{path}: Parsed matrix size ({len(depths)} depths) but expected nZ={nZ}."
        )

    depths = np.asarray(depths, dtype=float)
    C = np.asarray(C, dtype=float)  # shape (nZ, nR)

    # --- Range units
    ranges_km = np.asarray(ranges_km, dtype=float)
    if range_units == "m":
        ranges = ranges_km * 1000.0
        range_attrs = {"units": "m", "long_name": "range"}
    elif range_units == "km":
        ranges = ranges_km
        range_attrs = {"units": "km", "long_name": "range"}
    else:
        raise ValueError("range_units must be 'm' or 'km'.")

    it = _iter_from_filename(path)

    da = xr.DataArray(
        C[None, :, :],
        dims=("time", "depth", "range"),
        coords={
            "time": np.asarray([it], dtype=int),
            "depth": depths,
            "range": ranges,
        },
        name="c",
        attrs={"long_name": "sound speed", "units": "m/s", "source_file": path.name},
    )
    da["depth"].attrs.update({"units": "m", "positive": "down", "long_name": "depth"})
    da["range"].attrs.update(range_attrs)

    return xr.Dataset({"c": da})


def load_prt_sound_speed(
    paths: Union[str, Path, Sequence[Union[str, Path]]],
    *,
    range_units: str = "m",
) -> xr.Dataset:
    """
    Load one or many .prt files and stack into a time-indexed Dataset.

    Parameters
    ----------
    paths : str | Path | sequence
        - glob pattern (e.g., "baroc.*.prt") OR
        - directory Path OR
        - list of explicit files
    range_units : {"m","km"}
        Unit to store the range coordinate in.

    Returns
    -------
    xr.Dataset
        Dataset with variable 'c' and dims ('time','depth','range'),
        sorted by time (iteration).
    """
    if isinstance(paths, (str, Path)):
        p = Path(paths)
        if p.is_dir():
            files = sorted(p.glob("*.prt"))
        else:
            # treat as glob if it contains wildcard, otherwise single file
            s = str(paths)
            files = sorted(Path().glob(s)) if any(ch in s for ch in "*?[]") else [p]
    else:
        files = [Path(f) for f in paths]

    if not files:
        raise FileNotFoundError(f"No .prt files found for: {paths}")

    dsets = [_parse_prt_sound_speed(f, range_units=range_units) for f in files]
    ds = xr.concat(dsets, dim="time").sortby("time")

    return ds

def read_delay_timefile(
    path: Union[str, Path],
    *,
    header_lines: int = 7,
    alpha_min: float = -20.0,
    alpha_max: float = 20.0,
    alpha_step: float = 0.1,
    alpha_round_decimals: int = 1,
) -> xr.Dataset:
    """
    Read ONE .delay file (ONE time) that contains many launch-angle blocks.
    For each alpha block, keep only the final (delay, depth) pair.

    Returns
    -------
    xr.Dataset
        delay(time, alpha), depth(time, alpha)
        where alpha is a uniform grid and missing alphas are NaN.
    """
    path = Path(path)
    it = _iter_from_filename(path)

    # --- uniform alpha grid (inclusive ends)
    n_alpha = int(round((alpha_max - alpha_min) / alpha_step)) + 1
    alpha_grid = alpha_min + alpha_step * np.arange(n_alpha, dtype=float)
    alpha_grid = np.round(alpha_grid, alpha_round_decimals)

    delay = np.full((n_alpha,), np.nan, dtype=float)
    depth = np.full((n_alpha,), np.nan, dtype=float)

    # map alpha -> index for fast insertion
    alpha_to_idx = {a: i for i, a in enumerate(alpha_grid)}

    with path.open("r", errors="replace") as f:
        # skip header
        for _ in range(header_lines):
            f.readline()

        while True:
            # read alpha line (skip blank lines)
            alpha_line = f.readline()
            if not alpha_line:
                break  # EOF

            alpha_line = alpha_line.strip()
            if alpha_line == "":
                continue

            # alpha for this block (rounded to 0.1 deg)
            a = round(_parse_float(alpha_line), alpha_round_decimals)

            # read n line
            n_line = f.readline()
            if not n_line:
                break
            n = _parse_int(n_line)
            if n <= 0:
                # no pairs: store NaN and continue
                continue

            # read n rows; keep last numeric pair
            last_delay = np.nan
            last_depth = np.nan
            for _ in range(n):
                row = f.readline()
                if not row:
                    break
                toks = row.strip().split()
                if len(toks) < 2:
                    continue
                try:
                    last_delay = float(toks[0])
                    last_depth = float(toks[1])
                except ValueError:
                    continue

            # place into arrays if alpha is in our grid
            idx = alpha_to_idx.get(a, None)
            if idx is not None:
                delay[idx] = last_delay
                depth[idx] = last_depth
            # else: alpha outside expected grid, ignore silently

    ds = xr.Dataset(
        data_vars=dict(
            delay=(("time", "alpha"), delay[None, :], {"long_name": "final delay", "units": "s"}),
            depth=(("time", "alpha"), depth[None, :], {"long_name": "final depth", "units": "m", "positive": "down"}),
        ),
        coords=dict(
            time=np.asarray([it], dtype=int),
            alpha=alpha_grid,
        ),
        attrs=dict(source_file=path.name),
    )
    ds["time"].attrs.update({"long_name": "iteration"})
    ds["alpha"].attrs.update({"long_name": "launch angle", "units": "deg"})
    return ds


def load_delay_timeseries(
    paths: Union[str, Path, Sequence[Union[str, Path]]],
    **kwargs,
) -> xr.Dataset:
    """
    Load many .delay files (one per time) and concat into delay(time, alpha), depth(time, alpha).
    kwargs are passed to read_delay_timefile (e.g., alpha_min/max/step).
    """
    if isinstance(paths, (str, Path)):
        p = Path(paths)
        if p.is_dir():
            files = sorted(p.glob("*.delay"))
        else:
            s = str(paths)
            files = sorted(Path().glob(s)) if any(ch in s for ch in "*?[]") else [p]
    else:
        files = [Path(f) for f in paths]

    if not files:
        raise FileNotFoundError(f"No .delay files found for: {paths}")

    dsets = [read_delay_timefile(f, **kwargs) for f in files]
    ds = xr.concat(dsets, dim="time").sortby("time")
    return ds