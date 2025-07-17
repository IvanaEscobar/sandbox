import re
import numpy as np
import xarray as xr
from pathlib import Path


class _PRTDataStore:
    def __init__(self, source_depth=None, receiver_depth=None, receiver_range=None, soundspeed_table=None):
        self.source_depth = source_depth
        self.receiver_depth = receiver_depth
        self.receiver_range = receiver_range
        self.soundspeed_table = soundspeed_table

    def __repr__(self):
        return (
            f"_PRTDataStore(\n"
            f"  source_depth={self.source_depth},\n"
            f"  receiver_depth={self.receiver_depth},\n"
            f"  receiver_range={self.receiver_range},\n"
            f"  soundspeed_table=\n{self.soundspeed_table}\n)"
        )


def open_prt(fname):
    fname = Path(fname)
    if not fname.exists():
        raise FileNotFoundError(f"File not found: {fname}")

    # Patterns to match header lines
    source_re   = re.compile(r"Source\s+depths,\s+Sz\s*\(m\)", re.IGNORECASE)
    rcvz_re     = re.compile(r"Receiver\s+depths,\s+Rz\s*\(m\)", re.IGNORECASE)
    rcvr_re     = re.compile(r"Receiver\s+ranges,\s+Rr\s*\(km\)", re.IGNORECASE)
    ssmatrix_re = re.compile(r"Depth\s*\[\s*m\s*\]\s+Soundspeed\s*\[\s*m/s\s*\]", re.IGNORECASE)

    # Pattern to extract a single float after the PID prefix
    float_after_pid = re.compile(r"\)\s*([+-]?\d+(?:\.\d*)?)")

    # Storage
    source_depth = None
    receiver_depth = None
    receiver_range = None
    ss_data = []
    in_ss_block = False

    with open(fname, "r") as f:
        for line in f:
            # Scalar depth values
            if source_re.search(line):
                m = float_after_pid.search(next(f).strip())
                if m:
                    source_depth = float(m.group(1))
                continue

            if rcvz_re.search(line):
                m = float_after_pid.search(next(f).strip())
                if m:
                    receiver_depth = float(m.group(1))
                continue

            if rcvr_re.search(line):
                m = float_after_pid.search(next(f).strip())
                if m:
                    receiver_range = float(m.group(1))
                continue

            # Sound speed table
            if ssmatrix_re.search(line):
                in_ss_block = True
                continue

            if in_ss_block:
                if line.strip() == "" or not re.search(r"\d", line):
                    in_ss_block = False  # end of table
                    continue
                try:
                    values = list(map(float, line.strip().split()))
                    if len(values) >= 2:
                        ss_data.append(values[:2])
                except ValueError:
                    continue

    # Convert sound speed table to xarray Dataset
    soundspeed_table = None
    if ss_data:
        ss_array = np.array(ss_data)
        soundspeed_table = xr.Dataset(
            {"sound_speed": (["depth"], ss_array[:, 1])},
            coords={"depth": ss_array[:, 0]}
        )

    return _PRTDataStore(
        source_depth=source_depth,
        receiver_depth=receiver_depth,
        receiver_range=receiver_range,
        soundspeed_table=soundspeed_table,
    )
