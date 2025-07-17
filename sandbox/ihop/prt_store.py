import re
from numpy import array as _array
from xarray import Dataset as _Dataset
from pathlib import Path as _Path

# regex patterns
from .meta import pid_tid_re, array_pattern, source_re, rcvZ_re, rcvR_re,\
                  sound_ranges_re, sound_table_re

def open_prt(fname):
    fname = _Path(fname)
    if not fname.exists():
        raise FileNotFoundError(f"File not found: {fname}")

    # Storage
    source_depth = None
    receiver_depth = None
    receiver_range = None
    sound_ranges = None
    ss_data = []
    ss_depths = []
    in_ss_block = False

    # read through file
    with open(fname, "r") as f:
        for line in f:
            # scalar values
            if source_re.search(line):
                source_depth = _get_scalar(f)
                continue

            if rcvZ_re.search(line):
                receiver_depth = _get_scalar(f) 
                continue

            if rcvR_re.search(line):
                receiver_range = _get_scalar(f) 
                continue

            # 1d array values
            if sound_ranges_re.search(line):
                sound_ranges = _get_array(f) 
                continue

            # 2d array values
            if sound_table_re.search(line):
                in_ss_block = True
                continue

            if in_ss_block:
                if re.search(r"_+", line):  # end of table
                    in_ss_block = False
                    continue
                try:
                    m = pid_tid_re.match(line.strip())
                    if m:
                        fields = array_pattern.findall(m.group(1))
                        if len(fields) >= 2:
                            ss_depths.append(float(fields[0]))
                            ss_data.append(fields[1:])
                except ValueError:
                    continue

    # Convert sound speed table to xarray Dataset
    soundspeed_table = None
    if ss_data:
        ss_array = _array(ss_data, dtype=float)
        ss_depths = _array(ss_depths)
        soundspeed_table = _Dataset(
            {"sound_speed": (["depth","range"], ss_array)},
            coords={"depth": ss_depths, "range":sound_ranges}
        )

    return _PRTDataStore(
        source_depth=source_depth,
        receiver_depth=receiver_depth,
        receiver_range=receiver_range,
        sound_ranges=sound_ranges,
        soundspeed_table=soundspeed_table,
    )


class _PRTDataStore:
    def __init__(self, source_depth=None, 
                 receiver_depth=None, 
                 receiver_range=None, 
                 sound_ranges=None,
                 soundspeed_table=None):

        self.source_depth = source_depth
        self.receiver_depth = receiver_depth
        self.receiver_range = receiver_range
        self.sound_ranges = sound_ranges
        self.soundspeed_table = soundspeed_table

    def __repr__(self):
        return (
            f" Class: PRTDataStore(\n"
            f"  source_depth={self.source_depth},\n"
            f"  receiver_depth={self.receiver_depth},\n"
            f"  receiver_range={self.receiver_range},\n"
            f"  sound_ranges={self.sound_ranges},\n"
            f"  soundspeed_table=\n{self.soundspeed_table}\n)"
        )

def _get_scalar(f):
    m = pid_tid_re.match( next(f).strip() )
    if m:
        return float(m.group(1))
    return None

def _get_array(f):
    next_line = next(f).strip()
    m = pid_tid_re.match(next_line)
    if m:
        vals = array_pattern.findall(m.group(1))
        return _array([float(v) for v in vals])
    return None
