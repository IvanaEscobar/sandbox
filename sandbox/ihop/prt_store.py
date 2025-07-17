import re
from numpy import array as _array
from xarray import Dataset as _Dataset
from pathlib import Path as _Path


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


def open_prt(fname):
    fname = _Path(fname)
    if not fname.exists():
        raise FileNotFoundError(f"File not found: {fname}")

    # Patterns to match header lines
    source_re   = re.compile(r"Source\s+depths,\s+Sz\s*\(m\)", re.IGNORECASE)
    rcvZ_re     = re.compile(r"Receiver\s+depths,\s+Rz\s*\(m\)", re.IGNORECASE)
    rcvR_re     = re.compile(r"Receiver\s+ranges,\s+Rr\s*\(km\)", re.IGNORECASE)
    sound_ranges_re = re.compile(r"Profile\s+ranges\s*\[km\]", re.IGNORECASE)
    sound_table_re = re.compile(r"Depth\s*\[\s*m\s*\]\s+Soundspeed\s*\[\s*m/s\s*\]", re.IGNORECASE)

    # Pattern to extract a single float after the PID prefix
    pid_tid_re=re.compile(r"\(PID\.TID\s+\d{4}\.\d{4}\)\s+(.*)")
    array_pattern=re.compile(r"[+-]?\d+(?:\.\d+)?(?:[eE][+-]?\d+)?")

    # Storage
    source_depth = None
    receiver_depth = None
    receiver_range = None
    sound_ranges = None
    ss_data = []
    ss_depths = []
    in_ss_block = False

    with open(fname, "r") as f:
        for line in f:
            # scalar depth values
            if source_re.search(line):
                m = pid_tid_re.match( next(f).strip() )
                if m:
                    source_depth = float(m.group(1))
                continue

            if rcvZ_re.search(line):
                m = pid_tid_re.match( next(f).strip() )
                if m:
                    receiver_depth = float(m.group(1))
                continue

            if rcvR_re.search(line):
                m = pid_tid_re.match( next(f).strip() )
                if m:
                    receiver_range = float(m.group(1))
                continue

            # 1d array values
            if sound_ranges_re.search(line):
                next_line = next(f).strip()
                m = pid_tid_re.match(next_line)
                if m:
                    vals = array_pattern.findall(m.group(1))
                    sound_ranges = _array([float(v) for v in vals])
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
