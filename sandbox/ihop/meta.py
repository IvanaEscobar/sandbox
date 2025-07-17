import re

# Pattern to extract a single float after the PID prefix
pid_tid_re=re.compile(r"\(PID\.TID\s+\d{4}\.\d{4}\)\s+(.*)")
array_pattern=re.compile(r"[+-]?\d+(?:\.\d+)?(?:[eE][+-]?\d+)?")

# Patterns to match header lines
source_re   = re.compile(r"Source\s+depths,\s+Sz\s*\(m\)")
rcvZ_re     = re.compile(r"Receiver\s+depths,\s+Rz\s*\(m\)")
rcvR_re     = re.compile(r"Receiver\s+ranges,\s+Rr\s*\(km\)")
sound_ranges_re = re.compile(r"Profile\s+ranges\s*\[km\]")
sound_table_re = re.compile(r"Depth\s*\[\s*m\s*\]\s+Soundspeed\s*\[\s*m/s\s*\]")
