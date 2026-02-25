#!/usr/bin/env python3
import re
import numpy as np
import matplotlib.pyplot as plt


# -------------------------
# Parse *.env (sound speed)
# -------------------------
def read_env_ssp(env_file):
    """
    Extract (z, c) from a BELLHOP-style .env print file.
    Assumes the SSP table starts after the line containing 'DEPTH of bottom'
    and ends when a quoted keyword block begins (e.g., 'A', etc.).
    """
    with open(env_file, "r", errors="replace") as f:
        lines = f.read().splitlines()

    ssp = []
    started = False
    for line in lines:
        if not started:
            if "DEPTH of bottom" in line:
                started = True
            continue

        s = line.strip()
        # SSP section ends when the next quoted block begins (e.g. "'A' ...")
        if s.startswith("'"):
            break

        # typical SSP lines look like: "  200.0  1530.29  /"
        parts = s.replace("/", " ").split()
        if len(parts) >= 2:
            try:
                z = float(parts[0])
                c = float(parts[1])
                ssp.append((z, c))
            except ValueError:
                pass

    ssp = np.array(ssp, dtype=float)
    if ssp.size == 0:
        raise ValueError(f"Could not parse SSP from {env_file}")
    return ssp[:, 0], ssp[:, 1]


# -------------------------
# Parse *.ray (ray paths)
# -------------------------
_FLOAT_ONLY = re.compile(r"^\s*[-+]?\d+(\.\d*)?([Ee][+-]?\d+)?\s*$")
_INTS3 = re.compile(r"^\s*\d+\s+\d+\s+\d+\s*$")
_TWO_FLOATS = re.compile(
    r"^\s*[-+]?\d+(\.\d*)?([Ee][+-]?\d+)?\s+[-+]?\d+(\.\d*)?([Ee][+-]?\d+)?\s*$"
)

def read_ray_ascii(ray_file):
    """
    Read a BELLHOP ASCII .ray file written as repeated blocks:
        <angle>
        <npts> <...> <...>
        <r0> <z0>
        <r1> <z1>
        ...
    Where npts typically counts the (r0,z0) line plus the subsequent points.

    Returns:
        rays: list of dicts with keys: angle_deg, r_m (1D), z_m (1D)
    """
    with open(ray_file, "r", errors="replace") as f:
        lines = f.read().splitlines()

    # Find the first "angle block": a float-only line followed by a 3-int line
    # (skip the frequency block which is also float-only + ints)
    candidates = []
    for i in range(len(lines) - 1):
        if _FLOAT_ONLY.match(lines[i]) and _INTS3.match(lines[i + 1]):
            val = float(lines[i])
            if abs(val) <= 90 and val != 50.0:  # simple heuristic
                candidates.append(i)

    if not candidates:
        raise ValueError(f"Could not find ray blocks in {ray_file}")

    i = candidates[0]
    rays = []

    while i < len(lines) - 1:
        if not (_FLOAT_ONLY.match(lines[i]) and _INTS3.match(lines[i + 1])):
            i += 1
            continue

        angle = float(lines[i])
        npts = int(lines[i + 1].split()[0])

        # Find first coordinate line after the int line
        j = i + 2
        while j < len(lines) and not _TWO_FLOATS.match(lines[j]):
            j += 1
        if j >= len(lines):
            break

        # First coordinate line is usually included in npts
        r0, z0 = map(float, lines[j].split()[:2])
        rs = [r0]
        zs = [z0]

        # Remaining points: npts - 1
        j += 1
        for _ in range(npts - 1):
            if j >= len(lines):
                break
            parts = lines[j].split()
            if len(parts) < 2:
                break
            try:
                r = float(parts[0])
                z = float(parts[1])
            except ValueError:
                break
            rs.append(r)
            zs.append(z)
            j += 1

        rays.append({"angle_deg": angle, "r_m": np.array(rs), "z_m": np.array(zs)})

        # Continue scanning from where we stopped
        i = j

    return rays

import matplotlib as mpl
import sandbox as sb

mpl.rcParams.update({
    "axes.labelsize": 18,
    "axes.titlesize": 18,
    "xtick.labelsize": 16,
    "ytick.labelsize": 16,
    "legend.fontsize": 12,
})
# -------------------------
# Plot: SSP + rays subplots
# -------------------------
env_file = "MunkB_ray.env"
ray_file = "MunkB_ray.ray"
Z = 1600

z, c = read_env_ssp(env_file)
rays = read_ray_ascii(ray_file)

fig, (ax_ssp, ax_ray) = plt.subplots(
    1, 2,
    figsize=(8, 5),
    gridspec_kw={"width_ratios": [1, 4]},  # SSP : Rays
    constrained_layout=True
)
SLD = 150.0

ax_ssp.axhline(
    SLD,
    color="k",
    linestyle="--",
    linewidth=1,
    alpha=0.7
)

# --- Sound speed profile (narrow)
ax_ssp.plot(c, z, color="k", lw=2)
ax_ssp.invert_yaxis()
ax_ssp.set_xlabel("sound speed [m/s]")
ax_ssp.set_ylabel("depth [m]")

ax_ssp.set_ylim(Z,0)

# --- Ray trace (wide)
for ray in rays:
    if ray["z_m"][0] < 200:
        c = sb.utblue
    else:
        c = sb.utorange
    if ray["angle_deg"] < 0 and ray["angle_deg"] > -2:
        ax_ray.plot(ray["r_m"]/1000, ray["z_m"], lw=3, c=c, zorder=1000)
    elif ray["angle_deg"] < 13 and ray["angle_deg"] > -13:
        ax_ray.plot(ray["r_m"]/1000, ray["z_m"], lw=0.2, c=c)

ax_ray.axhline(
    SLD,
    color="k",
    linestyle="--",
    linewidth=1,
    alpha=0.7
)
ax_ray.text(
    0.98, SLD+80,
    "SLD = 150 m",
    ha="right",
    va="bottom",
    fontsize=12,
    fontweight="bold",
    color="k",
    transform=ax_ray.get_yaxis_transform()
)
ax_ray.invert_yaxis()
ax_ray.set_xlabel("range [km]")
ax_ray.text(
    0.5, 0.03,
    r'Ray paths for $\alpha=-1^\circ$',
    transform=ax_ray.transAxes,
    fontsize=14,
    fontweight="bold",
    va="center",
    ha="center"
)

ax_ray.set_ylim(Z,0)
ax_ray.set_xlim(0,100)

plt.show()
