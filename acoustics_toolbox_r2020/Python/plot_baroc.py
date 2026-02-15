from matplotlib.collections import LineCollection
import numpy as np
import matplotlib.pyplot as plt
import sandbox as sb
import cmocean.cm as cm

def plot_eigenrays(
    erays,
    ranges_km,
    ax=None,
    plot_alpha=False,
    alpha_clip=(0.0, 1.0),
    highlight_threshold=0.76,
    xlim=(0, 2580),
    ylim=(1800, 0),
    source_depth=73,
    receiver_depth=300,
    receiver_range=2542,  # km
    savepath=None,
    dpi=180,
    transparent=False,
):
    """
    Plot eigenray geometry. Optionally use per-ray alpha (transparency) from erays['alpha'].

    Required columns in erays:
      - 'ray'            : list/series of (N, 2) numpy arrays [range_m, depth_m]
      - 'bottom_bounces' : integer count per ray
    Optional (if plot_alpha=True):
      - 'alpha'          : float per ray used as transparency
    """

    # --- axes / figure
    if ax is None:
        fig, ax = plt.subplots(figsize=(10, 4))
    else:
        fig = ax.figure

    # --- background styling
    ax.set_facecolor(sb.utocean)
    fig.patch.set_facecolor("white")

    # --- prepare segments (convert range m -> km)
    segments = [ray.copy() for ray in erays["ray"].to_list()]
    for seg in segments:
        seg[:, 0] *= 1e-3

    # --- optional alpha array
    alphas = None
    if plot_alpha:
        alphas = np.clip(erays["alpha"].to_numpy(), *alpha_clip)

    # --- masks for draw order
    bottom_bounces = erays["bottom_bounces"].to_numpy()
    mask_bottom = bottom_bounces > 0
    mask_direct = ~mask_bottom  # includes 0 bounces

    # --- helper: add a line collection with optional alpha
    def add_rays(segs, a=None, *, color="silver", lw=0.03, zorder=1):
        if not segs:
            return
        lc = LineCollection(segs, colors=color, linewidths=lw, zorder=zorder)
        if a is not None:
            lc.set_alpha(a)
        ax.add_collection(lc)

    # --- split segments (and alphas if present)
    segs_bottom = [s for s, m in zip(segments, mask_bottom) if m]
    segs_direct = [s for s, m in zip(segments, mask_direct) if m]

    a_bottom = alphas[mask_bottom] if alphas is not None else None
    a_direct = alphas[mask_direct] if alphas is not None else None

    # --- draw: bottom first, then direct on top
    add_rays(segs_bottom, a_bottom, color="silver", lw=0.03, zorder=1)
    add_rays(segs_direct, a_direct, color="silver", lw=0.05, zorder=3)

    # --- optional highlight of strongest rays (always on top)
    if alphas is not None:
        mask_strong = alphas > highlight_threshold
        strong = [s for s, m in zip(segments, mask_strong) if m]
        add_rays(strong, None, color="white", lw=0.3, zorder=5)

    # --- bathymetry fill (solid)
    ax.fill_between(
        ranges_km,
        ylim[0] * np.ones_like(ranges_km),
        y2=ylim[0],
        color=sb.utearth,
        zorder=0,
    )

    # --- source & receiver markers
    ax.scatter(0, source_depth, s=100, color=sb.utyellow, edgecolor="k", zorder=100, label="source")
    ax.scatter(receiver_range, receiver_depth, s=100, color=sb.utorange, edgecolor="k", zorder=100, label="receiver")

    # --- axes formatting
    ax.autoscale()
    ax.invert_yaxis()
    ax.set_xlim(*xlim)
    ax.set_ylim(*ylim)
    ax.set_xlabel("range [km]")
    ax.set_ylabel("depth [m]")

    # --- optional save
    if savepath is not None:
        fig.savefig(savepath, bbox_inches="tight", dpi=dpi, transparent=transparent)

    return ax

def overlay_eigenrays(erays_subset, ax, *, color="white", lw=0.35, alpha=None, zorder=20):
    """Overlay a subset of rays on an existing axes."""
    if erays_subset is None or len(erays_subset) == 0:
        return

    segments = [ray.copy() for ray in erays_subset["ray"].to_list()]
    for seg in segments:
        seg[:, 0] *= 1e-3  # m -> km

    lc = LineCollection(segments, colors=color, linewidths=lw, zorder=zorder)
    if alpha is not None:
        lc.set_alpha(alpha)
    ax.add_collection(lc)

def plot_traveltime_vs_arrivalangle(
    arr,
    date,
    colorby="amp",
    alpha=None,
    ax=None,
    vmin=None,
    vmax=None,
    xlim=(1699, 1826),
    ylim=(-21, 21),
    savepath=None,
    dpi=180,
    transparent=True,
    add_colorbar=True,
):
    """
    Scatter of travel time vs arrival angle, colored by a chosen quantity.

    Parameters
    ----------
    arr : pandas.DataFrame (or dict-like)
        Must contain: 'time_of_arrival', 'angle_of_arrival', and the chosen colorby column.
    date : str
        String used in title.
    colorby : {'angle','amp','ampdB',None}
        Quantity used to color points. If None/False, points are black with no colorbar.
    ax : matplotlib.axes.Axes, optional
        If provided, plot into this axes (lets the caller control figsize/layout).
    vmin, vmax : float, optional
        If provided, override default limits for the colormap.
    xlim, ylim : tuple
        Axis limits.
    savepath : str or Path, optional
        If provided, saves figure to this path.
    """

    # --- choose axes / figure ownership
    created_ax = ax is None
    if created_ax:
        fig, ax = plt.subplots()
    else:
        fig = ax.figure

    # --- map colorby -> (column, cmap, cbar label, default vmin/vmax)
    styles = {
        "angle": dict(col="angle_of_departure", cmap="twilight",
                      label=r"declination angle [$^\circ$]", vmin=None, vmax=None),
        "amp":   dict(col="amp_magnitude", cmap=cm.amp,
                      label="amplitude", vmin=0, vmax=5e-6),
        "ampdB": dict(col="amp_dB", cmap=cm.amp,
                      label="amplitude [dB]",vmin=None, vmax=None),# vmin=-175, vmax=-120),
        "surface bounce": dict(col="surface_bounces", cmap="copper",
                               label="surface bounces",vmin=0,vmax=None),
        "bottom bounce": dict(col="bottom_bounces", cmap="copper",
                               label="bottom bounces",vmin=0,vmax=None),
        None:    dict(col=None, cmap=None, label=None, vmin=None, vmax=None),
        False:   dict(col=None, cmap=None, label=None, vmin=None, vmax=None),
    }
    if colorby not in styles:
        raise ValueError(f"colorby must be one of {list(styles.keys())}, got {colorby!r}")

    st = styles[colorby]

    # --- resolve vmin/vmax: only use defaults if user didn't pass them
    use_vmin = st["vmin"] if vmin is None else vmin
    use_vmax = st["vmax"] if vmax is None else vmax

    # --- scatter
    if st["col"] is None:
        sc = ax.scatter(arr["time_of_arrival"], arr["angle_of_arrival"], c="k")
    else:
        sc = ax.scatter(
            arr["time_of_arrival"],
            arr["angle_of_arrival"],
            c=arr[st["col"]],
            cmap=st["cmap"],
            vmin=use_vmin,
            vmax=use_vmax,
            alpha=alpha,
        )

    # --- optional colorbar
    if st["col"] is not None and add_colorbar:
        cbar = fig.colorbar(sc, ax=ax, orientation='horizontal', pad=0.15)
        cbar.set_label(st["label"], fontsize=12)
        cbar.ax.tick_params(labelsize=12) 

    # --- labels, limits, grid
    ax.set_title(f"BCG: {len(arr)} eigenray arrivals at {date}")
    ax.set_xlabel("travel time [sec]")
    ax.set_ylabel(r"arrival angle [$^\circ$]")
    ax.set_xlim(*xlim)
    ax.set_ylim(*ylim)
    ax.set_axisbelow(True)
    ax.grid(True, alpha=0.2)

    # --- optional save (does NOT force show/close)
    if savepath is not None:
        fig.savefig(savepath, bbox_inches="tight", dpi=dpi, transparent=transparent)

    return ax