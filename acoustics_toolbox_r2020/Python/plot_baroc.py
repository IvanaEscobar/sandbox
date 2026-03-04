
import numpy as np
import matplotlib as mpl
import matplotlib.pyplot as plt
from matplotlib.collections import LineCollection
from matplotlib.colors import LinearSegmentedColormap, to_rgb
from mpl_toolkits.axes_grid1.inset_locator import inset_axes


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
        float(ylim[0]) * np.ones_like(ranges_km),
        y2=float(ylim[0]),
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



def lighten_color(color, amount=0.5):
    c = np.array(to_rgb(color))
    return tuple(1 - (1 - c) * amount)


def darken_color(color, amount=0.7):
    c = np.array(to_rgb(color))
    return tuple(c * amount)


def stacked_amp_turquoise_cmap_frac(base_cmap, frac, n=256):
    """
    Build a colormap whose split occurs at `frac` (0..1) of the colormap.
    Below split: base_cmap. Above split: turquoise gradient.
    """
    if isinstance(base_cmap, str):
        base = mpl.colormaps[base_cmap]
    else:
        base = base_cmap

    frac = float(np.clip(frac, 0.0, 1.0))

    # build UT blue gradient
    utblue_color = sb.utblue
    utblue_dark  = darken_color(utblue_color, 0.6)
    utblue_light = lighten_color(utblue_color, 0.6)

    utblue_grad = LinearSegmentedColormap.from_list(
        "utblue_grad",
        [utblue_dark, utblue_color, utblue_light]
    )

    # allocate samples proportional to frac
    n0 = max(2, int(round(n * frac)))
    n1 = max(2, n - n0)

    colors = np.vstack([
        base(np.linspace(0, 1, n0)),
        utblue_grad(np.linspace(0, 1, n1)),
    ])

    return LinearSegmentedColormap.from_list("amp_plus_utblue_frac", colors)


def plot_arrivals_timeseries(
    arrs,
    deltaT,
    ax=None,
    cmap="viridis",
    vmin=None,
    vmax=None,
    # overlay options (choose ONE style)
    top_n=None,              # e.g., 10  -> overlay top 10 loudest each timestep
    top_db=None,             # e.g., -121 -> overlay all with amp_dB >= -121 each timestep
    # styling
    s_base=4,
    s_top=10,
    alpha_base=0.6,
    edgecolor_top="k",
    linewidth_top=0.4,
    rasterized=True,
    add_colorbar=True,
    hex_gridsize=220,
    swap_axes=False,
    yearly=True,
    tick_vals=None,
):
    """
    Scatter time series of arrivals: x=years, y=time_of_arrival, colored by amp_dB.
    Overlays top arrivals each timestep (either top_n OR top_db).

    Parameters
    ----------
    arrs : dict[int, pandas.DataFrame]
        Keys are iteration numbers. DataFrames have 'time_of_arrival' and 'amp_dB'.
    deltaT : float
        Timestep size in seconds.
    top_n : int, optional
        Overlay the top N arrivals per timestep (largest amp_dB).
    top_db : float, optional
        Overlay arrivals with amp_dB >= top_db per timestep.
    """

    if (top_n is not None) and (top_db is not None):
        raise ValueError("Choose only one overlay method: top_n OR top_db (not both).")

    if ax is None:
        fig, ax = plt.subplots(figsize=(11, 5))
    else:
        fig = ax.figure

    ax.set_facecolor(sb.utgray)          # plotting area background

    # --- collect into arrays for fast plotting
    xs, ys, cs = [], [], []
    xs_top, ys_top = [], []

    # build iteration list first
    its_all = sorted(arrs.keys())
    
    if yearly:
        sec_per_year = 24 * 3600 * 360  # 360-day model year
        its = [it for it in its_all if (it * deltaT) % sec_per_year == 0]
    else:
        its = its_all
    
    for it in its:
        years = (it * deltaT) / sec_per_year if yearly else (it * deltaT) / (24 * 3600)  # pick what you want
    
        df = arrs.get(it)
        if df is None or df.empty:
            continue


        # base points
        ttoa = df["time_of_arrival"].to_numpy()
        ampdb = df["amp_dB"].to_numpy()

        xs.append(np.full_like(ttoa, years, dtype=float))
        ys.append(ttoa.astype(float))
        cs.append(ampdb.astype(float))

        # overlay selection
        if top_n is not None:
            # top N by amp_dB (largest / least negative)
            idx = np.argsort(ampdb)[-top_n:]
            xs_top.append(np.full(idx.shape, years, dtype=float))
            ys_top.append(ttoa[idx].astype(float))

        elif top_db is not None:
            mask = ampdb >= top_db
            if np.any(mask):
                xs_top.append(np.full(mask.sum(), years, dtype=float))
                ys_top.append(ttoa[mask].astype(float))

    if not xs:
        raise ValueError("No data found in arrs to plot.")

    x = np.concatenate(xs)
    y = np.concatenate(ys)
    c = np.concatenate(cs)

    # --- ensure vmin/vmax are defined and are the colorbar bounds
    if vmin is None:
        vmin = float(np.nanmin(c))
    if vmax is None:
        vmax = float(np.nanmax(c))

    # --- standard normalization (keeps colorbar linear in dB)
    norm = mpl.colors.Normalize(vmin=vmin, vmax=vmax)

    if top_db is not None:
        # split location in *data fraction*, not forced to 0.5
        frac = (float(top_db) - vmin) / (vmax - vmin)
        cmap2 = stacked_amp_turquoise_cmap_frac(base_cmap=cmap, frac=frac, n=256)
    else:
        cmap2 = mpl.colormaps[cmap] if isinstance(cmap, str) else cmap


    # --- background: hexbin colored by amp_dB (not counts)
    if swap_axes:
        hx = y; hy = x
    else:
        hx = x; hy = y

    # Keep the same extent so both hexbin layers align exactly
    extent = (np.nanmin(hx), np.nanmax(hx), np.nanmin(hy), np.nanmax(hy))

    hb = ax.hexbin(
        hx, hy,
        C=c,
        reduce_C_function=np.max,
        gridsize=hex_gridsize,
        mincnt=1,
        cmap=cmap2,
        norm=norm,
        # extent=extent,
        # linewidths=0.0,
        zorder=1,
    )
    if top_db is not None:
        mask_hi = c >= float(top_db)

        # --- bottom layer: below-threshold hexes
        hb_hi = ax.hexbin(
            hx[mask_hi], hy[mask_hi],
            C=c[mask_hi],
            reduce_C_function=np.max,
            gridsize=hex_gridsize,
            mincnt=1,
            cmap=cmap2,
            norm=norm,
            # extent=extent,
            # linewidths=0.0,
            zorder=3000,
        )

    # Force the mappable limits (this guarantees colorbar bounds)
    hb.set_clim(vmin, vmax)


    # --- labels
    if not swap_axes:
        ax.set_xlabel("years")
        ax.set_ylabel("travel time [sec]")
    else:
        ax.set_xlabel("travel time [sec]")
        ax.set_ylabel("time [year]")

    ax.grid(True, alpha=0.25)
    ax.set_axisbelow(True)

    if add_colorbar:
        # --- horizontal colorbar ABOVE the plot (inside the axes, so layout is stable)
        cax = inset_axes(
            ax,
            width="95%",     # length of the bar
            height="6%",     # thickness
            loc="upper center",
            borderpad=-2.8
        )

        cbar = fig.colorbar(hb, cax=cax, orientation="horizontal")
        cbar.set_label("amplitude [dB]")
        cbar.ax.xaxis.set_label_position("top")
        cbar.ax.xaxis.set_ticks_position("top")

        if top_db is not None:
            ticks = np.array(cbar.get_ticks(), dtype=float)
            ticks = np.unique(np.concatenate([ticks, [vmin, float(top_db), vmax]]))
            ticks = ticks[(ticks >= vmin) & (ticks <= vmax)]
            ticks = np.sort(ticks)
            # ---- REMOVE specific unwanted ticks
            if tick_vals is not None:
                # remove_vals = np.array([-110, -120, -160], dtype=float)
                remove_vals = tick_vals
                ticks = ticks[~np.isclose(ticks[:, None], remove_vals).any(axis=1)]


            cbar.set_ticks(ticks)

            # marker line at top_db (horizontal cbar => vertical line)
            cbar.ax.axvline(float(top_db), color="k", linewidth=1)

    return ax
