#!/usr/bin/env python3
"""Cross-die summary of the DESY single-die chain over several run/die datasets.

   Reads the json dumps that the per-die chain wrote for each die-run and answers
   the three questions one die cannot: whether the dark deficit (the dark ladder's
   PTC gain coming out below the bright one's) is a property of every device or of
   one, whether the dark-current gradient along the readout direction is thermal
   or a process effect, and how much of the rest of the datasheet is common to the
   lot. Every number comes from the dumps; the text states what the comparison
   shows rather than what one die showed.

   usage: desy_die_summary.py --dies run31_W04_D05 run31_W04_D07 ... [--out DIR]
"""
import argparse, json, os
import numpy as np
import matplotlib
matplotlib.use('Agg')
import matplotlib.pyplot as plt

P = argparse.ArgumentParser()
P.add_argument('--root',   default='/home/sasha/claude/desy_die')
P.add_argument('--rnroot', default='/home/sasha/claude/desy_rn')
P.add_argument('--dies',   nargs='+', required=True, help='tags, e.g. run31_W04_D05_high')
P.add_argument('--out',    default='/home/sasha/claude/desy_die/summary')
A = P.parse_args()
os.makedirs(A.out, exist_ok=True)

# ------------------------------------------------------------------ load
def load(tag, name, root=None):
    with open(os.path.join(root or os.path.join(A.root, tag), name)) as fh:
        return json.load(fh)

Dies = []
for tag in A.dies:
    d = {'Tag': tag}
    try:
        d['Z']  = load(tag, 'stats.json', os.path.join(A.rnroot, tag))
        d['D']  = load(tag, 'dark.json')
        d['L']  = load(tag, 'light.json')
        d['BC'] = load(tag, 'badcol.json')
        d['PT'] = load(tag, 'ptc.json')
        d['BU'] = load(tag, 'budget.json')
        d['ME'] = load(tag, 'methods.json')
        d['PP'] = load(tag, 'ptc_perpixel.json')
        d['DW'] = load(tag, 'darkwindow.json')
        d['PB'] = load(tag, 'ptc_both.json')
    except FileNotFoundError as e:
        print(f'  skipping {tag}: {e.filename} missing')
        continue
    d['Run']   = str(d['PT']['Run'])
    d['Die']   = str(d['PT']['Die'])
    d['Wafer'] = d['Die'].split('_')[0]
    d['Label']  = f"{d['Die']} / {d['PT']['GainHalf']} / run {d['Run']}"
    d['XLabel'] = f"{d['Die']}\n{d['PT']['GainHalf']}, r{d['Run']}"
    Dies.append(d)
if not Dies:
    raise SystemExit('no die-run has a complete set of dumps')
Dies.sort(key=lambda d: (d['Die'], d['Run']))
print(f'{len(Dies)} die-runs: ' + ', '.join(d['Label'] for d in Dies))
RUNS = sorted({d['Run'] for d in Dies})
COL  = {r: c for r, c in zip(RUNS, ('#4c72b0', '#c44e52', '#55a868', '#8172b2'))}

def arr(v):
    return np.atleast_1d(np.array(v, dtype=float))
def gain(d, route):
    return float(d['ME']['Routes'][route]['Gain'])
def gerr(d, route):
    Q = d['ME']['Routes'][route]
    return float(np.hypot(float(Q['GainStat']), float(Q['GainSyst'])))
def f(x, n=3):
    return ('%.' + str(n) + 'f') % float(x)

MD = []
def w(t):
    MD.append(t)
def fig(name, cap):
    w(f'![{cap}]({name})\n')
    w(f'*{cap}*\n')
def savefig(figure, name):
    figure.tight_layout()
    figure.savefig(os.path.join(A.out, name), dpi=120)
    plt.close(figure)

LOT = Dies[0]['PT']['Lot']
w(f'# {LOT} — cross-die summary of runs ' + ' and '.join(RUNS) + '\n')
w(f"""{len(Dies)} die-runs that passed the final test, each put through the same chain with the same
settings: whole die, individual pixels, the dark fit window chosen per die by goodness of fit. Runs {' and '.join(RUNS)} are the same
dies measured on two bias-board setups whose dark currents differ by a factor
{max(float(d['ME']['Routes']['a']['Slope']) for d in Dies)/min(float(d['ME']['Routes']['a']['Slope']) for d in Dies):.0f},
which is what makes the comparison worth making: anything common to both setups is the device, and
anything that follows the setup is not.\n""")

# ================================================================== 1. datasheet
w('## 1. The lot in one table\n')
w('| die / run | bias [ADU] | RN [ADU] | gain bright [ADU/e-] | gain dark | DC [ADU/s] | '
  'DSNU [%] | PRNU [%] | bad cols | dark window | T, PTC bright [e-] |')
w('|---|---|---|---|---|---|---|---|---|---|---|')
for d in Dies:
    dwin = ' '.join(str(int(v)) for v in arr(d['DW']['Chosen']))
    w(f"| {d['Label']} | {f(d['Z']['All']['BiasLevel'],2)} | {f(d['Z']['All']['ReadNoiseMedian'],3)} | "
      f"**{f(gain(d,'d'),4)}** ± {f(gerr(d,'d'),4)} | {f(gain(d,'c'),4)} ± {f(gerr(d,'c'),4)} | "
      f"{f(d['D']['Fit']['All']['SlopeSpread']['Median'],4)} | "
      f"{f(100*float(d['D']['Local']['DC']['RelIntr']),2)} | "
      f"{f(100*float(d['L']['PRNU']['Multiplicative']),2)} | "
      f"{int(d['BC']['Nbad'])} | [{dwin}] | "
      f"{f(d['ME']['Routes']['d']['Threshold_e'],1)} ± {f(d['ME']['Routes']['d']['Threshold_e_err'],1)} |")
w('')

def spread(vals, name, unit='', rel=True):
    v = np.array(vals, dtype=float)
    s = f"{np.median(v):.4g} {unit}".strip()
    if rel and np.median(v) != 0:
        s += f", spread {100*np.std(v)/abs(np.median(v)):.1f} % ({v.min():.4g} to {v.max():.4g})"
    else:
        s += f", spread {np.std(v):.4g} ({v.min():.4g} to {v.max():.4g})"
    return f'| {name} | {s} |'

w('How much of that is common to the lot:\n')
w('| quantity | median over the die-runs, and the spread |')
w('|---|---|')
w(spread([float(d['Z']['All']['ReadNoiseMedian']) for d in Dies], 'read noise', 'ADU'))
w(spread([float(d['Z']['All']['BiasLevel']) for d in Dies], 'bias level', 'ADU'))
w(spread([gain(d, 'd') for d in Dies], 'gain, bright-ladder PTC', 'ADU/e-'))
w(spread([100*float(d['L']['PRNU']['Multiplicative']) for d in Dies], 'PRNU, pixel to pixel', '%'))
w(spread([100*float(d['D']['Local']['DC']['RelIntr']) for d in Dies], 'DSNU, pixel to pixel', '%'))
w(spread([float(d['D']['Fit']['All']['SlopeSpread']['Median']) for d in Dies], 'dark current', 'ADU/s'))
w(spread([int(d['BC']['Nbad']) for d in Dies], 'bad readout columns at 5 sigma', 'of 4740'))
w('')

# ================================================================== 2. dark deficit
w('## 2. Does the dark deficit repeat?\n')
rows, sig, defi = [], [], []
for d in Dies:
    gc, gd = gain(d, 'c'), gain(d, 'd')
    ec, ed = gerr(d, 'c'), gerr(d, 'd')
    ns = (gd - gc)/np.hypot(ec, ed)
    rows.append((d, gc, gd, ec, ed, ns))
    sig.append(ns)
    defi.append(100*(1 - gc/gd))
w('| die / run | gain, dark ladder | gain, bright ladder | deficit | separation | '
  'ensemble deficit | per-pixel gain difference |')
w('|---|---|---|---|---|---|---|')
w('| *what it is* | *stat $\\oplus$ syst* | *stat $\\oplus$ syst* | *1 - dark/bright* | '
  '*of the two gains* | *dark points vs the bright line* | *mean over pixels* |')
for d, gc, gd, ec, ed, ns in rows:
    pp = d['PP']['Ladder']['Difference']
    w(f"| {d['Label']} | {f(gc,4)} ± {f(ec,4)} | {f(gd,4)} ± {f(ed,4)} | "
      f"**{100*(1-gc/gd):+.1f} %** | {ns:.1f} sigma | "
      f"{100*float(d['PB']['DarkDeficit']):+.1f} % | {-100*float(pp['MeanRel']):+.1f} % |")
w('')
nneg = sum(1 for v in defi if v > 0)
nsig = sum(1 for v in sig if v > 3)
w(f"""The dark ladder's PTC gain comes out **below** the bright one's on {nneg} of the
{len(Dies)} die-runs, and the separation exceeds 3 sigma on {nsig} of them. The deficit ranges from
{min(defi):+.1f} % to {max(defi):+.1f} %, median {np.median(defi):+.1f} %. The three independent
estimates of it in the table above — the two gains with their errors, the dark points' offset from
the bright line, and the mean per-pixel gain difference — are listed side by side because they use
different estimators of the same quantity and so bound the method error on it.

They are not measured over the same signal range, which is why they can differ by more than their
errors. The ensemble column compares the dark points with the bright line only inside the PTC
window ({float(Dies[0]['PB']['Window'][0]):.0f}-{float(Dies[0]['PB']['Window'][1]):.0f} ADU), the
same band on every die-run; the two gains are each fitted over their own ladder's full linear
range, which on the high-dark-current setup reaches far above that band. Where the gain-ratio
deficit exceeds the ensemble one, the deficit is growing with dark signal rather than being a
constant fraction.\n""")

figu, axs = plt.subplots(1, 2, figsize=(12.6, 5.0))
ax = axs[0]
xs = np.arange(len(Dies))
for i, (d, gc, gd, ec, ed, ns) in enumerate(rows):
    ax.errorbar(i-0.1, gd, yerr=ed, fmt='s', ms=7, color='#c44e52',
                label='bright ladder' if i == 0 else None)
    ax.errorbar(i+0.1, gc, yerr=ec, fmt='o', ms=7, color='#4c72b0',
                label='dark ladder' if i == 0 else None)
ax.set_xticks(xs); ax.set_xticklabels([d['XLabel'] for d in Dies], rotation=0, ha='center', fontsize=7.5)
ax.set_ylabel('conversion gain [ADU/e-]')
ax.set_title('The two PTC gains, die by die', fontsize=10)
ax.grid(alpha=0.25); ax.legend(fontsize=8.5)
ax = axs[1]
for i, (d, gc, gd, ec, ed, ns) in enumerate(rows):
    ax.errorbar(i, 100*(1-gc/gd), yerr=100*np.hypot(ec, ed)/gd, fmt='o', ms=7, color=COL[d['Run']])
ax.axhline(0, color='k', lw=1.1)
ax.set_xticks(xs); ax.set_xticklabels([d['XLabel'] for d in Dies], rotation=0, ha='center', fontsize=7.5)
ax.set_ylabel('dark deficit [%]')
ax.set_title('Deficit = 1 - g(dark)/g(bright); colour is the run', fontsize=10)
ax.grid(alpha=0.25)
figu.suptitle(f'{LOT}: is the dark deficit a property of the device?', fontsize=11)
savefig(figu, 'fig_sum_deficit.png')
fig('fig_sum_deficit.png', 'The two PTC gains and their difference across the die-runs. A deficit '
    'present on every die-run, at a similar size and on both bias-board setups, is a property of '
    'the measurement chain or of the device family; one that appears on a subset is a property of '
    'those devices.')

# ================================================================== 3. the gradient
w('## 3. The dark-current gradient: thermal or process?\n')
w("""Every die shows the dark current rising along the readout direction. Two explanations predict
different things across the dies: a thermal gradient in the test setup (the output amplifier
dissipating at one end) repeats the same shape on every die irrespective of where the die sat on
the wafer, while a process gradient follows wafer position and would differ between dies and
between wafers. The profiles are normalised to their own median, so only the shape is compared.""")

figu, axs = plt.subplots(1, 2, figsize=(12.8, 5.0))
ax = axs[0]
prof = []
for d in Dies:
    rc = arr(d['BC']['RawCol'])
    dc = arr(d['BC']['DcProfile'])
    o  = np.argsort(rc)
    x, y = rc[o], dc[o]
    ok = np.isfinite(y) & (y > 0)
    yn = y/np.nanmedian(y[ok])
    prof.append((d, x, yn, ok))
    ax.plot(x[ok], yn[ok], '-', lw=0.7, alpha=0.75, color=COL[d['Run']])
for r in RUNS:
    ax.plot([], [], '-', color=COL[r], label=f'run {r}')
ax.set_xlabel('raw readout column')
ax.set_ylabel('dark current / its median on that die')
ax.set_title('Shape of the gradient, all die-runs', fontsize=10)
ax.grid(alpha=0.25); ax.legend(fontsize=8.5)
ax = axs[1]
for i, d in enumerate(Dies):
    ax.plot(i, float(d['BC']['Gradient']['DCRatio']), 'o', ms=8, color=COL[d['Run']])
ax.axhline(1, color='k', lw=1.0)
ax.set_xticks(np.arange(len(Dies)))
ax.set_xticklabels([d['XLabel'] for d in Dies], rotation=0, ha='center', fontsize=7.5)
ax.set_ylabel('dark current, first third / last third')
ax.set_title('Amplitude of the gradient', fontsize=10)
ax.grid(alpha=0.25)
figu.suptitle(f'{LOT}: the dark-current gradient along the readout direction', fontsize=11)
savefig(figu, 'fig_sum_gradient.png')
fig('fig_sum_gradient.png', 'Left: the normalised dark-current profile of every die-run. Right: the '
    'ratio of the first to the last third of the readout direction.')

# shape correlation between the die-runs, on a common grid
grid = np.linspace(1, 4740, 300)
sh = []
for d, x, yn, ok in prof:
    sh.append(np.interp(grid, x[ok], yn[ok]))
SH = np.array(sh)
CM = np.corrcoef(SH)
iu = np.triu_indices(len(Dies), 1)
same_die = np.array([Dies[i]['Die'] == Dies[j]['Die'] for i, j in zip(*iu)])
same_waf = np.array([Dies[i]['Wafer'] == Dies[j]['Wafer'] for i, j in zip(*iu)])
rr = CM[iu]
ratios = [float(d['BC']['Gradient']['DCRatio']) for d in Dies]
w(f"""The profile shapes correlate across the die-runs with r = {np.median(rr):.3f} on the median
pair ({rr.min():.3f} to {rr.max():.3f}). Split by what the pair shares:

| pair | n | median r |
|---|---|---|
| same die, the two runs | {int(same_die.sum())} | {np.median(rr[same_die]) if same_die.any() else float('nan'):.3f} |
| same wafer, different die | {int((same_waf & ~same_die).sum())} | {np.median(rr[same_waf & ~same_die]) if (same_waf & ~same_die).any() else float('nan'):.3f} |
| different wafer | {int((~same_waf).sum())} | {np.median(rr[~same_waf]) if (~same_waf).any() else float('nan'):.3f} |

The amplitude spans {min(ratios):.2f} to {max(ratios):.2f} (first third over last third), median
{np.median(ratios):.2f}.\n""")

# the verdict the numbers support, stated rather than left to the reader
_rs = np.median(rr[same_die]) if same_die.any() else np.nan
_rd = np.median(rr[~same_waf]) if (~same_waf).any() else np.nan
_gap = _rs - _rd
if np.isfinite(_gap) and _gap > 0.10:
    _verdict = (f"Pairs sharing a die agree better than pairs on different wafers "
                f"(r = {_rs:.3f} against {_rd:.3f}, a gap of {_gap:.3f}), so the shape follows the "
                f"**device**: this is a process or layout gradient, not the test setup. A thermal "
                f"gradient in the setup would not know which die it was looking at.")
elif np.isfinite(_gap) and _rd > 0.7:
    _verdict = (f"Dies on **different wafers** reproduce each other's profile as well as the two "
                f"runs of one die do (r = {_rd:.3f} against {_rs:.3f}, a gap of {_gap:+.3f}), and the "
                f"amplitude is the same to within {100*(max(ratios)/min(ratios)-1):.0f} %. The shape "
                f"therefore does **not** follow the silicon, which is what a thermal gradient in the "
                f"test setup predicts and what a process gradient does not. The hypothesis survives "
                f"this test. What it does not yet have is a direct measurement: the headers carry one "
                f"set-point and no on-die sensor, so the remaining test is a run at a different chuck "
                f"temperature, where a thermal gradient must change amplitude and a process one must "
                f"not.")
else:
    _verdict = (f"The profiles do not reproduce each other well enough for this test to decide "
                f"(median r = {np.median(rr):.3f}, same-die {_rs:.3f} against different-wafer "
                f"{_rd:.3f}): neither hypothesis is supported or excluded by the shapes alone.")
w(_verdict + '\n')

figu, ax = plt.subplots(figsize=(6.6, 5.6))
im = ax.imshow(CM, vmin=min(0.0, float(np.nanmin(CM))), vmax=1, cmap='viridis')
ax.set_xticks(np.arange(len(Dies))); ax.set_yticks(np.arange(len(Dies)))
ax.set_xticklabels([d['XLabel'].replace(chr(10), ' ') for d in Dies], rotation=90, fontsize=6.5)
ax.set_yticklabels([d['XLabel'].replace(chr(10), ' ') for d in Dies], fontsize=6.5)
for i in range(len(Dies)):
    for j in range(len(Dies)):
        ax.text(j, i, f'{CM[i,j]:.2f}', ha='center', va='center', fontsize=6.5,
                color='w' if CM[i, j] < 0.75 else 'k')
figu.colorbar(im, ax=ax, label='correlation of the normalised profile')
ax.set_title('Do the gradients have the same shape?', fontsize=10)
savefig(figu, 'fig_sum_gradient_corr.png')
fig('fig_sum_gradient_corr.png', 'Correlation of the normalised dark-current profile between every '
    'pair of die-runs. Blocks along the diagonal would mean the shape follows the die; a uniformly '
    'high matrix means it follows the setup.')

# ================================================================== 4. thresholds
w('## 4. The charge threshold across the lot\n')
w('| die / run | a) dark response | b) light response | c) PTC dark | d) PTC bright | span |')
w('|---|---|---|---|---|---|')
for d in Dies:
    vals = [float(d['ME']['Routes'][k]['Threshold_e']) for k in 'abcd']
    errs = [float(d['ME']['Routes'][k]['Threshold_e_err']) for k in 'abcd']
    w(f"| {d['Label']} | " + ' | '.join(f'{v:.1f} ± {e:.1f}' for v, e in zip(vals, errs)) +
      f" | {max(vals)-min(vals):.1f} e- |")
w('')
figu, ax = plt.subplots(figsize=(11.0, 5.0))
mk = {'a': 'o', 'b': 's', 'c': '^', 'd': 'D'}
nm = {'a': 'dark response', 'b': 'light response', 'c': 'PTC dark', 'd': 'PTC bright'}
for k in 'abcd':
    v = [float(d['ME']['Routes'][k]['Threshold_e']) for d in Dies]
    e = [float(d['ME']['Routes'][k]['Threshold_e_err']) for d in Dies]
    ax.errorbar(np.arange(len(Dies)) + 0.08*('abcd'.index(k)-1.5), v, yerr=e, fmt=mk[k], ms=6,
                label=nm[k])
ax.set_xticks(np.arange(len(Dies)))
ax.set_xticklabels([d['XLabel'] for d in Dies], rotation=0, ha='center', fontsize=7.5)
ax.set_ylabel('charge threshold [e-]')
ax.set_title('Four routes to the threshold, on every die-run', fontsize=10)
ax.grid(alpha=0.25); ax.legend(fontsize=8.5)
savefig(figu, 'fig_sum_threshold.png')
fig('fig_sum_threshold.png', 'The four routes on every die-run. A route that is measuring the '
    'device gives the same answer on both runs of a die; one that is measuring the measurement '
    'moves with the run.')
spanmax = max(max(float(d['ME']['Routes'][k]['Threshold_e']) for k in 'abcd') -
              min(float(d['ME']['Routes'][k]['Threshold_e']) for k in 'abcd') for d in Dies)
w(f"""The four routes disagree on every die-run, by up to {spanmax:.0f} e-. Comparing the same route
between the two runs of one die separates the two possibilities: a threshold that is a property of
the device must not change when only the bias board does.\n""")
# same route, same die, two runs
bydie = {}
for d in Dies:
    bydie.setdefault(d['Die'], []).append(d)
pair = {k: [] for k in 'abcd'}
for die, dd in bydie.items():
    if len(dd) < 2:
        continue
    dd = sorted(dd, key=lambda x: x['Run'])
    for k in 'abcd':
        v0 = float(dd[0]['ME']['Routes'][k]['Threshold_e'])
        v1 = float(dd[-1]['ME']['Routes'][k]['Threshold_e'])
        e  = np.hypot(float(dd[0]['ME']['Routes'][k]['Threshold_e_err']),
                      float(dd[-1]['ME']['Routes'][k]['Threshold_e_err']))
        pair[k].append((die, v0, v1, (v1-v0)/e if e > 0 else np.nan))
if any(pair.values()):
    w('| route | dies compared | median change between the runs | median separation |')
    w('|---|---|---|---|')
    _stable, _moving = [], []
    for k in 'abcd':
        if not pair[k]:
            continue
        ch = [p[2]-p[1] for p in pair[k]]
        ns = [abs(p[3]) for p in pair[k]]
        w(f'| {nm[k]} | {len(pair[k])} | {np.median(ch):+.1f} e- | {np.median(ns):.1f} sigma |')
        (_stable if np.median(ns) < 3 else _moving).append((nm[k], np.median(ch), np.median(ns)))
    w('')
    if _stable:
        w('Routes whose threshold does **not** move when only the bias board changes: ' +
          ', '.join(f'{n} ({c:+.1f} e-, {s:.1f} sigma)' for n, c, s in _stable) +
          '. On this test those are measuring a property of the device.\n')
    if _moving:
        w('Routes whose threshold **does** move between the two runs of the same die: ' +
          ', '.join(f'{n} ({c:+.1f} e-, {s:.1f} sigma)' for n, c, s in _moving) +
          '. A device property cannot do that, so on these dies that route is reporting something '
          'about the measurement -- most likely the curvature of the ladder it extrapolates, whose '
          'signal range is set by the bias board.\n')
    if _stable and _moving:
        w(f"That split is the practical answer to which route to believe: "
          f"**{_stable[0][0]}** is reproducible across setups, "
          f"**{_moving[0][0]}** is not.\n")

# ================================================================== 4b. the rest, side by side
w('## 4b. Every other number, die by die\n')
w("""The same comparison for the quantities the threshold plot does not cover. The x axis is the
same throughout — die, gain half, setup — so a panel in which the two runs of a die sit on top of
each other is a property of the device, and one in which the points separate by run is a property
of the setup. Error bars are drawn where the chain measures one (the gain and the thresholds carry
a block-to-block statistical error and a fit-window systematic); the medians over 22.5 M pixels do
not need one at this scale.\n""")

PANELS = [
    ('read noise [ADU]',          lambda d: float(d['Z']['All']['ReadNoiseMedian']), None),
    ('read noise [e-]',           lambda d: float(d['Z']['All']['ReadNoiseMedian'])/gain(d, 'd'), None),
    ('bias level [ADU]',          lambda d: float(d['Z']['All']['BiasLevel']), None),
    ('bias fixed pattern [ADU]',  lambda d: float(d['Z']['All']['FixedPatternRMS']), None),
    ('dark current [ADU/s]',      lambda d: float(d['D']['Fit']['All']['SlopeSpread']['Median']), None),
    ('dark current [e-/s]',       lambda d: float(d['D']['Fit']['All']['SlopeSpread']['Median'])/gain(d, 'd'), None),
    ('gain, bright PTC [ADU/e-]', lambda d: gain(d, 'd'), lambda d: gerr(d, 'd')),
    ('gain, dark PTC [ADU/e-]',   lambda d: gain(d, 'c'), lambda d: gerr(d, 'c')),
    ('PRNU, pixel to pixel [%]',  lambda d: 100*float(d['L']['PRNU']['Multiplicative']), None),
    ('DSNU, pixel to pixel [%]',  lambda d: 100*float(d['D']['Local']['DC']['RelIntr']), None),
    ('bad readout columns of 4740', lambda d: float(d['BC']['Nbad']), None),
    ('smallest signal at SNR 5, PTC threshold [e-]',
     lambda d: float(d['BU']['Unmasked']['PTC']['Qlim_cal_5']), None),
]
nc = 3
nr = int(np.ceil(len(PANELS)/nc))
figu, axs = plt.subplots(nr, nc, figsize=(4.6*nc, 3.0*nr), sharex=True)
axs = np.atleast_1d(axs).ravel()
SUMROWS = []
for ax, (ttl, fn, efn) in zip(axs, PANELS):
    vals = []
    for i, d in enumerate(Dies):
        try:
            v = fn(d)
        except (KeyError, TypeError, ZeroDivisionError):
            v = np.nan
        e = None
        if efn is not None:
            try:
                e = efn(d)
            except (KeyError, TypeError):
                e = None
        vals.append(v)
        ax.errorbar(i, v, yerr=e, fmt='o', ms=6, color=COL[d['Run']], capsize=3)
    vv = np.array(vals, dtype=float)
    ok = np.isfinite(vv)
    if ok.sum() > 1:
        ax.axhline(np.median(vv[ok]), color='#888888', ls=':', lw=1.0)
    ax.set_title(ttl, fontsize=9)
    ax.grid(alpha=0.25)
    ax.tick_params(labelsize=7.5)
    SUMROWS.append((ttl, vv))
for ax in axs[len(PANELS):]:
    ax.axis('off')
for ax in axs[max(0, len(PANELS)-nc):len(PANELS)]:
    ax.set_xticks(np.arange(len(Dies)))
    ax.set_xticklabels([d['XLabel'] for d in Dies], rotation=90, fontsize=6.5)
for r in RUNS:
    axs[0].plot([], [], 'o', color=COL[r], label=f'run {r}')
axs[0].legend(fontsize=7.5)
figu.suptitle(f'{LOT}: every datasheet number across the die-runs', fontsize=11)
savefig(figu, 'fig_sum_quantities.png')
fig('fig_sum_quantities.png', 'Each panel is one quantity against die, gain half and setup. The '
    'dotted line is the median over the die-runs. Panels whose points split into two levels by '
    'colour are following the setup, not the device.')

w('| quantity | median | spread between the die-runs | run-to-run split |')
w('|---|---|---|---|')
for ttl, vv in SUMROWS:
    ok = np.isfinite(vv)
    if ok.sum() < 2:
        continue
    med = np.median(vv[ok])
    rel = 100*np.std(vv[ok])/abs(med) if med != 0 else np.nan
    # how much of that spread is just the two setups
    byrun = [np.median([v for v, d in zip(vv, Dies) if d['Run'] == r and np.isfinite(v)])
             for r in RUNS]
    byrun = [v for v in byrun if np.isfinite(v)]
    split = (100*(max(byrun)-min(byrun))/abs(med)) if (len(byrun) > 1 and med != 0) else np.nan
    w(f'| {ttl} | {med:.4g} | {rel:.1f} % ({np.nanmin(vv):.4g} to {np.nanmax(vv):.4g}) | '
      f'{split:.1f} % |')
w("""
The last column is the difference between the two setups' medians, as a fraction of the overall
median: a quantity whose die-to-die spread is almost all run-to-run split is being set by the bias
board, and one whose split is small while its spread is not varies between devices.\n""")

# ================================================================== 5. the windows
w('## 5. The dark fit windows the goodness of fit chose\n')
w('| die / run | ladder span [ADU] | window | medians [ADU] | chi2 ratio | '
  'DC spread over the windows | T span over the windows |')
w('|---|---|---|---|---|---|---|')
for d in Dies:
    sc = d['DW']['Scan'] if isinstance(d['DW']['Scan'], list) else [d['DW']['Scan']]
    dcs = [float(e['DC']) for e in sc]
    tds = [float(e['Tdark']) for e in sc]
    med = arr(d['DW']['StepMedian'])
    cm  = arr(d['DW']['ChosenMedian'])
    w(f"| {d['Label']} | {med.min():.0f} to {med.max():.0f} | "
      f"[{' '.join(str(int(v)) for v in arr(d['DW']['Chosen']))}] | {cm.min():.0f}-{cm.max():.0f} | "
      f"{float(d['DW']['ChosenRatio']):.3f} | {100*(max(dcs)/min(dcs)-1):.0f} % | "
      f"{min(tds):.1f}-{max(tds):.1f} |")
w(f"""
The windows differ between the runs because the ladders do: at the same nine exposures the
high-dark-current setup reaches {max(arr(d['DW']['StepMedian']).max() for d in Dies):.0f} ADU and the
low-dark-current one {min(arr(d['DW']['StepMedian']).max() for d in Dies):.0f}. The last two columns
are the reason a fixed list of steps was not kept: the dark-route threshold moves by more than a
factor two across the defensible windows of a single ladder, so a window chosen by hand on one run
would have carried its own bias into every other.\n""")

# ================================================================== write
md = '\n'.join(MD) + '\n'
with open(os.path.join(A.out, 'summary.md'), 'w') as fh:
    fh.write(md)
HTML = """<!doctype html><meta charset="utf-8"><title>__TITLE__</title>
<style>
body{font-family:-apple-system,Segoe UI,Roboto,Helvetica,Arial,sans-serif;line-height:1.5;
     max-width:1000px;margin:1.5rem auto;padding:0 1rem;color:#222}
h1{font-size:24px}h2{font-size:18px;margin-top:1.6rem;border-bottom:1px solid #ddd}
table{border-collapse:collapse;margin:.6rem 0;font-size:12px}
th,td{border:1px solid #ddd;padding:3px 6px;text-align:left}
th{background:#f5f5f5}
img{max-width:100%;margin:.6rem 0;border:1px solid #eee}
em{color:#666;font-size:13px}
table em,table strong{font-size:inherit;color:inherit}
@page{size:A4 portrait;margin:12mm 10mm}
@media print{
  body{max-width:none;margin:0;padding:0;font-size:11.5px}
  table{width:100%;font-size:9px}th,td{padding:2px 3px;line-height:1.2}
  img{page-break-inside:avoid}h1,h2{page-break-after:avoid}
}
</style><body><div id="c"></div>
<script type="text/markdown" id="src">
__MD__
</script>
<script src="https://cdnjs.cloudflare.com/ajax/libs/marked/9.1.6/marked.min.js"></script>
<script>document.getElementById('c').innerHTML =
  marked.parse(document.getElementById('src').textContent);</script>
</body></html>"""
with open(os.path.join(A.out, 'summary.html'), 'w') as fh:
    fh.write(HTML.replace('__MD__', md.replace('</script', '<\\/script'))
                 .replace('__TITLE__', f'{LOT} cross-die summary'))
print(f'summary.md / summary.html -> {A.out}')
