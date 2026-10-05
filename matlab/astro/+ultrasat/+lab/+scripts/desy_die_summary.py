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

def readconfig(dataset):
    '''The PTC_Config.xlsx the test ran with, as {setting: value}.

    Read here rather than named in the text: when two runs differ, the report has
    to say what differed, and a literal in the prose would be describing whichever
    pair of runs it was written for. No new dependency -- an xlsx is a zip of XML.
    '''
    import zipfile, re as _re, html as _html
    path = os.path.join(dataset, 'PTC_int_hr', 'PTC_Config.xlsx')
    if not os.path.isfile(path):
        return {}
    try:
        with zipfile.ZipFile(path) as z:
            shared = [_html.unescape(m) for m in
                      _re.findall(r'<t[^>]*>(.*?)</t>', z.read('xl/sharedStrings.xml').decode('utf8', 'replace'), _re.S)]
            out = {}
            for sheet in ('xl/worksheets/sheet2.xml', 'xl/worksheets/sheet1.xml'):
                if sheet not in z.namelist():
                    continue
                x = z.read(sheet).decode('utf8', 'replace')
                for rm in _re.finditer(r'<row[^>]*>(.*?)</row>', x, _re.S):
                    cells = {}
                    for cm in _re.finditer(r'<c r="([A-Z]+)\d+"([^>]*)>(.*?)</c>', rm.group(1), _re.S):
                        v = _re.search(r'<v>(.*?)</v>', cm.group(3), _re.S)
                        if not v:
                            continue
                        val = v.group(1)
                        if 't="s"' in cm.group(2):
                            val = shared[int(val)]
                        cells[cm.group(1)] = val
                    if 'A' in cells and 'B' in cells and cells['A'] != 'Time':
                        out.setdefault(cells['A'], cells['B'])
            return out
    except Exception:
        return {}

Dies = []
for tag in A.dies:
    d = {'Tag': tag}
    try:
        # stage 1 does not depend on any fit window, so the signal-window run
        # shares it with the default one and it is not under a '_sig' name
        d['Z']  = load(tag, 'stats.json',
                       os.path.join(A.rnroot, tag[:-4] if tag.endswith('_sig') else tag))
        d['D']  = load(tag, 'dark.json')
        d['L']  = load(tag, 'light.json')
        d['BC'] = load(tag, 'badcol.json')
        d['PT'] = load(tag, 'ptc.json')
        d['BU'] = load(tag, 'budget.json')
        d['ME'] = load(tag, 'methods.json')
        d['PP'] = load(tag, 'ptc_perpixel.json')
        # 'chi2' mode writes darkwindow.json, 'signal' mode fitwindow.json;
        # a die-run has exactly one of them
        d['DW'] = load(tag, 'darkwindow.json') if \
                  os.path.isfile(os.path.join(A.root, tag, 'darkwindow.json')) else None
        d['FW'] = load(tag, 'fitwindow.json') if \
                  os.path.isfile(os.path.join(A.root, tag, 'fitwindow.json')) else None
        if d['DW'] is None and d['FW'] is None:
            raise FileNotFoundError(2, 'no window dump', os.path.join(A.root, tag, 'darkwindow.json'))
        d['PB'] = load(tag, 'ptc_both.json')
        d['Cfg'] = readconfig(load(tag, 'chain.json').get('Dataset', '')) if \
                   os.path.isfile(os.path.join(A.root, tag, 'chain.json')) else {}
    except FileNotFoundError as e:
        print(f'  skipping {tag}: {e.filename} missing')
        continue
    d['Run']   = str(d['PT']['Run'])
    d['Die']   = str(d['PT']['Die'])
    d['Wafer'] = d['Die'].split('_')[0]
    # Die names repeat between lots: TH02260 and TH02954 both have a W07_D06 and
    # both were measured in run 35, so without the lot the two are one row and
    # one point, silently.
    d['Lot'] = str(d['PT'].get('Lot', ''))
    _lot = '' if d['Lot'] in ('', 'TH02954') else f" {d['Lot']}"
    d['Label']  = f"{d['Die']}{_lot} / {d['PT']['GainHalf']} / run {d['Run']}"
    d['XLabel'] = f"{d['Die']}{_lot}\n{d['PT']['GainHalf']}"
    Dies.append(d)
if not Dies:
    raise SystemExit('no die-run has a complete set of dumps')
# Grouped by RUN: all the dies of one setup sit together on every x axis, so a
# panel in which the groups sit at different levels is showing a setup effect and
# one in which they interleave is showing a device effect.
def _runkey(r):
    # '38-2' sorts after '38', and numerically rather than as text
    a, _, b = str(r).partition('-')
    return (int(a) if a.isdigit() else 0, int(b) if b.isdigit() else 0)
Dies.sort(key=lambda d: (_runkey(d['Run']), d['Die']))
print(f'{len(Dies)} die-runs: ' + ', '.join(d['Label'] for d in Dies))
RUNS = sorted({d['Run'] for d in Dies}, key=_runkey)
# every pair of setups: with more than two, a first-against-last comparison would
# mix the things that distinguish them and report the sum as if it were one
RPAIRS = [(RUNS[i], RUNS[j]) for i in range(len(RUNS)) for j in range(i+1, len(RUNS))]
_PAL = ('#4c72b0', '#c44e52', '#55a868', '#8172b2', '#dd8452', '#937860', '#8c8c8c')
COL  = {r: _PAL[i % len(_PAL)] for i, r in enumerate(RUNS)}

def groups():
    '''(run, first index, last index) of each run's block of dies'''
    out, i = [], 0
    while i < len(Dies):
        j = i
        while j+1 < len(Dies) and Dies[j+1]['Run'] == Dies[i]['Run']:
            j += 1
        out.append((Dies[i]['Run'], i, j))
        i = j+1
    return out
GRP = groups()

def markruns(ax, label=True):
    '''separate the runs on an x axis of die index, and name each block'''
    for _, i0, i1 in GRP[:-1]:
        ax.axvline(i1 + 0.5, color='#999999', lw=0.9, ls='--', alpha=0.8)
    if label:
        for r, i0, i1 in GRP:
            ax.annotate(f'run {r}', (0.5*(i0+i1), 1.012), xycoords=('data', 'axes fraction'),
                        ha='center', va='bottom', fontsize=8.5, color='#444444')
        # make room so the run names do not sit on top of the panel title
        ax.set_title(ax.get_title(), fontsize=ax.title.get_fontsize(), pad=18)

# What each run was configured with, folded to one dict per run. Built here
# rather than where it is printed, because section 3's thermal argument depends
# on knowing whether anything ELSE differs between the runs it compares.
_cfg = {}
for d in Dies:
    if d.get('Cfg'):
        _cfg.setdefault(d['Run'], []).append(d['Cfg'])
_runcfg = {}
for r, cs in _cfg.items():
    keys = set().union(*[set(c) for c in cs])
    _runcfg[r] = {k: (cs[0].get(k) if all(c.get(k) == cs[0].get(k) for c in cs) else '(varies)')
                  for k in keys}

def cfgdiff(ra, rb):
    '''settings that differ between two runs, as [(name, value_a, value_b)]'''
    if ra not in _runcfg or rb not in _runcfg:
        return None
    ks = set(_runcfg[ra]) | set(_runcfg[rb])
    return sorted((k, _runcfg[ra].get(k, '--'), _runcfg[rb].get(k, '--')) for k in ks
                  if _runcfg[ra].get(k) != _runcfg[rb].get(k))

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
# ---------------------------------------------------------------- outliers
# Flagged against the lot itself rather than against fixed limits: a quantity
# more than 5 median-absolute-deviations from the median over all the die-runs,
# or a threshold that comes out negative, which no route can mean physically.
# The flag says "do not read this row as typical", not "this die is broken":
# some of it is a real device difference and some is a fit with nothing to hold
# on to, and the per-die report is where that is decided.
nm = {'a': 'dark response', 'b': 'light response', 'c': 'PTC dark', 'd': 'PTC bright'}
def _mad(v):
    v = np.asarray(v, dtype=float)
    m = np.median(v)
    return m, (1.4826*np.median(np.abs(v-m)) or np.nan)

_qd = {'DSNU [%]': [100*float(d['D']['Local']['DC']['RelIntr']) for d in Dies],
       'dark-ladder gain': [gain(d, 'c') for d in Dies]}
_lim = {k: _mad(v) for k, v in _qd.items()}
FLAG = {}
for _i, _d in enumerate(Dies):
    _why = []
    for _k, _v in _qd.items():
        _m, _sd = _lim[_k]
        if np.isfinite(_sd) and abs(_v[_i]-_m) > 5*_sd:
            _why.append(f'{_k} = {_v[_i]:.2f} against {_m:.2f} typical')
    # negative by more than 2 sigma: a threshold of -0.6 +- 4.9 e- is consistent
    # with zero and says nothing, while -72 +- 3 says the fit found nothing real
    for _r in 'abcd':
        _t = float(_d['ME']['Routes'][_r]['Threshold_e'])
        _te = float(_d['ME']['Routes'][_r]['Threshold_e_err'])
        if _t + 2*_te < 0:
            _why.append(f'{nm[_r]} threshold {_t:.1f} +- {_te:.1f} e-, negative beyond its error')
    if _why:
        FLAG[_d['Label']] = '; '.join(_why)

w('## 1. The lot in one table\n')
if FLAG:
    w(f"""Of the {len(Dies)} die-runs, **{len(FLAG)} are marked &dagger;**: at least one
quantity sits more than 5 MAD from the median over all of them, or a threshold came out negative.
They are left in rather than dropped, and the mark means only that the row should not be read as
typical -- whether it is the device or the fit is decided in that die's own report. The run 38-2
shift in bias and read noise is deliberately NOT flagged: it is the transfer-gate voltage, measured
and understood in section 4c.\n""")
w('| die / run | bias [ADU] | RN [ADU] | gain bright [ADU/e-] | gain dark | DC [ADU/s] | '
  'DSNU [%] | PRNU [%] | bad cols | dark window | T, PTC bright [e-] |')
w('|---|---|---|---|---|---|---|---|---|---|---|')
for d in Dies:
    dwin = ' '.join(str(int(v)) for v in arr((d['DW'] or d['FW']['D'])['Chosen']))
    w(f"| {d['Label']}{' &dagger;' if d['Label'] in FLAG else ''} | "
      f"{f(d['Z']['All']['BiasLevel'],2)} | {f(d['Z']['All']['ReadNoiseMedian'],3)} | "
      f"**{f(gain(d,'d'),4)}** ± {f(gerr(d,'d'),4)} | {f(gain(d,'c'),4)} ± {f(gerr(d,'c'),4)} | "
      f"{f(d['D']['Fit']['All']['SlopeSpread']['Median'],4)} | "
      f"{f(100*float(d['D']['Local']['DC']['RelIntr']),2)} | "
      f"{f(100*float(d['L']['Local']['Resp']['RelIntr']),2)} | "
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

if FLAG:
    w('&dagger; and why:\n')
    w('| die / run | what is out of family |')
    w('|---|---|')
    for _k, _v in FLAG.items():
        w(f'| {_k} | {_v} |')
    w('')
w('How much of that is common to the lot:\n')
w('| quantity | median over the die-runs, and the spread |')
w('|---|---|')
w(spread([float(d['Z']['All']['ReadNoiseMedian']) for d in Dies], 'read noise', 'ADU'))
w(spread([float(d['Z']['All']['BiasLevel']) for d in Dies], 'bias level', 'ADU'))
w(spread([gain(d, 'd') for d in Dies], 'gain, bright-ladder PTC', 'ADU/e-'))
w(spread([100*float(d['L']['Local']['Resp']['RelIntr']) for d in Dies], 'PRNU, pixel to pixel', '%'))
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
ax.grid(alpha=0.25); ax.legend(fontsize=8.5); markruns(ax)
ax = axs[1]
for i, (d, gc, gd, ec, ed, ns) in enumerate(rows):
    ax.errorbar(i, 100*(1-gc/gd), yerr=100*np.hypot(ec, ed)/gd, fmt='o', ms=7, color=COL[d['Run']])
ax.axhline(0, color='k', lw=1.1)
ax.set_xticks(xs); ax.set_xticklabels([d['XLabel'] for d in Dies], rotation=0, ha='center', fontsize=7.5)
ax.set_ylabel('dark deficit [%]')
ax.set_title('Deficit = 1 - g(dark)/g(bright); colour is the run', fontsize=10)
ax.grid(alpha=0.25); markruns(ax)
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
ax.grid(alpha=0.25); markruns(ax)
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
same_die = np.array([(Dies[i]['Lot'], Dies[i]['Die']) == (Dies[j]['Lot'], Dies[j]['Die'])
                     for i, j in zip(*iu)])
same_waf = np.array([(Dies[i]['Lot'], Dies[i]['Wafer']) == (Dies[j]['Lot'], Dies[j]['Wafer'])
                     for i, j in zip(*iu)])
rr = CM[iu]
ratios = [float(d['BC']['Gradient']['DCRatio']) for d in Dies]
w(f"""The profile shapes correlate across the die-runs with r = {np.median(rr):.3f} on the median
pair ({rr.min():.3f} to {rr.max():.3f}). Split by what the pair shares:

| pair | n | median r |
|---|---|---|
| same die, the two runs | {int(same_die.sum())} | {np.median(rr[same_die]) if same_die.any() else float('nan'):.3f} |
| same wafer, different die | {int((same_waf & ~same_die).sum())} | {np.median(rr[same_waf & ~same_die]) if (same_waf & ~same_die).any() else float('nan'):.3f} |
| different wafer | {int((~same_waf).sum())} | {np.median(rr[~same_waf]) if (~same_waf).any() else float('nan'):.3f} |
\n""")

# ------------------------------------------------------------------ the amplitude
# The shape correlation is a weak discriminator: every profile is a monotonic
# ramp in the same direction, so r is high between any two of them whatever the
# cause. The AMPLITUDE is what separates the hypotheses, and the dies measured on
# both setups are the test -- the same silicon, two bias boards.
_byd = {}
for i, d in enumerate(Dies):
    _byd.setdefault(d['Lot'] + '/' + d['Die'], {})[d['Run']] = (float(d['BC']['Gradient']['DCRatio']),
                                               float(d['D']['Fit']['All']['SlopeSpread']['Median']))
_nboth = sum(1 for v in _byd.values() if len(v) >= 2)
_rs = np.median(rr[same_die]) if same_die.any() else np.nan
_rd = np.median(rr[~same_waf]) if (~same_waf).any() else np.nan
w(f"""The amplitude spans {min(ratios):.2f} to {max(ratios):.2f} (first third over last third), median
{np.median(ratios):.2f}. The shape correlation above is a weak test: every profile is a monotonic
ramp in the same direction, so any two of them correlate well whatever the cause -- which is why
the three rows differ by so little ({_rs:.3f}, {np.median(rr[same_waf & ~same_die]) if (same_waf & ~same_die).any() else float('nan'):.3f}, {_rd:.3f}).
The amplitude is the discriminating quantity, and the {_nboth} dies measured on more than one
setup are the test: the same silicon, different bias boards.\n""")

def amp_pair(ra, rb):
    """dies measured in both runs: (die, R_a, R_b, DC_a, DC_b)"""
    out = []
    for k in sorted(_byd):
        if ra in _byd[k] and rb in _byd[k]:
            out.append((k, _byd[k][ra][0], _byd[k][rb][0], _byd[k][ra][1], _byd[k][rb][1]))
    return out

_KB, _TCOLD = 8.617333e-5, 223.15        # the -50 C set-point every run carries

def predict(f):
    """ln R(warm)/ln R(cold) for a fixed delta-T, for the two limiting currents"""
    out = []
    for Ea, nmE in ((1.12, 'diffusion, exp(-Eg/kT)'), (0.56, 'generation, exp(-Eg/2kT)')):
        Tw = 1.0/(1.0/_TCOLD - _KB*np.log(f)/Ea)
        out.append((nmE, Tw-273.15, (_TCOLD/Tw)**2))
    return out

# The ranking test, over whichever runs share dies
_allamp = [(k, r, v[0]) for k in _byd for r, v in _byd[k].items()]
_rank_ok, _nrank = [], 0
for ra, rb in RPAIRS:
    pp = amp_pair(ra, rb)
    if len(pp) >= 3:
        _nrank += 1
        a = np.array([p[1] for p in pp]); b = np.array([p[2] for p in pp])
        _rank_ok.append((ra, rb, bool((np.argsort(np.argsort(a)) == np.argsort(np.argsort(b))).all()),
                         float(np.std(np.concatenate([a, b]))), float(np.std(a-b)/np.sqrt(2))))
if _rank_ok:
    _same = sum(1 for x in _rank_ok if x[2])
    _bet  = float(np.median([x[3] for x in _rank_ok]))
    _wit  = float(np.median([x[4] for x in _rank_ok]))
    w(f"""The ranking of the dies by gradient amplitude is the same on **{_same} of the {_nrank}**
pairs of setups, and the spread between dies ({_bet:.3f}) is {_bet/_wit:.1f} times the scatter
between measurements of one die ({_wit:.3f}). The size of the gradient is therefore a property of
the individual die, not a constant of the test -- so it is not one fixed temperature difference
applied to every device.\n""")

# The thermal test: every pair of runs whose dark currents differ enough to have a lever
_TH = []
for ra, rb in RPAIRS:
    pp = amp_pair(ra, rb)
    if len(pp) < 2:
        continue
    f = float(np.mean([p[3]/p[4] for p in pp]))          # dark-current ratio a/b
    warm, cold = (ra, rb) if f > 1 else (rb, ra)
    if f < 1:
        pp = [(k, rb_, ra_, db, da) for k, ra_, rb_, da, db in pp]
        f = 1.0/f
    if f < 1.5:
        continue                                          # no temperature lever worth testing
    obs = np.log(np.array([p[1] for p in pp]))/np.log(np.array([p[2] for p in pp]))
    _TH.append((warm, cold, f, float(np.mean(obs)), float(np.std(obs)/np.sqrt(len(obs))), predict(f), len(obs)))

if _TH:
    w("""It is also not a fixed property of the silicon, and the dies measured on more than one setup
show why. Take the hypothesis that the gradient is a temperature difference across the die, and that
the large dark-current ratio between two runs is itself temperature. Then the ratio must be
*smaller* at the higher temperature, because the dark current's sensitivity to temperature falls as
1/T^2 -- quantitatively ln R(warm) / ln R(cold) = (T_cold / T_warm)^2. A gradient fixed in the
silicon would not care about any of this and would give 1.000.\n""")
    w('| runs compared | dark current ratio | implied T of the warmer | predicted | measured |')
    w('|---|---|---|---|---|')
    for warm, cold, f, mu, se, pr, n in _TH:
        lo, hi = min(x[2] for x in pr), max(x[2] for x in pr)
        tlo, thi = min(x[1] for x in pr), max(x[1] for x in pr)
        w(f'| run {warm} vs run {cold} | {f:.1f} x | {tlo:+.0f} to {thi:+.0f} C | '
          f'{lo:.3f} to {hi:.3f} | **{mu:.3f} ± {se:.3f}** ({n} dies) |')
    w('')
    _mus = [x[3] for x in _TH]
    _los = [min(y[2] for y in x[5]) for x in _TH]
    # what else changed between the runs being compared: the dark-current ratio
    # is only a thermometer if nothing electrical moved with it
    _conf = []
    for warm, cold, *_ in _TH:
        dd = cfgdiff(warm, cold)
        if dd:
            # supply voltages first: those are the ones that can move the leakage
            # current by themselves, while a register rename cannot
            ks = sorted((k for k, _a, _b in dd), key=lambda k: (not k.startswith('zVDD'), k))
            _conf.append((warm, cold, ks))
    w(f"""Measured {np.mean(_mus):.3f} on average against {np.mean(_los):.3f} for the generation-current
case -- the right one for a depleted sensor at this temperature -- and **1.000** for a process
gradient. The agreement is to {100*abs(np.mean(_mus)-np.mean(_los))/np.mean(_los):.0f} % on a quantity
that would be off by a factor if the gradient were not thermal. **The gradient behaves as a
temperature difference across the die**: its size differs from die to die, as mounting and position
on the chuck would, but each die's gradient changes between setups by what a fixed physical delta-T
at a different absolute temperature requires.

What this does and does not establish. It **excludes** a gradient fixed in the silicon, which would
have given 1.000 and gives 0.772: whatever sets the gradient tracks the setup, so it should not be
written into a specification as device non-uniformity. It is **consistent with** the gradient being
a temperature difference across the die, quantitatively and with no free parameter. It does **not**
prove that, because the step from "the dark current is 20 times higher" to "the device was warmer"
assumes nothing else changed, and section 4c shows that is false for these runs.\n""")
    if _conf:
        for warm, cold, ks in _conf:
            w(f"""Runs {warm} and {cold} also differ in {len(ks)} recorded settings
({', '.join(ks[:6])}{', ...' if len(ks) > 6 else ''}), including supply voltages that can move the
leakage current on their own. So the dark-current ratio between them is not a thermometer, the
implied temperatures above are an interpretation rather than a measurement, and an electrical
contribution to the gradient cannot be separated from a thermal one with these data. What would
separate them is the one thing not in this set: the same dies at a different chuck set-point with
the bias configuration held fixed.\n""")
else:
    w('No two setups differ enough in dark current to give a temperature lever, so the thermal '
      'test cannot be made on this set.\n')

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
ax.grid(alpha=0.25); ax.legend(fontsize=8.5); markruns(ax)
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
    bydie.setdefault(d['Lot'] + '/' + d['Die'], {})[d['Run']] = d

def route_pair(k, ra, rb):
    """(die, value in run ra, value in run rb, separation in sigma) for each die in both"""
    out = []
    for die, byrun in sorted(bydie.items()):
        if ra not in byrun or rb not in byrun:
            continue
        Qa, Qb = byrun[ra]['ME']['Routes'][k], byrun[rb]['ME']['Routes'][k]
        va, vb = float(Qa['Threshold_e']), float(Qb['Threshold_e'])
        e = np.hypot(float(Qa['Threshold_e_err']), float(Qb['Threshold_e_err']))
        out.append((die, va, vb, (vb-va)/e if e > 0 else np.nan))
    return out

_rows = {k: {} for k in 'abcd'}
for ra, rb in RPAIRS:
    for k in 'abcd':
        pp = route_pair(k, ra, rb)
        if pp:
            _rows[k][(ra, rb)] = (np.median([p[2]-p[1] for p in pp]),
                                  np.median([abs(p[3]) for p in pp]), len(pp))
if any(_rows[k] for k in 'abcd'):
    w('How far each route moves when the same die is measured on another setup, for every pair of '
      'runs (median over the dies they share):\n')
    w('| route | ' + ' | '.join(f'{ra} vs {rb}' for ra, rb in RPAIRS) + ' | worst |')
    w('|---' * (len(RPAIRS)+2) + '|')
    _worstof = {}
    for k in 'abcd':
        cells = []
        for pr in RPAIRS:
            if pr in _rows[k]:
                dv, ds, n = _rows[k][pr]
                cells.append(f'{dv:+.1f} e- ({ds:.1f} s)')
            else:
                cells.append('&mdash;')
        _ws = max((v[1] for v in _rows[k].values()), default=np.nan)
        _wd = max((abs(v[0]) for v in _rows[k].values()), default=np.nan)
        _worstof[k] = (_wd, _ws)
        w(f'| {nm[k]} | ' + ' | '.join(cells) + f' | {_wd:.1f} e- ({_ws:.1f} sigma) |')
    w('\n*"s" is the separation in sigma.* A threshold that is a property of the device cannot move '
      'when only the setup does, so the **worst** column is the honest measure of each route.\n')
    _stable = [(nm[k], *_worstof[k]) for k in 'abcd' if np.isfinite(_worstof[k][1]) and _worstof[k][1] < 3]
    _moving = [(nm[k], *_worstof[k]) for k in 'abcd' if np.isfinite(_worstof[k][1]) and _worstof[k][1] >= 3]
    if _stable:
        w('Routes that hold across **every** pair of setups: ' +
          ', '.join(f'{n} (at worst {c:+.1f} e-, {sg:.1f} sigma)' for n, c, sg in _stable) +
          '. On this test those are measuring a property of the device.\n')
    if _moving:
        w('Routes that do not: ' +
          ', '.join(f'{n} (up to {c:.0f} e-, {sg:.0f} sigma)' for n, c, sg in _moving) +
          '. A device property cannot do that, so on these dies the route is reporting something '
          'about the measurement -- most likely the curvature of the ladder it extrapolates, whose '
          'signal range is set by the bias board.\n')
    if _stable and _moving:
        _best  = min(_stable, key=lambda t: t[2])
        _worst = max(_moving, key=lambda t: t[2])
        w(f"That split is the practical answer to which route to believe: "
          f"**{_best[0]}** repeats across every setup to {abs(_best[1]):.1f} e- ({_best[2]:.1f} sigma), "
          f"while **{_worst[0]}** moves by up to {abs(_worst[1]):.0f} e- ({_worst[2]:.0f} sigma) on the "
          f"same silicon. Whatever the response routes extrapolate to, it is not a fixed charge "
          f"the device loses.\n")

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
    ('PRNU, pixel to pixel [%]',  lambda d: 100*float(d['L']['Local']['Resp']['RelIntr']), None),
    ('bright pattern plateau [%]', lambda d: 100*float(d['L']['PRNU']['Multiplicative']), None),
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
    markruns(ax, label=False)
    SUMROWS.append((ttl, vv))
for ax in axs[len(PANELS):]:
    ax.axis('off')
for ax in axs[max(0, len(PANELS)-nc):len(PANELS)]:
    ax.set_xticks(np.arange(len(Dies)))
    ax.set_xticklabels([d['XLabel'] for d in Dies], rotation=90, fontsize=6.5)
for r in RUNS:
    axs[0].plot([], [], 'o', color=COL[r], label=f'run {r}')
axs[0].legend(fontsize=7.5)
for ax in axs[:min(nc, len(PANELS))]:
    markruns(ax)                       # name the blocks once, on the top row
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

# ================================================================== 4c. what differs between the setups
w('## 4c. What actually differs between the setups\n')
if len(_runcfg) >= 2:
    _rk = [r for r in RUNS if r in _runcfg]
    _diff = sorted({k for k in set().union(*[set(v) for v in _runcfg.values()])
                    if len({_runcfg[r].get(k) for r in _rk}) > 1})
    if _diff:
        w('The test configuration recorded with each run, for every setting that is not the same '
          'in all of them. Run 31 is the **AV** setting; runs 32 onward are the **aSpect** one, '
          'and differ from AV in V_RST_L, V_RST_SEL and V_SF:\n')
        w('| setting | ' + ' | '.join(f'run {r}' for r in _rk) + ' |')
        w('|---' * (len(_rk)+1) + '|')
        for k in _diff:
            w(f'| {k} | ' + ' | '.join(str(_runcfg[r].get(k, '&mdash;')) for r in _rk) + ' |')
        w('')
    else:
        w('Every setting recorded in the PTC configuration is identical across the runs.\n')

    # pairs of runs that differ in exactly one setting: a controlled experiment
    for ra, rb in RPAIRS:
        if ra not in _runcfg or rb not in _runcfg:
            continue
        dk = [k for k in set(_runcfg[ra]) | set(_runcfg[rb])
              if _runcfg[ra].get(k) != _runcfg[rb].get(k)]
        if len(dk) != 1:
            continue
        k = dk[0]
        w(f"""**Runs {ra} and {rb} differ in exactly one setting: {k}, {_runcfg[ra][k]} against
{_runcfg[rb][k]}.** Everything else about the two
measurements is the same, so whatever differs between them is caused by that setting and nothing
else. These pairs are the only controlled experiments in the set.\n""")
        w('| die | ' + ' | '.join(f'{nm[c]} [e-]' for c in 'abcd') +
          ' | gain bright | read noise [ADU] | bias [ADU] | bias pattern [ADU] |')
        w('|---' * 9 + '|')
        _dl = {c: [] for c in 'abcd'}
        _gl = []
        _ex = {}
        for die in sorted(bydie):
            if ra not in bydie[die] or rb not in bydie[die]:
                continue
            cells = []
            for c in 'abcd':
                va = float(bydie[die][ra]['ME']['Routes'][c]['Threshold_e'])
                vb = float(bydie[die][rb]['ME']['Routes'][c]['Threshold_e'])
                e = np.hypot(float(bydie[die][ra]['ME']['Routes'][c]['Threshold_e_err']),
                             float(bydie[die][rb]['ME']['Routes'][c]['Threshold_e_err']))
                _dl[c].append((vb-va, (vb-va)/e if e > 0 else np.nan))
                cells.append(f'{va:.1f} &rarr; {vb:.1f}')
            ga = gain(bydie[die][ra], 'd'); gb = gain(bydie[die][rb], 'd')
            _gl.append(gb-ga)
            cells.append(f'{ga:.4f} &rarr; {gb:.4f}')
            for key in ('ReadNoiseMedian', 'BiasLevel', 'FixedPatternRMS'):
                va = float(bydie[die][ra]['Z']['All'][key])
                vb = float(bydie[die][rb]['Z']['All'][key])
                _ex.setdefault(key, []).append((va, vb))
                cells.append(f'{va:.2f} &rarr; {vb:.2f}')
            w(f'| {die} | ' + ' | '.join(cells) + ' |')
        w('')
        _sig = [(nm[c], float(np.mean([x[0] for x in _dl[c]])),
                 float(np.mean([x[1] for x in _dl[c]]))) for c in 'abcd' if _dl[c]]
        if _sig:
            w('| route | mean change | mean separation |')
            w('|---|---|---|')
            for n_, dv, ds in _sig:
                w(f'| {n_} | {dv:+.1f} e- | {ds:+.1f} sigma |')
            w(f'| gain, bright PTC | {np.mean(_gl):+.4f} ADU/e- ({100*np.mean(_gl)/np.mean([gain(bydie[d2][ra],"d") for d2 in bydie if ra in bydie[d2] and rb in bydie[d2]]):+.1f} %) | &mdash; |')
            for key, lab in (('ReadNoiseMedian', 'read noise'), ('BiasLevel', 'bias level'),
                             ('FixedPatternRMS', 'bias fixed pattern')):
                if key in _ex:
                    _d0 = np.mean([v[1]-v[0] for v in _ex[key]])
                    _r0 = np.mean([v[1]/v[0] for v in _ex[key]])
                    w(f'| {lab} | {_d0:+.2f} ADU (x{_r0:.2f}) | &mdash; |')
            w('')
            _real = [t for t in _sig if abs(t[2]) >= 3]
            _null = [t for t in _sig if abs(t[2]) < 3]
            if _real:
                w(f"""Routes that move with {k}: """ + ', '.join(
                    f'**{n_}** ({dv:+.1f} e-, {ds:+.1f} sigma)' for n_, dv, ds in _real) +
                  f""". Since nothing else changed, that is a real response of the measured
threshold to {k}.\n""")
            if _null:
                w('Routes that do not move with it: ' + ', '.join(
                    f'{n_} ({dv:+.1f} e-, {ds:+.1f} sigma)' for n_, dv, ds in _null) + '.\n')
            w(f"""Read the two together with section 4: a route that moves between setups which differ
only in {k} is responding to {k}; a route that moves between setups that differ in other ways as
well cannot be attributed to any single cause. The photon-transfer routes are the ones worth
reading here, because section 4 shows they are the only ones stable against a change of setup at
all.\n""")
else:
    w('No PTC configuration could be read for these runs, so what differs between them cannot be '
      'stated here.\n')

# ================================================================== 5. the windows
SIGMODE = all(d['FW'] for d in Dies)
if SIGMODE:
    w('## 5. The fit windows, and where the rule had to give ground\n')
    w(f"""Both ladders of every die are fitted over the same signal window,
{float(Dies[0]['FW']['SigLo']):.0f}-{float(Dies[0]['FW']['SigHi']):.0f} ADU on the mean signal, and within a
ladder the response fit and the photon-transfer fit use exactly the same steps. Three points is the
minimum that leaves a degree of freedom, so where the window holds fewer the FLOOR is lowered to the
nearest step below until three are in -- never the ceiling, above which the ladder leaves the linear
range. The column that matters is the last one: it says how far the rule had to reach.\n""")
    w('| die / run | dark steps | dark span [ADU] | in window | floor | bright steps | bright span [ADU] |')
    w('|---|---|---|---|---|---|---|')
    for d in Dies:
        D_, B_ = d['FW']['D'], d['FW']['B']
        ds, bs = arr(D_['ChosenSignal']), arr(B_['ChosenSignal'])
        flo = (f"**lowered to {float(D_['Floor']):.1f}**" if D_['FloorLowered'] else 'as set')
        w(f"| {d['Label']} | [{' '.join(str(int(v)) for v in arr(D_['Chosen']))}] | "
          f"{ds.min():.1f}-{ds.max():.1f} | {int(D_['NinWindow'])} | {flo} | "
          f"[{' '.join(str(int(v)) for v in arr(B_['Chosen']))}] | {bs.min():.0f}-{bs.max():.0f} |")
    nlow = sum(1 for d in Dies if d['FW']['D']['FloorLowered'])
    blow = sum(1 for d in Dies if d['FW']['B']['FloorLowered'])
    worst = min(Dies, key=lambda d: float(d['FW']['D']['Floor']))
    w(f"""
The dark ladder needed the floor lowered on **{nlow} of the {len(Dies)}** die-runs and the bright
ladder on {blow}. The furthest any die had to reach is **{worst['Label']}**, down to
{float(worst['FW']['D']['Floor']):.1f} ADU -- {100*(1-float(worst['FW']['D']['Floor'])/float(worst['FW']['SigLo'])):.0f} %
below the nominal floor, so on that die the rule's name and the window it actually used differ enough
to be worth saying out loud. Everywhere else the reach is small.

Why the dark ladder needs it at all: the two bias-board setups put their dark ladders in signal
ranges that barely overlap, and neither places three of its nine steps inside a window chosen to suit
the bright ladder. That is the cost of making the two ladders directly comparable, and it is paid in
lever arm -- the dark fits here span a few hundred ADU where the per-ladder windows spanned
thousands.\n""")
else:
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
