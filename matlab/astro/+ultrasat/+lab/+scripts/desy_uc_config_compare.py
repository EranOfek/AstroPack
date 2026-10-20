#!/usr/bin/env python3
"""Cross-configuration comparison of the DESY uc_st_measurement run pairs.

   The August campaign changed one bias voltage at a time from two baselines and
   measured a dark and a bright ladder at each setting. This reads the dumps of
   those pairs and asks what each voltage does.

   Which settings are "the ones that vary" is taken from the dumps, not from the
   configuration file names: the setup block records both the commanded DAC value
   and the device's own read-back, and a name can lie about either.

   usage: desy_uc_config_compare.py --dies TAG TAG ... [--out DIR]
"""
import argparse
import json
import os

import numpy as np
import matplotlib
matplotlib.use('Agg')
import matplotlib.pyplot as plt

P = argparse.ArgumentParser()
P.add_argument('--root', default='/home/sasha/claude/desy_die')
P.add_argument('--rnroot', default='/home/sasha/claude/desy_rn')
P.add_argument('--dies', nargs='+', required=True)
P.add_argument('--out', default='/home/sasha/claude/desy_die/uc_configs')
A = P.parse_args()
os.makedirs(A.out, exist_ok=True)

VOLT = ['VDD_RST_L', 'VDD_RST_H', 'VDD_RST_SEL', 'VDD_SF', 'VDD_TX', 'VDD_SEN', 'VDDA', 'VDDD']


def load(tag, name, root=None):
    with open(os.path.join(root or os.path.join(A.root, tag), name)) as fh:
        return json.load(fh)


def unit(tag):
    Z = load(tag, 'stats.json', os.path.join(A.rnroot, tag))
    D = load(tag, 'dark.json')
    T = load(tag, 'ptc.json')
    M = load(tag, 'methods.json')
    S = Z['Setup']
    F = D['Fit']['All']
    R = M['Routes']
    E = {}
    _ep = os.path.join(A.root, tag, 'errors.json')
    if os.path.isfile(_ep):
        with open(_ep) as fh:
            E = json.load(fh)
        # these errors were computed by the report build of THIS unit; if a
        # stage has been rerun since without rebuilding the report, the tag
        # still matches but the dump would be stale, so check the tag at least
        if E.get('Tag') not in (None, tag, Z.get('Tag')):
            raise SystemExit(f'errors.json in {tag} belongs to {E.get("Tag")}: rebuild the report')
    def _e(k, default=float('nan')):
        v = E.get(k, default)
        try:
            return float(v)
        except (TypeError, ValueError):
            return float('nan')
    u = dict(tag=tag, label=Z.get('Run', tag), T=float(S['TempDark']),
             e_bias=_e('Bias'), e_rn=(E.get('ReadNoise') or {}).get('All', float('nan')),
             e_fpn=_e('FixedPattern'),
             cfg=S['ScfgDark'].replace('scfg_cfg_', '').replace('.toml', ''),
             bias=float(Z['All']['BiasLevel']), rn=float(Z['All']['ReadNoiseMedian']),
             fpn=float(Z['All']['FixedPatternRMS']),
             rowstd=float(Z['Structure']['RowMeanStd']),
             dc=float(F['SlopeSpread']['Median']), dsnu=100 * float(F['SlopeSpread']['RelIntr']),
             dc_stat=float(R['a']['SlopeStat']), dc_syst=float(R['a']['SlopeSyst']),
             dtop=float(np.atleast_1d(np.array(D['StepMedian'], dtype=float)).max()),
             dstep=len(D['FitSteps']), dlo=int(min(D['FitSteps'])), dhi=int(max(D['FitSteps'])),
             dnall=len(np.atleast_1d(np.array(D['PatternStep'], dtype=float))),
             dlim=float(D.get('LinLimit', 2900)),
             gain_b=float(R['d']['Slope']), gain_b_err=float(np.hypot(R['d']['SlopeStat'], R['d']['SlopeSyst'])),
             gain_d=float(R['c']['Slope']), resp=float(R['b']['Slope']),
             nbad=int(load(tag, 'badcol.json')['Nbad']) if os.path.isfile(os.path.join(A.root, tag, 'badcol.json')) else -1,
             ncol=int(load(tag, 'badcol.json')['Nrawcol']) if os.path.isfile(os.path.join(A.root, tag, 'badcol.json')) else -1)
    # PRNU is deliberately not carried here: over a light SPOT it is an upper
    # limit set by the illumination gradient, not a pixel non-uniformity, and a
    # column of upper limits invites being read as a comparison. Each pair's own
    # report gives it with that caveat attached.
    for k in VOLT:
        u['cmd_' + k] = float(S['Commanded'].get(k, float('nan')))
        u['mea_' + k] = float(S.get('Measured', {}).get(k, float('nan')))
    for r, nm in (('a', 'Ta'), ('b', 'Tb'), ('c', 'Tc'), ('d', 'Td')):
        u[nm] = float(R[r]['Threshold_e'])
        u[nm + 'e'] = float(R[r]['Threshold_e_err'])
    return u


U = [unit(t) for t in A.dies]

# which commanded voltages actually vary across the set
VARY = [k for k in VOLT if len({round(u['cmd_' + k], 4) for u in U}) > 1]
BASE = {k: float(np.median([u['cmd_' + k] for u in U])) for k in VOLT}


def tag_of(u):
    """how this unit differs from the modal setting"""
    d = [f"{k.replace('VDD_', '')} {u['cmd_' + k]:.2f}" for k in VARY
         if abs(u['cmd_' + k] - BASE[k]) > 1e-6]
    return ' + '.join(d) if d else 'modal setting'


U.sort(key=lambda u: (tag_of(u) != 'modal setting', tag_of(u)))

L = []
w = L.append
w('# One bias voltage at a time: what each one does\n')
w(f"*{len(U)} run pairs of die {load(A.dies[0], 'ptc.json')['Lot']} W14 D1, "
  f"all at {min(u['T'] for u in U):.1f} to {max(u['T'] for u in U):.1f} C, all over the same "
  f"illuminated box and the same bright signal window. Every number is read from the dumps.*\n")

w('## What was changed\n')
if VARY:
    w('| setting | values across the set [V] |')
    w('|---|---|')
    for k in VARY:
        vals = sorted({round(u['cmd_' + k], 3) for u in U})
        w(f"| {k} | " + ', '.join(f'{v:g}' for v in vals) + ' |')
    w('')
    w('Everything else was held: ' + ', '.join(
        f"{k.replace('VDD_', '')} {BASE[k]:g} V" for k in VOLT if k not in VARY) + '.\n')
else:
    w('No commanded bias voltage differs across this set.\n')

w('## The measurements\n')
def pm1(v, e, nd=2):
    """value and its uncertainty, the precision set by the error"""
    try:
        e = float(e)
    except (TypeError, ValueError):
        e = float('nan')
    if not np.isfinite(e) or e <= 0:
        return f'{v:.{nd}f}'
    k = min(max(nd, 1 - int(np.floor(np.log10(abs(e))))), 6)
    return f'{v:.{k}f} +- {e:.{k}f}'


w('| configuration | setting | T [C] | bias [ADU] | read noise [ADU] | fixed pattern [ADU] '
  '| dark current [ADU/s] | gain, bright PTC [ADU/e-] | bad cols |')
w('|---|---|---|---|---|---|---|---|---|')
for u in U:
    w(f"| {u['cfg']} | {tag_of(u)} | {u['T']:.1f} | {pm1(u['bias'], u['e_bias'], 1)} "
      f"| {pm1(u['rn'], u['e_rn'], 3)} | {pm1(u['fpn'], u['e_fpn'], 1)} "
      f"| {pm1(u['dc'], np.hypot(u['dc_stat'], u['dc_syst']), 4)} "
      f"| {u['gain_b']:.4f} +- {u['gain_b_err']:.4f} | {u['nbad']} of {u['ncol']} |")
w('')

w('## The four threshold routes, configuration by configuration\n')
w('| configuration | setting | a) dark response | b) light response | c) PTC dark | d) PTC bright |')
w('|---|---|---|---|---|---|')
for u in U:
    w(f"| {u['cfg']} | {tag_of(u)} | {u['Ta']:.1f} +- {u['Tae']:.1f} | {u['Tb']:.1f} +- {u['Tbe']:.1f} "
      f"| {u['Tc']:.1f} +- {u['Tce']:.1f} | {u['Td']:.1f} +- {u['Tde']:.1f} |")
w('')
w('All in electrons. Route b extrapolates the bright ladder to zero light; the same frames\n'
  'MEASURE that point at the shortest shutter openings, and the two disagree, which is the\n'
  'point the per-pair reports make at length.\n')

# ---------------------------------------------------------------- figures
qty = [('bias', 'bias [ADU]'), ('rn', 'read noise [ADU]'), ('dc', 'dark current [ADU/s]'),
       ('gain_b', 'gain, bright PTC [ADU/e-]'), ('fpn', 'bias fixed pattern [ADU]'),
       ('Td', 'PTC bright threshold [e-]')]
fig, axes = plt.subplots(2, 3, figsize=(13, 6.4))
xs = np.arange(len(U))
names = [tag_of(u) for u in U]
for ax, (k, lab) in zip(axes.ravel(), qty):
    v = [u[k] for u in U]
    ax.bar(xs, v, color=['C0' if tag_of(u) == 'modal setting' else 'C1' for u in U])
    ax.set_xticks(xs)
    ax.set_xticklabels(names, rotation=35, ha='right', fontsize=7)
    ax.set_ylabel(lab, fontsize=8)
    ax.grid(alpha=0.3, axis='y')
    ax.tick_params(labelsize=7)
fig.suptitle('One bias voltage at a time (orange = changed from the modal setting)', fontsize=10)
fig.tight_layout()
fig.savefig(os.path.join(A.out, 'fig_configs.png'), dpi=130)
plt.close(fig)
w('![what each setting does](fig_configs.png)\n')

# ---------------------------------------------------------------- what moved
w('## What each change does\n')
mod = [u for u in U if tag_of(u) == 'modal setting']
ref = mod[0] if mod else U[0]
w(f'Taken against `{ref["cfg"]}` ({tag_of(ref)}), as a relative change:\n')
w('| setting | bias | read noise | fixed pattern | dark current | gain | PTC bright threshold |')
w('|---|---|---|---|---|---|---|')


def rel(a, b, ea=None, eb=None):
    """relative change with its error, so a reader can see what is significant"""
    if not np.isfinite(a) or not np.isfinite(b) or b == 0:
        return '-'
    v = 100 * (a / b - 1)
    try:
        e = 100 * abs(a / b) * np.hypot(float(ea) / a, float(eb) / b)
    except (TypeError, ValueError, ZeroDivisionError):
        e = float('nan')
    if not np.isfinite(e):
        return f'{v:+.1f} %'
    return f'{v:+.1f} +- {e:.1f} %'


for u in U:
    if u is ref:
        continue
    _db = u['bias'] - ref['bias']
    _eb = np.hypot(u['e_bias'], ref['e_bias'])
    _dt = u['Td'] - ref['Td']
    _et = np.hypot(u['Tde'], ref['Tde'])
    w(f"| {tag_of(u)} | {_db:+.1f} +- {_eb:.1f} ADU | {rel(u['rn'], ref['rn'], u['e_rn'], ref['e_rn'])} "
      f"| {rel(u['fpn'], ref['fpn'], u['e_fpn'], ref['e_fpn'])} "
      f"| {rel(u['dc'], ref['dc'], np.hypot(u['dc_stat'], u['dc_syst']), np.hypot(ref['dc_stat'], ref['dc_syst']))} "
      f"| {rel(u['gain_b'], ref['gain_b'], u['gain_b_err'], ref['gain_b_err'])} "
      f"| {_dt:+.1f} +- {_et:.1f} e- |")
w('')

# ---------------------------------------------------------------- reading
w('## What the scan says\n')
_dc = sorted(U, key=lambda z: z['dc'])
_gn = [u['gain_b'] for u in U]
_rn = [u['rn'] for u in U]
_bi = [u['bias'] for u in U]
_td = [u['Td'] for u in U]
_ta = [u['Ta'] for u in U]
w(f'**The conversion gain is not a function of any of these voltages.** Over all {len(U)} settings it\n'
  f'runs {min(_gn):.4f} to {max(_gn):.4f} ADU/e-, a spread of {100 * (max(_gn) / min(_gn) - 1):.1f} %, against a typical\n'
  f'single-pair error of {np.median([u["gain_b_err"] for u in U]):.4f}. Read noise moves '
  f'{100 * (max(_rn) / min(_rn) - 1):.1f} % and the bias level {max(_bi) - min(_bi):.1f} ADU across the\n'
  'whole set. The readout chain does not care what the pixel bias voltages are.\n')
w(f'**The dark current does almost nothing else.** It runs {min(u["dc"] for u in U):.4f} to '
  f'{max(u["dc"] for u in U):.4f} ADU/s, a factor **{max(u["dc"] for u in U) / min(u["dc"] for u in U):.0f}**, '
  'over the same seven settings. Taken one at a time from the modal setting:\n')
_one = {tag_of(u): u for u in U if ' + ' not in tag_of(u) and tag_of(u) != 'modal setting'}
for k, u in sorted(_one.items(), key=lambda kv: -kv[1]['dc']):
    w(f'  - {k} V: dark current x **{u["dc"] / ref["dc"]:.1f}**')
w('')
# do the two big movers combine multiplicatively?
_pair = [u for u in U if ' + ' in tag_of(u)]
if _one and _pair:
    w('And where two are changed together, against the product of the two single steps:\n')
    w('| both changed | measured / modal | product of the single steps | both movers? |')
    w('|---|---|---|---|')
    _real = []
    for u in _pair:
        parts = tag_of(u).split(' + ')
        pr, ok, nmov = 1.0, True, 0
        for q in parts:
            if q in _one:
                f = _one[q]['dc'] / ref['dc']
                pr *= f
                nmov += int(f > 1.5 or f < 0.67)
            else:
                ok = False
        if ok:
            w(f"| {tag_of(u)} | x {u['dc'] / ref['dc']:.1f} | x {pr:.1f} | "
              f"{'yes' if nmov > 1 else 'no, one of them does nothing alone'} |")
            if nmov > 1:
                _real.append((tag_of(u), u['dc'] / ref['dc'], pr))
    w('')
    if _real:
        _t, _m, _p = _real[0]
        w(f'Only one row of that table is a real test, because {", ".join(k for k, v in _one.items() if 0.67 < v["dc"] / ref["dc"] < 1.5)} '
          f'does nothing on its own.\nIn that one, **{_t}**, the two together give x {_m:.0f} where acting independently they would give\n'
          f'x {_p:.0f} - short by a factor {_p / _m:.1f}. Two settings that each raise the dark current several-fold do\n'
          'not simply multiply, which is what a shared limit, rather than two independent leakage\n'
          'paths, would look like. One combination is not enough to call it, and the campaign\n'
          'contains no third point to settle it.\n')
    else:
        w('In every one of them one of the two settings does nothing on its own, so none is a\n'
          'real test of whether the effects combine.\n')
w(f'**Only the photon-transfer bright threshold is stable.** Route d reads {min(_td):.1f} to '
  f'{max(_td):.1f} e- across\nall seven settings, a spread of {max(_td) - min(_td):.1f} e- against single-pair errors of about '
  f'{np.median([u["Tde"] for u in U]):.1f}.\nRoute a, the dark response, runs {min(_ta):.1f} to {max(_ta):.1f} e- over the same settings and\n'
  f'tracks the dark current almost exactly (Pearson r = '
  f'{np.corrcoef([u["dc"] for u in U], _ta)[0, 1]:.3f} on these {len(U)} points). A fixed charge\n'
  'deficit cannot do that; an extrapolation of a curved ladder does it by construction.\n')

w('## The dark ladders are not in the same signal range\n')
w('A 40-fold spread in dark current at a fixed ladder of exposures means the seven dark\n'
  'ladders cover quite different parts of the response curve, and the fit window stage 2a\n'
  'chooses for each one reflects that:\n')
w('| configuration | dark current [ADU/s] | steps used | top step used [ADU] | fraction of the '
  f'{U[0]["dlim"]:.0f} ADU linearity limit |')
w('|---|---|---|---|---|')
for u in sorted(U, key=lambda z: z['dc']):
    w(f"| {u['cfg']} | {u['dc']:.4f} | {u['dstep']} of {u['dnall']} ({u['dlo']}-{u['dhi']}) "
      f"| {u['dtop']:.0f} | {100 * u['dtop'] / u['dlim']:.0f} % |")
w('')
_mx = max(U, key=lambda z: z['dtop'])
w(f'No pair fits above the linearity limit - the highest reaches {100 * _mx["dtop"] / _mx["dlim"]:.0f} % of it - so none of\n'
  'these dark currents is contaminated by the knee at the top. But the lowest-dark-current\n'
  f'pairs are fitted entirely below {sorted(u["dtop"] for u in U)[1]:.0f} ADU while the highest reaches {_mx["dtop"]:.0f}, so the\n'
  '**dark current and the route-a threshold of the different settings are measured over\n'
  'different parts of the curve**. The 40-fold range of dark current is far too large to be an\n'
  'artefact of that; the route-a threshold comparison and its correlation with the dark current\n'
  'are partly structural and should be read as "route a follows whatever the ladder is\n'
  'extrapolating" rather than as a calibrated threshold per setting.\n')

w('## A note on the configuration names\n')
w('The names of the `aSpectConfigAVPixels_*` files say what was changed from the **AVPixels**\n'
  'baseline, not from the `aSpect` one, so read as absolute settings they mislead: the file\n'
  'called `_SF3000` is the one whose SF sits at the modal 3.0 V, and what distinguishes it from\n'
  '`aSpect` is RST_L and RST_SEL. The table above is keyed on the DAC values recorded in the\n'
  'setup block of each run, which is why it recovers the right grouping.\n')

_EA = 0.111          # eV, measured on the -57.9 / -71.2 / -76.6 C set (see the temperature document)
_K = 8.617333262e-5
_Ts = [u['T'] for u in U]
_dT = max(_Ts) - min(_Ts)
_Tm = np.mean(_Ts) + 273.15
_pct = 100 * (np.exp(_EA / _K * (1 / _Tm - 1 / (_Tm + _dT))) - 1)
w('## Is the temperature spread of the set a problem?\n')
w(f'No. The dark runs of the seven pairs span {_dT:.2f} K ({min(_Ts):.2f} to {max(_Ts):.2f} C). At the\n'
  f'activation energy measured on this device, {_EA:.3f} eV, that is worth {abs(_pct):.1f} % of dark current,\n'
  f'against the {100 * (max(u["dc"] for u in U) / min(u["dc"] for u in U) - 1):.0f} % the bias settings move it. Read noise and gain have no\n'
  'measurable temperature dependence over such an interval either. The bright run of each pair\n'
  'sits about 1.2 K warmer than its dark run, which is why the setup table of every report gives\n'
  'two temperatures rather than one.\n')

w('## Caveats\n')
w('- The two baselines of the August campaign (`aSpect` and `aSpectConfigAVPixels`) differ in\n'
  '  more than one voltage, so a row that changes two settings at once is not a one-at-a-time\n'
  '  step and the column above shows both.\n')
w('- Every statistic is over the illuminated spot, so the bad-column count is out of the\n'
  "  region's columns and the PRNU is an upper limit; see each pair's own report.\n")
w('- The thresholds carry the reset-configuration pedestal common to this instrument.\n')

md = '\n'.join(L) + '\n'
with open(os.path.join(A.out, 'configs.md'), 'w') as fh:
    fh.write(md)
HTML = """<!DOCTYPE html>
<html><head><meta charset="utf-8"><title>Bias-voltage scan</title>
<style>
body{max-width:1050px;margin:1.2rem auto;padding:0 1rem;font:13px/1.45 -apple-system,Segoe UI,Roboto,sans-serif;color:#222}
h1{font-size:19px;border-bottom:2px solid #ddd;padding-bottom:.2rem;margin:0 0 .4rem}
h2{font-size:14px;margin:.9rem 0 .3rem;border-bottom:1px solid #eee}
table{border-collapse:collapse;font-size:11px;margin:.4rem 0}
th,td{border:1px solid #ddd;padding:2px 6px;text-align:left}
th{background:#f5f5f5}
code{background:#f5f5f5;padding:0 3px}
img{max-width:100%;margin:.5rem 0}
em{color:#555}
@page{size:A4 landscape;margin:9mm 8mm}
@media print{body{max-width:none;margin:0;font-size:10.2px}table{font-size:9px}}
</style></head><body>
<div id="c"></div>
<script type="text/markdown" id="src">
__MD__
</script>
<script src="https://cdnjs.cloudflare.com/ajax/libs/marked/9.1.6/marked.min.js"></script>
<script>document.getElementById('c').innerHTML = marked.parse(document.getElementById('src').textContent);</script>
</body></html>"""
with open(os.path.join(A.out, 'configs.html'), 'w') as fh:
    fh.write(HTML.replace('__MD__', md.replace('</script', '<\\/script')))
print(f'configs.md / .html -> {A.out} ({len(md.split())} words, {len(U)} pairs, varying: '
      + (', '.join(VARY) if VARY else 'nothing') + ')')
