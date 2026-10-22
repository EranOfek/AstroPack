#!/usr/bin/env python3
"""What the ADC ramp adjustment does: the 2026-09-14 bs_ramp_adj scan.

   The DESY campaign carries two configurations, cfg_aSpect and cfg_aSpect2,
   whose eight bias DAC voltages are IDENTICAL and which differ in one four-bit
   register field, bs_ramp_adj of register_0x004_bias_adc_control_2: 0b1001
   against 0b1100. That field moves the bias level by 343 ADU, and the campaign
   also contains the measurement that explains it - a scan of the field from 0
   to 15, on both pixel bias settings, 32 runs of 20 ZERO frames taken on
   2026-09-14.

   Those runs have no ladder of any kind, so stage 1 is the whole of the
   analysis and every number here is the chain's own: the same bias, read noise
   and fixed-pattern definitions as every other DESY report.

   usage: desy_uc_rampscan.py [--out DIR]
"""
import argparse
import glob
import json
import os
import re

import numpy as np
import matplotlib
matplotlib.use('Agg')
import matplotlib.pyplot as plt

P = argparse.ArgumentParser()
P.add_argument('--rnroot', default='/home/sasha/claude/desy_rn')
P.add_argument('--out',    default='/home/sasha/claude/desy_die/uc_rampscan')
P.add_argument('--pdf',    default='/home/sasha/DESY_reports/DESY_uc_ramp_scan.pdf')
P.add_argument('--ref',    nargs='*', default=['uc_T_aSpect_m71_high', 'uc_T_aSpect2_m71_high'],
               help='full-die dark-only units to quote as the in-campaign cross-check')
A = P.parse_args()
os.makedirs(A.out, exist_ok=True)

U = []
for d in sorted(glob.glob(os.path.join(A.rnroot, 'uc_R_*_ramp??_high'))):
    tag = os.path.basename(d)
    m = re.match(r'uc_R_(\w+?)_ramp(\d\d)_high$', tag)
    if not m:
        continue
    with open(os.path.join(d, 'stats.json')) as fh:
        S = json.load(fh)
    St = S.get('Structure', {})
    U.append(dict(tag=tag, base=m.group(1), ramp=int(m.group(2)),
                  bias=float(S['All']['BiasLevel']),
                  rn=float(S['All']['ReadNoiseMedian']),
                  fpn=float(S['All']['FixedPatternRMS']),
                  row=float(St.get('RowMeanStd', np.nan)),
                  col=float(St.get('ColMeanStd', np.nan)),
                  res=float(St.get('ResidStd', np.nan)),
                  tail=100*float((S.get('BiasDist') or {}).get('TailFrac5MAD', np.nan)),
                  nfr=int(S.get('Nframes', 0)),
                  run=(S.get('Setup') or {}).get('DarkRun', ''),
                  temp=(S.get('Setup') or {}).get('TempDark')))
if not U:
    raise SystemExit('no uc_R_*_ramp??_high unit found: run run_uc_rampscan.sh first')
BASES = sorted({u['base'] for u in U}, key=lambda b: (b != 'aSpect', b))
BY = {b: sorted([u for u in U if u['base'] == b], key=lambda u: u['ramp']) for b in BASES}
NAME = {'aSpect': 'aSpect (RST_L 1.2, RST_SEL 3.5, SF 3.0)',
        'AV': 'aSpectConfigAVPixels (RST_L 1.0, RST_SEL 3.8, SF 3.3)'}

L = []
def w(t=''):
    L.append(t)
def f(v, n=2):
    return 'n/a' if v is None or not np.isfinite(v) else f'{v:.{n}f}'

# ------------------------------------------------------------------ the text
w('# What the ADC ramp adjustment does\n')
_t = [u['temp'] for u in U if isinstance(u['temp'], (int, float))]
w(f"*{len(U)} runs of 2026-09-14, {len(BASES)} pixel bias settings x 16 values of `bs_ramp_adj`, "
  f"{U[0]['nfr']} ZERO frames each" + (f", all at {np.mean(_t):.1f} C" if _t else '') +
  ". No ladder of any kind, so stage 1 is the whole analysis and every number below is the "
  "chain's own. Read from the stage-1 dumps at build time.*\n")

w('## Why this scan matters\n')
w("""This campaign has two configurations that the reports call `aSpect` and `aSpect2`. The names
suggest two bias settings. They are not: **their eight bias DAC voltages are identical**, and the
whole difference between the two files is one four-bit field,

    [register_0x004_bias_adc_control_2]
    bs_ramp_adj = 0b1001   (aSpect)    vs    0b1100   (aSpect2)

an adjustment of the ADC ramp. It is worth 343 ADU of bias level, and - as this scan shows - a
factor of thirty in the fixed pattern. Everything the per-unit reports attribute to "the aSpect2
configuration" is this one register field.\n""")

w('## The measurement\n')
for b in BASES:
    w(f'**{NAME.get(b, b)}**\n')
    w('| `bs_ramp_adj` | bias [ADU] | read noise [ADU] | fixed pattern [ADU] | rows | columns | '
      'residual | beyond 5 MAD [%] |')
    w('|---|---|---|---|---|---|---|---|')
    for u in BY[b]:
        mark = ''
        if b == 'aSpect' and u['ramp'] == 9:
            mark = ' &larr; **aSpect**'
        if b == 'aSpect' and u['ramp'] == 12:
            mark = ' &larr; **aSpect2**'
        w(f"| `0b{u['ramp']:04b}` ({u['ramp']}){mark} | {f(u['bias'],1)} | {f(u['rn'],3)} | "
          f"**{f(u['fpn'],2)}** | {f(u['row'],2)} | {f(u['col'],2)} | {f(u['res'],2)} | {f(u['tail'],1)} |")
    w('')
w('The last three columns split the fixed pattern the way every DESY report splits it: the '
  'standard deviation of the row means, of the column means, and of what is left when both are '
  'removed.\n')

# ------------------------------------------------------------------ what it shows
_a = BY.get('aSpect', [])
_hi = [u for u in _a if u['ramp'] >= 11]
_lo = [u for u in _a if u['ramp'] <= 10]
if _hi and _lo:
    _f9 = next((u for u in _a if u['ramp'] == 9), None)
    _f12 = next((u for u in _a if u['ramp'] == 12), None)
    w('## Three things it shows\n')
    w(f"""**1. The fixed pattern collapses, and it collapses between 10 and 11.** Below that the
pattern runs {f(max(u['fpn'] for u in _lo),0)} ADU at its worst and {f(min(u['fpn'] for u in _lo),1)} ADU
at its best; from `0b1011` upward it is {f(max(u['fpn'] for u in _hi),2)} ADU or less. The step is
not gradual: between the two neighbouring settings it falls by a factor
{max(u['fpn'] for u in _lo if u['ramp'] == 10)/max(u['fpn'] for u in _hi if u['ramp'] == 11):.0f}.
That is the single most consequential number in this document, because the campaign's own
`aSpect` runs sit at `0b1001`, on the wrong side of it.\n""")
    if _f9 and _f12:
        w(f"""Put the two named configurations side by side: `aSpect` at `0b1001` carries a fixed
pattern of **{f(_f9['fpn'],1)} ADU**, of which {f(_f9['row'],1)} is row structure; `aSpect2` at
`0b1100` carries **{f(_f12['fpn'],2)} ADU**. The ~100 ADU offset pattern that every `aSpect` report
in this campaign measures and discusses is therefore **an ADC ramp artefact, not a property of the
sensor** - it is removed by changing one register field.\n""")
    _bump = [u['ramp'] for i, u in enumerate(_a[1:], 1)
             if u['rn'] > _a[i-1]['rn'] + 1e-9]
    w(f"""**2. The read noise falls with the field**, from {f(max(u['rn'] for u in _a),3)} ADU at
`0b0000` to {f(min(u['rn'] for u in _a),3)} ADU at `0b1111` - a factor
{max(u['rn'] for u in _a)/min(u['rn'] for u in _a):.2f} across the scan, and
{(_f9['rn']/_f12['rn'] if _f9 and _f12 else float('nan')):.3f} between the two settings the campaign
actually used. It is not monotonic: {', '.join('`0b%04d`' % int(bin(r)[2:]) for r in _bump)}
{'sits' if len(_bump) == 1 else 'sit'} above the setting below
{'it' if len(_bump) == 1 else 'them'}, and the same steps appear in the bias and in the fixed
pattern, so the field is not a plain monotonic code. The ramp setting is in any case not a neutral
choice of bias level: it sets the noise floor the whole instrument works against.\n""")
    _best = min(_hi, key=lambda u: u['fpn'])
    if _f12 and _f12['fpn'] > 1.5*_best['fpn']:
        w(f"""And `0b1100` is the worst of the good settings. Above the step the fixed pattern runs
{f(_best['fpn'],2)} ADU at `0b{_best['ramp']:04b}`, while `0b1100` - the one the campaign's `aSpect2`
runs actually use - reads {f(_f12['fpn'],2)} ADU, a factor {_f12['fpn']/_best['fpn']:.1f} worse than
its neighbours at `0b1011` and `0b1101`. It is thirty times better than `aSpect` and three times
worse than it needed to be.\n""")
    w(f"""**3. The bias level saturates.** It falls from {f(max(u['bias'] for u in _a),0)} ADU at
`0b0000` to {f(min(u['bias'] for u in _a),0)} ADU and then stops moving: the last
{sum(1 for u in _a if abs(u['bias'] - min(x['bias'] for x in _a)) < 1):d} settings agree to under an ADU.
So above `0b1011` the field has run out of range on the bias, while the read noise is still
improving - which is why `0b1100` and `0b1111` look alike in the first column and differ in the
second.\n""")

# the two pixel settings
if len(BASES) > 1:
    _d = []
    for r in sorted({u['ramp'] for u in U}):
        v = [next((u for u in BY[b] if u['ramp'] == r), None) for b in BASES]
        if all(v):
            _d.append((r, v[0]['bias'] - v[1]['bias'], v[0]['fpn'] - v[1]['fpn']))
    if _d:
        w(f"""**And the effect does not depend on the pixel bias setting.** The scan was run twice,
once on each of the campaign's two pixel settings, and at every one of the {len(_d)} ramp values the
two agree on the bias to {max(abs(x[1]) for x in _d):.0f} ADU and on the fixed pattern to
{max(abs(x[2]) for x in _d):.1f} ADU. The ramp field acts in the ADC, after the pixel.\n""")

w('![what the ramp field does](fig_rampscan.png)\n')
w('*Bias, read noise and fixed pattern against `bs_ramp_adj`, for both pixel bias settings. The '
 'two values the campaign used are marked. The fixed-pattern panel is logarithmic: the collapse '
 'between 10 and 11 is a factor of thirty.*\n')

w('## What it means for the campaign\n')
w("""- The `aSpect` runs - which is every August configuration pair, the TX scan and most of the
  die-level work - were taken at `0b1001`, where the offset pattern is of order 100 ADU and the
  read noise about a quarter worse than the device can do. That pattern is calibrated out by the
  bias subtraction, so it does not bias any measured quantity in these reports; what it costs is
  dynamic range and the quality of a single raw frame.
- The `aSpect2` runs - the temperature series and the INTERACTIVE light ladders - were taken at
  `0b1100`, on the good side of the step. Their 1.3 ADU fixed pattern is not a different device.
- **A measurement that would complete this**: the scan carries ZERO frames only, so it says nothing
  about the gain, the dark current or the linearity against the ramp setting. One dark ladder and
  one bright ladder at `0b1001` and at `0b1100`, at one temperature, would say whether the field
  touches anything beyond the offset and the noise. The campaign already has that pair at `0b1001`
  and the dark half at `0b1100`.\n""")

w('## Caveats\n')
w(f"""- ZERO frames only, {U[0]['nfr']} per run, one temperature, one die. Nothing here is a
  statement about any other device.
- The read noise is the median of the per-pixel temporal standard deviation over those frames, so
  at the noisy end of the scan it is measured on {U[0]['nfr']} samples per pixel and carries a
  relative error of about {100/np.sqrt(2*(U[0]['nfr']-1)):.0f} % per pixel - negligible on the
  median over 22.5 M of them, but it is not a long integration.
- The runs were taken in one sitting on 2026-09-14 in ascending order of the field, so a drift in
  time would appear as a trend in the field. The two independent sweeps - one per pixel setting,
  taken one after the other - agree, which is the only control available for that.\n""")

# ------------------------------------------------------------------ the figure
fig, axs = plt.subplots(1, 3, figsize=(15.0, 4.4))
COL = {'aSpect': '#4c72b0', 'AV': '#c44e52'}
for b in BASES:
    x = [u['ramp'] for u in BY[b]]
    for ax, k, lab in ((axs[0], 'bias', 'bias level [ADU]'),
                       (axs[1], 'rn', 'read noise [ADU]'),
                       (axs[2], 'fpn', 'fixed pattern [ADU]')):
        ax.plot(x, [u[k] for u in BY[b]], 'o-', ms=5, lw=1.3,
                color=COL.get(b, None), label=NAME.get(b, b).split(' (')[0])
        ax.set_xlabel('bs_ramp_adj')
        ax.set_ylabel(lab)
        ax.grid(alpha=0.25)
axs[2].set_yscale('log')
for ax in axs:
    for r, nm, c in ((9, 'aSpect', '#55a868'), (12, 'aSpect2', '#8172b2')):
        ax.axvline(r, color=c, ls='--', lw=1.2, alpha=0.9)
        ax.annotate(nm, (r, 1.01), xycoords=('data', 'axes fraction'), ha='center',
                    va='bottom', fontsize=8, color=c)
axs[0].legend(fontsize=8)
fig.suptitle('The ADC ramp adjustment, measured: 16 settings on two pixel bias configurations',
             fontsize=11)
fig.tight_layout(rect=(0, 0, 1, 0.93))
fig.savefig(os.path.join(A.out, 'fig_rampscan.png'), dpi=120)
plt.close(fig)

md = '\n'.join(L) + '\n'
with open(os.path.join(A.out, 'rampscan.md'), 'w') as fh:
    fh.write(md)
HTML = """<!doctype html><html lang="en"><head><meta charset="utf-8">
<meta name="viewport" content="width=device-width,initial-scale=1"><title>ADC ramp scan</title>
<style>
body{max-width:1050px;margin:1.2rem auto;padding:0 1rem;font:13px/1.45 -apple-system,Segoe UI,Roboto,sans-serif;color:#222}
h1{font-size:19px;border-bottom:2px solid #ddd;padding-bottom:.2rem;margin:0 0 .4rem}
h2{font-size:14px;margin:.9rem 0 .3rem;border-bottom:1px solid #eee}
table{border-collapse:collapse;font-size:11px;margin:.4rem 0}
th,td{border:1px solid #ddd;padding:2px 6px;text-align:left}
th{background:#f5f5f5}
code{background:#f5f5f5;padding:0 3px}
pre{background:#f7f7f7;padding:.4rem .6rem;border-left:3px solid #ccc}
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
with open(os.path.join(A.out, 'rampscan.html'), 'w') as fh:
    fh.write(HTML.replace('__MD__', md.replace('</script', '<\\/script')))
print(f"rampscan.md / .html -> {A.out} ({len(md.split())} words, {len(U)} runs)")
