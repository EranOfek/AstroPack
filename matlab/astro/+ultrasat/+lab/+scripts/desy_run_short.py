#!/usr/bin/env python3
"""A short report on one aSpect wafer-tester run: several dies, one temperature.

The same analysis the DESY uc_data-test units receive - read noise and dark current in
electrons, the four routes to the gain and the intercept over one shared signal window,
and for each ladder the photon-transfer curve fitted twice and both ways of compensating
what is missing at the bottom. The figure and the measured quantities come from
desy_ptcfigs, which the comparison documents also use, so this report cannot quote a
different number from the per-die reports it is built on.

Example:
  desy_run_short.py --run 41 --lot TH02260 \
      --folder LOT_TH02260_41_sFT_PTCint_-50_2026-10-22 \
      --units run41_TH02260_W01_D04_high_sig,run41_TH02260_W03_D04_high_sig

Author : A.M. Krassilchtchikov (Oct 2026)
"""
import argparse
import collections
import glob
import os
import re
import sys

import numpy as np

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import desy_ptcfigs as PF

P = argparse.ArgumentParser(description=__doc__)
P.add_argument('--run', required=True, help='run number, as the chain names it')
P.add_argument('--lot', default='TH02260')
P.add_argument('--folder', required=True, help='run directory under the DESY root')
P.add_argument('--units', required=True, help='comma-separated unit tags, in order')
P.add_argument('--dieroot', default='/home/sasha/claude/desy_die')
P.add_argument('--dataroot', default='/Data1/DESY')
P.add_argument('--test', default='PTC_int_hr', help='the measurement directory in each die')
P.add_argument('--out', default=None, help='output directory (default desy_die/run<run>_short)')
A = P.parse_args()
OUT = A.out or os.path.join(A.dieroot, f'run{A.run}_short')
os.makedirs(OUT, exist_ok=True)
TAGS = [t for t in A.units.split(',') if t.strip()]

SETPOINT = None
_m = re.search(r'_(-?\d+)_\d{4}-\d\d-\d\d$', A.folder)
if _m:
    SETPOINT = float(_m.group(1))


def dieof(tag):
    """W01_D04 out of run41_TH02260_W01_D04_high_sig"""
    m = re.search(r'_(W\d+_D\d+)_', tag)
    return m.group(1) if m else tag


def frames(die):
    """what the run actually wrote for one die, by role and step."""
    d = os.path.join(A.dataroot, A.folder, f'LOT_{A.lot}_{die}', A.test)
    out = collections.defaultdict(set)
    n = collections.Counter()
    for f in glob.glob(os.path.join(d, '*.tif')):
        m = re.search(r'_#(\d+)_([A-Z]+)_(\d+)\.tif$', os.path.basename(f))
        if not m:
            continue
        out[m.group(2)].add(int(m.group(1)))
        n[m.group(2)] += 1
    return {k: (len(v), n[k]) for k, v in out.items()}


def pm(v, e, nd):
    if v is None:
        return '—'
    if e is None or not np.isfinite(float(e)) or float(e) <= 0:
        return f'{float(v):.{nd}f}'
    return f'{float(v):.{nd}f} ± {float(e):.{nd}f}'


D = []
for t in TAGS:
    s = PF.datasheet(A.dieroot, t)
    if s.get('d') is None:
        print(f'  ! {t}: no four-route output yet, skipped')
        continue
    s['Die'] = dieof(t)
    s['Frames'] = frames(s['Die'])
    D.append(s)
if not D:
    raise SystemExit('no reduced unit of this run found under %s' % A.dieroot)

L = []


def w(t=''):
    L.append(t)


NDIE = len(D)
w(f'# Run {A.run}, lot {A.lot} — {NDIE} dies at one temperature\n')
w(f'*The aSpect wafer-tester run `{A.folder}`, reduced exactly as the DESY `uc_data-test` '
  f'measurements were: one signal window of {PF.SIGLO:.0f}–{PF.SIGHI:.0f} ADU shared by all four '
  f'routes, the same per-pixel chain, and the same two readings of the low-signal bend. '
  f'{"Every die was measured at the same nominal temperature, so there is no temperature axis." if SETPOINT is not None else ""} '
  f'Every number below is read at build time from that die\'s own dumps.*\n')

w('## 1. What was measured\n')
_f = D[0]['Frames']
_b = _f.get('B', (0, 0))
_d = _f.get('D', (0, 0))
_z = _f.get('ZE', (0, 0))
w(f'Each die carries an intensity-stepped **light ladder** of {_b[0]} steps '
  f'({_b[1]} frames), a **dark ladder** of {_d[0]} steps ({_d[1]} frames) and '
  f'{_z[1]} **zero-exposure frames**. The light is a flat field over the whole die, so every '
  f'stage runs over all {"22.5 M" if True else ""} pixels of the gain half rather than a box. '
  + (f'The run name states a set point of {SETPOINT:+.0f} C; the wafer tester records no '
     f'per-die thermometry, so that is a commanded value, not a measurement.\n'
     if SETPOINT is not None else '\n'))
w()
w('| die | light ladder | dark ladder | zero frames |')
w('|---|---|---|---|')
for s in D:
    f = s['Frames']
    def cell(k):
        v = f.get(k)
        return '—' if not v else f'{v[0]} steps, {v[1]} frames'
    w(f'| `{s["Die"]}` | {cell("B")} | {cell("D")} | {f.get("ZE", (0, 0))[1]} |')
w()

w('## 2. What the four routes measure\n')
w('| die | read noise [e-] | dark current [e-/s] | a: dark response | b: light response | '
  'c: PTC dark | d: PTC bright | gain c [ADU/e-] | gain d [ADU/e-] |')
w('|---|---|---|---|---|---|---|---|---|')
for s in D:
    w(f'| `{s["Die"]}` | {pm(s["RN_e"], s["RN_e_err"], 3)} | {pm(s["DC_e"], s["DC_e_err"], 4)} | '
      f'{pm(s["a"], s["a_err"], 1)} | {pm(s["b"], s["b_err"], 1)} | {pm(s["c"], s["c_err"], 1)} | '
      f'{pm(s["d"], s["d_err"], 1)} | {pm(s["gc"], s["gc_err"], 3)} | '
      f'{pm(s["gd"], s["gd_err"], 3)} |')
w()
w('*Intercepts in electrons, gains in ADU per electron, statistical and systematic errors '
  'added in quadrature. The intercept is what a straight line fitted to a ladder gives where '
  'that ladder reaches zero signal: the line reaches zero at *minus* the quoted value, so a '
  'positive number is charge that appears to be missing at the bottom.*\n')


def spread(key):
    v = [float(s[key]) for s in D if s.get(key) is not None]
    if len(v) < 2:
        return None
    return min(v), max(v), 100.0 * (max(v) - min(v)) / abs(np.mean(v))


w('**Across the dies.**\n')
for key, name, nd in (('RN_e', 'read noise', 3), ('DC_e', 'dark current', 4),
                      ('gd', 'gain from the bright photon-transfer curve', 3),
                      ('d', 'intercept from the bright photon-transfer curve', 1),
                      ('b', 'intercept from the light response', 1)):
    sp = spread(key)
    if sp:
        w(f'- **{name}**: {sp[0]:.{nd}f} to {sp[1]:.{nd}f}, a spread of {sp[2]:.0f} per cent '
          f'over the {NDIE} dies.')
w()

w('## 3. The photon-transfer curves, fitted two ways\n')
w(f'One figure per die, the dark ladder on the left and the bright one on the right. The top '
  f'panel is the photon-transfer curve itself, the variance above the read noise against the '
  f'mean signal, with two straight lines: in **red** the fit over the window the four routes '
  f'share, drawn over the very points that produced it, which are ringed; in **blue dashed** '
  f'the same estimator given every point from 0 to {PF.FULLHI:.0f} ADU. The next two panels '
  f'give what each line leaves behind, in per cent.\n')
w(f'The **bottom panel** puts the two compensations side by side: in **red** a constant shift, '
  f'the same charge missing at every signal, which is what an intercept means; in **green** the '
  f'low-count model `Q_out = Q·exp(−D/Q)`, in which the missing charge grows towards low '
  f'signal. D is not fitted here but read from the fit that die\'s own report carries, and '
  f'where that fit is poor the markers are left open and its scatter is quoted. A perfect '
  f'compensation would put its points on zero all the way down.\n')
for s in D:
    fn = f'fig_ptc_{s["Tag"]}.png'
    nt = PF.ptcfig(s['Tag'], A.dieroot, os.path.join(OUT, fn))
    cap = f'**`{s["Die"]}`** — dark ladder left, bright ladder right.'
    if nt:
        cap += ' Note: ' + '; '.join(sorted(set(nt))) + '.'
    w(f'![{cap}]({fn})\n')
    w(f'*{cap}*\n')

md = '\n'.join(L) + '\n'
with open(os.path.join(OUT, 'short.md'), 'w') as fh:
    fh.write(md)

HTML = """<!doctype html><html lang="en"><head><meta charset="utf-8">
<meta name="viewport" content="width=device-width,initial-scale=1"><title>__TITLE__</title>
<style>
body{max-width:1250px;margin:1.2rem auto;padding:0 1rem;font:13px/1.45 -apple-system,Segoe UI,Roboto,sans-serif;color:#222}
h1{font-size:19px;border-bottom:2px solid #ddd;padding-bottom:.2rem;margin:0 0 .4rem}
h2{font-size:14px;margin:.9rem 0 .3rem;border-bottom:1px solid #eee;page-break-after:avoid}
table{border-collapse:collapse;font-size:10.5px;margin:.4rem 0;page-break-inside:avoid}
th,td{border:1px solid #ddd;padding:2px 5px;text-align:left;vertical-align:top}
th{background:#f5f5f5}
code{background:#f5f5f5;padding:0 3px}
em{color:#555}
img{max-width:100%;margin:.4rem 0;page-break-inside:avoid}
@media print{img{max-height:17.6cm;width:auto;display:block;margin:.15rem auto}}
@page{size:A4 landscape;margin:8mm 7mm}
@media print{body{max-width:none;margin:0;font-size:10px}table{font-size:8.8px}}
</style></head><body>
<div id="c"></div>
<script type="text/markdown" id="src">
__MD__
</script>
<script src="https://cdnjs.cloudflare.com/ajax/libs/marked/9.1.6/marked.min.js"></script>
<script>document.getElementById('c').innerHTML = marked.parse(document.getElementById('src').textContent);</script>
</body></html>"""
with open(os.path.join(OUT, 'short.html'), 'w') as fh:
    fh.write(HTML.replace('__TITLE__', f'Run {A.run} short report')
                 .replace('__MD__', md.replace('</script', '<\\/script')))
print(f'short.md / .html -> {OUT} ({len(md.split())} words, {NDIE} dies)')
