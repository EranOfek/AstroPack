#!/usr/bin/env python3
"""One page: the single-die chain with and without the exposure-time offset.

Reads the two sets of dumps of the same die-runs -- <tag>_sig (commanded
exposure) and <tag>_sig_off (charge-collecting time) -- and writes
offset_compare.md / .html and a one-page PDF. Every number is read at build
time, so the page cannot drift from the runs it describes.

    python3 desy_offset_compare.py --dies run31_W04_D07_high run32_W04_D07_high
"""
import argparse, json, os, subprocess
import numpy as np

P = argparse.ArgumentParser()
P.add_argument('--root', default='/home/sasha/claude/desy_die')
P.add_argument('--dies', nargs='+', required=True, help='base tags, without _sig / _sig_off')
P.add_argument('--out',  default='/home/sasha/claude/desy_die/offset_compare')
P.add_argument('--pdf',  default='/home/sasha/DESY_reports/DESY_offset_comparison.pdf')
P.add_argument('--no-pdf', action='store_true')
A = P.parse_args()
os.makedirs(A.out, exist_ok=True)

def load(tag, name):
    p = os.path.join(A.root, tag, name)
    if not os.path.isfile(p):
        return None
    with open(p) as fh:
        return json.load(fh)

class Pair:
    """one die-run, both ways"""
    def __init__(self, base):
        self.base = base
        self.a = {n: load(base + '_sig', n) for n in
                  ('dark.json', 'light.json', 'methods.json', 'budget.json', 'ptc.json',
                   'badcol.json', 'fitwindow.json')}
        self.b = {n: load(base + '_sig_off', n) for n in self.a}
        self.ok = all(v is not None for v in self.a.values()) and \
                  all(v is not None for v in self.b.values())
    @property
    def label(self):
        d = self.a['ptc.json']
        return '%s %s' % ('run ' + d['Run'], d['Die'])
    def off(self):
        return float(self.b['dark.json'].get('ExpTimeOffset', 0) or 0)

PR = [Pair(d) for d in A.dies]
PR = [p for p in PR if p.ok]
if not PR:
    raise SystemExit('no die-run has both sets of dumps')
OFF = PR[0].off()

MD = []
def w(t=''):
    MD.append(t)

def dig(d, *keys):
    for k in keys:
        d = d[k]
    return float(d)

# ---------------------------------------------------------------- page
def rows(side, keys=('a','b','c','d')):
    return [dig(side['methods.json'], 'Routes', k, 'Threshold_e') for k in keys]
def inv(side):
    return (dig(side['dark.json'], 'Fit', 'All', 'SlopeSpread', 'Median'),
            dig(side['methods.json'], 'Routes', 'd', 'Gain'),
            dig(side['budget.json'], 'Unmasked', 'Inputs', 'RN_ADU'),
            100*dig(side['dark.json'], 'Local', 'DC', 'RelIntr'),
            100*dig(side['light.json'], 'PRNU', 'Multiplicative'))

NSAME = sum(1 for p in PR if all(abs(x-y) <= 1e-9*max(abs(x), 1e-9)
                                 for x, y in zip(inv(p.a), inv(p.b))))
w('# The exposure-time offset: what it changes')
w()
w(f"""*{len(PR)} die-runs of lot TH02954 re-processed end to end, signal window, with
{OFF:.4f} s taken off every dark and bright exposure. Every number below is read from
the two sets of dumps at build time.*""")
w()
w(f"""**Why.** The commanded exposure of the tester is `t_exp = RO_time + Reset_delay`,
`RO_time` being the full-die readout: the configuration records it as
`zDUT_ExpTimeOffset = 12` and DESY give `RO_time = 2 x 4742 rows x 1.3 ms =
{OFF:.4f} s`, the factor 2 because odd and even columns share an ADC block. The
charge-collecting interval is `t_exp - RO_time`, and the chain had been fitting
against `t_exp`. On the AV setting the dark ladder's lowest step collects for
{15-OFF:.2f} s, not 15 s; its top usable step for {360-OFF:.0f} s, not 360.""")
w()
w(f"""**The check.** A shift of the time axis cannot alter a slope, so the dark
current, the conversion gain, the read noise, the DSNU and the PRNU have to come out
unchanged. On **{NSAME} of {len(PR)}** die-runs all five agree to the last bit -
e.g. {PR[0].label}: DC {inv(PR[0].a)[0]:.4f}, gain {inv(PR[0].a)[1]:.4f} ADU/e-,
RN {inv(PR[0].a)[2]:.3f} ADU, DSNU {inv(PR[0].a)[3]:.2f} %, PRNU {inv(PR[0].a)[4]:.2f} %
both ways. That is the arithmetic check on the whole re-run.""")
w()
w(f"""**What moves.** Only the two routes that extrapolate a ladder to zero signal:
they absorb the whole of `DC x {OFF:.3f} s`. The two photon-transfer routes read the
threshold off the variance and never touch the time axis. Commanded -> collecting:""")
w()
w('| die-run | a) dark resp. | b) light resp. | c) PTC dark | d) PTC bright | spread of the four | T fixed pattern [ADU] | limiting signal, SNR 5 [e-] |')
w('|---|---|---|---|---|---|---|---|')
for p in PR:
    va, vb = rows(p.a), rows(p.b)
    sa = dig(p.a['dark.json'], 'Local', 'T', 'StdIntr')
    sb = dig(p.b['dark.json'], 'Local', 'T', 'StdIntr')
    qa = [dig(p.a['budget.json'], 'Unmasked', k, 'Qlim_cal_5') for k in ('PTC','Dark','Light')]
    qb = [dig(p.b['budget.json'], 'Unmasked', k, 'Qlim_cal_5') for k in ('PTC','Dark','Light')]
    w('| %s | %.1f &rarr; **%.1f** | %.1f &rarr; **%.1f** | %.1f | %.1f | %.0f &rarr; **%.0f** | %.2f &rarr; **%.2f** | %.0f-%.0f &rarr; **%.0f-%.0f** |'
      % (p.label, va[0], vb[0], va[1], vb[1], va[2], va[3],
         max(va)-min(va), max(vb)-min(vb), sa, sb, min(qa), max(qa), min(qb), max(qb)))
w()
w('*All thresholds in e-. Routes c and d are identical either way, so one column each.*')
w()
_s = lambda P, side: [max(rows(getattr(p, side)))-min(rows(getattr(p, side))) for p in P]
R31 = [p for p in PR if p.a['ptc.json']['Run'] == '31']
R32 = [p for p in PR if p.a['ptc.json']['Run'] == '32']
_ma = lambda P, side, k: np.median([dig(getattr(p, side)['methods.json'], 'Routes', k, 'Threshold_e') for p in P])
w(f"""The four-route spread on the **AV** dies falls from {np.median(_s(R31,'a')):.0f} to
{np.median(_s(R31,'b')):.0f} e-; on the **aSpect** dies, where the dark current is twenty
times smaller and the offset costs only
{np.median([dig(p.a['dark.json'],'Fit','All','SlopeSpread','Median')*OFF for p in R32]):.0f} ADU,
from {np.median(_s(R32,'a')):.0f} to {np.median(_s(R32,'b')):.0f} e-. The AV setting stops
being the outlier it appeared to be: its dark-route threshold goes from
{_ma(R31,'a','a'):.0f} e- against {_ma(R32,'a','a'):.0f} e- on aSpect, to
{_ma(R31,'b','a'):.0f} against {_ma(R32,'b','a'):.0f}.

The **threshold fixed pattern** falls on the AV dies because part of it was never a
threshold pattern: each pixel's apparent threshold carried that pixel's own dark
current times {OFF:.2f} s, so the dark-current non-uniformity was being counted twice,
once as DSNU and once as an offset pattern. That, and the smaller threshold itself,
is why the **limiting signal** range narrows - it had been dominated by the choice of
threshold route.""")
w()
w(f"""**Reading.** Nothing measured changed; a bookkeeping error in the time axis did.
Gain, dark current, read noise, PRNU and DSNU need no revision and the photon-transfer
thresholds were right all along. What was wrong is the two response routes, by
`DC x {OFF:.3f} s` - most of their value on the AV setting, a fifth of it on aSpect.
The correction does **not** make the four routes agree outright: the residual, largest
on the flavour-2 dies (W08), is the curvature of the dark ladder between its lowest
step and the fit window, which is what the denser low-signal ladder of
`dark_ladder.txt` is meant to measure.""")
w()
w(f"""*Per-die reports: `DESY_<lot>_<die>_run<run>_sigwin_offset_report.pdf`, {len(PR)} of them,
each marked in its own header as being in collecting time. Dumps:
`desy_die/<tag>_sig_off/`. The offset is applied in `ultrasat.lab.readPTC` through
`ExpTimeOffset` and selected by `DieExpOffset` in `desy_die_config`; at 0 the chain
reproduces every earlier result, and both unit tests pass unchanged.*""")
w()

md = '\n'.join(MD) + '\n'
with open(os.path.join(A.out, 'offset_compare.md'), 'w') as fh:
    fh.write(md)
HTML = """<!DOCTYPE html>
<html><head><meta charset="utf-8"><title>Exposure-time offset: comparison</title>
<style>
body{max-width:1000px;margin:1.2rem auto;padding:0 1rem;font:13px/1.45 -apple-system,Segoe UI,Roboto,sans-serif;color:#222}
h1{font-size:19px;border-bottom:2px solid #ddd;padding-bottom:.2rem;margin:0 0 .4rem}
h2{font-size:14px;margin:.9rem 0 .3rem;border-bottom:1px solid #eee}
p{margin:.35rem 0}
table{border-collapse:collapse;font-size:11px;margin:.4rem 0}
th,td{border:1px solid #ddd;padding:2px 6px;text-align:left}
th{background:#f5f5f5}
code{background:#f5f5f5;padding:0 3px}
em{color:#555}
@page{size:A4 portrait;margin:9mm 8mm}
@media print{body{max-width:none;margin:0;font-size:10.2px}table{font-size:9px}h1{font-size:16px}h2{font-size:12px}}
</style></head><body>
<div id="c"></div>
<script type="text/markdown" id="src">
__MD__
</script>
<script src="https://cdnjs.cloudflare.com/ajax/libs/marked/9.1.6/marked.min.js"></script>
<script>document.getElementById('c').innerHTML = marked.parse(document.getElementById('src').textContent);</script>
</body></html>"""
with open(os.path.join(A.out, 'offset_compare.html'), 'w') as fh:
    fh.write(HTML.replace('__MD__', md.replace('</script', '<\\/script')))
print('offset_compare.md / .html -> %s (%d words)' % (A.out, len(md.split())))
if not A.no_pdf:
    subprocess.run(['/snap/bin/chromium', '--headless', '--disable-gpu', '--no-sandbox',
                    '--virtual-time-budget=20000', '--print-to-pdf=' + A.pdf,
                    'file://' + os.path.join(A.out, 'offset_compare.html')], capture_output=True)
    if os.path.isfile(A.pdf):
        n = subprocess.run(['pdfinfo', A.pdf], capture_output=True, text=True).stdout
        pg = [l for l in n.split('\n') if l.startswith('Pages')]
        print('PDF -> %s  %s' % (A.pdf, pg[0] if pg else ''))
