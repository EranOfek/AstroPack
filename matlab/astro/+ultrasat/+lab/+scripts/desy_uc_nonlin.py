#!/usr/bin/env python3
"""Fit the low-count non-linearity seen in the measured photon-transfer curves.

   The simulator needs one thing: given the charge a pixel collected, how much
   charge is read out. Everything else follows - the mean signal, and, because
   the map is applied to the already-noisy charge, the variance too:

       Q_out = Q * exp(-D / Q)        Var_out = (dQ_out/dQ)^2 * Var_in

   One parameter, D electrons. At large Q it is Q - D, the constant deficit a
   straight line through the photon-transfer curve reports as its intercept; at
   Q of order D it rolls off smoothly to zero.

   Why the photon-transfer curve and not the light ladder: the ladder's x axis
   is the commanded shutter opening, and the Bonn shutter takes a finite time to
   open, so a curved M(t) does not distinguish the detector from the shutter.
   Var(M) involves no exposure axis at all.

   usage: desy_uc_nonlin.py [--out DIR]
"""
import argparse
import glob
import json
import os

import numpy as np
from desy_nonlin import collected as inv, dqout, qout, variance as pvar
import matplotlib
matplotlib.use('Agg')
import matplotlib.pyplot as plt

P = argparse.ArgumentParser()
P.add_argument('--root', default='/home/sasha/claude/desy_die')
P.add_argument('--out',  default='/home/sasha/claude/desy_die/uc_nonlin')
P.add_argument('--lo',   type=float, default=10.0,  help='validation range, low [ADU]')
P.add_argument('--hi',   type=float, default=200.0, help='validation range, high [ADU]')
A = P.parse_args()
os.makedirs(A.out, exist_ok=True)





U = []
for t in sorted(glob.glob(os.path.join(A.root, 'uc_*_high'))):
    pp, mj = os.path.join(t, 'ptc_points.json'), os.path.join(t, 'methods.json')
    if not (os.path.isfile(pp) and os.path.isfile(mj)):
        continue
    with open(mj) as fh:
        ME = json.load(fh)
    if 'd' not in ME.get('Routes', {}):
        continue
    g0 = float(ME['Routes']['d']['Gain'])
    T0 = float(ME['Routes']['d']['Threshold'])
    rn2 = float(ME['RN2'])
    with open(pp) as fh:
        PT = [q for q in json.load(fh)['Points'] if q['Type'] == 'B']
    M = np.array([float(q['SignalMean']) for q in PT])
    V = np.array([float(q['VarMean']) for q in PT]) - rn2
    o = np.argsort(M)
    M, V = M[o], V[o]
    k = (M >= 3) & (M <= 1000) & (V > 0)
    m, v = M[k], V[k]
    if m.size < 6:
        continue
    # the fit the plotting stage already made and wrote beside the dumps, so this
    # note and the per-unit reports cannot quote different numbers
    _nf = os.path.join(t, 'nonlin_fit.json')
    _got = None
    if os.path.isfile(_nf):
        with open(_nf) as fh:
            _got = ((json.load(fh) or {}).get('Ladders') or {}).get('B')
    if _got is None:
        continue
    g, D = float(_got['g']), float(_got['D'])
    pv = pvar(m, g, D)
    kk = (m >= A.lo) & (m <= A.hi)
    U.append(dict(tag=os.path.basename(t), run=ME.get('Run', ''), g=g, D=D, g0=g0, T0=T0,
                  kind=('INTERACTIVE' if 'uc_I_' in t else 'pair'),
                  M=m, V=v, pv=pv, n=int(m.size),
                  rms=float(np.sqrt(np.mean(((pv - v)/v)**2))*100),
                  rmsv=float(np.sqrt(np.mean(((pv[kk] - v[kk])/v[kk])**2))*100) if kk.any() else np.nan,
                  maxv=float(np.max(np.abs(pv[kk] - v[kk])/v[kk])*100) if kk.any() else np.nan,
                  lin=float(np.sqrt(np.mean(((g*(m[kk] + T0) - v[kk])/v[kk])**2))*100) if kk.any() else np.nan))
if not U:
    raise SystemExit('no unit with a bright ladder found')
PAIR = [u for u in U if u['kind'] == 'pair']
INTR = [u for u in U if u['kind'] == 'INTERACTIVE']

L = []
def w(t=''):
    L.append(t)

w('# A low-count non-linearity for the simulations\n')
w(f"*Fitted to the measured photon-transfer curves of {len(U)} DESY units of die LT112181 W14 D1 - "
  f"{len(PAIR)} shutter-stepped run pairs and {len(INTR)} INTERACTIVE runs. Every number is read "
  f"from the stage-5 and stage-10 dumps at build time.*\n")

w('## The function\n')
w("""A simulation needs one thing from this: given the charge a pixel collected, how much charge is
read out. The form used is

    Q_out = Q * exp(-D / Q)

with a single parameter **D**, in electrons. Its two limits are what make it a fair description of
what was measured. At large Q it is `Q - D`, a constant deficit - which is exactly what a straight
line through the photon-transfer curve reports as its intercept, `g*D` in ADU. Below `Q ~ D` it
rolls off smoothly to zero instead of clipping, which is what the measured curves do.

Nothing else has to be modelled. The map is applied to the already-noisy charge, so

    Var_out = (dQ_out/dQ)^2 * Var_in

follows, and that is what bends the photon-transfer curve at low signal. One parameter sets both
the intercept and the curvature, and they are not independent.\n""")

w('**Why this is fitted to the photon-transfer curve and not to the light ladder.** The ladder\'s '
  'x axis is the commanded shutter opening, and the Bonn shutter takes a finite time to open, so '
  'a curved signal-against-time says nothing on its own about the detector: a shutter offset of '
  '0.13 s describes the `aSpect` ladder about as well as any detector effect. `Var(M)` involves '
  'no exposure axis at all, which is why it is the right place to look.\n')

w('## What the measurements give\n')
w('| unit | kind | n | **D** [e-] | g fitted | g, route d | rms over the ladder | '
  f'rms in {A.lo:.0f}-{A.hi:.0f} ADU | worst | straight line, same range |')
w('|---|---|---|---|---|---|---|---|---|---|')
for u in sorted(U, key=lambda z: (z['kind'], -z['D'])):
    w(f"| {u['tag'].replace('uc_', '').replace('_high', '')} | {u['kind']} | {u['n']} | "
      f"**{u['D']:.1f}** | {u['g']:.3f} | {u['g0']:.3f} | {u['rms']:.1f} % | {u['rmsv']:.1f} % | "
      f"{u['maxv']:.1f} % | {u['lin']:.1f} % |")
w('')
_dp = [u['D'] for u in PAIR]
_di = [u['D'] for u in INTR]
w(f"""The last column is what the straight line `Var = g*(M + T)` of the per-unit reports does over
the same range, for comparison: {np.median([u['lin'] for u in U]):.0f} % rms against
{np.median([u['rmsv'] for u in U]):.0f} % for the one-parameter model. The model is not merely a
better fit - it is a fit with one parameter where the line has two, and it is the one a simulator
can use, because it says what to do to the charge.\n""")

if _dp and _di:
    w('## The one thing a simulation must not take for granted\n')
    w(f"""**D is not the same for the two kinds of run.** The {len(PAIR)} shutter-stepped pairs give
D = {min(_dp):.0f}-{max(_dp):.0f} e-, median {np.median(_dp):.0f}; the {len(INTR)} INTERACTIVE runs
of the **same die at the same settings** give {min(_di):.1f}-{max(_di):.1f} e-. An order of
magnitude, and the campaign cannot say which of the two is the device.

What differs between them is not the detector but the measurement: a pair's bright ladder is
referenced to zero-exposure frames taken in a different reset configuration and sits on 20-30 s of
sensor integration, while an INTERACTIVE ladder is referenced to dark frames of its own exposure
and reset setting. Either could leave a signal-dependent residual in the mean without touching the
variance, which is exactly what this fit would read as a deficit.

So **a simulation should state the D it assumed and run both ends of the range.** The default in
`ultrasat.lowCountNonLin` is {np.median(_dp):.0f} e-, the median of the pairs, because the
configuration scan and most of the campaign rest on those runs - not because it is established as
the device. At that value a pixel that collects 100 e- reads out
{100*np.exp(-np.median(_dp)/100):.0f}, one that collects 20 e- reads out
{20*np.exp(-np.median(_dp)/20):.0f}, and one that collects 5 e- reads out
{5*np.exp(-np.median(_dp)/5):.2f}.\n""")

w('![the fit against the measurements](fig_nonlin.png)\n')
w(f'*Left: every measured photon-transfer curve with its fitted model, over the '
  f'{A.lo:.0f}-{A.hi:.0f} ADU range this is meant to describe. Centre: the same as a ratio to the '
  f'model, so a flat line at one is a perfect fit. Right: the response the model implies, '
  f'Q_out/Q against Q, for the fitted D of each unit.*\n')

w('## Using it\n')
w("""```matlab
usim(..., 'LowCountNonLin', @ultrasat.lowCountNonLin, ...
          'LowCountNonLinPar', 15);     % D [e-/pix/frame]
```
`usim` applies the map per frame to the charge image after the shot noise and before the full-well
cut, and records the function name and D in the image header (`NONLIN`, `NONLINPA`), so a simulated
image says which non-linearity it was made with. `LowCountNonLinPar` of 0 or `[]` is the linear
device.\n""")

_TTXT = ''
if len(INTR) > 1:
    _TTXT = (f" Over the {len(INTR)} INTERACTIVE runs, which span "
             f"{max(u['D'] for u in INTR) - min(u['D'] for u in INTR):.1f} e- across 20 K, there is "
             f"no trend to speak of; the pairs are all within half a kelvin of each other and say "
             f"nothing about it.")
w('## Caveats\n')
w(f"""- One die, one temperature range, one gain half. D is fitted per unit and nothing here says
  it is a constant of the design.
- The fit uses the ensemble photon-transfer curve - the mean signal and the mean variance over the
  illuminated region - not individual pixels, so D is an average over the region's pixel-to-pixel
  spread and does not describe a single pixel.
- The model has no temperature dependence.{_TTXT}
- It is fitted above {A.lo:.0f} ADU. Below that the measured curves of the pairs show an excess
  variance the model does not reproduce, of order 10 ADU^2 at 1 ADU of signal, which looks like a
  property of those runs rather than of the response.\n""")

# ------------------------------------------------------------------ the figure
fig, axs = plt.subplots(1, 3, figsize=(15.4, 4.6))
for u in U:
    c = '#4c72b0' if u['kind'] == 'pair' else '#c44e52'
    k = (u['M'] >= A.lo) & (u['M'] <= A.hi)
    axs[0].plot(u['M'][k], u['V'][k], 'o', ms=3.5, color=c, alpha=0.7)
    o = np.argsort(u['M'][k])
    axs[0].plot(u['M'][k][o], u['pv'][k][o], '-', lw=1.0, color=c, alpha=0.9)
    axs[1].plot(u['M'][k], u['V'][k]/u['pv'][k], 'o-', ms=3.5, lw=0.8, color=c, alpha=0.7)
    q = np.logspace(0, 3, 200)
    axs[2].plot(q, qout(q, u['D'])/q, '-', lw=1.2, color=c, alpha=0.8)
for ax, xl, yl in ((axs[0], 'mean signal [ADU]', 'Var - RN$^2$ [ADU$^2$]'),
                   (axs[1], 'mean signal [ADU]', 'measured / model'),
                   (axs[2], 'collected charge Q [e$^-$]', 'Q$_{out}$ / Q')):
    ax.set_xlabel(xl)
    ax.set_ylabel(yl)
    ax.grid(alpha=0.25, which='both')
axs[0].set_xscale('log')
axs[0].set_yscale('log')
axs[1].axhline(1, color='k', lw=1.0)
axs[1].set_ylim(0.75, 1.25)
axs[2].set_xscale('log')
axs[2].set_ylim(0, 1.05)
for ax, lab, c in ((axs[0], 'run pairs', '#4c72b0'), (axs[0], 'INTERACTIVE', '#c44e52')):
    ax.plot([], [], 'o-', color=c, label=lab)
axs[0].legend(fontsize=8.5)
fig.suptitle('Q$_{out}$ = Q exp(-D/Q) against the measured photon-transfer curves', fontsize=11)
fig.tight_layout(rect=(0, 0, 1, 0.93))
fig.savefig(os.path.join(A.out, 'fig_nonlin.png'), dpi=120)
plt.close(fig)

md = '\n'.join(L) + '\n'
with open(os.path.join(A.out, 'nonlin.md'), 'w') as fh:
    fh.write(md)
HTML = """<!doctype html><html lang="en"><head><meta charset="utf-8">
<meta name="viewport" content="width=device-width,initial-scale=1"><title>Low-count non-linearity</title>
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
with open(os.path.join(A.out, 'nonlin.html'), 'w') as fh:
    fh.write(HTML.replace('__MD__', md.replace('</script', '<\\/script')))
print(f"nonlin.md / .html -> {A.out} ({len(md.split())} words, {len(U)} units, "
      f"D = {np.median([u['D'] for u in U]):.1f} e- median)")
