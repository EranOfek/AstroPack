#!/usr/bin/env python3
"""What the low-count non-linearity does to a simulated ULTRASAT image.

   Three ultrasat.usim runs that differ ONLY in the parameter D of
   ultrasat.lowCountNonLin, with the same Seed, so the Poisson draws are
   identical and every difference below is the non-linearity and nothing else.
   The runs are made by ~/claude/desy_batch/usim_nonlin_compare.m, which writes
   usim_compare.json; this reads it, so nothing here is a number typed by hand.

   usage: desy_uc_nonlin_sim.py [--out DIR]
"""
import argparse
import glob
import json
import os

import numpy as np
import matplotlib
matplotlib.use('Agg')
import matplotlib.pyplot as plt

P = argparse.ArgumentParser()
P.add_argument('--dir',  default='/home/sasha/claude/desy_die/uc_nonlin')
P.add_argument('--root', default='/home/sasha/claude/desy_die')
P.add_argument('--out',  default='/home/sasha/claude/desy_die/uc_nonlin_sim')
A = P.parse_args()
os.makedirs(A.out, exist_ok=True)

with open(os.path.join(A.dir, 'usim_compare.json')) as fh:
    S = json.load(fh)
D    = np.atleast_1d(np.array(S['D'], dtype=float))
Mag  = np.atleast_1d(np.array(S['Mag'], dtype=float))
Sky  = np.atleast_1d(np.array(S['Sky'], dtype=float))
Noi  = np.atleast_1d(np.array(S['Noise'], dtype=float))
Flux = np.atleast_2d(np.array(S['Flux'], dtype=float))
SNR  = np.atleast_2d(np.array(S['SNR'], dtype=float))
Peak = np.atleast_1d(np.array(S['PeakLinear'], dtype=float))
i0   = int(np.argmin(np.abs(D)))                    # the linear run

# the measured D of the campaign, read from the per-unit dumps rather than quoted
FIT = {'pair': [], 'INTERACTIVE': []}
for t in sorted(glob.glob(os.path.join(A.root, 'uc_*_high'))):
    nj = os.path.join(t, 'nonlin_fit.json')
    if not os.path.isfile(nj):
        continue
    with open(nj) as fh:
        b = json.load(fh).get('Ladders', {}).get('B')
    if b is None:
        continue
    FIT['INTERACTIVE' if 'uc_I_' in t else 'pair'].append(float(b['D']))
NPAIR, NINT = len(FIT['pair']), len(FIT['INTERACTIVE'])
if not (NPAIR and NINT):
    raise SystemExit('no per-unit non-linearity fits under %s' % A.root)
DPAIR = f"{min(FIT['pair']):.0f}-{max(FIT['pair']):.0f} e-, median {np.median(FIT['pair']):.0f}"
DINT  = f"{min(FIT['INTERACTIVE']):.1f}-{max(FIT['INTERACTIVE']):.1f} e-"

def slope(Q, d):
    return np.exp(-d/Q)*(1.0 + d/Q)

L = []
def w(t=''):
    L.append(t)

w('# The low-count non-linearity in a simulated image\n')
w(f"*{len(D)} `ultrasat.usim` runs of tile {S['Tile']}, {S['Exposure']:.0f} s, differing only in "
  f"the parameter D of `ultrasat.lowCountNonLin`, with the same `Seed` ({S['Seed']}) so the "
  f"Poisson draws are identical and every difference below is the non-linearity alone. Built from "
  f"`usim_compare.json`, which the run itself writes.*\n")

w('## What is being applied\n')
w(f"""`ultrasat.usim` applies the map to the charge of one frame, after the noise and before the
full-well cut:

    Q_out = Q * exp(-D / Q)

D is a charge deficit in electrons. It was fitted to the photon-transfer curves of the DESY
campaign, where the {NPAIR} shutter-stepped run pairs give D = {DPAIR} and the {NINT} INTERACTIVE
runs of the same die at the same settings give {DINT}. Those two disagree by an order of
magnitude, which is why the runs below bracket them: **D = {D[1]:.0f}** and
**D = {D[2]:.0f} e-**, against the linear device. The companion note
`DESY_uc_low_count_nonlinearity.pdf` has the fits.\n""")

w('## What it does\n')
w('| | ' + ' | '.join(f'D = {d:.0f}' for d in D) + ' |')
w('|---' * (len(D)+1) + '|')
w(f"| sky [e-/pix/frame] | " + ' | '.join(f'{v:.3f}' for v in Sky) + ' |')
w(f"| sky, relative | " + ' | '.join(f'{v/Sky[i0]:.4f}' for v in Sky) + ' |')
w(f"| pixel noise, relative | " + ' | '.join(f'{v/Noi[i0]:.4f}' for v in Noi) + ' |')
for j, m in enumerate(Mag):
    w(f"| source flux, mag {m:.0f}, relative | " +
      ' | '.join(f'{Flux[k][j]/Flux[i0][j]:.4f}' for k in range(len(D))) + ' |')
for j, m in enumerate(Mag):
    w(f"| S/N, mag {m:.0f}, relative | " +
      ' | '.join(f'{SNR[k][j]/SNR[i0][j]:.4f}' for k in range(len(D))) + ' |')
w('')
_k = len(D)-1
w(f"""Read the three columns and the headline is in the first two rows. At **D = {D[1]:.0f} e- the
effect is negligible**: the sky loses {100*(1-Sky[1]/Sky[i0]):.1f} %, and nothing else moves at
all. At **D = {D[_k]:.0f} e- the sky loses {100*(1-Sky[_k]/Sky[i0]):.0f} %** - and point-source
photometry still changes by less than
{100*max(abs(Flux[_k][j]/Flux[i0][j] - 1) for j in range(len(Mag))):.0f} %, while the signal to
noise **improves** by {100*(np.median([SNR[_k][j]/SNR[i0][j] for j in range(len(Mag))])-1):.0f} %.\n""")

w('## Why a 20 % loss of sky costs nothing in photometry\n')
w(f"""The map acts on the **total** charge of a pixel, so what matters for a source is not the
deficit but the local slope of the map where that pixel sits,

    dQ_out/dQ = exp(-D/Q) * (1 + D/Q)

which goes to 1 as Q grows. ULTRASAT's PSF is sharp, so even a faint source puts most of its
charge into a few pixels that are far above the sky:\n""")
w('| | sky | ' + ' | '.join(f'mag {m:.0f}' for m in Mag) + ' |')
w('|---'*(len(Mag)+2) + '|')
w('| brightest pixel [e-] | ' + f'{Sky[i0]:.0f}' + ' | ' +
  ' | '.join(f'{p:.0f}' for p in Peak) + ' |')
w(f"| local slope at D = {D[_k]:.0f} | {slope(Sky[i0], D[_k]):.4f} | " +
  ' | '.join(f'{slope(p, D[_k]):.4f}' for p in Peak) + ' |')
w('')
w(f"""So the sky, at {Sky[i0]:.0f} e-, sits where the slope is {slope(Sky[i0], D[_k]):.3f}, while a
mag {Mag[-1]:.0f} source peaks at {Peak[-1]:.0f} e- where it is {slope(Peak[-1], D[_k]):.3f} and a
mag {Mag[0]:.0f} source at {Peak[0]:.0f} e- where it is {slope(Peak[0], D[_k]):.4f}. The sky level
is reduced by the deficit itself; the source is reduced only by the slope. And because the signal
and the noise of a pixel are multiplied by that same slope, the signal to noise is preserved -
it rises slightly here because the sky noise is attenuated while the brightest source pixels are
not.\n""")

w('## What this means for the simulations\n')
w(f"""- At the measured values the low-count non-linearity is, for ULTRASAT, **a background-level
  effect**: at D = {D[_k]:.0f} e- it removes {100*(1-Sky[_k]/Sky[i0]):.0f} % of the sky and under
  {100*max(abs(Flux[_k][j]/Flux[i0][j] - 1) for j in range(len(Mag))):.0f} % of a point source.
- A real reduction subtracts the background, so it will not propagate into point-source
  photometry. It **will** matter wherever the absolute background is used - surface brightness,
  anything calibrated against the sky level, and the estimated noise, which falls with it.
- It will also matter for sources whose pixels sit near the sky level rather than above it:
  extended emission, and point sources fainter than those simulated here.
- **The unresolved factor of ten in D does not propagate.** Between D = {D[1]:.0f} and
  D = {D[_k]:.0f} the simulated photometry differs by under
  {100*max(abs(Flux[_k][j]/Flux[1][j] - 1) for j in range(len(Mag))):.0f} %. It is the background
  that is uncertain at the 20 % level, not the sources.\n""")

w('![what the non-linearity does to a simulated image](fig_nonlin_sim.png)\n')
w('*Left: the sky level and the pixel noise against D. Centre: the source flux and the signal to '
  'noise, relative to the linear run, at each magnitude. Right: the local slope of the map against '
  'the charge in a pixel, with the sky and the brightest pixel of each source marked - the reason '
  'the two rows of the first panel behave so differently.*\n')

w('## Caveats\n')
w(f"""- One sky level. Everything above is at {Sky[i0]:.0f} e-/pix/frame, the default sky of tile
  {S['Tile']} at {S['Exposure']:.0f} s. A brighter sky moves every pixel up the slope curve and
  makes the effect smaller; a darker one makes it larger.
- The photometry is a plain aperture with a local annulus, not the pipeline's. It is good enough to
  show that the effect is small, not to quantify a residual of a per cent.
- D is applied as a constant. The lab measures it per unit over a limited signal range, and nothing
  says it is constant across a device, or in time, or with temperature.
- The faintest source here, mag {Mag[-1]:.0f}, still peaks at {Peak[-1]:.0f} e- against a
  {Sky[i0]:.0f} e- sky. The regime where this effect would genuinely bite - a source whose pixels
  are at the sky level - is not covered by this test.\n""")

# ------------------------------------------------------------------ the figure
fig, axs = plt.subplots(1, 3, figsize=(15.2, 4.4))
axs[0].plot(D, Sky/Sky[i0], 'o-', ms=7, color='#4c72b0', label='sky level')
axs[0].plot(D, Noi/Noi[i0], 's-', ms=7, color='#c44e52', label='pixel noise')
axs[0].set_xlabel('D [e$^-$]')
axs[0].set_ylabel('relative to the linear run')
axs[0].legend(fontsize=9)
for j, m in enumerate(Mag):
    axs[1].plot(D, Flux[:, j]/Flux[i0, j], 'o-', ms=5, label=f'mag {m:.0f}')
axs[1].axhline(1, color='k', lw=1.0)
axs[1].set_xlabel('D [e$^-$]')
axs[1].set_ylabel('source flux, relative')
axs[1].set_ylim(0.9, 1.1)
axs[1].legend(fontsize=8, ncol=2)
q = np.logspace(0.5, 4.5, 300)
for d, c in ((D[1], '#55a868'), (D[_k], '#8172b2')):
    axs[2].plot(q, slope(q, d), '-', lw=1.5, color=c, label=f'D = {d:.0f} e$^-$')
axs[2].axvline(Sky[i0], color='#4c72b0', ls='--', lw=1.2)
axs[2].annotate('sky', (Sky[i0], 0.02), xycoords=('data', 'axes fraction'), fontsize=8,
                color='#4c72b0', ha='center')
for p, m in zip(Peak, Mag):
    axs[2].plot([p], [slope(p, D[_k])], 'v', ms=7, color='#8172b2', clip_on=False)
    axs[2].annotate(f'{m:.0f}', (p, slope(p, D[_k])), textcoords='offset points',
                    xytext=(0, 7), fontsize=7.5, ha='center', color='#8172b2')
axs[2].set_xscale('log')
axs[2].set_xlabel('charge in the pixel [e$^-$]')
axs[2].set_ylabel('local slope dQ$_{out}$/dQ')
axs[2].set_ylim(0, 1.05)
axs[2].legend(fontsize=9, loc='lower right')
for ax in axs:
    ax.grid(alpha=0.25, which='both')
fig.suptitle('The low-count non-linearity in a simulated ULTRASAT image: '
             'the sky moves, the sources do not', fontsize=11)
fig.tight_layout(rect=(0, 0, 1, 0.93))
fig.savefig(os.path.join(A.out, 'fig_nonlin_sim.png'), dpi=120)
plt.close(fig)

md = '\n'.join(L) + '\n'
with open(os.path.join(A.out, 'nonlin_sim.md'), 'w') as fh:
    fh.write(md)
HTML = """<!doctype html><html lang="en"><head><meta charset="utf-8">
<meta name="viewport" content="width=device-width,initial-scale=1"><title>Non-linearity in simulation</title>
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
with open(os.path.join(A.out, 'nonlin_sim.html'), 'w') as fh:
    fh.write(HTML.replace('__MD__', md.replace('</script', '<\\/script')))
print(f"nonlin_sim.md / .html -> {A.out} ({len(md.split())} words)")
