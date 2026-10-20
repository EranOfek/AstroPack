#!/usr/bin/env python3
"""Prepare one DESY uc_st_measurement run PAIR for the single-die chain.

The chain needs two things from the data before it can run on this instrument,
and neither is a constant of the test:

  the REGION.  The light is a spot of about 130 x 106 pixels, not a flat field,
      and it moves between campaigns. Without --ccdsec this tool finds it as the
      >FRAC-of-peak contour of the brightest frame of the bright run. With
      --ccdsec it checks the box it is given instead, and reports what fraction
      of the spot's flux falls inside it, so that comparing several runs over
      ONE box is a measured choice rather than an assumption.

  the BRIGHT FIT WINDOW.  The bright ladder is a 50-step shutter ladder whose
      first steps carry no light at all, so the aSpect default of "steps 1-4" is
      meaningless here. The steps are selected by their measured mean signal in
      the region, over --band, which is what makes one signal range comparable
      across configurations.

It writes the DieSelect selector that ~/claude/desy_batch/run_uc_pair.sh takes,
and a json record of how every number in it was chosen.

Example:
  desy_uc_pair_setup.py --die /Data1/uc_data-test/LT112181_W14_D1 \
      --dark DC_2026-08-15_19-52-23 --bright PTCexpsh_2026-08-19_14-13-56 \
      --tag uc_SF3300_2026-08_high --ccdsec "2046 2151 2446 2575" \
      --out ~/claude/desy_ucdata/pair_SF3300.m
"""
import argparse
import glob
import json
import os
import re
import sys

import numpy as np
from astropy.io import fits

P = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
P.add_argument('--die', required=True, help='die directory holding the run directories')
P.add_argument('--dark', default='', help='dark-ladder (DC) run, full name or without the die tag; '
                                          'empty for an INTERACTIVE run, which has no dark ladder')
P.add_argument('--bright', required=True, help='bright-ladder (PTCexpsh) run, likewise')
P.add_argument('--tag', required=True, help='output tag; names the dump directory and the PDF')
P.add_argument('--run', default='', help='short configuration label for the report title; defaults to --tag')
P.add_argument('--ccdsec', default='', help='"x1 x2 y1 y2" to use a given box instead of finding one')
P.add_argument('--band', nargs=2, type=float, default=[50.0, 1000.0],
               help='[ADU] mean-signal band of the bright fit window (default 50 1000)')
P.add_argument('--frac', type=float, default=0.10, help='contour of the peak that defines the spot (default 0.10)')
P.add_argument('--block', type=int, default=16, help='block side of the local spreads (default 16)')
P.add_argument('--gain', default='high', choices=['high', 'low'])
P.add_argument('--out', required=True, help='selector .m to write')
P.add_argument('--flavour', default='', help='flavour of this die, with its provenance')
P.add_argument('--drop', nargs='*', default=[],
               help='file-name fragments of frames to leave out (acquisition faults)')
P.add_argument('--json', default='', help='record of the choices; defaults to the selector with .json')
A = P.parse_args()

die_tag = os.path.basename(os.path.normpath(A.die))
run_label = A.run or A.tag


def runpath(name):
    """accept a run directory named in full or without the leading die tag"""
    for cand in (name, f'{die_tag}_{name}'):
        if os.path.isdir(os.path.join(A.die, cand)):
            return cand
    sys.exit(f'no run directory {name} (nor {die_tag}_{name}) in {A.die}')


dark = runpath(A.dark) if A.dark else ''
bright = runpath(A.bright)


def frames(run, mode):
    """{step key: [paths]} of one image mode and gain channel, by file name.

    Two layouts. The PTCexpsh/DC runs put the step in MP-INDEX and the mode in
    the name. The INTERACTIVE runs put MP-INDEX at 0 for everything and carry
    the step in the name as 'expsh<seconds>s', so there the key is the shutter
    opening itself and the steps are its ascending rank.
    """
    out = {}
    pat = os.path.join(A.die, run, 'measurement', 'raw_images',
                       f'*_{A.gain.upper()}_{mode}_DLRAW_I*_M*_R*.fits')
    for f in sorted(glob.glob(pat)):
        m = re.search(r'_M(\d+)_R(\d+)\.fits$', f)
        out.setdefault(int(m.group(1)), []).append(f)
    if out:
        return out
    # the INTERACTIVE layout
    allf = sorted(glob.glob(os.path.join(A.die, run, 'measurement', 'raw_images',
                                         f'*_{A.gain.upper()}_*_DLRAW_I*_M*_R*.fits')))
    keyed = {}
    for f in allf:
        c = re.search(r'_(?:HIGH|LOW)_(.+?)_DLRAW_I\d+_M\d+_R\d+\.fits$', os.path.basename(f))
        if not c:
            continue
        ctx = c.group(1)
        sh = re.search(r'expsh([0-9.]+)s$', ctx)
        if mode == 'LIGHT' and sh:
            keyed.setdefault(float(sh.group(1)), []).append(f)
        elif mode != 'LIGHT' and ctx.endswith('_' + mode):
            keyed.setdefault(0.0, []).append(f)
    if not keyed:
        return {}
    order = sorted(keyed)
    return {i: keyed[k] for i, k in enumerate(order)}


def read(path, sec=None):
    """one frame, or a [y0:y1, x0:x1] section of it, scaled"""
    with fits.open(path, memmap=True, do_not_scale_image_data=True) as hd:
        bz = hd[0].header['BZERO']
        if sec is None:
            return hd[0].section[:, :].astype(np.float64) + bz
        y0, y1, x0, x1 = sec
        return hd[0].section[y0:y1, x0:x1].astype(np.float64) + bz


light = frames(bright, 'LIGHT')
if not light:
    sys.exit(f'no {A.gain.upper()} LIGHT frames in {bright}')
steps = sorted(light)
top, bot = steps[-1], steps[0]

# ---- the spot, from the brightest frame minus the faintest
hi = read(light[top][0])
lo = read(light[bot][0])
diff = hi - lo
# block-average to find the spot; the frame need not be square (a windowed
# readout is 4740 columns by however many rows it was told to read)
nblk = 20
ny = (diff.shape[0] // nblk) * nblk
nx = (diff.shape[1] // nblk) * nblk
blk = diff[:ny, :nx].reshape(ny // nblk, nblk, nx // nblk, nblk).mean(axis=(1, 3))
iy, ix = np.unravel_index(np.argmax(blk), blk.shape)
cy, cx = iy * nblk + nblk // 2, ix * nblk + nblk // 2       # 0-based: axis0 = column, axis1 = readout row
half = min(250, cy, cx, diff.shape[0] - cy, diff.shape[1] - cx)
sub = diff[cy - half:cy + half, cx - half:cx + half]
peak = float(np.percentile(sub, 99.99))
ys, xs = np.where(sub > A.frac * peak)
found = [int(cx - half + xs.min()) + 1, int(cx - half + xs.max()) + 1,      # X = readout rows, 1-based
         int(cy - half + ys.min()) + 1, int(cy - half + ys.max()) + 1]      # Y = readout columns, 1-based

if A.ccdsec:
    sec = [int(v) for v in A.ccdsec.replace(',', ' ').split()]
    if len(sec) != 4:
        sys.exit('--ccdsec wants four numbers: "x1 x2 y1 y2"')
else:
    sec = found

# how much of the spot the box actually holds, against the >frac contour
mask = np.zeros_like(diff, dtype=bool)
mask[cy - half:cy + half, cx - half:cx + half] = sub > A.frac * peak
inbox = np.zeros_like(mask)
inbox[sec[2] - 1:sec[3], sec[0] - 1:sec[1]] = True
flux_all = float(diff[mask].sum())
flux_in = float(diff[mask & inbox].sum())
held = flux_in / flux_all if flux_all > 0 else float('nan')
npix_out = int((mask & ~inbox).sum())

# ---- the bright ladder in that box: one frame per step is enough to choose a window
y0, y1, x0, x1 = sec[2] - 1, sec[3], sec[0] - 1, sec[1]
base = read(light[bot][0], (y0, y1, x0, x1))
ladder = []
for s in steps:
    m = read(light[s][0], (y0, y1, x0, x1))
    ladder.append(float((m - base).mean()))
ladder = np.array(ladder)

lo_b, hi_b = A.band
sel = [s for s, v in zip(steps, ladder) if lo_b <= v <= hi_b]
if len(sel) < 3:
    sys.exit(f'only {len(sel)} bright steps fall in {lo_b}-{hi_b} ADU: widen --band')
# MATLAB steps are the MP index + 1
fit_b = [s + 1 for s in sel]
sig = [float(v) for s, v in zip(steps, ladder) if s in sel]
# the PTC window must hold the response window, with a hair of margin
gain_range = [float(np.floor(min(sig) * 0.98)), float(np.ceil(max(sig) * 1.02))]

rec = {'Tag': A.tag, 'Run': run_label, 'Die': A.die, 'DarkRun': dark, 'BrightRun': bright, 'Gain': A.gain,
       'SpotCentre': {'Column': int(cy) + 1, 'ReadoutRow': int(cx) + 1}, 'SpotPeakADU': peak,
       'SpotContour': A.frac, 'SpotBoxFound': found, 'CCDSEC': sec,
       'SpotFluxHeld': held, 'SpotPixelsOutside': npix_out,
       'Band': [lo_b, hi_b], 'FitStepsB': fit_b, 'FitStepsSignal': sig,
       'GainRange': gain_range, 'Block': A.block, 'DropFrames': A.drop, 'Flavour': A.flavour,
       'LadderStep': [s + 1 for s in steps], 'LadderSignal': [float(v) for v in ladder],
       'Source': os.path.basename(__file__)}

drop_m = ''
if A.drop:
    # struct() treats a cell value as per-element data and would build a struct
    # ARRAY, so the list has to be wrapped in a second pair of braces
    drop_m = ", ...\n    'DropFrames', {{" + ', '.join(f"'{d}'" for d in A.drop) + '}}'
contig = fit_b == list(range(fit_b[0], fit_b[-1] + 1))
steps_m = f'{fit_b[0]}:{fit_b[-1]}' if contig else '[' + ' '.join(str(s) for s in fit_b) + ']'

with open(A.out, 'w') as fh:
    fh.write(f"""% Driver: one DESY uc_st_measurement run PAIR through the single-die chain.
%   Written by {os.path.basename(__file__)}; the json beside it records how every
%   number here was chosen, and re-running that tool reproduces this file.
%     dark ladder   {dark}
%     bright ladder {bright}
%   CCDSEC is the illuminated region, [x1 x2 y1 y2] with x along the readout rows
%   and y along the readout columns. The spot of this run sits at readout column
%   {int(cy)+1}, readout row {int(cx)+1}, and its >{A.frac*100:.0f} %-of-peak contour is
%   [{found[0]} {found[1]} {found[2]} {found[3]}]. The box below holds {held*100:.1f} % of the light inside that
%   contour ({npix_out} of its pixels fall outside).
%   FitStepsB are the bright steps whose mean signal in that box lies in
%   {lo_b:.0f}-{hi_b:.0f} ADU ({min(sig):.0f} to {max(sig):.0f} ADU over {len(fit_b)} steps). The aSpect default
%   [1 2 3 4] would fit the four shortest shutter openings, which carry no light.
DieSelect = struct( ...
    'Source',   'ucfits', ...
    'DieDir',   '{A.die}', ...
    'Test',     '{dark}+{bright}', ...
    'CCDSEC',   [{sec[0]} {sec[1]} {sec[2]} {sec[3]}], ...
    'Tag',      '{A.tag}', ...
    'Lot',      'LT112181', ...
    'Run',      '{run_label}', ...
    'Folder',   '', ...
    'Die',      'W14_D1', ...
    'Gain',     '{A.gain}', ...
    'Flavour',  '{A.flavour}', ...
    'Block',    {A.block}, ...
    'IntensityScale', 1, ...     % the bright X axis is the shutter opening in seconds
    'GainRange', [{gain_range[0]:.0f} {gain_range[1]:.0f}], ...
    'FitStepsB', {steps_m}{drop_m});
""")

jpath = A.json or os.path.splitext(A.out)[0] + '.json'
with open(jpath, 'w') as fh:
    json.dump(rec, fh, indent=1)

print(f'{A.tag}: spot at col {int(cy)+1} row {int(cx)+1}, peak {peak:.0f} ADU; '
      f'box holds {held*100:.1f} % of the >{A.frac*100:.0f} % contour ({npix_out} px outside); '
      f'bright window {len(fit_b)} steps, {min(sig):.0f}-{max(sig):.0f} ADU -> {A.out}')
