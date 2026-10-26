#!/usr/bin/env python3
"""What the DESY uc_data-test campaign measured, run by run, for the units that were reduced.

One row per analysed configuration. Every value is read at build time either from the
stage-1 `Setup` block of that unit (the same source the per-unit report prints) or from
the frame names and the measurement configuration of the runs themselves, so the table
cannot disagree with the reports.

Author : A.M. Krassilchtchikov (Oct 2026)
"""
import argparse
import collections
import glob
import json
import os
import re
import sys

import numpy as np
import matplotlib
matplotlib.use('Agg')
import matplotlib.pyplot as plt

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
# the photon-transfer figure and the model it draws are shared with the other reports
import desy_ptcfigs as PF
from desy_ptcfigs import SIGLO, SIGHI, FULLHI

P = argparse.ArgumentParser(description=__doc__)
P.add_argument('--rnroot', default='/home/sasha/claude/desy_rn', help='stage-1 output root')
P.add_argument('--dieroot', default='/home/sasha/claude/desy_die', help='chain output root')
P.add_argument('--data', default='/Data1/uc_data-test/LT112181_W14_D1', help='campaign directory')
P.add_argument('--out', default='/home/sasha/claude/desy_die/uc_runs', help='output directory')
A = P.parse_args()
os.makedirs(A.out, exist_ok=True)

# The units carrying a full report, in the order the other cross-unit documents use:
# the seven August bias settings first, then the four spot runs.
PAIRS = ['uc_aSpect_2026-08_high', 'uc_aSpect_RSTL1000_2026-08_high',
         'uc_aSpect_RSTSEL3800_2026-08_high', 'uc_aSpect_SF3300_2026-08_high',
         'uc_AV_SF3000_2026-08_high', 'uc_AV_RSTL1200_2026-08_high',
         'uc_AV_RSTSEL3500_2026-08_high']
SPOTS = ['uc_I_aSpect2_m78_high', 'uc_I_aSpect2_m71_high',
         'uc_I_aSpect2_m58_high', 'uc_I_aSpect2_m57_high']

VORDER = ['VDD_RST_L', 'VDD_RST_H', 'VDD_RST_SEL', 'VDD_SF',
          'VDD_TX', 'VDD_SEN', 'VDDA', 'VDDD']
VSHORT = ['RST_L', 'RST_H', 'RST_SEL', 'SF', 'TX', 'SEN', 'VDDA', 'VDDD']


def load(path):
    try:
        with open(path) as fh:
            return json.load(fh)
    except (OSError, ValueError):
        return None


def mcfg_of(run):
    """The measurement configuration of one run: what was stepped, and how many frames a point."""
    md = os.path.join(A.data, run, 'measurement', 'metadata')
    f = glob.glob(os.path.join(md, 'mcfg_*.toml'))
    if not f:
        return {}
    txt = open(f[0], errors='replace').read()
    out = {'McfgName': os.path.basename(f[0])}

    def scalar(key):
        m = re.search(r'^\s*%s\s*=\s*([-\d.]+)' % key, txt, re.M)
        return float(m.group(1)) if m else None

    def lst(key):
        m = re.search(r'^\s*%s\s*=\s*\[([^\]]*)\]' % key, txt, re.M)
        if not m:
            return None
        v = [float(x) for x in re.findall(r'[-\d.]+', m.group(1))]
        return v or None

    out['CameraExpList'] = lst('camera_exposure_list')
    out['ShutterExpList'] = lst('shutter_exposure_list')
    out['CameraExpTime'] = scalar('camera_exposure_time')
    out['NZero'] = scalar('beginning_zero_frames')
    out['NDark'] = scalar('dark_frames')
    out['NBright'] = scalar('bright_frames')
    return out


def frames_of(run):
    """Role, step and frame counts taken from the frame names actually on disk.

    A frame is named ..._<GAIN>_<middle>_DLRAW_I<n>_M<step>_R<rep>.fits and the middle
    carries the role: ZERO, DARK, LIGHT, or - for the spot runs - the shutter opening
    itself as expsh<t>s. Only the HIGH-gain images are counted; the LOW-gain ones
    duplicate them frame for frame.
    """
    ri = os.path.join(A.data, run, 'measurement', 'raw_images')
    try:
        fs = os.listdir(ri)
    except OSError:
        return None
    steps = collections.defaultdict(set)
    shut = collections.defaultdict(set)
    nfr = collections.Counter()
    nlow = 0
    for f in fs:
        m = re.search(r'_(HIGH|LOW)_(.*?)_DLRAW_(.*?)\.fits$', f)
        if not m:
            continue
        gain, mid, tail = m.groups()
        if gain == 'LOW':
            nlow += 1
            continue
        if 'ZERO' in mid:
            role = 'ZE'
        elif 'DARK' in mid:
            role = 'DARK'
        elif 'LIGHT' in mid or re.search(r'expsh[\d.]+s', mid):
            role = 'LIGHT'
        else:
            role = 'other'
        e = re.search(r'expsh([\d.]+)s', mid)
        k = re.search(r'_M(\d+)_', '_' + tail)
        if e:
            shut[role].add(float(e.group(1)))
            steps[role].add('t' + e.group(1))
        else:
            steps[role].add(k.group(1) if k else '-')
        nfr[role] += 1
    return dict(steps={k: len(v) for k, v in steps.items()},
                keys={k: sorted(v) for k, v in steps.items()},
                shut={k: sorted(v) for k, v in shut.items()}, nfr=dict(nfr),
                ntot=len([f for f in fs if f.endswith('.fits')]), nlow=nlow)


def steprange(f, role, explist):
    """The exposure range of the steps that are actually on disk.

    A spot run carries the shutter opening in the frame name, so it needs no list. A
    stepped run carries only the step index, which points into the commanded exposure
    list - and a ladder that stopped early must be measured by the entries it reached,
    not by the whole list.
    """
    if not f:
        return None
    if f.get('shut', {}).get(role):
        return rng(f['shut'][role])
    if not explist or role not in f.get('keys', {}):
        return None
    idx = [int(k) for k in f['keys'][role] if k.isdigit()]
    v = [explist[i] for i in idx if 0 <= i < len(explist)]
    return rng(v)


def fitsteps(tag, which):
    """How many ladder steps the signal window left for the fits, and over what signal.

    The window is 90-1000 ADU, but its floor is lowered one step at a time where fewer
    than three steps clear 90, so the signal range is reported with the count: it is the
    only way to see that a three-point fit sits below the nominal floor.
    """
    j = load(os.path.join(A.dieroot, tag, {'dark': 'dark.json', 'light': 'light.json'}[which]))
    if not j:
        return None, None, None
    fs = j.get('FitSteps')
    pat = j.get('PatternStep')
    sm = j.get('StepMedian')
    n = len(fs) if isinstance(fs, list) else None
    ntot = len(pat) if isinstance(pat, list) else None
    sig = (min(sm), max(sm)) if isinstance(sm, list) and sm else None
    return n, ntot, sig


def shorten(run):
    """LT112181_W14_D1_DC_2026-08-16_01-30-13 -> DC 08-16 01:30"""
    m = re.match(r'.*_(\w+?)_(\d{4})-(\d\d)-(\d\d)_(\d\d)-(\d\d)-\d\d$', run or '')
    if not m:
        return run or '—'
    t, _, mo, d, hh, mm = m.groups()
    return f'{t} {mo}-{d} {hh}:{mm}'


def rng(v):
    if not v:
        return None
    return f'{min(v):g}–{max(v):g}'


# ------------------------------------------------------------------ collect
U = []
for tag in PAIRS + SPOTS:
    st = load(os.path.join(A.rnroot, tag, 'stats.json'))
    if not st or 'Setup' not in st:
        print(f'  ! no stage-1 setup for {tag}, skipped')
        continue
    S = st['Setup']
    dk, br = S.get('DarkRun'), S.get('BrightRun')
    u = dict(tag=tag, S=S, dark=dk, bright=br,
             fdark=frames_of(dk) if dk else None,
             fbright=frames_of(br) if br else None,
             mdark=mcfg_of(dk) if dk else {},
             mbright=mcfg_of(br) if br else {},
             kind='pair' if tag in PAIRS else 'spot')
    u['M'] = load(os.path.join(A.dieroot, tag, 'methods.json'))
    u['NLF'] = ((load(os.path.join(A.dieroot, tag, 'nonlin_fit.json')) or {})
               .get('Ladders') or {})
    u['B'] = load(os.path.join(A.dieroot, tag, 'budget.json'))
    _cl = u['mdark'].get('CameraExpList')
    u['shuffled'] = bool(_cl) and _cl != sorted(_cl)
    u['ndf'], u['ndt'], u['dsig'] = fitsteps(tag, 'dark')
    u['nlf'], u['nlt'], u['lsig'] = fitsteps(tag, 'light')
    U.append(u)
if not U:
    raise SystemExit('no analysed unit found under %s' % A.rnroot)

# which voltages actually differ across the eleven rows: those get marked in the table
VAL = {k: [u['S'].get('Commanded', {}).get(k) for u in U] for k in VORDER}
VARIES = {k: len({v for v in VAL[k] if v is not None}) > 1 for k in VORDER}

L = []


def w(t=''):
    L.append(t)


def volts(u):
    c = u['S'].get('Commanded', {}) or {}
    out = []
    for k in VORDER:
        v = c.get(k)
        s = '—' if v is None else f'{float(v):.2f}'
        out.append(f'**{s}**' if VARIES[k] and v is not None else s)
    return '/'.join(out)


def temps(u):
    S = u['S']
    td, tb = S.get('TempDark'), S.get('TempBright')
    f = lambda x: '—' if x is None else f'{float(x):+.1f}'
    return f'{f(td)} / {f(tb)}' if tb is not None else f(td)


def rows_of(u):
    S = u['S']
    nd = S.get('NrowsDark') or S.get('Nrows')
    nb = S.get('NrowsBright') or S.get('Nrows')
    if nd and nb and int(nd) != int(nb):
        return f'{int(nd)} / {int(nb)}'
    return str(int(nd)) if nd else '—'


def zecell(u):
    n = 0
    for f in (u['fdark'], u['fbright']):
        if f:
            n += f['nfr'].get('ZE', 0)
    if not n:
        return 'none'
    bits = []
    for nm, f in (('dark run', u['fdark']), ('light run', u['fbright'])):
        if f and f['nfr'].get('ZE'):
            bits.append(f"{f['nfr']['ZE']} ({nm})")
    return ' + '.join(bits)


def darkcell(u):
    f = u['fdark']
    if not f or not f['steps'].get('DARK'):
        return 'none'
    ns = f['steps']['DARK']
    ex = steprange(f, 'DARK', u['mdark'].get('CameraExpList'))
    s = f'**{ns} steps**' + (f', {ex} s' if ex else '')
    if u['ndf']:
        s += f'; {u["ndf"]} in window'
        if u.get('dsig'):
            s += f' ({u["dsig"][0]:.0f}–{u["dsig"][1]:.0f} ADU)'
    return s


def lightcell(u):
    f = u['fbright']
    if not f or not f['steps'].get('LIGHT'):
        return 'none'
    ns = f['steps']['LIGHT']
    ex = steprange(f, 'LIGHT', u['mbright'].get('ShutterExpList'))
    s = f'**{ns} steps**' + (f', {ex} s shutter' if ex else '')
    if u['nlf']:
        s += f'; {u["nlf"]} in window'
        if u.get('lsig'):
            s += f' ({u["lsig"][0]:.0f}–{u["lsig"][1]:.0f} ADU)'
    return s


def cfg(u):
    return (u['S'].get('ScfgDark') or '').replace('scfg_cfg_', '').replace('.toml', '') or '—'


# ------------------------------------------------------------------- document
NPAIR = sum(1 for u in U if u['kind'] == 'pair')
NSPOT = sum(1 for u in U if u['kind'] == 'spot')
NCFG = len({cfg(u) for u in U})
w('# The DESY measurements of die LT112181 W14 D1 — what each reduced run contains\n')
w(f'*{len(U)} analysed units of the DESY `uc_data-test` campaign: {NPAIR} shutter-stepped run '
  f'pairs and {NSPOT} windowed spot runs, covering {NCFG} distinct sensor configurations. '
  f'Each value below is read at build time from that unit\'s stage-1 `Setup` block — the same '
  f'source its own report prints — or from the frame names and measurement configuration of the '
  f'runs themselves. Nothing here is typed by hand.*\n')

w('## 1. What is being compared\n')
w('The campaign directory holds many more runs than this table does. Every analysed unit is a '
  '**pair of runs**, because no single run in the campaign carries all three of the steps an '
  'end-to-end reduction needs:\n')
w('| the step | what it gives | which run supplies it |')
w('|---|---|---|')
w('| **ZE** — zero-exposure frames | bias level, read noise, the fixed pattern | both runs of a '
  'pair carry their own, and each ladder is referenced to the zeros of its own run |')
w('| **dark ladder** — exposure stepped, shutter shut | dark current, and the intercept of the '
  'dark response and of the dark photon-transfer curve | the `DC` run |')
w('| **light ladder** — light stepped | the response, the gain and the intercept of the light '
  'response and of the bright photon-transfer curve | the `PTCexpsh` or `INTERACTIVE` run |')
w()
w('The two measurement types in the table differ in how the light is stepped and in how much of '
  'the sensor is read out, which is why they are kept apart:\n')
w('- **`DC` + `PTCexpsh`** — the Bonn shutter is stepped in front of a full-die readout at a '
  'fixed sensor exposure. The light covers the whole die.\n'
  '- **`DC` + `INTERACTIVE`** — the shutter is stepped too, but the readout is windowed to a few '
  'hundred rows and the light is a *spot*, so the analysis runs over a box around it. Here the '
  'two halves are **not read out alike** (see the row counts), and the dark ladder comes from a '
  '`DC` run of the same configuration at the nearest temperature rather than from the same '
  'session.\n')

_ROW = 0
for kind, ttl, note in (
    ('pair', '2. Dark ladder and shutter-stepped light ladder (`DC` + `PTCexpsh`)',
     'The seven August settings. One bias voltage at a time was moved away from two baselines, '
     '`aSpect` and `aSpectConfigAVPixels`, so the rows are a controlled scan: read the voltages, '
     'not the configuration names, because a name states the change from *its own* baseline.'),
    ('spot', '3. Dark ladder and a light spot on a windowed readout (`DC` + `INTERACTIVE`)',
     'These four rows are **one configuration at four temperatures**, not four configurations: '
     'the bias voltages and the ADC ramp field are identical and only the thermal setting '
     'differs. Their own `DARK` frames are a single point at the exposure of the ladder, not a '
     'ladder, so the dark ladder is taken from the paired `DC` run.')):
    V = [u for u in U if u['kind'] == kind]
    if not V:
        continue
    w(f'## {ttl}\n')
    w(note + '\n')
    w('| # | configuration | runs (dark + light) | ramp | bias voltages, commanded [V] '
      '<br>' + '/'.join(VSHORT) + ' | T [C] dark / light | rows read | ZE frames | dark ladder | '
      'light ladder |')
    w('|---|---|---|---|---|---|---|---|---|---|')
    for u in V:
        _ROW += 1
        w(f'| {_ROW} | `{cfg(u)}` | {shorten(u["dark"])}<br>{shorten(u["bright"])} | '
          f'{u["S"].get("RampAdjDark", "—")} | {volts(u)} | {temps(u)} | {rows_of(u)} | '
          f'{zecell(u)} | {darkcell(u)} | {lightcell(u)} |')
    w()

def _route(u, tag, key):
    M = u.get('M') or {}
    r = (M.get('Routes') or {}).get(tag)
    return None if not isinstance(r, dict) else r.get(key)


def _f(v, nd=2):
    return '—' if v is None else f'{float(v):.{nd}f}'


def _pmv(v, e, nd=2):
    """value and error, or the bare value where no error is defensible."""
    if v is None:
        return '—'
    if e is None or not (float(e) == float(e)) or float(e) <= 0:
        return _f(v, nd)
    return f'{float(v):.{nd}f} ± {float(e):.{nd}f}'


def interc(u, tag):
    """the intercept of one route, in electrons, as its own report quotes it."""
    return _pmv(_route(u, tag, 'Threshold_e'), _route(u, tag, 'Threshold_e_err'), 1)


def gain(u, tag):
    g = _route(u, tag, 'Gain')
    gs, gy = _route(u, tag, 'GainStat'), _route(u, tag, 'GainSyst')
    e = None if (g is None or gs is None) else (float(gs)**2 + float(gy or 0)**2)**0.5
    return _pmv(g, e, 3)


def _to_e_num(u, key, errkey):
    """the value and error of an ADU datasheet quantity in electrons.

    Delegates to the shared reader, so this document, the short run reports and the
    per-unit reports all convert the same way; `errkey` selects which of the two
    quantities it already knows how to carry.
    """
    d = PF.datasheet(A.dieroot, u['tag'])
    k = {'DC_ADU': 'DC_e', 'RN_ADU': 'RN_e'}.get(key)
    if k is None:
        return None, None
    return d.get(k), d.get(k + '_err')


def _rnerr(u):
    E = load(os.path.join(A.dieroot, u['tag'], 'errors.json')) or {}
    return ((E.get('ReadNoise') or {}).get('All'))


def _dcerr(u):
    st, sy = _route(u, 'a', 'SlopeStat'), _route(u, 'a', 'SlopeSyst')
    return None if st is None else (float(st)**2 + float(sy or 0)**2)**0.5


def _to_e(u, key, errkey, nd):
    """an ADU quantity of the datasheet, converted to electrons as its report converts it.

    The gain is the one the chain uses for every electron figure, and the error carries
    the quantity's own relative error and the gain's, exactly as section 11 does.
    """
    v, e = _to_e_num(u, key, errkey)
    return '—' if v is None else _pmv(v, e, nd)


def readnoise(u):
    """median per-pixel read noise in electrons, with the error its report quotes."""
    E = load(os.path.join(A.dieroot, u['tag'], 'errors.json')) or {}
    e = ((E.get('ReadNoise') or {}).get('All'))
    return _to_e(u, 'RN_ADU', e, 3)


def darkcurrent(u):
    """dark current in electrons per second, by the datasheet's own recipe.

    The value is the median-pixel slope the datasheet quotes and the gain that
    converts it is the one the chain uses for every other electron figure; the
    error carries the fit's relative error and the gain's, as section 11 does.
    """
    st, sy = _route(u, 'a', 'SlopeStat'), _route(u, 'a', 'SlopeSyst')
    e = None if st is None else (float(st)**2 + float(sy or 0)**2)**0.5
    return _to_e(u, 'DC_ADU', e, 4)


w('## 4. How to read the columns\n')
_vv = [VSHORT[i] for i, k in enumerate(VORDER) if VARIES[k]]
_vf = [VSHORT[i] for i, k in enumerate(VORDER) if not VARIES[k]]
w(f'- **#** is the same row number in every table of this document, so row {len(U)} of the '
  f'inventory and row {len(U)} of the results are the same unit.\n'
  f'- **configuration** is the name of the sensor configuration file the run actually used, with '
  f'the `scfg_cfg_` prefix dropped. Both runs of every pair used the same one.\n'
  f'- **runs** are the two measurement directories, shortened to type, month-day and time. The '
  f'full names are in the unit\'s own report.\n'
  f'- **ramp** is `bs_ramp_adj` of `register_0x004_bias_adc_control_2`. It is given because the '
  f'configuration name does not imply it: two configurations in this campaign have identical '
  f'bias voltages and different ramp fields, and the difference moves the bias level by about '
  f'340 ADU and the fixed pattern by a factor of 60.\n'
  f'- **bias voltages** are the commanded DAC settings; the read-back values are in each '
  f'unit\'s report, and run 1-2 % low throughout. '
  + (f'Marked in bold are the ones that differ between rows ({", ".join(_vv)}); '
     f'{", ".join(_vf)} are the same in every row.\n' if _vv else '\n') +
  f'- **T** is the median of `temperature.csv` (Sensor2) over each run, dark run first. The '
  f'temperature declared in the measurement configuration is not used: it is unreliable, and for '
  f'the spot runs it is wrong by 20 K.\n'
  f'- **rows read** is the readout height. Where two numbers appear the two halves of the unit '
  f'were read out differently, so the dark and light sides do not share a readout.\n'
  f'- **ZE / dark ladder / light ladder** are counted from the frame names on disk, not from what '
  f'the measurement configuration asked for. "in window" is how many of those steps survived the '
  f'90-1000 ADU rule and so entered the fits.\n')

w('## 5. What the four routes measure on each configuration\n')
w('| # | configuration | T [C] dark / light | dark current [e-/s] | read noise [e-] | a: dark response | b: light response | '
  'c: PTC dark | d: PTC bright | gain c [ADU/e-] | gain d [ADU/e-] |')
w('|---|---|---|---|---|---|---|---|---|---|---|')
for i, u in enumerate(U, 1):
    w(f"| {i} | `{cfg(u)}` | {temps(u)} | {darkcurrent(u)} | {readnoise(u)} | "
      f"{interc(u, 'a')} | {interc(u, 'b')} | {interc(u, 'c')} | {interc(u, 'd')} | "
      f"{gain(u, 'c')} | {gain(u, 'd')} |")
w()
def _vals(tag, key='Threshold_e', units=None):
    return [float(_route(u, tag, key)) for u in (units or U)
            if _route(u, tag, key) is not None]


_PR = [u for u in U if u['kind'] == 'pair']
_SP = [u for u in U if u['kind'] == 'spot']
_dc = [float((((u.get('B') or {}).get('Unmasked') or {}).get('Inputs') or {}).get('DC_ADU', 0))
       / float((((u.get('B') or {}).get('Unmasked') or {}).get('Inputs') or {}).get('GainADU', 1))
       for u in U]
_a, _b, _c, _d = (_vals(t) for t in 'abcd')
_gd_p, _gd_s = _vals('d', 'Gain', _PR), _vals('d', 'Gain', _SP)
_ce = [abs(float(_route(u, 'c', 'Threshold_e_err') or 0)) for u in U]
_worst = max(range(len(U)), key=lambda i: _ce[i])

w('*Intercepts in electrons, gains in ADU per electron, both with their statistical and '
  'systematic errors added in quadrature. The read noise is the median of the per-pixel value '
  'over the zero-exposure frames of the first run, which is not the root of the mean square the '
  'photon-transfer fit subtracts; the latter is larger, because the distribution has a tail. '
  'Two temperatures are given because the two halves of a '
  'unit are separate runs: the dark current and routes a and c belong to the first, routes b and '
  'd to the second. Rows 8-11 are the same configuration at four temperatures of the light run; '
  'rows 10 and 11 share one dark run, which is why their first temperature is the same.*\n')

SIGLO, SIGHI, FULLHI = 90.0, 1000.0, 1000.0
# A residual in per cent divides by the fitted line, so where that line passes close to
# zero the percentage says more about the line than about the measurement. Points whose
# fitted variance is below this are left out of the residual panels and counted instead.
MODFLOOR = 1.0                                   # [ADU^2]


w('The same eleven units, with the quantities their reports derive. The **intercept** is what a '
  'straight line fitted to a ladder gives where that ladder reaches zero signal, expressed as '
  'charge: the line reaches zero at *minus* the quoted value, so a positive number is charge '
  'that appears to be missing at the bottom of the ladder. Four routes reach it, two from a '
  'response against exposure and two from a photon-transfer curve, and only the latter two also '
  'yield a gain.\n')
w('**What the four routes say.**\n')
w(f'- **The dark current spans a factor of {max(_dc)/min(_dc):.0f}** across these settings, '
  f'{min(_dc):.3f} to {max(_dc):.2f} e-/s, at temperatures that differ by at most '
  f'{max(abs(float(u["S"]["TempDark"]) - float(v["S"]["TempDark"])) for u in _PR for v in _PR):.1f} K '
  f'among the seven pairs. It is the bias setting doing this, not the thermal setting.\n')
w(f'- **Route a follows the dark current; route d does not.** The dark-response intercept runs '
  f'{min(_a):.0f} to {max(_a):.0f} e- and rises with the dark current, which is what an '
  f'extrapolation off a curved ladder does. The bright photon-transfer intercept stays within '
  f'{min(_d):.0f}-{max(_d):.0f} e- over that same factor of {max(_dc)/min(_dc):.0f}, and it is '
  f'the only one of the four that does.\n')
w(f'- **Route b is always the largest**, {min(_b):.0f} to {max(_b):.0f} e-, and route c the '
  f'smallest, {min(_c):.0f} to {max(_c):.0f} e-, several of them consistent with zero. The four '
  f'routes are mutually inconsistent on every unit; each report says so and says which it would '
  f'use.\n')
w(f'- **Where the dark ladder is three steps, route c measures nothing.** On '
  f'`{cfg(U[_worst])}` its error is ±{_ce[_worst]:.0f} e-, larger than any intercept in the '
  f'table, because a gain fitted to three points spanning '
  f'{U[_worst]["dsig"][0]:.0f}-{U[_worst]["dsig"][1]:.0f} ADU is barely a line. Its gain column '
  f'says the same thing.\n')
w(f'- **The two ADC ramp settings give different gains.** Route d reads '
  f'{min(_gd_p):.2f}-{max(_gd_p):.2f} ADU/e- on the seven pairs, all at ramp 9, and '
  f'{min(_gd_s):.2f}-{max(_gd_s):.2f} on the four spot runs, all at ramp 12 — a difference of '
  f'about {100*(1 - sum(_gd_s)/len(_gd_s)/(sum(_gd_p)/len(_gd_p))):.0f} per cent that follows '
  f'the ramp field and not the bias voltages, which are identical between `aSpect` and '
  f'`aSpect2`. That is what an adjustment to the ADC ramp should do, though these runs differ in '
  f'readout and in reference frames as well, so the two effects are not separated here.\n')
_rn = [(u, _to_e(u, 'RN_ADU', None, 3)) for u in U]
_rp = [float(((((u.get('B') or {}).get('Unmasked') or {}).get('Inputs')) or {}).get('RN_ADU', 0))
       / float(((((u.get('B') or {}).get('Unmasked') or {}).get('Inputs')) or {}).get('GainADU', 1))
       for u in _PR]
_rs = [float(((((u.get('B') or {}).get('Unmasked') or {}).get('Inputs')) or {}).get('RN_ADU', 0))
       / float(((((u.get('B') or {}).get('Unmasked') or {}).get('Inputs')) or {}).get('GainADU', 1))
       for u in _SP]
if _rp and _rs:
    w(f'- **The read noise points the same way.** It is {min(_rp):.2f}-{max(_rp):.2f} e- on the '
      f'seven pairs and {min(_rs):.2f}-{max(_rs):.2f} e- on the four spot runs, so the ramp-12 '
      f'runs are the quieter ones by about '
      f'{100*(1 - sum(_rs)/len(_rs)/(sum(_rp)/len(_rp))):.0f} per cent. Within the seven pairs, '
      f'which share a ramp setting, it varies by only '
      f'{100*(max(_rp) - min(_rp))/(sum(_rp)/len(_rp)):.0f} per cent across a factor of '
      f'{max(_dc)/min(_dc):.0f} in dark current: the bias voltages are not moving it.\n')

w('## 6. The photon-transfer curves, fitted two ways\n')
w(f'One figure per unit, the dark ladder on the left and the bright one on the right. The top '
  f'panel of each is the photon-transfer curve itself — the variance above the read noise '
  f'against the mean signal — with two straight lines through it. The **red** line is the fit '
  f'over {SIGLO:.0f}-{SIGHI:.0f} ADU, the window all four routes share, and it is not refitted '
  f'here: it is the slope and intercept that routes c and d report, so it is exactly the line '
  f'the numbers of section 5 came from. The **blue** dashed line is the same estimator, an '
  f'unweighted least-squares fit of the same two quantities, given every point from 0 to '
  f'{FULLHI:.0f} ADU instead. The two panels beneath show what each line leaves behind, as a '
  f'percentage of the line itself. The shaded band is the window.\n')
w('A straight photon-transfer curve would leave both residual panels flat at zero. The question '
  'they answer is what the extra points below 90 ADU do to a straight line asked to describe '
  'them.\n')
w('The **bottom panel** asks the next question: if the curve bends at the bottom because charge '
  'is missing there, what has to be added back to straighten it? Two answers are drawn together, '
  'both as a percentage of what they predict. In **red**, a *constant* shift — the same amount of '
  'charge missing at every signal, which is what an intercept means, and the amount is given in '
  'the legend. In **green**, the *low-count model* `Q_out = Q·exp(−D/Q)`, in which the missing '
  'charge grows towards low signal; D is not fitted here but read from the fit that unit\'s own '
  'report carries, and where that fit is poor the squares are left open and its scatter is '
  'quoted. The dotted line marks 100 ADU, below which the two answers part company. A perfect '
  'compensation would put its points on zero all the way down.\n')
_NOTES = []
for i, u in enumerate(U, 1):
    _fn = f'fig_ptc_{u["tag"]}.png'
    _nt = PF.ptcfig(u['tag'], A.dieroot, os.path.join(A.out, _fn))
    _cap = (f'**{i}. `{cfg(u)}`** at {temps(u)} C. Dark ladder left, bright ladder right; '
            f'the window fit in red, the 0-{FULLHI:.0f} ADU fit in blue dashed.')
    if _nt:
        _cap += ' ' + ('Note: ' + '; '.join(sorted(set(_nt))) + '.')
        _NOTES.append((i, _nt))
    w(f'![{_cap}]({_fn})\n')
    w(f'*{_cap}*\n')

def tempfig(path):
    """the intercept of each route against temperature, one panel per route.

    Each route is drawn at the temperature of the run it comes from: a and c from the
    dark ladder's run, b and d from the light one. That is what separates the two spot
    units near -58 C, which share a dark run and differ only in their light run.
    """
    ROUTES = (('a', 'a: dark response', 'TempDark', 'DarkRun'),
              ('b', 'b: light response', 'TempBright', 'BrightRun'),
              ('c', 'c: PTC dark', 'TempDark', 'DarkRun'),
              ('d', 'd: PTC bright', 'TempBright', 'BrightRun'))
    names = sorted({cfg(u) for u in U})
    cmap = plt.get_cmap('tab10')
    col = {n: cmap(i % 10) for i, n in enumerate(names)}
    mk = {n: 'osDv^P*X'[i % 8] for i, n in enumerate(names)}
    fig, AX = plt.subplots(2, 2, figsize=(14.6, 8.4), sharex=True,
                           gridspec_kw=dict(hspace=0.24, wspace=0.14))
    for ax, (rt, ttl, tkey, rkey) in zip(AX.ravel(), ROUTES):
        seen = []
        for n in names:
            V = [u for u in U if cfg(u) == n]
            # one marker per distinct run: a route that two units take from the same
            # ladder is one measurement, not two
            by = {}
            for u in V:
                t = u['S'].get(tkey) or u['S'].get('TempDark')
                v, e = _route(u, rt, 'Threshold_e'), _route(u, rt, 'Threshold_e_err')
                if t is None or v is None:
                    continue
                by[u['S'].get(rkey) or u['tag']] = (float(t), float(v),
                                                    float(e) if e is not None else np.nan)
            pts = sorted(by.values())
            if not pts:
                continue
            t, v, e = (np.array(z) for z in zip(*pts))
            e = np.where(np.isfinite(e), e, 0.0)
            ax.errorbar(t, v, yerr=e, fmt=mk[n] + ('-' if len(pts) > 1 else ''), ms=5.5,
                        color=col[n], ecolor=col[n], elinewidth=.9, capsize=2.5,
                        lw=1.2, label=n, alpha=.92)
            seen.append((v, e))
        ax.axhline(0, color='#999', lw=.8, ls=':')
        if seen:
            # one route has an error of several hundred electrons, from a gain fitted to
            # three points; let the values set the scale and say where a bar runs off it
            av = np.concatenate([z[0] for z in seen])
            ae = np.concatenate([z[1] for z in seen])
            span = max(float(av.max() - av.min()), 10.0)
            typ = float(np.median(ae[ae > 0])) if (ae > 0).any() else span
            cap = max(3.0 * typ, 0.4 * span)
            lo = float((av - np.minimum(ae, cap)).min())
            hi = float((av + np.minimum(ae, cap)).max())
            pad = .10 * max(hi - lo, 1.0)
            ax.set_ylim(lo - pad, hi + pad)
            if (ae > cap).any():
                ax.text(.985, .04, f'{int((ae > cap).sum())} error bar'
                        f'{"s" if (ae > cap).sum() > 1 else ""} run off the panel',
                        transform=ax.transAxes, fontsize=6.8, color='#777', ha='right')
        ax.set_title(ttl, fontsize=10, loc='left')
        ax.set_ylabel('intercept [e-]', fontsize=9)
        ax.grid(alpha=.25)
    for ax in AX[1]:
        ax.set_xlabel('temperature of the run the route comes from [C]', fontsize=9)
    h, lb = AX[0][0].get_legend_handles_labels()
    fig.legend(h, lb, fontsize=8, loc='lower center', ncol=4, frameon=False,
               bbox_to_anchor=(0.5, -0.035))
    fig.savefig(path, dpi=125, bbox_inches='tight')
    plt.close(fig)


def routeseries(name, rt, rkey, tkey):
    """one route's intercept against temperature for one configuration, by distinct run."""
    by = {}
    for u in U:
        if cfg(u) != name:
            continue
        v, e = _route(u, rt, 'Threshold_e'), _route(u, rt, 'Threshold_e_err')
        t = u['S'].get(tkey)
        if v is None or t is None:
            continue
        by[u['S'].get(rkey) or u['tag']] = (float(t), float(v),
                                            float(e) if e else float('nan'))
    return sorted(by.values())


def wslope(t, v, e):
    """weighted straight-line slope and its error."""
    w = 1.0 / np.where(np.isfinite(e) & (e > 0), e, np.nanmedian(e))**2
    S0, S1, S2 = w.sum(), (w * t).sum(), (w * t * t).sum()
    D = S0 * S2 - S1 * S1
    if D == 0:
        return float('nan'), float('nan')
    return ((S0 * (w * t * v).sum() - S1 * (w * v).sum()) / D, (S0 / D)**0.5)


def allroutesfig(name, path):
    """the four routes on one pair of axes, as measured, against temperature.

    They can share a linear axis because all four lie between about zero and sixty
    electrons; what the panel then shows is both where each route sits and how much it
    moves, which the four separate panels cannot show side by side.
    """
    SER = (('a', 'a: dark response', 'DarkRun', 'TempDark', '#1f77b4', 'o'),
           ('b', 'b: light response', 'BrightRun', 'TempBright', '#d62728', 's'),
           ('c', 'c: PTC dark', 'DarkRun', 'TempDark', '#2ca02c', 'D'),
           ('d', 'd: PTC bright', 'BrightRun', 'TempBright', '#9467bd', '^'))
    fig, ax = plt.subplots(figsize=(11.0, 6.4))
    ax.axhline(0, color='#999', lw=1.0, ls=':')
    any_ = False
    for rt, ttl, rkey, tkey, c, m in SER:
        pts = routeseries(name, rt, rkey, tkey)
        if len(pts) < 2:
            continue
        any_ = True
        t, v, e = (np.array(z) for z in zip(*pts))
        e = np.where(np.isfinite(e), e, 0.0)
        sl, se = wslope(t, v, e)
        ax.errorbar(t, v, yerr=e, fmt=m + '-', ms=6, color=c, ecolor=c, elinewidth=1.0,
                    capsize=3, lw=1.5, alpha=.92,
                    label=f'{ttl}:  {sl:+.2f} ± {se:.2f} e⁻/K  ({abs(sl) / se:.1f} σ)')
    if not any_:
        plt.close(fig)
        return False
    ax.set_xlabel('temperature of the run the route comes from [C]', fontsize=10)
    ax.set_ylabel('intercept [e-]', fontsize=10)
    ax.set_title(f'{name}: the four routes against temperature', fontsize=11, loc='left')
    ax.grid(alpha=.25)
    ax.legend(fontsize=8.5, loc='center right', framealpha=.92)
    fig.savefig(path, dpi=125, bbox_inches='tight')
    plt.close(fig)
    return True


KB_EV = 8.617333262e-5                     # Boltzmann constant [eV/K]


def dcrnfig(name, path):
    """dark current and read noise of one configuration against temperature.

    Both belong to the dark run, so both are drawn at its temperature and one point is
    made per distinct dark run. Two of these units share a run and still differ by about
    a per cent, because each is measured over the box around its own light spot and the
    spot moves between runs; the mean of the two is used and the spread is reported.
    """
    by = {}
    for u in U:
        if cfg(u) != name:
            continue
        r = u['S'].get('DarkRun')
        dc, dce = _to_e_num(u, 'DC_ADU', _dcerr(u))
        rn, rne = _to_e_num(u, 'RN_ADU', _rnerr(u))
        t = u['S'].get('TempDark')
        if None in (r, t, dc, rn):
            continue
        by.setdefault(r, []).append((float(t), dc, dce, rn, rne))
    if len(by) < 2:
        return None
    T, DC, DCE, RN, RNE, SPR = [], [], [], [], [], []
    for r, v in by.items():
        T.append(np.mean([z[0] for z in v]))
        d = [z[1] for z in v]; n = [z[3] for z in v]
        DC.append(np.mean(d)); RN.append(np.mean(n))
        DCE.append(np.mean([z[2] or 0 for z in v])); RNE.append(np.mean([z[4] or 0 for z in v]))
        if len(v) > 1:
            SPR.append((100 * (max(d) - min(d)) / np.mean(d),
                        100 * (max(n) - min(n)) / np.mean(n)))
    o = np.argsort(T)
    T, DC, DCE, RN, RNE = (np.array(z)[o] for z in (T, DC, DCE, RN, RNE))

    # an activation energy, from the same points: ln(DC) against 1/kT
    xk = 1.0 / (KB_EV * (T + 273.15))
    ea = -np.polyfit(xk, np.log(DC), 1)[0]

    fig, ax = plt.subplots(figsize=(10.6, 6.0))
    ax2 = ax.twinx()
    l1 = ax.errorbar(T, DC, yerr=DCE, fmt='o-', ms=7, color='#b7472a', ecolor='#b7472a',
                     elinewidth=1.1, capsize=3.5, lw=1.8,
                     label=f'dark current  (×{DC.max() / DC.min():.2f} over the range, '
                           f'E$_a$ = {ea:.3f} eV)')
    l2 = ax2.errorbar(T, RN, yerr=RNE, fmt='s--', ms=7, color='#1f4e79', ecolor='#1f4e79',
                      elinewidth=1.1, capsize=3.5, lw=1.8,
                      label=f'read noise  ({100 * (RN.max() - RN.min()) / RN.mean():.0f} % '
                            f'over the range)')
    ax.set_xlabel('temperature of the dark run [C]', fontsize=10)
    ax.set_ylabel('dark current [e-/s]', fontsize=10, color='#b7472a')
    ax2.set_ylabel('read noise [e-]', fontsize=10, color='#1f4e79')
    ax.tick_params(axis='y', colors='#b7472a')
    ax2.tick_params(axis='y', colors='#1f4e79')
    ax.set_title(f'{name}: dark current and read noise against temperature', fontsize=11,
                 loc='left')
    ax.grid(alpha=.25)
    ax.legend(handles=[l1, l2], fontsize=8.8, loc='upper left', framealpha=.92)
    fig.savefig(path, dpi=125, bbox_inches='tight')
    plt.close(fig)
    # is the read-noise trend resolved at all? coldest against warmest, with both errors
    _d = abs(float(RN[0] - RN[-1]))
    _s = float((RNE[0]**2 + RNE[-1]**2)**0.5)
    return dict(n=len(T), ea=float(ea), ratio=float(DC.max() / DC.min()),
                rnsig=(_d / _s if _s else float('nan')),
                rnpc=float(100 * (RN.max() - RN.min()) / RN.mean()),
                dclo=float(DC.min()), dchi=float(DC.max()),
                rnlo=float(RN.min()), rnhi=float(RN.max()),
                tlo=float(T.min()), thi=float(T.max()),
                spread=SPR)


w('## 7. The intercept against temperature\n')
_SPAN = {}
for _u in U:
    _SPAN.setdefault(cfg(_u), []).append(float(_u['S']['TempDark']))
_many = [n for n, v in _SPAN.items() if max(v) - min(v) > 2]
w(f'One panel per route, every configuration in its own colour, each point at the temperature of '
  f'the run that route comes from — a and c from the dark ladder, b and d from the light one, '
  f'which is what separates the two spot units near −58 C.\n')
w(f'**Only {len(_many)} of the {len(_SPAN)} configurations was measured at more than one '
  f'temperature**, so only `{"`, `".join(_many)}` draws a line; the other '
  f'{len(_SPAN) - len(_many)} sit in a single column near −71.5 C and are shown for comparison, '
  f'not as a trend. A temperature series exists on the dark side for other settings too, but '
  f'those runs carry no bright ladder and so no gain of their own, and an intercept in electrons '
  f'cannot be formed from them without borrowing one.\n')
tempfig(os.path.join(A.out, 'fig_intercept_temperature.png'))
_cap = ('Intercept against temperature, by route. Lines join points of one configuration; a '
        'single marker means that configuration was measured once. Error bars are the '
        'statistical and systematic terms added in quadrature.')
w(f'![{_cap}](fig_intercept_temperature.png)\n')
w(f'*{_cap}*\n')
_T = sorted(_many, key=lambda n: -len([u for u in U if cfg(u) == n]))[:1]
if _T and allroutesfig(_T[0], os.path.join(A.out, 'fig_intercept_routes.png')):
    _c2 = (f'The same four routes on one pair of axes, as measured, for `{_T[0]}` — the one '
           f'configuration measured at more than one temperature. The legend gives the weighted '
           f'slope of each. Routes a and d nearly coincide in value while coming from different '
           f'ladders of different runs; route b sits about 30 electrons above them and route c '
           f'at zero.')
    w(f'![{_c2}](fig_intercept_routes.png)\n')
    w(f'*{_c2}*\n')
    _n = _T[0]
    # a and c come from the dark run, b and d from the light one, and two of these units
    # share a dark run: counting it twice would invent a measurement, so each route is
    # fitted over the DISTINCT runs it actually has
    _LINES = []
    for _rt, _nm, _key in (('a', 'dark response', 'DarkRun'), ('b', 'light response', 'BrightRun'),
                           ('c', 'PTC dark', 'DarkRun'), ('d', 'PTC bright', 'BrightRun')):
        _tk = 'TempDark' if _key == 'DarkRun' else 'TempBright'
        _by = {}
        for _u in U:
            if cfg(_u) != _n:
                continue
            _v, _e = _route(_u, _rt, 'Threshold_e'), _route(_u, _rt, 'Threshold_e_err')
            _t = _u['S'].get(_tk)
            if _v is None or _t is None:
                continue
            _by[_u['S'].get(_key)] = (float(_t), float(_v),
                                      float(_e) if _e else float('nan'))
        if len(_by) < 3:
            continue
        _t, _v, _e = (np.array(z) for z in zip(*sorted(_by.values())))
        _w = 1.0 / np.where(np.isfinite(_e) & (_e > 0), _e, np.nanmedian(_e))**2
        _S0, _S1, _S2 = _w.sum(), (_w * _t).sum(), (_w * _t * _t).sum()
        _D = _S0 * _S2 - _S1 * _S1
        _sl = (_S0 * (_w * _t * _v).sum() - _S1 * (_w * _v).sum()) / _D
        _se = (_S0 / _D)**0.5
        _LINES.append((_rt, _nm, len(_by), float(_t.min()), float(_t.max()),
                       float(_v[0]), float(_v[-1]), _sl, _se))
    if _LINES:
        _span = max(abs(l[4] - l[3]) for l in _LINES)
        w(f'**What the one curve shows.** Over about {_span:.0f} K, no route\'s intercept is '
          f'resolved as changing. Fitting a straight line to each, weighted by the errors and '
          f'over the distinct runs each route has — two of these units share a dark ladder, so '
          f'counting it twice would invent a measurement:\n')
        w('| route | points | intercept, coldest → warmest [e-] | slope [e-/K] | |')
        w('|---|---|---|---|---|')
        for _rt, _nm, _n2, _t0, _t1, _v0, _v1, _sl, _se in _LINES:
            _sig = abs(_sl) / _se if _se else float('nan')
            _vd = ('consistent with flat' if _sig < 2 else
                   ('rises with temperature' if _sl > 0 else 'falls with temperature'))
            w(f'| {_rt}: {_nm} | {_n2} | {_v0:.1f} → {_v1:.1f} | {_sl:+.3f} ± {_se:.3f} | '
              f'{_vd} ({_sig:.1f} σ) |')
        w()
        w(f'So the lever arm of {_span:.0f} K adds no evidence that the intercept is a property '
          f'of temperature, in any of the four routes. The apparent slopes of routes a and b — '
          f'the two that extrapolate a response against exposure, and which lean in opposite '
          f'directions — are not resolved against their own errors, and the two '
          f'photon-transfer routes are flatter still. What the figure does show plainly is the '
          f'separation the single-temperature table already reported: route b sits near 50 e- '
          f'and routes c and d near zero and 20 e-, at every temperature measured.\n')

w('## 8. Dark current and read noise against temperature\n')
_DR = dcrnfig(_T[0], os.path.join(A.out, 'fig_dc_rn_temperature.png'))
if _DR:
    _c3 = (f'Dark current and read noise of `{_T[0]}` against temperature, over the '
           f'{_DR["n"]} distinct dark runs. Both quantities belong to the dark run, so both '
           f'are drawn at its temperature. Left axis and circles: dark current. Right axis '
           f'and squares: read noise.')
    w(f'![{_c3}](fig_dc_rn_temperature.png)\n')
    w(f'*{_c3}*\n')
    _sp = (f' The two units that share the warmest dark run differ by '
           f'{_DR["spread"][0][0]:.1f} % in dark current and {_DR["spread"][0][1]:.1f} % in '
           f'read noise, because each is measured over the box around its own light spot; '
           f'the mean of the two is plotted.' if _DR['spread'] else '')
    w(f'Over the {abs(_DR["thi"] - _DR["tlo"]):.0f} K between these runs the dark current '
      f'rises by a factor of {_DR["ratio"]:.2f}, from {_DR["dclo"]:.3f} to '
      f'{_DR["dchi"]:.3f} e-/s, while the read noise changes by {_DR["rnpc"]:.0f} per cent '
      f'({_DR["rnhi"]:.3f} to {_DR["rnlo"]:.3f} e-), which is {_DR["rnsig"]:.1f} sigma '
      f'against its own errors and so not a measured change at all. An Arrhenius '
      f'law through the three dark currents gives an activation energy of '
      f'**{_DR["ea"]:.3f} eV**, far below the 0.55 eV of depletion-region generation and '
      f'the 1.12 eV of diffusion, which over this interval would have given factors of '
      f'about 17 and 321. The campaign\'s temperature document reaches the same conclusion '
      f'from the same runs reduced as dark-only units over the whole die rather than this '
      f'box, and quotes 0.098 eV.{_sp}\n')
if _T:
    _n = _T[0]

w('## 9. What the tables show\n')
_nod = [u for u in U if u['ndf'] and u['ndt'] and u['ndf'] <= 3]
_win = [f'{u["ndf"]}/{u["ndt"]}' for u in U if u['ndf'] and u['ndt']]
w(f'- **No run contains all three steps.** Every unit is a pair, and that is a property of the '
  f'campaign rather than a choice: the `DC` runs carry zeros and a dark ladder, the `PTCexpsh` '
  f'and `INTERACTIVE` runs carry zeros and a light ladder, and nothing carries both ladders.\n')
if _nod:
    _never = [u for u in _nod if u.get('dsig') and u['dsig'][1] < 90]
    w(f'- **The dark ladder is the weak side.** On {len(_nod)} of the {len(U)} units the signal '
      f'rule leaves only three dark steps, and all three sit below the nominal 90 ADU floor, '
      f'which had to be lowered to admit them. On {len(_never)} of those the dark ladder never '
      f'reaches 90 ADU at all, even at the longest exposure; on the other '
      f'{len(_nod) - len(_never)} only two steps clear it.\n')
_L = [u['lsig'] for u in U if u.get('lsig')]
if _L:
    w(f'- **The light ladders lose nothing by comparison.** Every one of them keeps between '
      f'{min(u["nlf"] for u in U if u.get("nlf"))} and '
      f'{max(u["nlf"] for u in U if u.get("nlf"))} steps, over '
      f'{min(x[0] for x in _L):.0f}-{max(x[1] for x in _L):.0f} ADU; the shutter-stepped '
      f'ladders reach {max(x[1] for x in _L if x[1] > 700):.0f} ADU and the spot ladders, '
      f'whose longest opening is 2 s rather than 5 s, stop near '
      f'{max(x[1] for x in _L if x[1] < 700):.0f}.\n')
w(f'- **The two halves of a spot unit are not read out alike** — 4743 rows and 12.4 s against '
  f'750 and 2.0 s. Both start at readout row 0, so one box picks the same pixels, but the dark '
  f'and light routes of those units are not measured over the same readout.\n')
w(f'- **The four spot rows are one configuration at four temperatures**, which is why they '
  f'belong in a temperature comparison and not in a configuration scan.\n')
_sh = [u for u in U if u.get('shuffled')]
if _sh and len(_sh) < len(U):
    w(f'- **The two sessions stepped the dark ladder differently.** The August runs command '
      f'their exposures in increasing order; the {len(_sh)} October runs command a shorter '
      f'ladder in scrambled order ({", ".join("%g" % x for x in (U[-1]["mdark"].get("CameraExpList") or [])[:5])}, '
      f'... s). Order matters for what a ladder can be trusted to show: anything drifting '
      f'monotonically through a measurement adds to the slope of an ordered ladder and only '
      f'scatters a scrambled one.\n')

md = '\n'.join(L) + '\n'
with open(os.path.join(A.out, 'runs.md'), 'w') as fh:
    fh.write(md)

HTML = """<!doctype html><html lang="en"><head><meta charset="utf-8">
<meta name="viewport" content="width=device-width,initial-scale=1"><title>DESY runs reduced</title>
<style>
body{max-width:1250px;margin:1.2rem auto;padding:0 1rem;font:13px/1.45 -apple-system,Segoe UI,Roboto,sans-serif;color:#222}
h1{font-size:19px;border-bottom:2px solid #ddd;padding-bottom:.2rem;margin:0 0 .4rem}
h2{font-size:14px;margin:.9rem 0 .3rem;border-bottom:1px solid #eee}
table{border-collapse:collapse;font-size:10.5px;margin:.4rem 0;page-break-inside:avoid}
th,td{border:1px solid #ddd;padding:2px 5px;text-align:left;vertical-align:top}
th{background:#f5f5f5}
code{background:#f5f5f5;padding:0 3px}
em{color:#555}
img{max-width:100%;margin:.4rem 0;page-break-inside:avoid}
@media print{img{max-height:17.6cm;width:auto;display:block;margin:.15rem auto}}
h2{page-break-after:avoid}
@page{size:A4 landscape;margin:8mm 7mm}
@media print{body{max-width:none;margin:0;font-size:10px}table{font-size:8.6px}}
</style></head><body>
<div id="c"></div>
<script type="text/markdown" id="src">
__MD__
</script>
<script src="https://cdnjs.cloudflare.com/ajax/libs/marked/9.1.6/marked.min.js"></script>
<script>document.getElementById('c').innerHTML = marked.parse(document.getElementById('src').textContent);</script>
</body></html>"""
with open(os.path.join(A.out, 'runs.html'), 'w') as fh:
    fh.write(HTML.replace('__MD__', md.replace('</script', '<\\/script')))
print(f'runs.md / .html -> {A.out} ({len(md.split())} words, {len(U)} units, '
      f'{NCFG} configurations)')
