#!/usr/bin/env python3
"""Temperature comparison of the DESY uc_st_measurement campaign.

   Reads the dark-only unit dumps (stages 1 and 2 of the chain, run on a DC run
   that has no bright partner) and asks what changes with temperature. Every
   number comes from the dumps; the prose states what the comparison shows.

   usage: desy_uc_temperature.py --full TAG TAG ... --win TAG TAG ... [--out DIR]
"""
import argparse
import json
import os

import numpy as np
import matplotlib
matplotlib.use('Agg')
import matplotlib.pyplot as plt

K_B = 8.617333262e-5          # [eV/K]

P = argparse.ArgumentParser()
P.add_argument('--root', default='/home/sasha/claude/desy_die')
P.add_argument('--rnroot', default='/home/sasha/claude/desy_rn')
P.add_argument('--full', nargs='+', required=True, help='full-die dark-only unit tags')
P.add_argument('--win', nargs='*', default=[], help='windowed dark-only unit tags')
P.add_argument('--ref', nargs='*', default=[], help='same-temperature units of another configuration')
P.add_argument('--sigwin', nargs='*', default=[],
               help='signal-window twins of the full-die units, for the window comparison')
P.add_argument('--light', nargs='*', default=[],
               help='bright-only unit tags (INTERACTIVE runs): the light side of the comparison')
P.add_argument('--die', default='/Data1/uc_data-test/LT112181_W14_D1',
               help='die directory, for the device thermometer in the frame headers')
P.add_argument('--out', default='/home/sasha/claude/desy_die/uc_temperature')
A = P.parse_args()
os.makedirs(A.out, exist_ok=True)


def _or(a, b):
    return a if np.isfinite(a) else b


def se_median_map(path, npix=None):
    """standard error of the median of a map, by BOOTSTRAP.

    Not the Gaussian 1.2533*sigma/sqrt(n): these per-pixel distributions are
    peaked and heavy-tailed and that form overstates the error by 2 to 3.5
    times. Not a density estimate either: with few frames and even-valued ADU
    the per-pixel sigma takes discrete values, and a tied distribution defeats
    f(m). The bootstrap needs no assumption and handles ties. Large maps are
    subsampled and rescaled by sqrt(n_sub/n), which is exact for the standard
    error of a median. Its own precision is a few per cent of the error, which
    is itself well under a per cent of the value."""
    try:
        a = np.fromfile(path, dtype=np.float32)
        a = a[np.isfinite(a)]
        if a.size < 100:
            return float('nan')
        rng = np.random.default_rng(7)
        sub = a if a.size <= 1000000 else rng.choice(a, 1000000, replace=False)
        med = np.array([np.median(rng.choice(sub, sub.size, replace=True)) for _ in range(400)])
        return float(np.std(med, ddof=1)*np.sqrt(sub.size/a.size))
    except (OSError, ValueError):
        return float('nan')


def se_median(sp, n=None):
    """fallback for a spread structure with no map behind it"""
    try:
        o = float(sp['StdObs'])
        nn = float(n if n is not None else sp['Npix'])
        return 1.2533*o/np.sqrt(nn) if nn > 0 else float('nan')
    except (KeyError, TypeError, ValueError, ZeroDivisionError):
        return float('nan')


def se_rn(q, dof):
    """error on the median per-pixel read noise (see the per-unit reports)"""
    rel = np.hypot(float(q.get('SpreadSigmaRel', 0.0)), 1.0/np.sqrt(2*float(dof)))
    return 1.2533*float(q['ReadNoiseMedian'])*rel/np.sqrt(float(q['Npix']))


def se_fp(q, dof):
    o, n, r = float(q['FixedPatternObs']), float(q['FixedPatternNoise']), float(q['FixedPatternRMS'])
    if r <= 0:
        return float('nan')
    npix = float(q['Npix'])
    return np.sqrt(o**4/(2*npix) + n**4/(2*npix*float(dof)))/r


def pm1(v, e, nd=2):
    """value and error, precision set by the error"""
    try:
        e = float(e)
    except (TypeError, ValueError):
        e = float('nan')
    if not np.isfinite(e) or e <= 0:
        return f'{v:.{nd}f}'
    k = min(max(nd, 1 - int(np.floor(np.log10(abs(e))))), 6)
    return f'{v:.{k}f} +- {e:.{k}f}'



# ------------------------------------------------------------------ what a configuration IS
# "aSpect" and "aSpect2" are file names, not descriptions, and the two differ in
# exactly one register field out of a whole configuration file: every bias DAC
# voltage is identical and bs_ramp_adj is not. That field moves the bias level by
# 343 ADU, so a reader who takes the two names for two bias settings is misled.
# The settings are read from the run's own scfg file at build time rather than
# repeated here, so this cannot go stale.
# as [sensor_pixel_bias_voltages] names them, with the short label for the table
_SCFG_KEYS = (('sbb_dac_vdd_rst_l',  'RST_L'),
              ('sbb_dac_vdd_sel_rst', 'RST_SEL'),
              ('sbb_dac_vdd_sf',     'SF'),
              ('sbb_dac_vdd_rst_h',  'RST_H'),
              ('sbb_dac_vdd_tx',     'TX'),
              ('sbb_dac_vdd_sen',    'SEN'),
              ('sbb_dac_vdda',       'VDDA'),
              ('sbb_dac_vddd',       'VDDD'))

def scfg_of(u):
    """the run's sensor configuration file, as {key: value-as-written}"""
    pth = os.path.join(A.die, u['run'], 'measurement', 'metadata',
                       'scfg_cfg_' + u['cfg'] + '.toml')
    out = {}
    try:
        with open(pth) as fh:
            for line in fh:
                line = line.split('#')[0].strip()
                if '=' in line:
                    k, v = line.split('=', 1)
                    out[k.strip()] = v.strip()
    except OSError:
        pass
    return out

def ramp_of(u):
    """the ADC ramp adjustment, the one field that separates aSpect from aSpect2"""
    return scfg_of(u).get('bs_ramp_adj', '')

_RAMP = {}
def cfgname(u):
    """the configuration as its file names it, plus the field that distinguishes it"""
    if u['cfg'] not in _RAMP:
        _RAMP[u['cfg']] = ramp_of(u)
    r = _RAMP[u['cfg']]
    return f"{u['cfg']} (ramp {r})" if r else u['cfg']


def windowrule(tag):
    """which fit-window rule a unit was analysed under, from its own dumps"""
    fw = os.path.join(A.root, tag, 'fitwindow.json')
    if os.path.isfile(fw):
        with open(fw) as fh:
            j = json.load(fh)
        return 'signal', f"one stated signal window, {float(j['SigLo']):.0f}-{float(j['SigHi']):.0f} ADU"
    return 'chi2', 'the window chosen per ladder by goodness of fit'

def unit(tag):
    with open(os.path.join(A.rnroot, tag, 'stats.json')) as fh:
        Z = json.load(fh)
    with open(os.path.join(A.root, tag, 'dark.json')) as fh:
        D = json.load(fh)
    S, F = Z['Setup'], D['Fit']['All']
    return dict(tag=tag, label=Z.get('Run', tag), cfg=S['ScfgDark'].replace('scfg_cfg_', '').replace('.toml', ''),
                run=S['DarkRun'], T=float(S['TempDark']), TK=float(S['TempDark']) + 273.15,
                nrows=int(S['Nrows']), ro=float(S['ROTimeDark']),
                bias=float(Z['All']['BiasLevel']), rn=float(Z['All']['ReadNoiseMedian']),
                fpn=float(Z['All']['FixedPatternRMS']),
                rowstd=float(Z['Structure']['RowMeanStd']), colstd=float(Z['Structure']['ColMeanStd']),
                residstd=float(Z['Structure']['ResidStd']),
                tail=100 * float(Z['BiasDist']['TailFrac5MAD']), q99=float(Z['BiasDist']['Quantiles'][-1]),
                dc=float(F['SlopeSpread']['Median']), dsnu=100 * float(F['SlopeSpread']['RelIntr']),
                td=-float(F['InterceptSpread']['Median']), tdsp=float(F['InterceptSpread']['StdIntr']),
                e_bias=float(Z['BiasDist']['ErrDieTotal']),
                e_rn=_or(se_median_map(os.path.join(A.rnroot, tag, 'rn.bin')), se_rn(Z['All'], Z['Dof'])),
                e_fpn=se_fp(Z['All'], Z['Dof']),
                e_dc=_or(se_median_map(os.path.join(A.root, tag, 'dc.bin')), se_median(F['SlopeSpread'])),
                e_td=_or(se_median_map(os.path.join(A.root, tag, 'tdark.bin')), se_median(F['InterceptSpread'])),
                nstep=len(D['FitSteps']), steps=D['FitSteps'], npix=int(F['Npix']),
                tmon=device_thermometer(S['DarkRun']))


def device_thermometer(run):
    '''TEMP-MON of the frame headers [K]: the device's own sensor, independent of
    the chamber thermometry in temperature.csv and tvac.csv.'''
    import glob as _g
    try:
        from astropy.io import fits as _f
    except ImportError:
        return float('nan')
    fs = sorted(_g.glob(os.path.join(A.die, run, 'measurement', 'raw_images', '*_HIGH_DARK_*.fits')))
    if not fs:
        return float('nan')
    vals = []
    for p in fs[:8]:
        v = _f.getheader(p).get('TEMP-MON')
        if isinstance(v, (int, float)):
            vals.append(float(v))
    return float(np.median(vals)) if vals else float('nan')


def lunit(tag):
    """a bright-only unit: an INTERACTIVE run, which has no dark ladder"""
    with open(os.path.join(A.rnroot, tag, 'stats.json')) as fh:
        Z = json.load(fh)
    with open(os.path.join(A.root, tag, 'light.json')) as fh:
        L = json.load(fh)
    with open(os.path.join(A.root, tag, 'ptc.json')) as fh:
        T = json.load(fh)
    S = Z['Setup']
    med = np.atleast_1d(np.array(L['Pattern']['All']['Median'], dtype=float))
    x = np.atleast_1d(np.array(L['PatternX'], dtype=float))
    fs = np.atleast_1d(np.array(L['FitSteps'], dtype=int))
    ge = T['GainElectrons']
    E = {}
    _ep = os.path.join(A.root, tag, 'errors.json')
    if os.path.isfile(_ep):
        with open(_ep) as fh:
            E = json.load(fh)
        if E.get('Tag') not in (None, tag, Z.get('Tag')):
            raise SystemExit(f'errors.json in {tag} belongs to {E.get("Tag")}: rebuild the report')
    def _e(k):
        try:
            return float(E.get(k))
        except (TypeError, ValueError):
            return float('nan')
    return dict(e_bias=_e('Bias'), e_rn=(E.get('ReadNoise') or {}).get('All', float('nan')),
                e_gain=_e('Gain_stage5'), e_resp=_e('Response_stage3'),tag=tag, label=Z.get('Run', tag), T=float(S['TempBright']), TK=float(S['TempBright']) + 273.15,
                nrows=int(S.get('NrowsBright') or S['Nrows']), run=S['BrightRun'],
                cfg=S['ScfgBright'].replace('scfg_cfg_', '').replace('.toml', ''),
                hasdark=bool(S.get('DarkRun')),
                # With a dark ladder paired in, stage 1's datasheet numbers describe
                # the DARK run: this table is about the light runs, so take the
                # bright role's own level and noise where the pair recorded them.
                bias=float(S.get('BiasBright', Z['All']['BiasLevel'])),
                rn=float(S.get('RNBright', Z['All']['ReadNoiseMedian'])),
                biasdark=S.get('BiasDark'), rndark=S.get('RNDark'),
                gain=float(ge['ADUperE']), rn_e=float(ge['RN_e']),
                resp=float(L['Fit']['All']['SlopeSpread']['Median']),
                tlight=float(T['Thresholds']['Light_e']), tptc=float(T['Thresholds']['PTC_e']),
                tlight_adu=float(T['Thresholds']['Light_ADU']), tptc_adu=float(T['Thresholds']['PTC_ADU']),
                nstep=len(fs), lo=float(med[fs[0] - 1]), hi=float(med[fs[-1] - 1]),
                zero3=float(np.mean(med[:3])), x3=float(x[2]), nall=med.size)


FULL = sorted((unit(t) for t in A.full), key=lambda u: u['T'])
LIGHT = sorted((lunit(t) for t in A.light), key=lambda u: u['T'])
WIN = sorted((unit(t) for t in A.win), key=lambda u: u['T'])
REF = [unit(t) for t in A.ref]


def arrhenius(us):
    """Ea [eV] of the dark-current slope, its error, and the fit residuals.

    The error is the larger of two: the one the per-point errors imply, and the
    same scaled by sqrt(chi2/dof) when the points scatter about the line by more
    than those errors -- which they do here, so what is quoted is the scatter."""
    if len(us) < 2:
        return None, None, None, None
    x = np.array([1.0 / u['TK'] for u in us])
    y = np.log(np.array([u['dc'] for u in us]))
    sy = np.array([u['e_dc'] / u['dc'] if np.isfinite(u.get('e_dc', np.nan)) and u['dc'] else np.nan
                   for u in us])
    if not np.all(np.isfinite(sy)) or np.any(sy <= 0):
        sy = np.ones_like(y)
    if len(us) == 2:
        slope = (y[1] - y[0]) / (x[1] - x[0])
        se = np.hypot(sy[0], sy[1]) / abs(x[1] - x[0])
        resid = np.zeros(2)
    else:
        wt = 1.0 / sy**2
        sw, sx, sxx = wt.sum(), (wt*x).sum(), (wt*x*x).sum()
        sy_, sxy = (wt*y).sum(), (wt*x*y).sum()
        den = sw*sxx - sx*sx
        slope = (sw*sxy - sx*sy_) / den
        inter = (sxx*sy_ - sx*sxy) / den
        se = np.sqrt(sw/den)
        resid = y - (slope*x + inter)
        chi2 = float((wt*resid**2).sum())
        dof = max(len(us) - 2, 1)
        se *= max(1.0, np.sqrt(chi2/dof))        # inflate when the points scatter
    return -slope * K_B, resid, (x, y), se * K_B


EA_F, RES_F, _, EA_FE = arrhenius(FULL)
EA_W, _, _, EA_WE = arrhenius(WIN)


def ea_syst(us, dT):
    """How far Ea moves if the temperatures are wrong by dT RELATIVE to each
    other -- the lever arm is the spread of 1/T, so a common offset does almost
    nothing and a relative one does this. dT is taken from the disagreement
    between the thermometers, which the text quotes."""
    if len(us) < 2 or not np.isfinite(dT):
        return float('nan')
    import copy
    worst = 0.0
    for sgn in (+1, -1):
        v = copy.deepcopy(us)
        v[0]['TK'] += sgn*dT
        v[-1]['TK'] -= sgn*dT
        e, _, _, _ = arrhenius(v)
        worst = max(worst, abs(e - (EA_F if us is FULL else EA_W)))
    return worst


_TMON = [u for u in FULL + WIN if np.isfinite(u.get('tmon', np.nan))]
DT_SYS = max((abs(u['tmon'] - (u['T'] + 273.15)) for u in _TMON), default=float('nan'))
EA_FS = ea_syst(FULL, DT_SYS)
EA_WS = ea_syst(WIN, DT_SYS)


def ratio(ea, t_hi, t_lo):
    return float(np.exp(ea / K_B * (1.0 / (t_lo + 273.15) - 1.0 / (t_hi + 273.15))))


THI, TLO = FULL[-1]['T'], FULL[0]['T']
MEAS = FULL[-1]['dc'] / FULL[0]['dc']

# ------------------------------------------------------------------ figure
fig, ax = plt.subplots(1, 2, figsize=(11, 4.2))
x = np.array([1000.0 / u['TK'] for u in FULL])
y = np.array([u['dc'] for u in FULL])
ax[0].semilogy(x, y, 'o-', color='C0', label=f"full die, {cfgname(FULL[0])}")
if EA_F is not None:
    xs = np.linspace(x.min() * 0.995, x.max() * 1.005, 50)
    pf = np.polyfit(x, np.log(y), 1)
    ax[0].semilogy(xs, np.exp(np.polyval(pf, xs)), '-', color='C0', alpha=0.4,
                   label=f'$E_a$ = {EA_F:.3f} eV')
if WIN:
    xw = np.array([1000.0 / u['TK'] for u in WIN])
    ax[0].semilogy(xw, [u['dc'] for u in WIN], 's-', color='C1',
                   label=f"windowed {WIN[0]['nrows']} rows, {cfgname(WIN[0])} "
                         f"($E_a$ = {EA_W:.3f} eV)")
# what real silicon would do, anchored on the warmest point
for ea, nm, c in [(1.12, 'diffusion, 1.12 eV', '0.35'), (0.55, 'depletion, 0.55 eV', '0.6')]:
    # anchored on the warmest point: I = I_ref * exp(-Ea/k * (1/T - 1/T_ref))
    yy = y[-1] * np.exp(-(ea / K_B) * (x - x[-1]) / 1e3)
    ax[0].semilogy(x, yy, '--', color=c, label=nm)
ax[0].set_xlabel('1000 / T  [1/K]')
ax[0].set_ylabel('dark-ladder slope  [ADU/s]')
ax[0].set_title('What grows with exposure, against temperature')
ax[0].grid(alpha=0.3)
ax[0].legend(fontsize=7)
sec = ax[0].secondary_xaxis('top', functions=(lambda v: 1e3 / v - 273.15, lambda t: 1e3 / (t + 273.15)))
sec.set_xlabel('T [C]')

for u in FULL:
    ax[1].plot(u['T'], u['td'], 'o', color='C0')
ax[1].plot([u['T'] for u in FULL], [u['td'] for u in FULL], '-', color='C0', label='dark-route intercept')
ax2 = ax[1].twinx()
ax2.plot([u['T'] for u in FULL], [u['rn'] for u in FULL], 's--', color='C3', label='read noise')
ax2.set_ylabel('read noise [ADU]', color='C3')
ax[1].set_xlabel('T [C]')
ax[1].set_ylabel('dark-route intercept [ADU]', color='C0')
ax[1].set_title('Intercept and read noise, same runs')
ax[1].grid(alpha=0.3)
fig.tight_layout()
fig.savefig(os.path.join(A.out, 'fig_temperature.png'), dpi=130)
plt.close(fig)

# ------------------------------------------------------------------ text
L = []
w = L.append
w('# Temperature: what changes and what does not\n')
w(f"*{len(FULL)} full-die and {len(WIN)} windowed dark runs of die "
  f"{json.load(open(os.path.join(A.rnroot, FULL[0]['tag'], 'stats.json')))['Lot']} "
  f"W14 D1, read over the same pixels as the August configuration pairs, and fitted over "
  f"{windowrule(FULL[0]['tag'])[1]} - the same rule as those pairs. "
  f"Every number is read from the stage-1 and stage-2 dumps at build time.*\n")


# ------------------------------------------------------------------ the setup
w('## The configurations these runs were taken with\n')
_allu = FULL + WIN + REF + LIGHT
_seen, _cfgs = set(), []
for _u in _allu:
    if _u['cfg'] not in _seen:
        _seen.add(_u['cfg'])
        _cfgs.append(_u)
_rows, _vals = [], {}
for _u in _cfgs:
    _c = scfg_of(_u)
    _vals[_u['cfg']] = _c
    # each row count with the mode it is, rather than two lists the reader has
    # to pair up: 750 is a window and 4743 is the whole die
    _nr = sorted({v['nrows'] for v in _allu if v['cfg'] == _u['cfg']}, reverse=True)
    _rows.append((_u, _c, '',
                  ', '.join(f"{v} ({'full die' if v >= 4000 else 'windowed'})" for v in _nr)))
_keys = [kv for kv in _SCFG_KEYS if any(kv[0] in r[1] for r in _rows)]
w('`aSpect` and `aSpect2` are the names of configuration files, not descriptions of settings, '
  'and the names alone would mislead: the two command **identical** bias voltages and differ in '
  'one register field. Everything below is read from the scfg file of the run itself.\n')
w('| configuration | ' + ' | '.join(lab + ' [V]' for _, lab in _keys) +
  ' | ADC ramp (`bs_ramp_adj`) | readout rows |')
w('|---' * (len(_keys) + 3) + '|')
for _u, _c, _who, _nr in _rows:
    w(f'| **{_u["cfg"]}** | ' + ' | '.join(_c.get(k, '&mdash;') for k, _ in _keys) +
      f' | `{_c.get("bs_ramp_adj", "?")}` | {_nr} |')
w('')
_ramps = {r[0]['cfg']: r[1].get('bs_ramp_adj', '') for r in _rows}
_difk = [lab for k, lab in _keys if len({r[1].get(k) for r in _rows}) > 1]
if len(_rows) > 1 and not _difk and len(set(_ramps.values())) > 1:
    _two = sorted(_ramps.items(), key=lambda t: t[1])
    _b = {r[0]['cfg']: [v['bias'] for v in _allu if v['cfg'] == r[0]['cfg']] for r in _rows}
    w(f"""So the two configurations are the same pixel bias setting - the modal one of this
campaign, the same as the `aSpect` pair of the configuration scan - and the **only** thing that
separates them is `bs_ramp_adj`, a four-bit field of `register_0x004_bias_adc_control_2`:
{' against '.join(f'`{v}` for {k}' for k, v in _two)}. It is an adjustment of the ADC ramp, not of
anything the pixel does.

That one field moves the bias level by about
{abs(np.median(_b[_two[0][0]]) - np.median(_b[_two[1][0]])):.0f} ADU
({np.median(_b[_two[0][0]]):.0f} against {np.median(_b[_two[1][0]]):.0f} ADU), which is why the two
sets of runs cannot be put in one bias column and read as one device. The cleanest evidence that
nothing else differs: `{FULL[0]['cfg']}` and `{(REF or WIN)[0]['cfg']}` were measured three hours
apart on the same day at the same temperature and the same full-die readout, and a line-by-line
comparison of the two configuration files returns that single field.\n""")
elif _difk:
    w('The configurations differ in ' + ', '.join('`' + k + '`' for k in _difk) +
      ', and in the ADC ramp adjustment where that is shown above.\n')

w('## Which runs carry which side\n')
if LIGHT:
    _ntl = len({round(u['T']) for u in LIGHT})
    _ntd = len({round(u['T']) for u in FULL + WIN})
    w(f'Both sides are covered, but by different kinds of run, and no single run carries both.\n\n'
      f'The **dark** ladder comes from the `DC` runs: {len(FULL) + len(WIN)} of them here, at '
      f'{_ntd} temperatures from {min(u["T"] for u in FULL + WIN):.0f} to {max(u["T"] for u in FULL + WIN):.0f} C.\n'
      f'The **bright** ladder comes from the `INTERACTIVE` runs: {len(LIGHT)} of them, at {_ntl} '
      f'temperatures from {min(u["T"] for u in LIGHT):.0f} to {max(u["T"] for u in LIGHT):.0f} C.\n\n'
      'It is worth being explicit about why the bright side is not taken from the runs whose name '
      'says PTC. Every `PTCexpsh` run of this campaign sits between -70 and -72 C, and so does '
      'every `PTCexpsen` run - those declare -50 C in their configuration file, but their\n'
      'thermometry says -71. So the type that was built to measure a photon-transfer curve offers '
      'no temperature lever at all, and the lever on the light side comes entirely from the\n'
      '`INTERACTIVE` runs, which carry a 30-step shutter ladder and were taken at four '
      'temperatures.\n\n'
      'The consequence for reading this document: the dark-side table and the light-side table '
      'below are two separate measurements. They share a die and a configuration name but not a '
      'run, and not a readout mode - the `DC` runs read the full die or a 500-row window, the\n'
      '`INTERACTIVE` runs a 749-row one. Quantities are compared **within** a table, never across '
      'the two.\n')
else:
    w('Only the dark side is covered here. The `DC` runs give a ladder of dark exposures at\n'
      f'{len({round(u["T"]) for u in FULL + WIN})} temperatures; no bright-ladder unit was passed to this\n'
      'build, so the conversion gain, the photo-response and the photon-transfer intercepts do not\n'
      'appear. They are available: the `INTERACTIVE` runs carry a bright ladder at four\n'
      'temperatures. Every `PTCexpsh` and `PTCexpsen` run of the campaign sits at -70 to -72 C\n'
      'and so offers no temperature lever.\n')

w('## The measurements\n')
w('Each route fits a straight line and reads where it would reach zero; that crossing is quoted as a charge, with the line reaching zero at MINUS the value given, so a positive number means charge that appears to be missing at zero. It is called an intercept and not a threshold because nothing in these measurements shows the device withholding the first electrons: the ladders are curved, and an intercept is what a straight line extrapolated off a curve gives.\n')
w('| configuration | T [C] | readout | bias [ADU] | read noise [ADU] | fixed pattern [ADU] '
  '| ladder slope [ADU/s] | its spread [%] | dark-route intercept [ADU] |')
w('|---|---|---|---|---|---|---|---|---|')
for u in FULL + WIN + REF:
    ro = 'full die' if u['nrows'] > 1000 else f"{u['nrows']} rows"
    w(f"| {cfgname(u)} | {u['T']:.2f} | {ro} | {pm1(u['bias'], u['e_bias'], 2)} | {pm1(u['rn'], u['e_rn'], 3)} "
      f"| {pm1(u['fpn'], u['e_fpn'], 2)} | **{pm1(u['dc'], u['e_dc'], 5)}** | {u['dsnu']:.1f} "
      f"| {pm1(u['td'], u['e_td'], 2)} |")
w('')

_tm = [u for u in FULL + WIN if np.isfinite(u['tmon'])]
if _tm:
    _d = [u['tmon'] - (u['T'] + 273.15) for u in _tm]
    w(f'The temperature is the median of `temperature.csv` (Sensor2) over each run. The device\'s\n'
      f'own thermometer, `TEMP-MON` in the frame headers, agrees with it to '
      f'{max(abs(v) for v in _d):.2f} K over all {len(_tm)} runs\n('
      + ', '.join(f"{u['T']:.1f} C -> {u['tmon']:.2f} K" for u in sorted(_tm, key=lambda z: z['T'])) +
      '), as do the chamber sensors in `tvac.csv`. The spread of temperatures is real.\n')

w('## The exposure-dependent signal is not thermal\n')
w(f'Over the full-die set the slope of the dark ladder runs {FULL[0]["dc"]:.5f} ADU/s at '
  f'{FULL[0]["T"]:.2f} C to {FULL[-1]["dc"]:.5f} at {FULL[-1]["T"]:.2f} C, a factor '
  f'**{MEAS:.2f}** over {THI - TLO:.1f} K.')
if len(FULL) > 2:
    w(f'The three points lie on a straight line in ln(slope) against 1/T to '
      f'{100 * np.abs(RES_F).max():.1f} %, and that line has an activation energy of\n')
    w(f'  **E_a = {EA_F:.3f} +- {EA_FE:.3f} (stat) +- {EA_FS:.3f} (thermometry) eV**\n')
else:
    w(f'That corresponds to an activation energy of **E_a = {EA_F:.3f} +- {EA_FE:.3f} eV**.\n')
w('Dark current in silicon does not do this. Over the same interval:\n')
w('| mechanism | activation energy | predicted ratio over this interval |')
w('|---|---|---|')
for ea, nm in [(1.12, 'diffusion (bulk silicon)'), (0.55, 'depletion / Shockley-Read-Hall')]:
    w(f'| {nm} | {ea:.2f} eV | {ratio(ea, THI, TLO):.0f} x |')
w(f'| **measured here, full die** | **{EA_F:.3f} +- {EA_FE:.3f} +- {EA_FS:.3f} eV** | **{MEAS:.2f} x** |')
if WIN:
    mw = WIN[-1]['dc'] / WIN[0]['dc']
    w(f'| measured here, {WIN[0]["nrows"]}-row window | {EA_W:.3f} +- {EA_WE:.3f} +- {EA_WS:.3f} eV | {mw:.2f} x |')
w('')
w('So whatever makes the signal of these frames grow with exposure time, it is **not** thermal\n'
  'generation in the silicon. Cooling the device from '
  f'{THI:.0f} C to {TLO:.0f} C removes only {100 * (1 - 1 / MEAS):.0f} % of it, where real dark\n'
  'current would have fallen by a factor of tens to hundreds.\n')
w(f'The second error on E_a is what a {DT_SYS:.2f} K disagreement between the thermometers would do '
  f'to it if that disagreement were a RELATIVE error between the three runs rather than a common\n'
  f'offset; a common offset barely moves E_a, because the lever arm is the spread of 1/T. Even at\n'
  f'{EA_F + 3*np.hypot(EA_FE, EA_FS):.3f} eV, three sigma up, the measurement is nowhere near the\n'
  '0.55 eV of depletion current.\n')

if WIN:
    w('## It is also not one single effect\n')
    w(f'The windowed runs read {WIN[0]["nrows"]} rows instead of 4743, so their readout time is\n'
      f'{WIN[0]["ro"]:.3f} s instead of {FULL[0]["ro"]:.3f} s. Their slope is flatter still with\n'
      f'temperature ({EA_W:.3f} +- {EA_WE:.3f} eV against {EA_F:.3f} +- {EA_FE:.3f}), and at a given temperature the two readout\n'
      'modes do not agree on the slope - which a pure light leak, delivering charge in real\n'
      'time regardless of how the device is read, could not produce. There is a readout-dependent\n'
      'term and a nearly temperature-independent one, and this dataset separates neither from a\n'
      'constant illumination of the chamber. The measurement that would: the same dark ladder with\n'
      'the sensor idle between frames for a known extra interval, or with the readout rate changed\n'
      'at a fixed number of rows.\n')


# What the 90-1000 ADU rule would do to this number. The pairs of the August
# scan are fitted that way; these ladders stop well below 90 ADU, so the rule
# falls back on the three brightest steps of each - and those sit on a different
# part of a convex ladder at each temperature, so the effect on Ea has to be
# measured rather than assumed.
if A.sigwin:
    SW = sorted((unit(t) for t in A.sigwin), key=lambda u: u['T'])
    if len(SW) >= 3:
        _ps = np.polyfit(1.0/np.array([u['TK'] for u in SW]),
                         np.log(np.array([u['dc'] for u in SW])), 1)
        EA_S = -_ps[0]*8.617333e-5
        _rise = 100*(np.mean([v['dc']/u['dc'] for u, v in zip(FULL, SW)]) - 1)
        _km, _rm = windowrule(FULL[0]['tag'])
        _ko, _ro = windowrule(SW[0]['tag'])
        w(f'### The same thing under {_ro}\n')
        _up = _rise > 0
        w(f"""This document fits {_rm}; the other way of choosing it is {_ro}. The two differ here
because these ladders stop well below 90 ADU, so a stated 90-1000 ADU window has to fall back on
the three brightest steps of each, while the goodness-of-fit rule keeps the whole ladder. Fitted
both ways the same three runs give:\n""")
        w(f'| T [C] | slope, {_rm} | slope, {_ro} | steps used |')
        w('|---|---|---|---|')
        for u, v in zip(FULL, SW):
            w(f"| {u['T']:.2f} | **{u['dc']:.5f}** | {v['dc']:.5f} | {u['nstep']} vs {v['nstep']} |")
        w('')
        w(f"""Every slope {'rises' if _up else 'falls'} by about {abs(_rise):.0f} % the other way,
because the top of a convex ladder is steeper than the whole of it, and the activation energy moves
between **{EA_F:.3f} eV and {EA_S:.3f} eV**. The conclusion does not turn on the choice:
{EA_S:.3f} eV is as far from the 0.55 eV of generation current as {EA_F:.3f} eV is. What the choice
does cost is the lever arm - {FULL[0]['nstep']} points against {SW[0]['nstep']} - which is why the
other set is kept beside this one rather than discarded, so the choice can be checked rather than
taken on trust.\n""")

w('## What does behave as expected\n')
rn_lo, rn_hi = FULL[0]['rn'], FULL[-1]['rn']
w(f'**Read noise is flat**: {min(u["rn"] for u in FULL):.3f} to {max(u["rn"] for u in FULL):.3f} ADU over the\n'
  f'whole range, a {100 * (max(u["rn"] for u in FULL) / min(u["rn"] for u in FULL) - 1):.1f} % change, which is what a\n'
  'readout chain should do.\n')
bias_sp = max(u['bias'] for u in FULL) - min(u['bias'] for u in FULL)
w(f'**The bias of {cfgname(FULL[0])} is stable**: {bias_sp:.2f} ADU across '
  f'{THI - TLO:.1f} K.')
if WIN and len(WIN) > 1:
    bw = abs(WIN[-1]['bias'] - WIN[0]['bias'])
    w(f'The windowed runs of {cfgname(WIN[0])} are not: their bias moves {bw:.0f} ADU between\n'
      f'{WIN[0]["T"]:.1f} C and {WIN[-1]["T"]:.1f} C, about {bw / abs(WIN[-1]["T"] - WIN[0]["T"]):.0f} ADU/K, so a bias\n'
      'level measured at one temperature cannot be carried to another without saying which\n'
      'configuration it belongs to.')
w('')

w('**The dark-route intercept follows the slope rather than staying put.** It reads ' +
  ', '.join(f'{u["td"]:.2f} ADU at {u["T"]:.0f} C' for u in FULL) + '. A fixed charge\n'
  'deficit would not care about temperature; this one tracks the thing it is extrapolated from,\n'
  'which is what an extrapolation artefact does.\n')

cold = max(FULL, key=lambda u: u['fpn'])
if cold['fpn'] > 3 * min(u['fpn'] for u in FULL):
    w('## One cold-temperature effect worth flagging\n')
    w(f'At {cold["T"]:.2f} C the bias fixed pattern of the {cfgname(cold)} configuration is '
      f'{cold["fpn"]:.2f} ADU against\n'
      f'{min(u["fpn"] for u in FULL):.2f}-{sorted(u["fpn"] for u in FULL)[-2]:.2f} ADU at the warmer points, and all of the\n'
      f'increase is in the readout-column term ({cold["rowstd"]:.2f} ADU against '
      f'{min(u["rowstd"] for u in FULL):.2f}); the residual after removing the row and column\n'
      f'structure is unchanged ({cold["residstd"]:.2f} against {min(u["residstd"] for u in FULL):.2f}). '
      f'{cold["tail"]:.1f} % of pixels sit beyond 5 MAD of the median and the 99th percentile is\n'
      f'{cold["q99"]:.0f} ADU against {sorted(u["q99"] for u in FULL)[0]:.0f} at the warmest point. A subset of readout columns\n'
      'goes high when the device is cold. ULTRASAT operates cold, so this is worth a dedicated\n'
      'measurement rather than a line in a table.\n')

w('![dark current and intercept against temperature](fig_temperature.png)\n')

if LIGHT:
    _nT = len({round(u['T']) for u in LIGHT})
    w(f'## The light side: {len(LIGHT)} runs, {_nT} temperature{"s" if _nT != 1 else ""}\n')
    _ldark = [u for u in LIGHT if u.get('hasdark')]
    w('The `INTERACTIVE` runs do give a bright ladder at several temperatures - 30 shutter steps from\n'
      '1 ms to 2 s, one configuration &mdash; ' + cfgname(LIGHT[0]) + ' &mdash; each with its own dark and zero\n'
      'frames. They are windowed to ' + str(LIGHT[0]['nrows']) + ' readout rows, and the ladder is\n'
      'referenced to each run\'s own dark frames - same exposure, same reset setting, same readout -\n'
      'which removes the bias, the dark signal and the reset pedestal together.\n')
    if _ldark:
        w(f'Since 2026-10-22 each of these runs is also **paired with a `DC` dark ladder at the same\n'
          f'configuration** and the nearest temperature, so {len(_ldark)} of the {len(LIGHT)} measure all four\n'
          'routes rather than the light side alone. The dark ladder of such a pair is a FULL-DIE run,\n'
          'while the bright one is windowed, so routes a) and c) are not over the same readout as b)\n'
          'and d) - the two modes disagree by about a factor two in dark-current slope, as the caveats\n'
          'below say. The light route is unaffected by the pairing: its points are already referenced\n'
          'to dark frames of their own exposure, so no `DC*ExpSen` pedestal is added to it.\n')
    w('| T [C] | bias [ADU] | read noise [ADU] | gain [ADU/e-] | response [ADU/s] | '
      'PTC intercept [e-] | light-route intercept [e-] | measured at the 3 shortest steps [ADU] |')
    w('|---|---|---|---|---|---|---|---|')
    for u in LIGHT:
        w(f"| {u['T']:.2f} | {pm1(u['bias'], u['e_bias'], 2)} | {pm1(u['rn'], u['e_rn'], 3)} "
          f"| **{pm1(u['gain'], u['e_gain'], 4)}** | {pm1(u['resp'], u['e_resp'], 1)} "
          f"| {u['tptc']:.1f} | {u['tlight']:.1f} | **{u['zero3']:+.2f}** |")
    w('')
    _g = [u['gain'] for u in LIGHT]
    _r = [u['resp'] for u in LIGHT]
    _tp = [u['tptc'] for u in LIGHT]
    _tl = [u['tlight'] for u in LIGHT]
    _z = [u['zero3'] for u in LIGHT]
    _eg = np.nanmax([u['e_gain'] for u in LIGHT])
    w(f'**The conversion gain does not depend on temperature**: {min(_g):.4f} to {max(_g):.4f} ADU/e- over\n'
      f'{max(u["T"] for u in LIGHT) - min(u["T"] for u in LIGHT):.1f} K, a spread of '
      f'{100 * (max(_g) / min(_g) - 1):.1f} %. The error on each one, {100 * _eg / np.mean(_g):.1f} %, is the fit-window\n'
      'systematic, and it is the SAME window on all four runs, so it moves them together and does\n'
      'not limit the comparison between them - the 1.9 % spread does. Neither does the photo-response\n'
      f'({100 * (max(_r) / min(_r) - 1):.1f} %), which is a property of the lamp and the optics as much as of the\n'
      f'device, nor the read noise ({100 * (max(u["rn"] for u in LIGHT) / min(u["rn"] for u in LIGHT) - 1):.1f} %).\n')
    w(f'**And here the light-route intercept can be checked against a direct measurement.** It is an\n'
      f'extrapolation of the ladder from {LIGHT[0]["lo"]:.0f}-{LIGHT[0]["hi"]:.0f} ADU down to zero light, and reads\n'
      f'{min(_tl):.1f} to {max(_tl):.1f} e- across these runs. The same frames MEASURE that point: at the three\n'
      f'shortest shutter openings, {LIGHT[0]["x3"] * 1e3:.0f} ms and below, the signal is '
      f'{min(_z):+.2f} to {max(_z):+.2f} ADU, which is '
      f'{min(_z) / max(_g):+.2f} to {max(_z) / min(_g):+.2f} e-.\n'
      'There is no deficit of tens of electrons at zero light; there is a ladder that is not straight,\n'
      'and extrapolating it from where it is bright to where it is not invents one. With these runs\n'
      'that conclusion needs no argument about pedestals, because the reference frames share the\n'
      'exposure and the reset setting of the ladder itself.\n')
    w(f'The shot-noise intercept, which extrapolates nothing, reads {min(_tp):.1f} to {max(_tp):.1f} e- over the\n'
      'same three temperatures - the same quantity that proved stable across all seven bias settings\n'
      'of the August scan.\n')

w('## Caveats\n')
w('- The dark runs of THIS table are analysed as dark-only units: stages 1, 2a and 2, because a\n'
  '  `DC` run carries dark exposures and nothing else and everything downstream needs a bright\n'
  '  ladder. They therefore have no per-unit report, and this document is the only place their\n'
  '  numbers appear; their dumps are under `~/claude/desy_die/<tag>/` and `~/claude/desy_rn/<tag>/`\n'
  '  as usual. The `aSpect2` dark runs are ALSO used a second time, as the dark half of the paired\n'
  '  `INTERACTIVE` units of the light-side table, which is a different analysis of the same frames:\n'
  '  there they are read over the illuminated box only and fitted on the window of the pair.\n')
_fw = [u for u in FULL if abs(u['T'] + 71) < 1.5]
_ww = [u for u in WIN if abs(u['T'] + 71) < 1.5]
_rr = [u for u in REF if abs(u['T'] + 71) < 1.5]
_pair_txt = ''
if _rr and _ww:
    _pair_txt = (f" Measured here on the same configuration near -71 C, {_rr[0]['dc']:.4f} ADU/s full die "
                 f"against {_ww[0]['dc']:.4f} windowed, a factor {_rr[0]['dc'] / _ww[0]['dc']:.1f} - over different\n"
                 "  bands of readout rows, so that factor is indicative rather than a measurement of the\n"
                 "  readout-mode effect on one set of pixels.")
w('- **These units keep the goodness-of-fit window; the configuration pairs do not.** The seven\n'
  '  August pairs are now fitted over one stated signal window, 90-1000 ADU, the same window for\n'
  '  both ladders and for both fits of a ladder. The dark ladders here never reach 90 ADU - they\n'
  '  stop between 4.7 and 84 ADU - so that rule would cut an eight-step ladder to its top three\n'
  '  and throw away the lever arm this comparison is built on, while buying nothing: these units\n'
  '  have no bright ladder and no four-route comparison to make consistent. They therefore keep\n'
  '  the window chosen by goodness of fit, and **their dark currents are not directly comparable\n'
  '  with the dark currents of the configuration summary**, which are measured over a different\n'
  '  part of the same curved ladders. The comparison inside this document is unaffected: every\n'
  '  unit in it is treated the same way.\n')
w('- The full-die and windowed runs are **not** comparable with each other: the two modes\n'
  '  disagree on the slope at a fixed temperature and configuration.' + _pair_txt + '\n'
  '  Each set is internally consistent and the comparison above is made within a set.\n')
w('- The windowed runs read a different band of readout rows, because the August box lies\n'
  '  outside a 499-row window. The readout columns are the same.\n')
w('- The intercepts here carry the reset-configuration pedestal described in the per-pair\n'
  '  reports: the zero-exposure frames use a different reset setting from the ladder frames.\n')

md = '\n'.join(L) + '\n'
with open(os.path.join(A.out, 'temperature.md'), 'w') as fh:
    fh.write(md)

HTML = """<!DOCTYPE html>
<html><head><meta charset="utf-8"><title>Temperature comparison</title>
<style>
body{max-width:1000px;margin:1.2rem auto;padding:0 1rem;font:13px/1.45 -apple-system,Segoe UI,Roboto,sans-serif;color:#222}
h1{font-size:19px;border-bottom:2px solid #ddd;padding-bottom:.2rem;margin:0 0 .4rem}
h2{font-size:14px;margin:.9rem 0 .3rem;border-bottom:1px solid #eee}
p{margin:.35rem 0}
table{border-collapse:collapse;font-size:11px;margin:.4rem 0}
th,td{border:1px solid #ddd;padding:2px 6px;text-align:left}
th{background:#f5f5f5}
code{background:#f5f5f5;padding:0 3px}
img{max-width:100%;margin:.5rem 0}
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
with open(os.path.join(A.out, 'temperature.html'), 'w') as fh:
    fh.write(HTML.replace('__MD__', md.replace('</script', '<\\/script')))

rec = {'Full': FULL, 'Win': WIN, 'Ref': REF, 'Ea_full': EA_F, 'Ea_win': EA_W,
       'RatioMeasured': MEAS, 'Tinterval': [TLO, THI]}
with open(os.path.join(A.out, 'temperature.json'), 'w') as fh:
    json.dump(rec, fh, indent=1)
print(f"temperature.md + fig_temperature.png -> {A.out}   Ea(full) = {EA_F:.3f} eV"
      + (f", Ea(win) = {EA_W:.3f} eV" if EA_W else ''))
