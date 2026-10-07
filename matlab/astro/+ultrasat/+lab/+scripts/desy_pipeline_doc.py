#!/usr/bin/env python3
"""The DESY wafer-test (PTCint) analysis pipeline: a generated specification.

Builds pipeline.md / pipeline.html and, through headless chromium, the PDF
DESY_pipeline_description.pdf. Everything quantitative in the document is read
from the stage dumps at build time -- the json files, the binary maps and the
scripts themselves -- so no number in it can drift away from the chain that
produced it. Prose that states a formula cites the file that implements it.

Two layers, as the document says in its own front matter: a methods body with
the derivations, and an operational appendix with every command.

Usage:
    python3 desy_pipeline_doc.py                 # defaults below
    python3 desy_pipeline_doc.py --example run32_W04_D07_high_sig
"""
import argparse, json, os, re, subprocess
import numpy as np
from scipy.special import gammaincinv

HERE = os.path.dirname(os.path.abspath(__file__))

P = argparse.ArgumentParser()
P.add_argument('--root',    default='/home/sasha/claude/desy_die')
P.add_argument('--rnroot',  default='/home/sasha/claude/desy_rn')
P.add_argument('--example', default='run32_W04_D07_high_sig',
               help='the die carried through the whole document')
P.add_argument('--exdef',   default='run32_W04_D07_high',
               help='the same die under the default window convention')
P.add_argument('--contrast', default='run31_W04_D07_high_sig',
               help='the same die on the other bias setup, used where the contrast matters')
P.add_argument('--conref',  default='run31_W04_D07_high')
P.add_argument('--batch',   default='/home/sasha/claude/desy_batch')
P.add_argument('--reports', default='/home/sasha/DESY_reports')
P.add_argument('--out',     default='/home/sasha/claude/desy_die/pipeline')
P.add_argument('--pdf',     default='/home/sasha/DESY_reports/DESY_pipeline_description.pdf')
P.add_argument('--no-pdf',  action='store_true')
A = P.parse_args()
os.makedirs(A.out, exist_ok=True)

# ---------------------------------------------------------------- loading
def load(tag, name, root=None):
    p = os.path.join(root or A.root, tag, name)
    if not os.path.isfile(p):
        return None
    with open(p) as fh:
        return json.load(fh)

class Die:
    """Every dump of one die-run, loaded lazily by attribute name."""
    NAMES = {'dark': 'dark.json', 'light': 'light.json', 'badcol': 'badcol.json',
             'ptc': 'ptc.json', 'budget': 'budget.json', 'varspread': 'varspread.json',
             'lowsignal': 'lowsignal.json', 'perpixel': 'ptc_perpixel.json',
             'methods': 'methods.json', 'points': 'ptc_points.json',
             'both': 'ptc_both.json', 'chain': 'chain.json',
             'dw': 'darkwindow.json', 'fw': 'fitwindow.json'}
    def __init__(self, tag):
        self.tag  = tag
        self.base = tag[:-4] if tag.endswith('_sig') else tag
        self.sig  = tag.endswith('_sig')
        self.dir  = os.path.join(A.root, tag)
        self.rn   = os.path.join(A.rnroot, self.base)
        self._c   = {}
    def __getattr__(self, k):
        if k not in self.NAMES:
            raise AttributeError(k)
        if k not in self._c:
            self._c[k] = load(self.tag, self.NAMES[k])
        return self._c[k]
    @property
    def stats(self):
        if 'stats' not in self._c:
            self._c['stats'] = load(self.base, 'stats.json', root=A.rnroot)
        return self._c['stats']
    @property
    def size(self):
        return [int(v) for v in self.ptc['Size']]
    @property
    def npix(self):
        ny, nx = self.size
        return ny*nx
    def route(self, k):
        return self.methods['Routes'][k]

E  = Die(A.example)      # the worked example, nominal setup
ED = Die(A.exdef)        # the same die, default window
C  = Die(A.contrast)     # the same die, the other bias setup
CD = Die(A.conref)

MD = []
def w(t=''):
    MD.append(t)
def f(x, n=3):
    return ('%.' + str(n) + 'f') % float(x)
def eng(x):
    """a number for prose: 3 significant figures, no exponent for moderate values"""
    x = float(x)
    if x == 0:
        return '0'
    if 1e-3 <= abs(x) < 1e5:
        return ('%.3g' % x)
    return ('%.2e' % x)
def block(lines):
    """a display formula or derivation, monospaced. No LaTeX: the whole report
    family renders through marked.js alone and a maths renderer would be a new
    dependency."""
    w('```')
    for L in lines:
        w(L)
    w('```')
    w()

# ---------------------------------------------------------------- introspection
def jkeys(obj, prefix='', depth=2):
    """flatten a dump to (key, kind) pairs, so the document can list what a
    stage actually writes instead of what it was once documented to write"""
    out = []
    if isinstance(obj, dict):
        for k, v in obj.items():
            kk = prefix + '.' + k if prefix else k
            if isinstance(v, dict) and depth > 0:
                out += jkeys(v, kk, depth-1)
            elif isinstance(v, dict):
                out.append((kk, 'struct(%d)' % len(v)))
            elif isinstance(v, list):
                out.append((kk, 'array[%d]' % len(v)))
            elif isinstance(v, bool):
                out.append((kk, 'bool'))
            elif isinstance(v, (int, float)):
                out.append((kk, 'scalar'))
            else:
                out.append((kk, 'text'))
    return out

def binmaps(die):
    """every binary map a stage left, with the element size inferred from the
    file length and the pixel count -- 4 bytes is MATLAB single, 8 is double"""
    rows = []
    for d in (die.dir, die.rn):
        for fn in sorted(os.listdir(d)):
            if not fn.endswith('.bin'):
                continue
            n = os.path.getsize(os.path.join(d, fn))
            el = n/die.npix if die.npix else 0
            kind = {4: 'single', 8: 'double', 1: 'logical/uint8'}.get(round(el), '%.3g B/px' % el)
            whole = abs(el - round(el)) < 1e-6
            rows.append((fn, n, kind if whole else 'not whole-die (%d B)' % n,
                         'stage 1' if d == die.rn else ''))
    return rows

def srclines(name):
    p = os.path.join(HERE, name)
    if not os.path.isfile(p):
        return 0
    with open(p) as fh:
        return sum(1 for _ in fh)

def stagetimes(die):
    if die.chain is None:
        return {}
    return {s['Name']: float(s['Seconds']) for s in die.chain['Stages']}

# ---------------------------------------------------------------- derived numbers
def ladder_gof(die):
    """the ensemble dark ladder against its own straight line: residuals, the
    error on each step mean, chi2 and the formal error on the slope"""
    D  = die.dark
    x  = np.atleast_1d(np.array(D['ExpTime'], dtype=float))
    y  = np.atleast_1d(np.array(D['StepMedian'], dtype=float))
    v  = np.atleast_1d(np.array(D['VarStep'], dtype=float))
    nf = np.atleast_1d(np.array(D['PatternNframes'], dtype=float))[
         np.atleast_1d(np.array(D['FitSteps'], dtype=int)) - 1]
    r  = y - np.polyval(np.polyfit(x, y, 1), x)
    se = np.sqrt(v/(die.npix*nf))
    wt = 1.0/se**2
    xb = float(np.sum(wt*x)/np.sum(wt))
    return dict(x=x, y=y, resid=r, se=se, chi2=float(np.sum((r/se)**2)),
                dof=max(x.size-2, 1), formal=float(1.0/np.sqrt(np.sum(wt*(x-xb)**2))))

LINLIM = 2900.0
def window_span(die):
    """the ensemble dark current over every contiguous window of >=3 steps
    inside the linear range -- the honest span of the window choice"""
    if die.dw and die.dw.get('Scan'):
        v = [float(q['DC']) for q in die.dw['Scan']]
        return min(v), max(v), len(v), 'the chi2-mode scan grid'
    if die.fw:
        x = np.atleast_1d(np.array(die.fw['D']['X'], dtype=float))
        y = np.atleast_1d(np.array(die.fw['D']['SignalMean'], dtype=float))
        kmax = int(np.argmax(y > LINLIM)) if np.any(y > LINLIM) else y.size
        v = [np.polyfit(x[i:j+1], y[i:j+1], 1)[0]
             for i in range(kmax) for j in range(i+2, kmax)]
        if v:
            return float(min(v)), float(max(v)), len(v), 'refitted here from the per-step means'
    return None, None, 0, ''

def dark_precision():
    '''Which window convention determines the dark current and the dark-route
    threshold more precisely, over every die-run that has both. The total
    (stat (+) syst) relative error decides; nothing here is a judgement.'''
    import glob as _g
    rows = {}
    for d in sorted(_g.glob(os.path.join(A.root, 'run*_sig'))):
        b = d[:-4]
        if not os.path.isfile(os.path.join(b, 'methods.json')):
            continue
        Ra = json.load(open(os.path.join(b, 'methods.json')))['Routes']['a']
        Rb = json.load(open(os.path.join(d, 'methods.json')))['Routes']['a']
        g2 = lambda R, k: float(np.hypot(R[k+'Stat'], R[k+'Syst'])/abs(R[k]))
        rows[os.path.basename(b)] = (os.path.basename(b)[3:].split('_')[0],
                                     (g2(Ra, 'Slope'), g2(Rb, 'Slope')),
                                     (g2(Ra, 'Threshold'), g2(Rb, 'Threshold')))
    win = lambda v: v[1][0] < v[1][1] and v[2][0] < v[2][1]
    return dict(rows=rows, n=len(rows), ndef=sum(1 for v in rows.values() if win(v)),
                **{'def': sorted({'run ' + v[0] for v in rows.values() if win(v)}),
                   'sig': sorted({'run ' + v[0] for v in rows.values() if not win(v)})})

def ladder_rates(die):
    """apparent dark current step by step: S(t)/t against the fitted slope"""
    fw = die.fw
    if fw is None:
        x = np.array(die.dw['X'], dtype=float)
        y = np.array(die.dw['StepMedian'], dtype=float)
    else:
        x = np.array(fw['D']['X'], dtype=float)
        y = np.array(fw['D']['SignalMean'], dtype=float)
    return x, y, y/x

def chi2exp(dof):
    """Median of chi2(dof)/dof, which is the expectation a MEDIAN chi2/dof must
    be compared with. The median of a chi2(dof) variate is 2*G^-1(dof/2, 1/2)
    where G^-1 is the inverse regularised lower incomplete gamma, because
    chi2(dof)/2 ~ Gamma(dof/2, 1). MATLAB spells the same call
    gammaincinv(0.5, dof/2) -- its arguments are in the other order."""
    return 2.0*gammaincinv(dof/2.0, 0.5)/dof

def readconfig(dataset):
    """The PTC_Config.xlsx the test ran with, as {setting: value}.

    Same reader as desy_die_summary.readconfig, copied rather than imported
    because that module parses argv at import time. An xlsx is a zip of XML, so
    this needs no new dependency.
    """
    import zipfile, re as _re, html as _html
    path = os.path.join(dataset, 'PTC_int_hr', 'PTC_Config.xlsx')
    if not os.path.isfile(path):
        return {}
    try:
        with zipfile.ZipFile(path) as z:
            shared = [_html.unescape(m) for m in
                      _re.findall(r'<t[^>]*>(.*?)</t>',
                                  z.read('xl/sharedStrings.xml').decode('utf8', 'replace'), _re.S)]
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

CFG_E = readconfig(E.chain['Dataset']) if E.chain else {}
CFG_C = readconfig(C.chain['Dataset']) if C.chain else {}
CFGDIFF = sorted(k for k in set(CFG_E) | set(CFG_C)
                 if CFG_E.get(k) != CFG_C.get(k))

# the eleven stages, in the order the drivers run them. 'obj' is what the
# PTCAnalysis object carries at that point; 'holds' is where the result lives
# once the stage has finished, which is a file and not the object.
STAGES = [
 dict(n='1',  key='desy_rn_single_die',    src='desy_rn_single_die.m',
      label='bias and read noise',
      reads='the ZE frames only',
      method='zeroNoiseStats, rawColGeom',
      obj='Zero, ZeroNoise, NZero, ZeroStats, ParityMap, Frames, Sidecar, Info',
      holds='desy_rn/<tag>/stats.json + bias.bin, rn.bin, rn_raw.bin, rawcol.bin',
      what='bias level, bias fixed pattern with its row/column/residual split, per-pixel read noise and its intrinsic spread, the frame-to-frame common mode, column parity'),
 dict(n='2a', key=('desy_die_fitwindow', 'desy_die_darkwindow'),
      src='desy_die_fitwindow.m / desy_die_darkwindow.m',
      label='the fit window',
      reads='the dark ladder (chi2 mode) or both ladders (signal mode), streamed once',
      method='stepMaps, stepInventory, solveFit',
      obj='Zero, ZeroNoise (the maps of stage 1, re-measured)',
      holds='fitwindow.json (signal mode) or darkwindow.json (chi2 mode)',
      what='which steps of which ladder every later fit is allowed to use'),
 dict(n='2',  key='desy_die_dark',         src='desy_die_dark.m',
      label='dark ladder',
      reads='the dark ladder, streamed',
      method="perPixelFits('D'), stepFixedPattern('D'), paramSpread, localSpread",
      obj='the stage-1 state; the fit is a return value, not a property',
      holds='dark.json + dc.bin, dc_var.bin, tdark.bin, chi2.bin, nused.bin',
      what='per-pixel dark current and dark-route threshold, their spreads with the fit noise removed, DSNU per step'),
 dict(n='3',  key='desy_die_light',        src='desy_die_light.m',
      label='bright ladder',
      reads='the bright ladder, streamed; dc.bin and dc_var.bin of stage 2',
      method="perPixelFits('B'), stepFixedPattern('B'), paramSpread, localSpread",
      obj='the stage-1 state',
      holds='light.json + resp.bin, resp_var.bin, tlight.bin, bchi2.bin, bnused.bin',
      what='per-pixel photo-response, PRNU from the per-step fixed pattern, light-route threshold with both fit variances propagated'),
 dict(n='4',  key='desy_die_badcol',       src='desy_die_badcol.m',
      label='bad readout columns',
      reads='no frames: the maps of stages 1-3',
      method='badColumns, rawColGeom',
      obj='Frames, Sidecar, ParityMap only (no subtractZero needed)',
      holds='badcol.json + rawcol.bin, mask.bin',
      what='the column mask stages 5-10 use, and the gradient of every map along the readout direction'),
 dict(n='5',  key='desy_die_ptc',          src='desy_die_ptc.m',
      label='photon transfer curve',
      reads='the bright steps inside the gain window, streamed',
      method='stepMaps, solveFit, a null simulation',
      obj='the stage-1 state',
      holds='ptc.json + gain.bin, gain_var.bin, gchi2.bin, gain_col.bin, gain_block.bin, ptc_offset.bin, ptc_cloud.bin',
      what='the conversion gain per pixel, per readout column and per 32x32 block, and the threshold the shot noise implies'),
 dict(n='6',  key='desy_die_budget',       src='desy_die_budget.m',
      label='noise budget',
      reads='no frames and no object: the json of stages 1-5',
      method='budgetCurve (called as a static method)',
      obj='none -- this stage never constructs a PTCAnalysis',
      holds='budget.json',
      what='sigma_eff and SNR in electrons, the limiting signal, and which term dominates where, with every threshold carried side by side'),
 dict(n='7',  key='desy_die_varspread',    src='desy_die_varspread.m',
      label='variance distributions',
      reads='every frame of both ladders plus the ZE frames',
      method='varSpread, an integer-frame null simulation',
      obj='the stage-1 state',
      holds='varspread.json',
      what='how much the per-pixel variance really differs between pixels at every step, against the spread a set of identical pixels would show'),
 dict(n='8',  key='desy_die_lowsignal',    src='desy_die_lowsignal.m',
      label='low-signal prediction',
      reads='the steps of both ladders below the low-signal limit',
      method='stepMaps and a per-pixel chi2 resampling of the prediction',
      obj='the stage-1 state',
      holds='lowsignal.json + lowsignal_resid.bin',
      what='whether each pixel variance is explained by RN_i^2 + g(S_i + T), as a distribution, a calibration and a residual map'),
 dict(n='9',  key='desy_die_ptc_perpixel', src='desy_die_ptc_perpixel.m',
      label='per-pixel PTC of each ladder',
      reads='both ladders, streamed',
      method='stepMaps, solveFit, a null simulation per ladder',
      obj='the stage-1 state',
      holds='ptc_perpixel.json + gainD.bin, gainB.bin, interD.bin, interB.bin, gain_diff.bin',
      what='a gain per pixel on the dark and on the bright ladder separately, and the dark deficit as a per-pixel difference'),
 dict(n='10', key='desy_die_methods',      src='desy_die_methods.m',
      label='four routes, with errors',
      reads='both ladders, streamed, inside each block of the die',
      method='solveFit per block and per window',
      obj='the stage-1 state',
      holds='methods.json',
      what='the gain and the charge threshold by four independent routes, each with a block-scatter and a fit-window error'),
 dict(n='10+', key='desy_die_ptc_export',  src='desy_die_ptc_export.m',
      label='export the PTC points',
      reads='both ladders and the ZE frames, streamed',
      method='stepMaps, trimmed means, varspread SpreadRel',
      obj='the stage-1 state',
      holds='ptc_points.json, ptc_data.mat',
      what='one row per ladder point with the plotted mean pair and the full pixel distribution of the excess variance, so any figure can be remade without the frames'),
]

ST_E = stagetimes(E)
# stage 1 depends on no fit window, so the signal-mode run shares the default
# mode's and does not record its time; take it from the sibling record
for _k, _v in stagetimes(ED).items():
    ST_E.setdefault(_k, _v)
def secs(key):
    ks = (key,) if isinstance(key, str) else key
    for k in ks:
        if k in ST_E:
            return '%.0f s' % ST_E[k]
    return '--'

LOT, DIE, RUN = E.ptc['Lot'], E.ptc['Die'], E.ptc['Run']
NY, NX = E.size
GOF_E, GOF_C = ladder_gof(E), ladder_gof(C)
SPAN_E = window_span(E)
SPAN_C = window_span(C)
DPREC  = dark_precision()

# ================================================================== Part 0
w('# The DESY wafer-test analysis pipeline')
w()
w(f'*Generated from the chain itself on build. Worked example: {LOT} {DIE}, '
  f'run {RUN} (the nominal aSpect setup), with run {C.ptc["Run"]} of the same die '
  f'alongside wherever the contrast between the two bias setups is what makes a '
  f'step necessary.*')
w()
w('## What this document is')
w()
w(f"""The pipeline takes the frames of one ULTRASAT BSI die as the DESY / aSpect
wafer tester writes them and produces, for that die, every number a datasheet or
a noise budget needs: bias, read noise, dark current, photo-response, the two
non-uniformities, the conversion gain, the charge threshold by four independent
routes, and the smallest signal the device can measure. It then compares dies,
wafers, lots and bias setups against one another.

It is a chain of **{len(STAGES)} steps** -- ten numbered stages, the window
stage 2a that decides what the later fits may use, and an export. Each is a MATLAB script that
constructs one `ultrasat.lab.PTCAnalysis` object, streams the frames it needs
past it exactly once, and writes its result to a json file and a set of raw
binary maps. Nothing is passed between stages in memory: **the state of the
pipeline is the dumps on disk**, which is what makes any stage re-runnable on
its own and what lets the report, the summary and this document be rebuilt
without touching a frame.

The document has two layers. The body (Parts 1 to 7) is the method: what each
step computes, the formula it computes it with, and why that formula and not the
obvious one -- most of the non-obvious choices in this chain exist because an
estimator that looks right gives a measurably wrong answer on this device, and
each of those is recorded where it bites. Part 8 is the operational layer:
every command, the dependency graph of what must be re-run after what, and the
measured cost. Part 8 can be read alone.""")
w()
w('### The stages at a glance')
w()
w('| # | stage | reads | cost | what it settles |')
w('|---|---|---|---|---|')
for S in STAGES:
    w(f"| {S['n']} | `{S['src'].split('/')[0].strip()}` | {S['reads']} | {secs(S['key'])} | {S['what']} |")
w()
if E.chain:
    w(f"""Measured on the worked example: {int(E.chain['Nfiles'])} TIFF files,
{float(E.chain['Bytes'])/1e9:.1f} GB, {float(E.chain['TotalSeconds'])/60:.0f} minutes
end to end for the whole {NY} x {NX} = {NY*NX/1e6:.1f} M-pixel half, individual
pixels throughout. Four dies at a time is free on one node.""")
    w()
w('### Notation')
w()
w("""| symbol | meaning |
|---|---|
| S | the recorded signal of a pixel, bias subtracted, in ADU |
| Q | the charge the pixel collected, in electrons |
| T | the charge threshold, the part of Q that is collected but not recorded; `S = g(Q - T)` |
| g | the conversion gain in ADU per electron (so `Q = S/g + T`) |
| RN | the read noise of a pixel in ADU, from the zero-exposure frames |
| DC | the dark current of a pixel in ADU/s |
| V | a per-pixel temporal variance, measured from the repeats of one step |
| E | the excess variance `V - RN^2`, the variance the signal added |
| t | the integration time of a dark step, in s |
| int | the illumination of a bright step, the config value times 1000 |
| f | 1 when a fixed pattern is calibrated out, 0 for a single raw frame |
| stat (+) syst | two errors added in quadrature: the scatter between blocks of the die, and the fit window |

Two spreads appear throughout and they are **not** interchangeable. The spread
over the **whole die** is a total non-uniformity, dominated on this device by
large-scale structure. The **local** spread is the residual to a 32x32 block
median with the measurement noise removed, and that is the pixel-to-pixel term
a noise budget needs. Every stage reports both; Part 3 derives the second.

Formulas are written in monospace rather than typeset: the whole report family
renders through `marked.js` alone, and a maths renderer would add a dependency
to every one of them.""")
w()

# ================================================================== Part 1
w('## 1. The measurement and its conventions')
w()
w('### 1.1 What is on disk')
w()
if E.chain:
    w(f"""One die-run is one directory. The worked example is

```
{E.chain['Dataset']}/
    PTC_int_hr/                  the frames and PTC_Config.xlsx
    Calib/  SPI/
    Ultrasat_BSI_L_{LOT}_{DIE}.log
    Ultrasat_BSI_L_{LOT}_{DIE}_Result.txt
```

{int(E.chain['Nfiles'])} TIFF files and {float(E.chain['Bytes'])/1e9:.1f} GB. Three
sidecars matter and each answers a different question:""")
w()
w("""| file | read by | what the chain takes from it |
|---|---|---|
| `*_Result.txt` | `ultrasat.lab.readResult` | whether the die passed final test. The pass flag is the line `Pass<TAB>1` and the bin is `Soft Bin<TAB>1`; only FT = pass dies are analysed |
| `PTC_int_hr/PTC_Config.xlsx` | `readPTCConfig`, and `readconfig` in the summary and in this document | every bias voltage the test ran with. **The bias settings are not in the Result file** -- a run cannot be identified by its header alone |
| `General/Lots_Cassette_table.xlsx` | the summary | the flavour, which is a property of the **wafer**: Lot ID, Wafer ID, Flavour ID, RBS mask, aSpect cassette |

The text files are ISO-8859 encoded, so `grep` treats them as binary; the chain
and every ad-hoc check use `LC_ALL=C grep -a`.

**Die names repeat between lots.** Two different lots each have a `W07_D06`, and
both were measured in the same run, so the lot has to appear in the output tag
and in every summary label and die-pairing key. Without it two different dies
silently become one die measured twice, and every paired test reads nonsense.""")
w()
w('### 1.2 One frame')
w()
w("""A TIFF row begins with 2 counter columns (row number and a frame counter)
and then carries the pixel payload of **both readout halves side by side**: the
left half is the low-gain readout, the right half the high-gain one, 4740 columns
each in a 9482-wide TIFF. `ultrasat.lab.readPTC` selects one half and, by
default, returns it in the **DESY orientation**, which is the half transposed and
turned by 180 degrees:""")
block(["D = rot90(Half.', 2)",
       "",
       "so that   D(i,j) = Half(Height - j + 1, Wh - i + 1)"])
w(f"""The consequence is used in every stage and is worth stating once: **in the
DESY orientation the readout columns run along the image rows.** Every claim in
the analysis about a gradient "along the readout direction", about column pairing,
or about a bad column, is a statement about image rows, and the index arithmetic
that converts between the two lives in one place,
`ultrasat.lab.PTCAnalysis.rawColGeom`, which reads it back out of the `RAWSEC`,
`RAWXOFF` and `ORIENT` header cards that `readPTC` writes. A raw column index is
counted from 1 at the first pixel column of the selected half, so odd indices are
detector columns 1, 3, 5, ... and the parity split (`Parity = 'rawcol'`) is a
physical split between the two interleaved readout chains, not a checkerboard.

The worked example uses the high-gain half: {NY} rows x {NX} columns =
{NY*NX/1e6:.1f} M pixels.""")
w()
w('### 1.3 The two ladders and the zero frames')
w()
_fx = E.fw['D'] if E.fw else None
_fb = E.fw['B'] if E.fw else None
if _fx:
    _dsig = np.array(_fx['SignalMean'], dtype=float)
    _bsig = np.array(_fb['SignalMean'], dtype=float)
    w(f"""Three kinds of frame, tagged in the file name and parsed by
`ultrasat.lab.parseFrameName`:

| tag | what varies | steps | repeats | the example's range |
|---|---|---|---|---|
| ZE | nothing: zero exposure | 1 | {int(E.stats['Nframes'])} | the bias |
| D | integration time t [s] | {len(_fx['Step'])} | {int(np.median(_fx['Nframes']))} | {_dsig.min():.1f} to {_dsig.max():.1f} ADU at t = {_fx['X'][0]:.0f} to {_fx['X'][-1]:.0f} s |
| B | illumination int | {len(E.light['PatternStep'])} | {int(np.median(_fb['Nframes']))} | {_bsig.min():.1f} to {_bsig.max():.1f} ADU over the {len(_fb['Step'])} steps the window stage reads |

`IntensityScale = 1000` turns the config's illumination value into the "int" of
the DESY plots, so a bright step's x value is the config number times 1000.

**Three repeats per step is the single most consequential number in the whole
chain.** A per-pixel variance from three frames has 2 degrees of freedom: it is
chi2 distributed with a 100 % relative error and a long tail, it is quantised
(a variance of three integers can only take multiples of 1/18), and its median
sits a factor ln 2 below its mean. Part 3 derives each of those and Parts 5.5,
5.7 and 5.9 are shaped entirely by them.""")
w()
w('### 1.4 What belongs to what')
w()
w("""A quantity measured on one die-run can belong to the lot, the wafer, the die
or the run, and attributing it to the wrong level is the easiest mistake in the
whole exercise:

| level | what belongs to it | where it is read |
|---|---|---|
| lot | the process split | the directory name |
| wafer | **the flavour** | `Lots_Cassette_table.xlsx`, never the frames |
| die | the gain, the response, the column defects, the dark-current *amplitude* | measured |
| run | every bias voltage, and therefore the dark-current *level* | `PTC_Config.xlsx` |
""")
if CFGDIFF:
    w(f"""The two setups of the worked example differ in **{len(CFGDIFF)} recorded
settings**, read from the two `PTC_Config.xlsx` files at build time:""")
    w()
    w(f'| setting | run {RUN} | run {C.ptc["Run"]} |')
    w('|---|---|---|')
    for k in CFGDIFF:
        w(f'| `{k}` | {CFG_E.get(k, "--")} | {CFG_C.get(k, "--")} |')
    w()
    w(f"""This table is the reason a whole section of the summary exists. Run
{C.ptc["Run"]} and run {RUN} give the same dies a factor
{float(C.route("a")["Slope"])/float(E.route("a")["Slope"]):.0f} in dark current, and
the obvious reading -- a temperature difference -- is **not** established, because
they also differ in the reset and source-follower voltages, any of which moves
leakage on its own. The rule the chain follows is: read the per-run config before
attributing any between-run difference to anything.""")
w()

# ================================================================== Part 2
w('## 2. The data model: what the object holds, and what the chain keeps')
w()
w('### 2.1 One class, two modes')
w()
w(f"""Everything is done by one class, `ultrasat.lab.PTCAnalysis`
({srclines('../@PTCAnalysis/PTCAnalysis.m') or 1152} lines plus
{len([1 for _ in STAGES])} stage scripts and 12 methods in their own files). It is
constructed on a device directory and reads nothing until asked:""")
block(["P = ultrasat.lab.PTCAnalysis(DeviceDir, 'CCDSEC',[], 'Gain','high', 'Parity','rawcol');",
       "P.read;            % frame inventory and sidecars; sets Mode",
       "P.subtractZero;    % the bias frame, its per-pixel noise, ZeroStats"])
w(f"""`CCDSEC` decides the mode and the mode decides everything about the memory
behaviour:

| | `region` | `full` |
|---|---|---|
| selected by | `CCDSEC = [Xmin Xmax Ymin Ymax]` | `CCDSEC = []` |
| what `read` loads | the pixels of the region, as an `AstroImage` array in `AI` | the inventory and sidecars only |
| the per-pixel ladder | cached in `Dark.Mean`, `Bright.Mean` and the variance cubes | never built |
| per-pixel fits | two passes over the cached cube | one streaming pass, running sums only |
| cost on this die | a {NY}x{NX} region is the whole half | {NY*NX*4/1e9:.1f} GB per cached ladder step map |

The arithmetic that forces streaming: a cached whole-die ladder of both ladders
would be {NY}x{NX}x{int(np.sum(np.atleast_1d(np.array(E.dark['PatternNframes'])))) + int(np.sum(np.atleast_1d(np.array(E.light['PatternNframes']))))} frames
in single precision, about
{NY*NX*(int(np.sum(np.atleast_1d(np.array(E.dark['PatternNframes'])))) + int(np.sum(np.atleast_1d(np.array(E.light['PatternNframes'])))))*4/1e9:.0f} GB.
The streamed mode holds six to thirteen running-sum maps instead, about 3 GB, and
reads every frame exactly once. **The whole single-die chain runs in `full`
mode.** The region mode is what the earlier reports used, on the DESY 100x100
analysis window `CCDSEC = [1361 1460 1861 1960]`, and it is still the way to
cross-check a pixel-level result by hand.

The streamed mode imposes one contract: **the fit window must be given as an
explicit step list.** The `'auto'` rule resolves a window from the levels of a
cached ladder, which full mode does not have, so the chain determines the window
in its own stage (2a) and passes the result down as `DieFitStepsD` /
`DieFitStepsB`. That is the whole reason stage 2a exists as a separate stage
rather than as a line inside stage 2.""")
w()
w('### 2.2 The configuration properties')
w()
w("""Set at construction or assigned afterwards; nothing is read until `read`.

| property | unit / values | default | what it controls | who overrides it in the chain |
|---|---|---|---|---|
| `DeviceDir` | path | `''` | the die-run to analyse | `DieDev`, built by the config from run / lot / die |
| `Test` | name | `PTC_int_hr` | the frame sub-directory and file tag | -- |
| `CCDSEC` | `[Xmin Xmax Ymin Ymax]` | the DESY 100x100 window | region or full mode | every stage sets `[]` |
| `Gain` | `high` / `low` / `raw` | `high` | which readout half | `DieGain` |
| `Orient` | `desy` / `tiff` | `desy` | the orientation of the returned image | -- |
| `Combiner` | `mean` / `median` | `mean` | how ZE frames and step repeats are combined | -- |
| `FitRange` | `[Low High]` ADU | `[1000 2500]` | the response-fit window by signal | superseded by explicit steps |
| `FitSteps` | `struct('D',[],'B',[])` | empty | explicit response-fit steps | **set by stage 2a** |
| `AutoMinSteps`, `AutoMinFrac` | count, fraction | 3, 0.15 | the `'auto'` window rule | unused in full mode |
| `IntensityScale` | -- | 1000 | the bright x axis | -- |
| `GainRange` | `[Low High]` ADU | `[300 2500]` | the mean-signal window of the PTC gain fit | `DieGainRange` = `[80 1000]` |
| `SatLevel` | ADU | 15000 | steps above this never enter a gain fit | -- |
| `GainADU` | ADU/e- | `[]` = measured | force a gain instead of measuring it | -- |
| `GainEstimator` | `temporal` / `diff` / `spatial` | `temporal` | which variance estimator defines `GainUsed` | -- |
| `ExpSen` | s | `[]` = `PTC_ExpTime` | the exposure of the bright frames, needed by the light route | -- |
| `Parity` | `none` / `rawcol` | `none` | also split every statistic by readout-column parity | every stage sets `rawcol` |
""")
w('### 2.3 The result properties, and which path fills them')
w()
w("""This is the part that is easy to get wrong when reading the code, so it is
stated plainly. There are **two** ways to use the class:

* `P.run` -- the ensemble path: `read`, `subtractZero`, `combineSteps`,
  `fitResponse('D')`, `fitResponse('B')`, `fitGain`, `threshold`. It fills
  `Dark`, `Bright`, `DarkFit`, `BrightFit`, `PTC` and `Threshold`, and it is what
  the region-mode reports used.
* the **individual-pixel path**, which is what the single-die chain uses. Each
  stage calls `read` and `subtractZero` and then **one analysis method whose
  result is a return value**, not a property. `P.perPixelFits('D')`,
  `P.stepFixedPattern('B')`, `P.badColumns`, `P.zeroNoiseStats`,
  `P.perPixelThreshold`, `P.noiseBudget` all return a structure; the stage writes
  that structure to json and its maps to `.bin`, and the object is discarded when
  MATLAB exits.

So `P.DarkFit` is **empty** during the single-die chain, and looking for the dark
current there would find nothing. The chain's state is the dumps.

| property | shape | filled by | holds |
|---|---|---|---|
| `Mode` | text | `read` | `region` or `full` |
| `Frames` | table | `read` | the frame inventory: file, type, step, repeat, exposure, intensity |
| `Sidecar` | struct | `read` | `Result`, `Log`, `Config`, `Calib` |
| `Info` | struct | `read` | `Lot`, `Wafer`, `Device`, `Base` |
| `ParityMap` | logical [Ny Nx] | `read` (when `Parity='rawcol'`) | true on odd raw readout columns |
| `AI` | AstroImage array | `read`, region mode only | the pixels, one element per frame |
| `Zero` | single [Ny Nx] | `subtractZero` | the combined bias frame |
| `ZeroNoise` | single [Ny Nx] | `subtractZero` | the per-pixel std over the ZE frames: the read-noise map |
| `NZero` | count | `subtractZero` | how many ZE frames went into it |
| `ZeroStats` | struct | `subtractZero` | `zeroStats` output, and `.Parity.Even/.Odd` when split |
| `Dark`, `Bright` | struct | `combineSteps` | `X`, `Step`, `Nframes`, the per-step region scalars, and in region mode the `Mean` and `VarTemporal` cubes |
| `DarkFit`, `BrightFit` | struct | `fitResponse` | the ensemble response fit |
| `PTC` | struct | `fitGain` | `GainUsed`, `GainSource` and the ensemble PTC |
| `Threshold` | struct | `threshold` | the dark- and light-route thresholds, ensemble |

`subtractZero` does **not** subtract anything from the frames: it builds the bias
frame and its noise, and the subtraction happens when a step is loaded. That is
why every stage calls it even when it only wants `ZeroNoise`.""")
w()
w('### 2.4 Step-level access, and why it is public')
w()
w("""Two methods give a stage one ladder step at a time:""")
block(["L = P.stepInventory(Type)        % one row per step: Step, X, Nframes",
       "[M, V, Nf] = P.stepMaps(Type, Step)",
       "%   M  [Ny Nx]  the mean over the repeats, bias subtracted",
       "%   V  [Ny Nx]  the per-pixel temporal variance over the repeats",
       "%   Nf          how many repeats went into them"])
w("""They were protected until stage 2a needed them. The window stage has to solve
a whole grid of candidate windows, and doing that by calling a fit once per
candidate would read the ladder once per candidate. With `stepMaps` public it
holds the per-step maps of one ladder in memory, accumulates every candidate's
sums in the same pass, and the scan costs **one** pass over the frames instead of
one per window. The comment in the class says exactly that, so the next reader
does not narrow the access again.""")
w()
w('### 2.5 What a stage leaves behind')
w()
_bm = binmaps(E)
w(f"""The example die's output directory holds {len([1 for r in _bm if not r[3]])} binary
maps written by stages 2 to 10 and {len([1 for r in _bm if r[3]])} written by stage 1,
plus {len([f for f in os.listdir(E.dir) if f.endswith('.json')])} json files and
{len([f for f in os.listdir(E.dir) if f.endswith('.png')])} figures. A `.bin` is a
bare MATLAB `fwrite` of the map in column-major order with no header, so its
element size is the file length divided by the pixel count -- which is how the
table below identifies it:""")
w()
w('| map | bytes | element | written by |')
w('|---|---|---|---|')
for fn, n, kind, where in _bm:
    w(f'| `{fn}` | {n:,} | {kind} | {where or "a stage of this die"} |')
w()
w(f"""Reading one back is a two-line job and the appendix gives the snippet. The
json files carry every scalar, every per-step array and every summary structure;
their complete key inventory is in appendix A.2
({sum(len(jkeys(getattr(E, k), depth=3)) for k in ('dark','light','badcol','ptc','budget','methods') if getattr(E, k))}
keys over the six main dumps).""")
w()

# ================================================================== Part 3
w('## 3. The shared numerical machinery')
w()
w("""Nine pieces of mathematics are used by more than one stage. They live as
methods of the class, so there is one implementation of each, and they are
derived here once rather than restated per stage. Every one of them exists in
the form it does because the obvious form was tried first and gave a measurably
wrong answer on this device; those cases are named.""")
w()
w('### 3.1 Bias, read noise and the fixed pattern')
w()
w(f"""From {int(E.stats['Nframes'])} zero-exposure frames,
`ultrasat.lab.PTCAnalysis.zeroStats` forms the bias frame as the combiner over
the frames and the read-noise map as the per-pixel standard deviation across
them, and reports **four** different read noises, which are four different
things:""")
block(["RN_temporal = median_i  sd_k( F_k(i) )           per pixel over frames, then median over pixels",
       "RN_rms      = sqrt( mean_i  var_k( F_k(i) ) )    the quadratic mean of the same map",
       "RN_diff     = sd_i( F_1(i) - F_2(i) ) / sqrt(2)  free of any fixed pattern",
       "RN_spatial  = median_k  sd_i( F_k(i) )           includes the fixed pattern"])
w(f"""`RN_diff` and `RN_spatial` bracket the fixed pattern: the first cannot see it,
the second contains it in full. On the example die the temporal median is
{f(E.stats['All']['ReadNoiseMedian'],3)} ADU and the bias fixed pattern is
{f(E.stats['All']['FixedPatternRMS'],2)} ADU, so the two differ by a factor
{float(E.stats['All']['FixedPatternRMS'])/float(E.stats['All']['ReadNoiseMedian']):.1f}
and quoting the wrong one would misstate the read noise by that much.

The fixed pattern itself needs its own sampling noise removed. The bias map is an
average of N frames, so each of its pixels carries `var/N` of temporal noise on
top of the real pattern:""")
block(["sigma_obs^2 = sigma_fixed^2 + mean_i( sigma_i^2 ) / N",
       "",
       "=>  sigma_fixed = sqrt( sigma_obs^2 - mean(sigma^2)/N )"])
w(f"""With N = {int(E.stats['Nframes'])} and a read noise of
{f(E.stats['All']['ReadNoiseMedian'],2)} ADU the term removed is
{f(float(E.stats['All']['ReadNoiseMedian'])**2/float(E.stats['Nframes']),3)} ADU^2,
which is {100*(float(E.stats['All']['ReadNoiseMedian'])**2/float(E.stats['Nframes']))/float(E.stats['All']['FixedPatternRMS'])**2:.0f} %
of the observed variance -- not negligible, and it is removed in quadrature
rather than ignored.

One estimator choice in `zeroNoiseStats` is worth recording because it looks
like a detail and is not: the per-frame **common mode** is the clipped *mean*
level of each frame, never the median. The pixels are integers, so the median of
millions of them is quantised to whole ADU and a frame-to-frame drift of
{f(E.stats['CommonMode']['Std'],4)} ADU -- which is what this device actually has
-- would read exactly zero. The deviation of each frame's level from the mean of
those levels is removed before the per-pixel noise is computed, so that a common
offset does not inflate it, while the mean level stays in so that `BiasLevel`
remains the absolute bias. The unsubtracted value is reported as well.""")
w()
w('### 3.2 The weighted straight line, streamed')
w()
w("""Almost everything in the chain is a straight line fitted per pixel. For
points `(x_k, y_k)` with weights `w_k` the weighted least-squares line
`y = a + b x` follows from the five sums""")
block(["Sw   = sum w_k          Swx = sum w_k x_k        Swy  = sum w_k y_k",
       "Swxx = sum w_k x_k^2    Swxy = sum w_k x_k y_k    Swyy = sum w_k y_k^2",
       "",
       "D = Sw*Swxx - Swx^2",
       "",
       "b = (Sw*Swxy - Swx*Swy) / D          Var(b) = Sw   / D",
       "a = (Swy*Swxx - Swx*Swxy) / D        Var(a) = Swxx / D",
       "                                     Cov(a,b) = -Swx / D"])
w("""which is why the fit can be **streamed**: the sums are maps of the same shape
as the image, every step adds its contribution to them, and no frame is ever
needed twice. `accumulateFit` adds one step and `solveFit` solves. The weighted
residual sum of squares, and therefore chi2, is expanded from the same sums,""")
block(["sum w (y - a - b x)^2 = Swyy - 2a*Swy - 2b*Swxy + a^2*Sw + 2ab*Swx + b^2*Swxx"])
w(f"""so chi2 needs no second pass either. The expansion is algebraically exact
and numerically safe here because the residuals are never small compared with the
double-precision resolution of `y^2`. `solveFit` keeps a second, unweighted set
of sums as well, which is what makes its residual rms agree with the two-pass
loop the region mode runs.

A pixel is included in a step only where the signal is finite and inside the
window, so `Nok` -- the number of points that pixel actually got -- is a map too,
and fewer than three points returns NaN rather than an unconstrained line.

**The weights are the measured variance, and that is a physical choice, not a
convenience.** The variance of a ladder point is""")
block(["sigma_k,i^2 = [ V_k + ( RN_i^2 - median_i RN^2 ) ] / Nrep_k      [ADU^2]"])
w("""with `V_k` the median over pixels of the per-pixel temporal variance of step
k, corrected for the chi2 median bias of section 3.3. Only the per-step *median*
enters, so a pixel's weight does not correlate with its own data -- which would
bias the fit. The alternative, a modelled weight `RN^2 + g*S_k`, is implemented
(`'Weights','model'`) and is wrong at the bottom of a dark ladder: the shot noise
of a ladder point follows the **collected** charge Q, while the recorded signal
is `S = g(Q - T)`, because the first T electrons are lost *after* they have
fluctuated. A model of the recorded signal therefore under-states the variance of
the lowest steps and over-weights them, and on a dark ladder the recorded signal
can even be negative. The same fact is what makes the photon-transfer intercept
`RN^2 + g*T` instead of the read noise, so getting it wrong here and getting the
threshold right in stage 5 are not independent mistakes.""")
w()
w('### 3.3 Three frames: the chi2 facts that shape the chain')
w()
w(f"""A per-pixel variance measured from `Nrep` frames is""")
block(["V_i = T_i * X / nu          X ~ chi2(nu),   nu = Nrep - 1"])
w(f"""with `T_i` the pixel's true variance. With {int(np.median(np.atleast_1d(np.array(E.dark['PatternNframes']))))} frames,
`nu = {int(np.median(np.atleast_1d(np.array(E.dark['PatternNframes']))))-1}`, and four consequences follow that between them
dictate the design of stages 5, 7, 9 and 10.

**(a) The estimator is unbiased in the mean and biased low in the median.**
`E[V] = T`, but the median of a chi2 is not its mean. For `nu = 2` the chi2 is an
exponential of mean 2, so""")
block(["median( chi2(2) ) = 2 ln 2",
       "",
       "median(V) = T * 2 ln 2 / 2 = T ln 2 = 0.693 T",
       "",
       "=>  T = median(V) / ln 2 = 1.4427 * median(V)"])
w("""and that factor 1/ln 2 = 1.4427 is the `Chi2MedianFactor` the export applies.
It is exact for three frames, not an approximation.

**(b) A median chi2/dof must be compared with its own median, not with 1.** The
median of `chi2(nu)/nu` is""")
block(["Chi2DofExpected(nu) = 2 * G^-1( nu/2, 1/2 ) / nu"])
w("""where `G^-1` is the inverse regularised lower incomplete gamma function
(`gammaincinv`), because `chi2(nu)/2` is a `Gamma(nu/2, 1)` variate. The values
the chain actually meets:""")
w()
w('| dof | points fitted | median of chi2/dof | note |')
w('|---|---|---|---|')
for _nu in (1, 2, 3, 4, 5, 10, 50):
    _note = ''
    if _nu == 2:
        _note = 'exactly ln 2'
    if _nu == 1:
        _note = 'three points fitted with two parameters'
    w(f'| {_nu} | {_nu+2} | {chi2exp(_nu):.4f} | {_note} |')
w()
w(f"""The approach to 1 is slow: even at 50 dof the median is
{chi2exp(50):.3f}. Comparing a measured median chi2/dof against 1 would therefore
reject every correct fit in this chain, and the window stage -- whose whole
criterion is a median chi2/dof -- would pick its window by an artefact. Every
dump that carries a chi2 carries its expectation next to it
(`Chi2DofExpected`, `Chi2Exp`).

**(c) The spread of V over pixels is dominated by the estimator.** With `T_i`
independent of `X`,""")
block(["E[V]     = E[T]",
       "E[V^2]   = E[T^2] * E[X^2]/nu^2 = E[T^2] * (nu^2 + 2nu)/nu^2 = E[T^2](1 + 2/nu)",
       "Var[V]   = E[T^2](1 + 2/nu) - E[T]^2",
       "         = Var[T]*(1 + 2/nu) + (2/nu)*E[T]^2"])
w("""The second term is there even when every pixel is identical, and for
`nu = 2` it alone gives `sd/mean = 1`: a 100 % spread of a quantity that does not
vary at all. Inverting for the intrinsic part is `varSpread`:""")
block(["Var[T] = ( Var[V] - (2/nu) E[V]^2 ) / (1 + 2/nu)",
       "",
       "StdNoise = sqrt(2/nu) * MeanVar          the all-pixels-identical spread",
       "StdIntr  = sqrt( max( (StdObs^2 - StdNoise^2) / (1 + 2/nu), 0 ) )"])
w("""and its significance uses the sampling error of the observed variance of a
Gamma(`nu/2`) variate,""")
block(["SE = sqrt( (2/K^2 + 6/K^3) * MeanVar^4 / Npix ) / (1 + 2/nu),     K = nu/2"])
w("""reported together with a 95 % upper limit `sqrt(VarT + 1.645*SE)/MeanVar`, so a
non-detection is quoted as a limit rather than as a zero.

**(d) The distribution is quantised.** A variance of three integers can only
take multiples of 1/18, so both the measured and the predicted distributions are
combs. Stage 7 therefore simulates its null **with integer frames**: a continuous
chi2 null would differ from the data in a way that has nothing to do with the
pixels, and the figure shows the simulated comb falling on the measured one
tooth for tooth.""")
w()
w('### 3.4 Taking the fit noise out of a spread')
w()
w("""The observed spread of a fitted parameter over the pixels is not the
pixel-to-pixel variation of that parameter: it also contains the noise of the fit
itself, which depends on the lever arm and therefore on the setup. Comparing two
setups without removing it compares their ladders, not their devices.
`paramSpread` removes it in quadrature, using the analytic parameter variance of
section 3.2:""")
block(["sigma_obs^2 = sigma_intr^2 + sigma_fit^2",
       "",
       "StdIntr = sqrt( max( StdObs^2 - StdFit^2, 0 ) )",
       "RelIntr = StdIntr / |median|",
       "Sigma   = (StdObs^2 - StdFit^2) / SE,     SE = StdObs^2 * sqrt(2/(N-1))"])
w("""Two cautions are built into it.

**The two sides must be estimated the same way.** The observed spread is robust
by default, `1.4826 * MAD` -- the factor being `1/Phi^-1(3/4)`, which makes the
MAD a consistent estimator of a Gaussian sigma -- because hot pixels and cosmic
rays inflate a plain standard deviation. A robust observed spread must then be
paired with the **median** of the fit variances, not their mean: a noisy pixel
has a larger fitted variance and a wider parameter error, so a mean-based
`StdFit` against a MAD-based `StdObs` subtracts too much and makes every fixed
pattern look smaller than it is. Both pairings are computed and reported
(`StdFit`, `StdFitRobust`).

**It does not apply to a chi2-distributed parameter.** For a heavy-tailed
distribution the robust observed spread is much the smaller of the two, so the
subtraction returns an "intrinsic" spread of exactly zero at a meaningless
significance. That is the case for the per-pixel PTC slope of stages 5 and 9,
whose points are variances, and those stages compare the observed spread with a
**null simulation** instead -- a Monte Carlo in which every pixel is given
exactly the same gain and the same number of frames. Every anomaly in the
measured spread was first reproduced to three digits by that null before it was
believed.""")
w()
w('### 3.5 The pixel-to-pixel part of a map')
w()
_lb = E.dark['Local']['DC']
w(f"""`localSpread` answers the question a noise budget asks: how much do
*neighbouring* pixels differ? It takes the residual of the map to a `Block x
Block` median, so everything varying on scales above `Block` is absorbed, then
removes the known per-pixel measurement sigma in quadrature exactly as section
3.4 does:""")
block(["Res(i) = M(i) - median( M over the Block x Block block containing i )",
       "StdObs  = 1.4826 * MAD( Res )",
       "StdIntr = sqrt( max( StdObs^2 - StdFit^2, 0 ) )",
       "RelIntr = StdIntr / |median of the block medians|"])
w(f"""`Block = 32` is a compromise with an explicit criterion. Too small and the
block median follows the pixel-to-pixel variation itself, removing the very
signal being measured; too large and the large-scale structure survives. The
standard error of a median of `n` Gaussian samples is `sqrt(pi/2) * sigma /
sqrt(n) = 1.2533 sigma / sqrt(n)`, so with 32x32 = 1024 pixels per block the
block median carries {1.2533/32*100:.1f} % of the spread being measured, while
the structure inside one block is far below it.

The two spreads of the same quantity on the example die, both computed by the
chain: the dark current spreads
**{100*float(E.dark['Fit']['All']['SlopeSpread']['RelIntr']):.1f} %** over the whole
die and **{100*float(_lb['RelIntr']):.2f} %** pixel to pixel. The first is a real
non-uniformity and the wrong number for a budget; the second is the DSNU. The
check that the detrending removes structure rather than signal is that the local
value agrees with the same quantity measured independently inside the DESY
100x100 window, where there is no large-scale structure to remove.""")
w()

w('### 3.6 Non-uniformity measured step by step, not from a slope')
w()
w("""The obvious way to measure PRNU is the pixel-to-pixel spread of the fitted
response slope. On this dataset that is almost all fit noise and the chain does
not use it. `stepFixedPattern` measures the pattern on **each step
separately**: the per-pixel mean map of one step is the average of `Nrep`
frames, so""")
block(["sigma_obs^2(k) = sigma_fixed^2(k) + V_k / Nrep_k",
       "",
       "sigma_fixed(k) = sqrt( sigma_obs^2(k) - V_k / Nrep_k )",
       "RelFixed(k)    = sigma_fixed(k) / median signal of step k"])
w(f"""with `V_k` again the median per-pixel temporal variance corrected by the
chi2 median factor. One step then measures the pattern from all
{NY*NX/1e6:.1f} M pixels at about 1.4 % precision, while the published bright
window holds only three closely spaced intensity steps, which fixes a pixel's
slope to a few per cent -- five times coarser than the ~0.5 % pattern being
measured. Measured on the example die: the response slope spreads
{100*float(E.light['Fit']['All']['SlopeSpread']['RelIntr']):.2f} % after
deconvolution, against a per-step pattern that plateaus at
{100*float(E.light['PRNU']['Multiplicative']):.2f} %, and the second is the
quantity that keeps its meaning when the fit window changes.

Running the pattern against signal over every step separates the two kinds of
non-uniformity, since one is proportional to the signal and the other is not:""")
block(["sigma_fixed^2(S) = Additive^2 + ( Multiplicative * S )^2"])
w(f"""fitted weighted in the variables `S^2` against `sigma^2`. **Additive** is
the offset fixed pattern in ADU -- the DSNU of the bias and of the threshold,
visible as the rise of the relative pattern at low signal -- and
**Multiplicative** is the PRNU, its floor. Additive is the offset term the noise
budget needs and the light-route threshold spread cannot supply it, being almost
all fit noise. On the example die the decomposition gives Additive =
{f(E.light['PRNU']['Additive'],2)} ADU and Multiplicative =
{100*float(E.light['PRNU']['Multiplicative']):.2f} %.""")
w()
w('### 3.7 The noise budget')
w()
w(f"""`budgetCurve` is a pure function of scalars, so stage 6 and any later
re-evaluation share one formula. For an incident charge Q in electrons the pixel
collects `Qc`, and""")
block(["Qc              = Q - max(T, 0)",
       "",
       "sigma_eff^2(Q)  = RN^2  +  Qc  +  DC*t                       the three irreducible terms",
       "                  + [ (1-f) * sigma_off      ]^2             offset fixed pattern",
       "                  + [ (1-f) * sigma_DC * t   ]^2             DSNU",
       "                  + [ (1-f) * PRNU * Qc      ]^2             photo-response FPN",
       "",
       "SNR(Q)          = Qc / sigma_eff(Q)",
       "Qlim            = the lowest Q at which SNR reaches SNRdet"])
w(f"""everything in electrons, so only the read noise is converted (`RN_e =
RN_ADU / g`); the shot-noise variance of `Qc` electrons is `Qc` electrons squared
and needs no gain at all. `f = 1` when the fixed patterns are removed by
calibration and `f = 0` for a single raw frame, and **both** curves are returned,
because the dark pattern of this detector was measured to repeat to 95-103 %
between runs -- it is static, so `f = 1` is defensible -- while a single raw
frame is what an instrument actually reads out. The limiting signal is found by
interpolating the SNR curve in `log Q`.

Two details in that formula were decided against alternatives.

**`Qc = Q - max(T, 0)`, not `max(Q - T, 0)`.** Only a *positive* threshold
removes charge. A negative fitted threshold means charge is present at zero
illumination, which the bias and dark subtraction take out; it is not extra
signal, and all that survives of it is its pixel-to-pixel spread, which is
already the offset term. Clamping the difference instead would let `Qc` exceed
`Q` and hand exactly those setups with a negative fitted threshold an SNR they
do not have -- and three die-runs in this campaign do fit a negative threshold.

**Every fixed-pattern term is the local (block-detrended) spread** of section
3.5, never the spread over the die. On the example die using the die-wide dark
spread instead of the local one would inflate the DSNU term by a factor
{float(E.dark['Fit']['All']['SlopeSpread']['RelIntr'])/float(E.dark['Local']['DC']['RelIntr']):.1f}
and the budget would be reporting a dark-current ramp as if it were pixel noise.""")
w()
_RA = E.route('a')
w('### 3.8 The two errors, and the one that cannot be quoted')
w()
w(f"""Every number in stage 10 carries two errors and the second is usually the
larger:

**stat -- the scatter between independent parts of the die.** The die is split
into `{int(E.methods['NBlock'])} x {int(E.methods['NBlock'])}` =
{int(E.methods['NBlock'])**2} blocks of
{int(E.methods['BlockSize'][0])} x {int(E.methods['BlockSize'][1])} pixels, the whole
route is run inside each block, and the error is the standard error over blocks.

**syst -- the fit window.** Each route is refitted over every variant of the
chosen window and the error is half the full range.

The reason the statistical error is a block scatter and not a formal fit error is
worth deriving on the example die, because it is the single most common
misreading of these numbers. The ensemble dark ladder is averaged over
{NY*NX/1e6:.1f} M pixels, so the standard error on each step's mean is""")
block(["SE_k = sqrt( V_k / ( Npix * Nrep_k ) )"])
w(f"""which comes to about {np.median(GOF_E['se']):.4f} ADU. The residuals of those
means to their own straight line are""")
block(['step    ' + '  '.join('%8.0f s' % v for v in GOF_E['x']),
       'resid   ' + '  '.join('%8.2f  ' % v for v in GOF_E['resid']) + '  ADU',
       'SE      ' + '  '.join('%8.4f  ' % v for v in GOF_E['se']) + '  ADU',
       '',
       'chi2 = %.3g on %d degree%s of freedom' % (GOF_E['chi2'], GOF_E['dof'],
                                                  '' if GOF_E['dof'] == 1 else 's')])
w(f"""The straight-line model is rejected absolutely -- each point sits
{np.max(np.abs(GOF_E['resid']/GOF_E['se'])):.0f} standard errors off the line --
because the ladder is curved, which Part 4 is about. The formal error such a fit
hands back is {GOF_E['formal']:.2g} ADU/s, which is
{float(_RA['SlopeStat'])/GOF_E['formal']:.0f} times smaller than the error the
chain quotes. **There is no defensible formal error on a parameter whose model is
rejected at that level**, and the block scatter is quoted precisely because it
measures something real: how much the answer changes between parts of the same
die. On run {C.ptc['Run']} of the same die the same arithmetic gives chi2 =
{GOF_C['chi2']:.3g} on {GOF_C['dof']} dof, so the effect is not peculiar to one
setup -- it is what averaging 22.5 M pixels does to any model that is slightly
wrong.""")
w()
w("""The window systematic has a known weakness and the per-die reports state it:
it is computed over the two to four *variants of the chosen window*, not over
every window the ladder admits. Part 4 quantifies the difference.""")
w()

# ================================================================== Part 4
w('## 4. Choosing the fit window')
w()
w('### 4.1 Why there has to be a window at all')
w()
_xr, _yr, _rr = ladder_rates(E)
_xc, _yc, _rc = ladder_rates(C)
w(f"""Neither ladder is a straight line over its whole length, and a response fit
reports the tangent of whatever part of it is fitted. The clearest way to see it
is the *apparent* dark current step by step, `S(t)/t`, which for a straight line
through the origin would be constant:""")
w()
w(f'| t [s] | run {RUN}: S [ADU] | S/t [ADU/s] | run {C.ptc["Run"]}: S [ADU] | S/t [ADU/s] |')
w('|---|---|---|---|---|')
for _i in range(len(_xr)):
    _cs = f'{_yc[_i]:.1f}' if _i < len(_yc) else '--'
    _cr = f'{_rc[_i]:.3f}' if _i < len(_rc) else '--'
    w(f'| {_xr[_i]:.0f} | {_yr[_i]:.1f} | {_rr[_i]:.3f} | {_cs} | {_cr} |')
w()
w(f"""It rises monotonically on both setups, by a factor
{_rr[-1]/_rr[2]:.1f} on run {RUN} and {_rc[-1]/_rc[2]:.1f} on run
{C.ptc['Run']} between the third step and the top. Both ends bend, for different
reasons:

* **at low signal** the charge threshold eats the first electrons, so the
  measured signal sits *above* the line extrapolated from higher steps;
* **at high signal** the integral non-linearity above about
  {LINLIM:.0f} ADU, which only a high-dark-current run reaches at all -- run
  {C.ptc['Run']}'s ladder goes to {_yc.max():.0f} ADU while run {RUN}'s stops at
  {_yr.max():.0f} ADU.

One fixed list of steps therefore cannot be the straight part of both setups,
which is why the window is **measured per die** in its own stage rather than
configured. Two conventions exist, both implemented, and the file names of the
outputs say which was used.""")
w()

w("### 4.2 Convention A: the widest window that still fits a line (`DieWindowMode = 'chi2'`)")
w()
w(f"""`desy_die_darkwindow` scans **every** contiguous window of three or more
steps whose top step is below the linearity limit `DieLinLimit =
{LINLIM:.0f} ADU`, fits the per-pixel weighted line over each, and records the
median chi2/dof over the die together with its expectation from section 3.3(b).
A window is *eligible* when""")
block(["median chi2/dof  <=  DieDarkTol * Chi2DofExpected(dof),      DieDarkTol = 1.10"])
w("""and among the eligible windows the chain takes the **widest one anchored at
the top of the linear range**, ties going to the longer lever arm.

The anchoring is not cosmetic and the argument for it is the most important
single decision in this part of the chain. A chi2 criterion *alone* rewards the
windows where the data constrain the line **least**: the lowest steps of a dark
ladder are read-noise dominated, so their error bars are relatively large,
curvature hides inside them, and a window at the bottom of the ladder can have an
excellent chi2 while measuring a dark current that is far from the device's. That
is not a hypothetical. On one die of this campaign an unanchored two-sided rule
chose the bottom four steps, spanning -3 to 18 ADU, and read a dark current of
0.19 ADU/s against the 0.38 the top of the ladder gives -- a factor two, with a
better chi2. Anchoring at the top removes the failure mode by construction.
Before the rule was changed, the windows every die had already chosen were
checked: 11 of 12 were anchored already, and re-running the stage on all of them
moved not one window.""")
w()
if ED.dw and ED.dw.get('Scan'):
    _sc = ED.dw['Scan']
    _ch = [int(v) for v in ED.dw['Chosen']]
    w(f"""The scan of the example die under this convention, as the stage writes
it. `eligible` is the chi2 test, `anchored` is the top-of-range condition, and
the chosen window is the widest row with both:""")
    w()
    w('| steps | n | lever [s] | chi2/dof | expected | ratio | eligible | anchored | DC [ADU/s] | T [ADU] |')
    w('|---|---|---|---|---|---|---|---|---|---|')
    for q in _sc:
        _st = [int(v) for v in q['Steps']]
        _el = 'yes' if float(q['Ratio']) <= 1.10 else '--'
        _an = 'yes' if q.get('Anchored') else '--'
        _mk = ' **<-- chosen**' if _st == _ch else ''
        w(f"| {' '.join(str(v) for v in _st)}{_mk} | {int(q['Nsteps'])} | {float(q['Lever']):.0f} | "
          f"{float(q['Chi2Dof']):.4f} | {float(q['Chi2Exp']):.4f} | {float(q['Ratio']):.3f} | "
          f"{_el} | {_an} | {float(q['DC']):.4f} | {float(q['Tdark']):.2f} |")
    w()
    _dcs = [float(q['DC']) for q in _sc]
    w(f"""Read down the `DC` column: the same ladder, the same pixels, the same
estimator, and the dark current ranges from {min(_dcs):.4f} to
{max(_dcs):.4f} ADU/s depending only on which steps are given to the fit. That
range -- a factor {max(_dcs)/min(_dcs):.2f} -- is the quantity Part 3.8 called the
true window systematic, and it is {0.5*(max(_dcs)-min(_dcs))/float(ED.route('a')['SlopeSyst']):.0f}
times the systematic the route actually quotes.""")
w()
w("### 4.3 Convention B: one signal window for both ladders (`DieWindowMode = 'signal'`)")
w()
_FW = E.fw
w(f"""`desy_die_fitwindow` abandons goodness of fit as the criterion and imposes
comparability instead. It takes **every step whose mean signal falls in
`[DieSigLo DieSigHi]` = [{_FW['SigLo']:.0f} {_FW['SigHi']:.0f}] ADU**, the same
window for the dark and the bright ladder, and that one list is then used by the
response fit **and** by the photon-transfer fit of that ladder. The two
measurements of one ladder therefore cover the same charge over the same points,
which is what makes the four threshold routes of Part 6 comparable at all.

The mean signal is defined exactly as the exported PTC points define it -- the
mean over the pixels outside the top 0.1 % of the temporal variance, which is
where the cosmic rays are -- so the window stage and the plots cannot disagree
about which steps are in the band.

A fit of fewer than three points has no degree of freedom left to judge it by and
`solveFit` returns NaN. Rather than fail, the **floor** is lowered to the next
step below the band, one step at a time, until three steps are in; the ceiling is
never raised, because raising it would walk into the non-linearity. The stage
says loudly when it does this and records it, so that a reader can tell a
three-step window inside the band from one that had to reach below it.""")
w()
for _lk, _nm in (('D', 'dark'), ('B', 'bright')):
    _L = _FW[_lk]
    _sg = np.array(_L['SignalMean'], dtype=float)
    _cs = [int(v) for v in _L['Chosen']]
    w(f"**The {_nm} ladder of the example die**: {len(_L['Step'])} steps measured, "
      f"{int(_L['NinWindow'])} inside the band, floor "
      + (f"**lowered to {float(_L['Floor']):.1f} ADU**" if _L['FloorLowered'] else 'left at the band edge')
      + f", chosen steps `{_cs}`.")
    w()
    w('| step | x | mean signal [ADU] | in the fit |')
    w('|---|---|---|---|')
    for _i, _s in enumerate(_L['Step']):
        w(f"| {int(_s)} | {float(_L['X'][_i]):g} | {_sg[_i]:.1f} | "
          f"{'**yes**' if int(_s) in _cs else ''} |")
    w()
w(f"""The bootstrap problem this creates is worth naming because it is the kind of
thing that silently breaks a chain: the central config refuses to run when the
window dump is missing -- that check is what stops a stage from using a window
belonging to another die or another tolerance -- but the stage that *writes* the
dump has to call the config first. The stage therefore sets
`DieWindowBootstrap = true` before calling it, which is the only case in which
the missing-dump check is waived.""")
w()
w('### 4.4 What the choice costs')
w()
_sp = SPAN_E
w(f"""The two conventions pick a **different dark window on 15 of the 16**
die-runs where both were computed, so results under the two are different
measurements of the device, not two readings of one. Neither dominates:

* the common 100-1000 ADU window **narrows** the four-route spread on the
  high-dark-current setup, from 142 to 106 e- with all four routes moving closer,
  because that setup's dark ladder is long enough to reach the band;
* it **widens** the spread on the low-dark-current setups, from 21 to 27 e-,
  narrower on only 1 of 12, because there the band is at the very top of a short
  ladder and the fit loses its lever arm.
* which convention measures the **dark** quantities more precisely is decided
  per run and measured, not assumed: taking the total (stat (+) window) relative
  error of the dark current and of the dark-route threshold over the
  {DPREC['n']} die-runs that have both, the default window wins on
  {DPREC['ndef']} and the signal window on {DPREC['n']-DPREC['ndef']}, and the
  split is exactly by setup -- default on the long-ladder run{'' if len(DPREC['def'])==1 else 's'}
  ({', '.join(DPREC['def'])}), signal on the low-dark-current ones
  ({', '.join(DPREC['sig'])}). On the example die it is
  {100*DPREC['rows'][ED.tag][1][0]:.2f} % against {100*DPREC['rows'][ED.tag][1][1]:.2f} %
  on the dark current and {100*DPREC['rows'][ED.tag][2][0]:.1f} % against
  {100*DPREC['rows'][ED.tag][2][1]:.1f} % on the dark threshold, the signal window
  winning both because the default one reaches down into the low-signal knee and
  pays for it in the window term.

The rule in force for the production results is the **signal** window, because
comparability between the routes is what the campaign was asked to settle; the
default-window dumps are kept because they are the better dark-current
measurement. Whichever is used, the span of the choice is reported with the
number: refitting the example die's ladder over every contiguous window of three
or more steps inside the linear range ({_sp[2]} windows, {_sp[3]}) puts the dark
current anywhere between **{_sp[0]:.4f}** and **{_sp[1]:.4f} ADU/s**.""")
w()

# ================================================================== Part 5
SBY = {S['n']: S for S in STAGES}
_sub = {'1': '5.1', '2a': '5.2', '2': '5.3', '3': '5.4', '4': '5.5', '5': '5.6',
        '6': '5.7', '7': '5.8', '8': '5.9', '9': '5.10', '10': '5.11', '10+': '5.12'}
def stagehdr(n):
    S = SBY[n]
    w(f"### {_sub[n]} Stage {n} -- {S['label']}")
    w()
    w(f"`{S['src']}`" + (f" + `{S['src'].replace('.m', '_plots.py')}`"
                         if os.path.isfile(os.path.join(HERE, S['src'].replace('.m', '_plots.py'))) else ''))
    w()
    w('| | |')
    w('|---|---|')
    w(f"| reads | {S['reads']} |")
    w(f"| class methods used | `{S['method']}` |")
    w(f"| the object carries | {S['obj']} |")
    w(f"| leaves behind | `{S['holds']}` |")
    w(f"| cost, example die | {secs(S['key'])} |")
    w()

w('## 5. The stages')
w()
w("""Each stage is given in one template: what it reads, the formula, the
estimator decision behind that formula, what it writes, and what it measured on
the worked example. Only the decisions that changed an answer are recorded -- the
chain has a dozen of them and each is a place where the obvious choice was
measurably wrong.""")
w()

# ---------------------------------------------------------------- stage 1
stagehdr('1')
Z = E.stats
w(f"""The only stage that reads nothing but the zero-exposure frames, which is
why it is cheap and why it is shared: its result does not depend on any fit
window, so the two window conventions share one run of it and the output lives
under `desy_rn/` rather than in the die directory.

It applies section 3.1 to the {int(Z['Nframes'])} ZE frames and reports, for all
pixels and separately for the even and odd readout columns:""")
w()
w('| quantity | example die | how |')
w('|---|---|---|')
w(f"| bias level | {f(Z['All']['BiasLevel'],2)} ADU | median of the combined frame |")
w(f"| read noise, median | {f(Z['All']['ReadNoiseMedian'],3)} ADU | per-pixel sd over frames, then median |")
w(f"| read noise, even / odd columns | {f(Z['Even']['ReadNoiseMedian'],3)} / {f(Z['Odd']['ReadNoiseMedian'],3)} ADU | the parity split |")
w(f"| bias fixed pattern | {f(Z['All']['FixedPatternRMS'],2)} ADU | spatial spread, sampling noise removed |")
w(f"| common mode | {f(Z['CommonMode']['Std'],4)} ADU | clipped mean per frame, frame to frame |")
if 'Structure' in Z:
    _S = Z['Structure']
    w(f"| bias pattern: row / column / residual | {f(_S['RowMeanStd'],2)} / {f(_S['ColMeanStd'],2)} / {f(_S['ResidStd'],2)} ADU | row and column means of the bias map, and what is left |")
w(f"| read noise, robust | {f(Z['All']['ReadNoiseRobust'],3)} ADU | chi2-median corrected |")
w(f"| noisy-pixel tail | {100*float(Z['All']['TailFrac']):.2f} % | pixels above 2x the median read noise |")
w(f"| read-noise spread, intrinsic | {100*float(Z['All']['SpreadSigmaRel']):.0f} % on sigma | `varSpread`, section 3.3(c) -- tail dominated, see below |")
w()
w(f"""**Why the spread of the noise map needs `varSpread` and not a standard
deviation.** With {int(Z['Nframes'])} frames each pixel's sigma carries
{int(Z['Nframes'])-1} degrees of freedom and so about
{100*np.sqrt(2.0/(int(Z['Nframes'])-1))/2:.0f} % sampling scatter on sigma itself.
The medians of the map are solid, but the *width* of the observed distribution is
mostly sampling: reporting it raw would claim a pixel-to-pixel variation of the
read noise that is not there. Section 3.3(c) removes it, and the result is
reported with a 95 % upper limit so that a non-detection reads as a limit.

What survives the deconvolution on this device is large --
{100*float(Z['All']['SpreadSigmaRel']):.0f} % on sigma -- and it is **not** a
Gaussian width: it is carried by the noisy tail. The observed spread of sigma^2 is
{f(Z['All']['Spread']['StdObs'],1)} ADU^2 against a mean of
{f(Z['All']['Spread']['MeanVar'],2)}, while {100*float(Z['All']['TailFrac']):.2f} % of
pixels sit above twice the median noise. So the right summary of the read noise
of this die is the median plus a tail fraction, and that is how the budget uses
it: `RN` enters as the median and the tail enters the bad-column mask of stage 4,
never as a sigma on a Gaussian.

**The readout-column pairing** is measured here and is the stage's most useful
by-product: the median read noise of raw column 2k-1 correlates with that of 2k
and not with the next column across the pair boundary. On the example die the
within-pair correlation is r = {f(E.chain and load(E.tag,'rnplots.json') and load(E.tag,'rnplots.json')['PairR'] or float('nan'),3)}
against {f(load(E.tag,'rnplots.json')['CrossR'] if load(E.tag,'rnplots.json') else float('nan'),3)}
across pairs. That tells the chain the two interleaved readout chains share
something, which is why `Parity = 'rawcol'` is set on every stage and why every
statistic in every later stage is reported three times: all, even, odd.""")
w()

# ---------------------------------------------------------------- stage 2a
stagehdr('2a')
w("""Derived in full in Part 4. In one line: it decides which steps of which
ladder every later fit may use, writes that decision to `fitwindow.json` (signal
mode) or `darkwindow.json` (chi2 mode), and the central config refuses to run any
later stage against a window dump belonging to a different die, a different
tolerance or a different signal band -- which is the guard that makes the
per-stage re-runnability safe.""")
w()

# ---------------------------------------------------------------- stage 2
stagehdr('2')
D = E.dark
_fsd = [int(v) for v in np.atleast_1d(np.array(D['FitSteps']))]
w(f"""The dark ladder is streamed one step at a time and the weighted per-pixel
line of section 3.2 is accumulated. Per pixel,""")
block(["S(t) = DC * t + I_D",
       "",
       "DC     = Slope          [ADU/s]   the pixel's dark current",
       "T_dark = -Intercept     [ADU]     the charge threshold, dark route",
       "",
       "Var(DC)     = Sw   / D            section 3.2, used by stage 3 and by paramSpread",
       "Var(T_dark) = Swxx / D"])
w(f"""over the steps the window stage chose, `{_fsd}` -- x values
{', '.join('%g' % v for v in np.atleast_1d(np.array(D['ExpTime'])))} s, step medians
{', '.join('%.1f' % v for v in np.atleast_1d(np.array(D['StepMedian'])))} ADU.
`VarSlope` and `VarIntercept` are the fit noise that section 3.4 removes from the
pixel-to-pixel spreads, and `dc_var.bin` is written out because stage 3 needs it
to propagate the dark current into the light-route threshold.

Measured on the example die:""")
w()
_sl, _ic = D['Fit']['All']['SlopeSpread'], D['Fit']['All']['InterceptSpread']
w('| quantity | value | note |')
w('|---|---|---|')
w(f"| dark current, median pixel | {f(_sl['Median'],4)} ADU/s | |")
w(f"| dark current, mean pixel | {f(_sl['Mean'],4)} ADU/s | the distribution is right-skewed by {100*(float(_sl['Mean'])/float(_sl['Median'])-1):.1f} % |")
w(f"| spread over the die | {100*float(_sl['RelIntr']):.1f} % | observed {f(_sl['StdRobust'],3)}, fit noise {f(_sl['StdFitRobust'],3)} ADU/s |")
w(f"| spread pixel to pixel (DSNU) | {100*float(D['Local']['DC']['RelIntr']):.2f} % | `localSpread`, section 3.5 |")
w(f"| dark-route threshold | {f(-float(_ic['Median']),2)} ADU | = -intercept |")
w(f"| its spread, pixel to pixel | {f(D['Local']['T']['StdIntr'],2)} ADU | the offset fixed pattern the budget uses ({100*float(D['Local']['T']['RelIntr']):.1f} % of the level) |")
w(f"| median chi2/dof | {f(D['Fit']['All']['MedianChi2Dof'],4)} | against {f(D['Chi2DofExpected'],4)} expected ({f(float(D['Fit']['All']['MedianChi2Dof'])/float(D['Chi2DofExpected']),3)}x) |")
w()
w("""**The decision that matters here is the weight**, derived in section 3.2:
`Weights = 'measured'`. The dark ladder is where a modelled weight fails
hardest, because its lowest steps are read-noise dominated and their *measured*
signal can be negative while the charge that fluctuated was positive.

**And the mean/median distinction is not cosmetic.** The per-pixel dark current
is right-skewed, so the mean over pixels and the median pixel differ by several
per cent and they answer different questions: the median pixel is the datasheet
number, the mean is what an ensemble fit of the whole die returns and therefore
what route a) of stage 10 reports. Both are written out and the reports say which
is which, because an earlier draft quoted one in one section and the other in
another.""")
w()

# ---------------------------------------------------------------- stage 3
stagehdr('3')
L = E.light
_fsb = [int(v) for v in np.atleast_1d(np.array(L['FitSteps']))]
w(f"""The same machinery on the bright ladder, every frame of which is taken at
the same sensor exposure `ExpSen = {f(L['ExpSen'],0)} s`, so the x axis is
illumination and not time:""")
block(["S(int) = R * int + I_B",
       "",
       "R       = Slope                     [ADU per intensity unit]",
       "T_light = DC * ExpSen - I_B         [ADU]   the charge threshold, light route",
       "",
       "Var(T_light) = ExpSen^2 * Var(DC) + Var(I_B)"])
w(f"""The dark term is there because a bright frame integrates dark charge for
`ExpSen` seconds like any other, so the intercept at zero illumination is not
`-T` but `DC*ExpSen - T`. `DC` and `Var(DC)` come from stage 2 through
`dc.bin` and `dc_var.bin`, and the two fits are independent, which is why their
variances simply add.

**This term is also the single largest trap in the chain, and Part 6 is about
it**: on a high-dark-current setup `DC*ExpSen` is larger than the answer, so the
"light" route stops being an independent measurement and becomes a restatement of
the dark one.

Measured on the example die over steps `{_fsb}`:""")
w()
_rs = L['Fit']['All']['SlopeSpread']
w('| quantity | value | note |')
w('|---|---|---|')
w(f"| photo-response, median pixel | {f(_rs['Median'],0)} ADU per intensity unit | |")
w(f"| its spread, deconvolved | {100*float(_rs['RelIntr']):.2f} % | mostly fit noise: observed {f(_rs['StdRobust'],0)}, fit {f(_rs['StdFitRobust'],0)} |")
w(f"| PRNU, from the per-step pattern | {100*float(L['PRNU']['Multiplicative']):.2f} % | section 3.6 -- **this** is the PRNU |")
w(f"| offset fixed pattern (Additive) | {f(L['PRNU']['Additive'],2)} ADU | section 3.6 |")
w(f"| PRNU, pixel to pixel | {100*float(L['Local']['Resp']['RelIntr']):.2f} % | `localSpread` of the response map |")
w(f"| light-route threshold | {f(E.route('b')['Threshold'],2)} ADU | |")
w(f"| median chi2/dof | {f(L['Fit']['All']['MedianChi2Dof'],4)} | against {f(L['Chi2DofExpected'],4)} expected |")
w()
w(f"""**Why the PRNU does not come from the spread of `R`.** Derived in section
3.6: the window holds {len(_fsb)} closely spaced steps, so a pixel's slope is
fixed to a few per cent, five times coarser than the pattern being measured, and
the observed spread is almost all fit noise -- visible in the table above, where
the fit noise {f(_rs['StdFitRobust'],0)} is
{100*float(_rs['StdFitRobust'])/float(_rs['StdRobust']):.0f} % of the observed
spread {f(_rs['StdRobust'],0)}. The per-step route measures the same pattern from
{NY*NX/1e6:.1f} M pixels on every one of the
{len(np.atleast_1d(np.array(L['PatternStep'])))} steps of the ladder, independent
of any fit window at all.""")
w()

# ---------------------------------------------------------------- stage 4
stagehdr('4')
BC = E.badcol
w(f"""Reads no frames at all: the three per-pixel maps of stages 1 to 3 are
enough, and the whole-die response map costs a full ladder read, so recomputing
it here would be waste. Two criteria, both applied to the **profile along the raw
readout columns** (image rows in the DESY orientation, section 1.2):""")
block(["bad if   median ZE noise of the column  >  median + NoiseSigma * robust sigma",
       "   or    median ZE noise of the column  >  NoiseFactor * median",
       "   or    median bright slope            <  median - RespSigma * robust sigma",
       "   or    median bright slope            <  RespFactor * median",
       "",
       "NoiseSigma = %g   NoiseFactor = %g   RespSigma = %g   RespFactor = %g"
       % (float(BC['NoiseSigmaCut']), float(BC['NoiseFactor']),
          float(BC['RespSigmaCut']), float(BC['RespFactor']))])
w(f"""**Both forms of each criterion are needed and that is the decision worth
recording.** A plain ratio to the median finds nothing here: the column-to-column
spread of the read noise is a few per cent
({100*float(BC['NoiseSigma'])/float(BC['NoiseMedian']):.1f} % on the example die),
so a column twice as noisy is a {2*float(BC['NoiseMedian'])/float(BC['NoiseSigma']):.0f}-sigma
outlier but only 2x the median -- the sigma test is what actually catches it. The
ratio test is kept because it catches the catastrophic case where the robust sigma
is itself inflated.

The dark-current profile is computed and reported but **does not mask**: a column
with more leakage is still a working column, and masking on leakage would remove
exactly the pixels a dark-current measurement is about.

On the example die {int(BC['Nbad'])} of {int(BC['Nrawcol'])} raw columns are
flagged ({100*(1-float(BC['GoodFraction'])):.2f} % of the pixels), of which
{int(BC['NbadInPairs'])} fall in complete pairs -- which is the readout-pairing of
stage 1 showing up again in the defects. The effect of masking on the ensemble
numbers is small and is quoted so that it can be checked:
read noise {f(BC['Effect']['RNall'],4)} -> {f(BC['Effect']['RNgood'],4)} ADU,
dark current {f(BC['Effect']['DCall'],4)} -> {f(BC['Effect']['DCgood'],4)} ADU/s.

The stage also measures the **gradient** of every map along the readout
direction, in thirds of raw column, which is what the cross-die gradient tests of
Part 7 are built on: on the example die the dark current rises by a factor
{f(BC['Gradient']['DCRatio'],3)} from one end of the die to the other while the
response changes by {f(BC['Gradient']['RespRatio'],4)} and the read noise by
{f(BC['Gradient']['RNRatio'],4)}. A gradient in the leakage with none in the
response is already a strong hint about its cause.""")
w()

# ---------------------------------------------------------------- stage 5
stagehdr('5')
PT = E.ptc
w("""The photon transfer curve, and the stage where the charge threshold stops
being an extrapolation. Per pixel, over the bright steps inside `DieGainRange`:""")
block(["S = g (Q - T)          the recorded signal, in ADU, of Q collected electrons",
       "Var(S) = g^2 Var(Q) = g^2 Q        because the loss happens AFTER the fluctuation",
       "       = g^2 (S/g + T) = g S + g^2 T",
       "",
       "with the read noise:   Var = g S + ( RN^2 + g * T_ADU ),      T_ADU = g T",
       "",
       "=>  slope     = g         the conversion gain, ADU/e-",
       "    intercept = RN^2 + g*T_ADU      NOT the read noise"])
w(f"""The last line is the point of the stage. The intercept of a photon transfer
curve is routinely read as the read noise; here it is the read noise **plus the
shot noise of the charge that was collected and not recorded**, so the curve
*measures* the threshold rather than assuming one. Inverting, each step gives""")
block(["Q = (Var - RN^2)/g^2          the charge actually collected",
       "S = g (Q - T)                 the signal recorded",
       "=> T follows step by step, with no extrapolation at all"])
w("""and a real threshold must then come out the same at every step -- a stronger
test than any extrapolating route passing on its own.

**The statistics of this stage are not those of stages 2 to 4 and the difference
decides its whole design.** A ladder point here is a per-pixel *variance* from
three frames: 2 degrees of freedom, chi2 distributed, 100 % relative error, long
tail, quantised (section 3.3). Four consequences, each verified against a null
simulation in which every pixel is given exactly the same gain:""")
w()
_N = PT['Null']
_U = PT['Unmasked']['All']
w(f"""| effect | measured | the null predicts | so the stage |
|---|---|---|---|
| the fitted slope is unbiased in the mean, biased low in the median | mean {f(_U['GainMean'],4)}, median {f(_U['GainMedian'],4)} ADU/e- | median {f(_N['Median'],4)} against truth {f(_N['Truth'],4)} ({100*(1-float(_N['MedianBias'])):.0f} % low) | quotes the **mean** as the gain |
| a fraction of pixels fit a negative slope | -- | {100*float(_N['FracNegative']):.1f} % | does not map the gain per pixel |
| the observed spread is not an intrinsic spread | MAD {f(_U['GainMAD'],4)} | null MAD {f(_N['MAD'],4)}, ratio {f(_U['MADoverNull'],4)} | compares with the null, not with `paramSpread` |
| one pixel's gain is good to about | {100*float(_U['GainMAD'])/float(_U['GainMean']):.0f} % | {100*float(_N['MAD'])/float(_N['Truth']):.0f} % | measures the gain where it *is* measurable |""")
w()
w(f"""The last row is the stage's answer to a real question. A per-pixel gain map
is not a measurement at that precision, but **averaging the variances first
is**: one readout column holds {NX} pixels and one 32x32 block holds 1024, so""")
block(["                        pixels/unit   observed     null      intrinsic",
       "gain per readout column  %6d       %.4f     %.4f     %.4f  (%.2f %%)"
       % (int(PT['Column']['N'] and NX), float(PT['Column']['StdObs']), float(PT['Column']['StdNull']),
          float(PT['Column']['StdIntr']), 100*float(PT['Column']['RelIntr'])),
       "gain per %dx%d block      %6d       %.4f     %.4f     %.4f  (%.2f %%)"
       % (int(PT['Block']), int(PT['Block']), int(PT['Block'])**2,
          float(PT['BlockGain']['StdObs']), float(PT['BlockGain']['StdNull']),
          float(PT['BlockGain']['StdIntr']), 100*float(PT['BlockGain']['RelIntr']))])
w(f"""In both lines the observed spread is only just above the null, and subtracting
the null in quadrature is what turns it into a measurement:
{100*float(PT['Column']['RelIntr']):.2f} % between readout columns and
{100*float(PT['BlockGain']['RelIntr']):.2f} % between blocks, both real. A
pixel-to-pixel gain variation is **not** detected: there the observed spread is
{f(_U['MADoverNull'],3)} times the null, consistent with 1.

The gain the stage reports for the die is {f(_U['GainMean'],4)} ADU/e- (the mean
over pixels) with the ensemble value {f(PT['GainEnsemble'],4)}, from the steps
spanning {f(PT['GainRange'][0],0)} to {f(PT['GainRange'][1],0)} ADU. Which steps
those are depends on the mode: in signal mode they are the window stage's list,
and in chi2 mode they are the steps whose mean signal falls inside
`DieGainRange`, whose lower edge had to be lowered from 100 to 80 ADU once two
dies turned out to have their lowest bright step at 99.3 and 92.9 ADU and were
silently losing it. The stage refits over {len(PT['Scan'])} window variants and
the spread of those is the gain's window systematic,
{100*float(PT['GainSystematic']['Rel']):.2f} %.

The stage also runs a **closure test** that is worth naming because it is the one
internal check that can fail: it predicts the PTC intercept from the *light*
route's threshold and the read noise, `RN^2 + g*T_light`, and compares it with the
measured intercept. On the example die it predicts
{f(PT['Closure']['Predicted'],1)} ADU^2 against
{f(PT['Closure']['Measured'],1)} measured, a ratio of
{f(PT['Closure']['Ratio'],3)} -- so the two routes do **not** close, and the
threshold the shot noise implies is {f(PT['Thresholds']['PTC_e'],1)} e- against the
light route's {f(PT['Thresholds']['Light_e'],1)} e- and the dark route's
{f(PT['Thresholds']['Dark_e'],1)} e-. All three are written to `ptc.json` so that
nothing downstream has to re-derive them.""")
w()

# ---------------------------------------------------------------- stage 6
stagehdr('6')
BU = E.budget
UM = BU['Unmasked']
w(f"""The only stage that constructs no object and reads no frames: the five
measured maps of stages 1 to 5 reduce to seven scalars, and `budgetCurve`
(section 3.7) turns them into curves. The inputs on the example die:""")
w()
_IN = UM['Inputs']
w('| input | value | from |')
w('|---|---|---|')
w(f"| read noise | {f(_IN['RN_ADU'],3)} ADU = {f(float(_IN['RN_ADU'])/float(_IN['GainADU']),3)} e- | stage 1 |")
w(f"| gain | {f(_IN['GainADU'],4)} ADU/e- | stage 5 |")
w(f"| dark current | {f(_IN['DC_ADU'],4)} ADU/s = {f(float(_IN['DC_ADU'])/float(_IN['GainADU']),4)} e-/s | stage 2 |")
w(f"| DSNU, local | {f(_IN['SigmaDC_ADU'],4)} ADU/s | stage 2, `localSpread` |")
w(f"| offset fixed pattern | {f(_IN['SigmaTdark_ADU'],2)} ADU (dark) / {f(_IN['SigmaTlight_ADU'],2)} ADU (light) | stages 2 and 3 |")
w(f"| PRNU, local | {100*float(_IN['PRNU']):.2f} % | stage 3 |")
w(f"| exposure | {f(_IN['ExpTime'],0)} s | the bright ladder |")
w()
w(f"""and the output is the limiting signal at SNR = {int(UM['PTC']['SNRdet'])}, run
once **per threshold route** because the stage deliberately refuses to choose one:""")
w()
w('| threshold used | T [e-] | Qlim calibrated [e-] | Qlim raw [e-] |')
w('|---|---|---|---|')
for _k, _lab in (('PTC', 'the shot-noise route'), ('Dark', 'the dark response route'),
                 ('Light', 'the light response route')):
    if _k in UM:
        w(f"| {_lab} | {f(UM[_k]['Threshold_e'],1)} | **{f(UM[_k]['Qlim_cal_5'],0)}** | {f(UM[_k]['Qlim_raw_5'],0)} |")
w()
_ql = [float(UM[k]['Qlim_cal_5']) for k in ('PTC', 'Dark', 'Light') if k in UM]
w(f"""**That table is the reason the stage refuses.** The threshold choice alone
moves the limiting signal from {min(_ql):.0f} to {max(_ql):.0f} e- -- a factor
{max(_ql)/min(_ql):.1f} -- against
{abs(float(UM['PTC']['Qlim_cal_gmax'])-float(UM['PTC']['Qlim_cal_gmin'])):.1f} e- for
the gain's own window systematic and a fraction of an electron for the bad-column
mask. No other term in the budget comes close, so a budget that quietly adopted
one route would be reporting a choice as a measurement. Part 6 is about which
route to believe; this stage carries all of them.

**Every fixed-pattern term here is the local spread**, for the reason given in
section 3.7: a budget asks what varies between neighbouring pixels, and the
die-wide figures of stages 2 and 3 are dominated by a 2:1 dark-current ramp that
any flat field removes.""")
w()

# ---------------------------------------------------------------- stage 7
stagehdr('7')
VS = E.varspread
_vsteps = VS['Steps'] if isinstance(VS['Steps'], list) else [VS['Steps']]
w(f"""Stage 5 needed to know how far a per-pixel variance can be trusted; this
stage measures it, at every step of both ladders and at zero signal. For each
step it takes the distribution of the per-pixel temporal variance over the whole
die and compares it with the distribution the same measurement would give **if
every pixel were identical**, then deconvolves with section 3.3(c).

Three things here are easy to get wrong and each was got wrong first.

**The null is simulated with integer frames.** A variance of three integers can
only take multiples of 1/18, so both distributions are combs. A continuous chi2
null would differ from the data in a way that has nothing to do with the pixels,
and the figure would show a mismatch that is pure arithmetic. Simulating
{int(VS['Nsim']):,} pixels with integer-rounded frames puts the simulated comb on
the measured one tooth for tooth.

**The width is trimmed, top 0.1 %, and the same rule is applied to the null.** A
cosmic ray in one of three frames puts a pixel's variance at 10^7 ADU^2. On the
long dark steps the untrimmed standard deviation of V is some 2700 times the chi2
expectation, almost all of it carried by 0.001 % of the pixels. A median-based
width will not do either: the MAD of a quantised variance ties *exactly* with the
null, because both are pinned to the same comb teeth.

**The significance counts the null's own simulation error, and it dominates.**
The data has {NY*NX/1e6:.1f} M pixels against the null's {int(VS['Nsim'])/1e6:.1f} M,
so above a few hundred ADU what the stage can state is an upper limit, and those
are what the dumps carry.

The measured profile on the example die -- the intrinsic pixel-to-pixel spread of
the *true* variance, as a fraction of it:""")
w()
w('| step | signal [ADU] | dof | observed | null | intrinsic | note |')
w('|---|---|---|---|---|---|---|')
for _q in _vsteps:
    _ty, _sg = _q['Type'], float(_q['Signal'])
    if not (_ty == 'ZE' or _sg < 1 or _sg > 900):
        continue
    _un = _q['Unmasked']
    _note = {'ZE': 'the read noise alone', 'D': 'dark signal', 'B': 'photo-signal'}.get(_ty, '')
    w(f"| {_ty}{int(_q['Step']) if _ty != 'ZE' else ''} | {_sg:.1f} | {int(_q['Dof'])} | "
      f"{100*float(_un['StdObs'])/float(_un['MeanVar']):.0f} % | "
      f"{100*float(_un['StdNoise'])/float(_un['MeanVar']):.0f} % | "
      f"**{100*float(_un['RelIntr']):.0f} %** | {_note} |")
w()
w("""Read down the last two columns: at zero signal the variance is read noise,
which really does vary a great deal from pixel to pixel; on the dark ladder the
intrinsic spread tracks the DSNU; on the bright ladder, where shot noise
dominates, it falls to a few per cent, which is all the PRNU can produce. The
observed column is nearly constant at about 100 % throughout -- that is the
estimator, not the device, and it is exactly what section 3.3(c) says a
three-frame variance must look like.""")
w()

# ---------------------------------------------------------------- stage 8
stagehdr('8')
LS = E.lowsignal
_ls = LS['Steps'] if isinstance(LS['Steps'], list) else [LS['Steps']]
w(f"""Stage 7 measured how much the per-pixel variance varies; this stage asks
what it is **made of**, pixel by pixel, at the steps below `DieLowMax =
{f(LS['LowMax'],0)} ADU` where the read noise, the dark signal and the photo-signal
are all comparable. The prediction uses only quantities measured elsewhere:""")
block(["sigma^2_pred,i  =  RN_i^2  +  g * ( S_i + T )",
       "",
       "RN_i   the read-noise map of stage 1, from the ZE frames: independent of these frames",
       "S_i    this pixel's own measured signal at this step",
       "g      the gain of stage 5                 T   the charge threshold",
       "",
       "on the example die:  g = %.4f ADU/e-,  T = %.2f ADU" % (float(LS['Gain']), float(LS['Threshold']))])
w("""**The prediction is never compared with the data directly, and that is the
decision this stage turns on.** It is first *measured the way the data were*:
each predicted pixel is sampled through its own chi2 with the frames rounded to
integers, exactly as the detector rounds them, and only then are the two
distributions compared. The reason is that the predictor carries noise from both
its terms -- `S_i` is a mean of three frames, and `RN_i^2` is itself a variance
from five, whose long tail makes its sampling error as large as its spread.
Comparing a noisy predictor with the data directly dilutes the calibration slope
to about 0.5 at the low bright steps and invites the conclusion that noisy pixels
are quiet under illumination. Running both sides through the same binning removes
the effect exactly, with no correction factor to argue about.

Three comparisons, because each can fail differently: the **distribution** of
measured against resampled-predicted variances, the **calibration** (pixels
binned by predicted variance, mean measured against mean predicted -- a slope away
from 1 points at the gain or the threshold), and a **residual map**, which says
where on the die the prediction fails if it does.

The bias frames are included as a wiring check: there the prediction is circular
by construction, because the read-noise map is built from those very frames, and
it must come out exact.""")
w()
w('| step | signal [ADU] | measured V | predicted | difference |')
w('|---|---|---|---|---|')
for _q in _ls:
    _ty = _q['Type']
    w(f"| {('bias' if _ty == 'ZE' else _ty + str(int(_q['Step'])))} | {float(_q['Signal']):.0f} | "
      f"{float(_q['MeanV']):.1f} | {float(_q['MeanPred']):.1f} | "
      f"**{100*float(_q['ResidRel']):+.2f} %**{' (the wiring check)' if _q.get('Circular') else ''} |")
w()
_lb2 = [q for q in _ls if q['Type'] == 'B']
_ld2 = [q for q in _ls if q['Type'] == 'D']
if _lb2 and _ld2:
    w(f"""The bright ladder is explained to
{abs(100*float(_lb2[-1]['ResidRel'])):.2f} % at {float(_lb2[-1]['Signal']):.0f} ADU,
and so is its pixel-to-pixel width. The dark ladder is **not**: its variance runs
{abs(100*float(_ld2[-1]['ResidRel'])):.1f} % below the prediction at the top of the
range while the widths still match. So it is the *level*, not the uniformity,
that is wrong -- dark charge produces less shot noise than photo-charge of the
same measured size, which is the dark deficit that stages 9 and 10 then measure
as a gain.""")
w()

# ---------------------------------------------------------------- stage 9
stagehdr('9')
PP = E.perpixel
_dD, _dB, _df = PP['Ladder']['D'], PP['Ladder']['B'], PP['Ladder']['Difference']
w("""Stage 5 fits the bright ladder per pixel. This fits **each ladder
separately**, per pixel, and keeps the two apart, because the ensemble curves do
not agree: the dark ladder carries about 10 % less variance than the bright-ladder
line at the same measured signal. An ensemble comparison cannot say whether that
is something every pixel does or something a subset carries; a per-pixel
difference can.

For each pixel and each ladder, x is that pixel's own mean signal at a step and y
its own **total** temporal variance there, so the fitted intercept is
`RN^2 + g*T`.""")
w(f"""**Subtracting each pixel's own `RN_i^2` first -- which the ensemble fits of
stage 10 do, and should -- is wrong here.** `RN_i^2` is itself measured from
{int(E.stats['Nframes'])} frames, carries about 50 % error with a long tail, and is
subtracted once, so it lands entirely in the intercept. Doing it inflated the
dark intercept's spread without changing its centre, which is the signature of
adding noise rather than removing a bias. The whole-die constant is taken out at
the ensemble level instead, where it is measured from
{NY*NX/1e6:.1f} M pixels.

Measured on the example die, against a null in which every pixel has the same
gain:""")
w()
w('| | dark ladder | bright ladder | difference |')
w('|---|---|---|---|')
w(f"| ensemble gain [ADU/e-] | {f(_dD['GainEnsemble'],4)} | {f(_dB['GainEnsemble'],4)} | "
  f"{100*(1-float(_dD['GainEnsemble'])/float(_dB['GainEnsemble'])):.1f} % |")
w(f"| per-pixel slope, mean | {f(_dD['Slope']['Mean'],4)} | {f(_dB['Slope']['Mean'],4)} | "
  f"mean of the difference {f(_df['Mean'],4)} |")
w(f"| per-pixel slope, MAD | {f(_dD['Slope']['MAD'],4)} | {f(_dB['Slope']['MAD'],4)} | {f(_df['MAD'],4)} |")
w(f"| MAD over the null | {f(_dD['Slope']['MADoverNull'],4)} | {f(_dB['Slope']['MADoverNull'],4)} | {f(_df['MADoverNull'],4)} |")
w()
w(f"""The difference of the two per-pixel gains has a MAD
{f(_df['MADoverNull'],3)} times the null, i.e. the *spread* of the difference is
what two independent noisy fits would give anyway, while its **centre** is shifted
by {100*abs(float(_df['MeanRel'])):.1f} %. Read together: the deficit is not
carried by a subset of pixels, it is something the whole population does. That is
the result the ensemble comparison could not reach.""")
w()

# ---------------------------------------------------------------- stage 10
stagehdr('10')
w("""The stage that puts the four routes side by side with errors. Written out in
full, with `I` for an intercept:""")
block(["a)  dark response     S(t)   = DC * t  + I_D      ->  T = -I_D                  (no gain)",
       "b)  light response    S(int) = R * int + I_B      ->  T = DC * ExpSen - I_B      (no gain)",
       "c)  PTC, dark ladder    Var - RN^2 = g (S + T)    ->  g = slope,  T = I/g",
       "d)  PTC, bright ladder  Var - RN^2 = g (S + T)    ->  g = slope,  T = I/g"])
w("""Only c) and d) measure a gain: a response curve contains no noise, so it
cannot. All four give a threshold, and they disagree -- which is the point of
putting them in one table rather than choosing one.

**The photon-transfer routes are fitted on `Var - RN^2`**, the pixel's own read
noise taken out before the fit rather than subtracted from the intercept
afterwards. The term is small, but it makes the intercept mean one thing only,
`g*T`, and it removes a quantity that varies strongly from pixel to pixel from
what is otherwise a whole-die constant.

**Averages inside a block are means of both axes over one common set of
pixels** (those outside the top 0.1 % of the variance). `Var = g*S + c` holds per
pixel, so a median on one axis and a mean on the other is not a point on any
curve; and because the dark signal is right-skewed while the bright signal is
not, a mixed pair biases the two ladders differently -- which is exactly how the
dark deficit was once over-estimated by a factor of a few.

Both errors come from this stage and both are derived in section 3.8: the
statistical one is the scatter over the
{int(E.methods['NBlock'])}x{int(E.methods['NBlock'])} blocks, the systematic one is
half the range over the window variants.""")
w()
w(f'| route | slope | threshold [ADU] | threshold [e-] | windows refitted |')
w('|---|---|---|---|---|')
_un = {'a': 'ADU/s', 'b': 'ADU/intensity', 'c': 'ADU/e-', 'd': 'ADU/e-'}
_nd = {'a': 4, 'b': 1, 'c': 4, 'd': 4}
for _k in 'abcd':
    _R = E.route(_k)
    _n = _nd[_k]
    _v = (float(_R['Gain']), float(_R['GainStat']), float(_R['GainSyst'])) if _R.get('Gain') is not None \
         else (float(_R['Slope']), float(_R['SlopeStat']), float(_R['SlopeSyst']))
    w(f"| {_k}) {_R['Name']} | {_v[0]:.{_n}f} +- {_v[1]:.{_n}f} +- {_v[2]:.{_n}f} {_un[_k]} | "
      f"{float(_R['Threshold']):.2f} +- {float(_R['ThresholdStat']):.2f} +- {float(_R['ThresholdSyst']):.2f} | "
      f"**{float(_R['Threshold_e']):.1f} +- {float(_R['Threshold_e_err']):.1f}** | {len(_R['WindowValues'])} |")
w()
_gc, _gd = float(E.route('c')['Gain']), float(E.route('d')['Gain'])
_ec = float(np.hypot(E.route('c')['GainStat'], E.route('c')['GainSyst']))
_ed = float(np.hypot(E.route('d')['GainStat'], E.route('d')['GainSyst']))
_cons = E.methods['Routes'].get('constrained')
w(f"""The two gains differ by {100*abs(_gd-_gc)/_gd:.1f} % with combined errors of
{np.hypot(_ec,_ed):.4f}, a {abs(_gd-_gc)/np.hypot(_ec,_ed):.0f} sigma separation --
the dark deficit again, now as a gain with an error bar on it.

The stage then runs the test that settles whether that is a gain difference or an
offset: **force the bright gain on the dark points and fit only the offset.**""")
if _cons:
    _cr = np.atleast_1d(np.array(_cons['Residual'], dtype=float))
    w()
    block(["forced gain          %.4f ADU/e-        (the bright-ladder value)" % float(_cons['GainImposed']),
           "fitted offset        %+.2f ADU^2   =>  T = %+.2f ADU" % (float(_cons['Offset']),
                                                                     float(_cons['Threshold'])),
           "residuals            " + '  '.join('%+.1f' % v for v in _cr) + "  ADU^2",
           "",
           "residual rms         %.2f ADU^2" % float(_cons['ResidRMS']),
           "   with the gain free %.2f ADU^2  (gain %.4f, offset %+.2f)"
           % (float(_cons['FreeResidRMS']), float(_cons['FreeGain']), float(_cons['FreeOffset'])),
           "   so forcing the gain is worse by a factor %.0f"
           % (float(_cons['ResidRMS'])/max(float(_cons['FreeResidRMS']), 1e-12))])
    w("""The residuals run monotonically from one end of the ladder to the other,
which is the signature of a wrong slope and not of a wrong offset. **A shared gain
cannot be rescued by any offset: the two ladders differ in slope.**""")
w()

# ---------------------------------------------------------------- stage 10+
stagehdr('10+')
PX = E.points
w(f"""Not an analysis stage but the one that makes every figure reproducible
without the frames. It writes one row per ladder point -- the bias frames, every
dark step and every bright step -- carrying both the point that appears on a PTC
figure and the full pixel distribution of the **excess** variance""")
block(["E_i = V_i - RN_i^2"])
w(f"""the variance the signal added to that pixel, with the read noise of stage 1
taken out pixel by pixel. The distribution is stored three ways so that any plot
can be remade: a fine histogram (edges and counts, exact for any binned view), a
quantile table, and a random subsample of {int(PX['Meta']['SampleN']):,} pixels
with their signals, for scatter plots. The subsample uses the **same** pixels at
every point, so one pixel can be followed up the ladder; their indices and read
noise are in `Meta`.

The definition of the plotted point is where this stage earns its place, and it
took two attempts:""")
block(["x = mean over pixels of the pixel's mean signal       (NOT a median)",
       "y = mean over pixels of the pixel's excess variance   (NOT a median)",
       "both over ONE common set of pixels: those outside the top 0.1 % of the variance",
       "",
       "VarMeanChi2      = median V * 1/ln2            the chi2 median-to-mean factor, section 3.3(a)",
       "VarMeanCorrected = VarMeanChi2 * sqrt(1 + SpreadRel^2)    SpreadRel from stage 7"])
w(f"""The first version used a median on the variance axis (a plain mean is
destroyed by cosmic rays) times the chi2 median-to-mean factor, with a plain
median on the signal axis. That is not a point on any curve: `Var = g*S + c` holds
per pixel, so averaging over pixels needs `E[Var]` against `E[S]` -- a mean on both
axes, over the same pixels. The dark signal is right-skewed while the bright
signal is not, so the mixed pair biased the two ladders differently. The second
factor is needed because the first is exact only for identical pixels: when the
true variance itself spreads by a relative width `SpreadRel`, the corrected median
estimates the median rather than the mean, and `SpreadRel` is
{f([q for q in (VS['Steps'] if isinstance(VS['Steps'], list) else [VS['Steps']]) if q['Type']=='D'][-1]['Unmasked']['RelIntr'],2)}
on the dark ladder against under 0.06 on the bright one -- so omitting it biases
the two ladders differently too. **This stage therefore depends on stage 7**, and
a die exported before stage 7 ran carries `SpreadRel = 0` and an uncorrected open-
symbol set; the dependency graph of Part 8 records it.""")
w()

# ================================================================== Part 6
def pedestal(die):
    """how much of the light route's threshold is the extrapolated dark pedestal"""
    Rb, Ra = die.route('b'), die.route('a')
    ped = float(Ra['Slope'])*float(die.light['ExpSen'])
    return ped, float(Rb['Threshold']), 100*ped/float(Rb['Threshold'])

w('## 6. The four routes, and what the disagreement means')
w()
w(f"""The four thresholds of stage 10 do not agree, on any die, and the campaign
was asked to say which to believe. The answer is not a preference; it follows from
three measurements.""")
w()
w('### 6.1 The response pair is not two measurements')
w()
_pE, _tE, _fE = pedestal(E)
_pC, _tC, _fC = pedestal(C)
w(f"""Route b) subtracts a dark pedestal, `DC_fit * ExpSen`, because a bright frame
integrates dark charge like any other (section 5.4). The pedestal uses the dark
ladder's **fitted slope**, so route b) inherits route a)'s extrapolation. How much
of route b) that is depends entirely on the dark current:""")
w()
w('| | pedestal `DC_fit * ExpSen` [ADU] | light intercept [ADU] | T_light [ADU] | pedestal as a fraction of the answer |')
w('|---|---|---|---|---|')
w(f"| run {RUN} (low dark current) | {_pE:.2f} | {float(E.route('b')['WindowValues'][0][1]):+.2f} | {_tE:.2f} | **{_fE:.0f} %** |")
w(f"| run {C.ptc['Run']} (20x the dark current) | {_pC:.2f} | {float(C.route('b')['WindowValues'][0][1]):+.2f} | {_tC:.2f} | **{_fC:.0f} %** |")
w()
w(f"""On the high-dark-current setup the pedestal is **larger than the answer**.
Route b) there is route a) with a {abs(float(C.route('b')['WindowValues'][0][1])):.0f} ADU
correction, not an independent measurement -- and the bright ladder itself is
nearly identical between the two runs (slopes
{float(E.route('b')['Slope']):.0f} and {float(C.route('b')['Slope']):.0f} ADU per
intensity unit, {100*abs(float(C.route('b')['Slope'])/float(E.route('b')['Slope'])-1):.1f} %
apart), so nothing about the photo-response can explain a threshold that differs
between the runs by a factor
{float(C.route('b')['Threshold_e'])/float(E.route('b')['Threshold_e']):.1f}. The
dark pedestal explains all of it.

So on a high-dark-current run the two "response" routes report **one** quantity:
the intercept of the dark ladder.""")
w()
w('### 6.2 That intercept is not a fixed charge offset')
w()
w("""A threshold is a fixed number of electrons, so a fit over any window of a
ladder obeying `S = DC*t - T` must return the same `T`. It does not. Reading the
`T` column of the window scan of section 4.2, and the same scan on the other
setup:""")
w()
for _d, _lab in ((ED, f'run {RUN}'), (CD, f'run {C.ptc["Run"]}')):
    if _d.dw and _d.dw.get('Scan'):
        _T = [float(q['Tdark']) for q in _d.dw['Scan']]
        _n3 = [q for q in _d.dw['Scan'] if int(q['Nsteps']) == 3]
        w(f"* **{_lab}**: over the {len(_T)} candidate windows the dark-route "
          f"threshold runs from {min(_T):.1f} to {max(_T):.1f} ADU, a factor "
          f"{max(_T)/max(min(_T), 1e-9):.1f}, and it varies **monotonically** with "
          f"where the window sits -- not randomly, as a noisy measurement of one "
          f"number would.")
w()
w(f"""A quantity that moves by a factor of two to four with the choice of window,
monotonically, is measuring the **curvature** of the ladder, in ADU. Its size in
ADU therefore scales with the dark-current scale, which is why the
high-dark-current run's response thresholds are the large ones.""")
w()
w('### 6.3 The variance settles it')
w()
w(f"""If ~{float(C.route('a')['Threshold_e']):.0f} e- really were collected and not
recorded, that charge carried Poisson noise, so the measured variance at a
recorded signal `S` would be that of `S + T` electrons and the photon-transfer
intercept would read the same ~{float(C.route('a')['Threshold_e']):.0f} e-. It does
not:""")
w()
w('| route | run ' + str(RUN) + ' [e-] | run ' + str(C.ptc['Run']) + ' [e-] | change between the setups |')
w('|---|---|---|---|')
for _k in 'abcd':
    _a, _b = E.route(_k), C.route(_k)
    _ds = abs(float(_b['Threshold_e'])-float(_a['Threshold_e']))/np.hypot(float(_a['Threshold_e_err']), float(_b['Threshold_e_err']))
    w(f"| {_k}) {_a['Name']} | {float(_a['Threshold_e']):.1f} +- {float(_a['Threshold_e_err']):.1f} | "
      f"{float(_b['Threshold_e']):.1f} +- {float(_b['Threshold_e_err']):.1f} | {_ds:.1f} sigma |")
w()
w(f"""**Route d) moves by {abs(float(C.route('d')['Threshold_e'])-float(E.route('d')['Threshold_e'])):.1f} e-
between two setups of the same die -- {abs(float(C.route('d')['Threshold_e'])-float(E.route('d')['Threshold_e']))/np.hypot(float(E.route('d')['Threshold_e_err']), float(C.route('d')['Threshold_e_err'])):.1f} sigma --
while route a) moves by {abs(float(C.route('a')['Threshold_e'])-float(E.route('a')['Threshold_e'])):.0f} e-.**
Charge that carries no shot noise was never collected, so the large response
numbers are not a charge threshold. The conclusion the chain reports is therefore:

* **the bright-ladder photon-transfer route is the one to quote**. It involves no
  extrapolation and no dark ladder, and it is reproducible across a change of
  setup;
* the dark-ladder photon-transfer route is the same idea but weaker here: its
  ladder is short on a low-dark-current run and its own variance misbehaves (the
  dark deficit), so its error is large;
* the two response routes are useful as a **diagnostic of ladder curvature**, not
  as a threshold.

Nothing in the chain has been changed to enforce that conclusion: all four routes
are still computed, all four are reported with their errors, and the noise budget
still carries each of them, so a reader can disagree with the recommendation
without re-running anything.""")
w()
w('### 6.4 An estimator change that is proposed but not applied')
w()
_m15E = float(np.array(E.fw['D']['SignalMean'])[0]) if E.fw else float('nan')
_m15C = float(np.array(C.fw['D']['SignalMean'])[0]) if C.fw else float('nan')
w(f"""The pedestal of route b) is an *extrapolated* dark signal. The dark signal
actually present in an `ExpSen = {f(E.light['ExpSen'],0)} s` exposure is measured
directly -- it is the first step of the dark ladder. Substituting it:""")
w()
w('| | extrapolated pedestal | measured step 1 | T_light as computed [e-] | with the measured pedestal [e-] | route d) [e-] |')
w('|---|---|---|---|---|---|')
for _d, _m in ((E, _m15E), (C, _m15C)):
    _p, _t, _fr = pedestal(_d)
    _b0 = float(_d.route('b')['WindowValues'][0][1])
    _g = float(_d.methods['GainForElectrons'])
    w(f"| run {_d.ptc['Run']} | {_p:.2f} ADU | {_m:.2f} ADU | {float(_d.route('b')['Threshold_e']):.1f} | "
      f"**{(_m-_b0)/_g:.1f}** | {float(_d.route('d')['Threshold_e']):.1f} |")
w()
w(f"""The inter-run gap collapses from
{abs(float(C.route('b')['Threshold_e'])-float(E.route('b')['Threshold_e'])):.0f} e- to
{abs(((_m15C-float(C.route('b')['WindowValues'][0][1]))/float(C.methods['GainForElectrons'])) - ((_m15E-float(E.route('b')['WindowValues'][0][1]))/float(E.methods['GainForElectrons']))):.0f} e-,
and both land within a few electrons of route d). It is **not** applied, for a
stated reason: the first dark step of the low-dark-current run measures
{_m15E:.2f} ADU, which is negative as a dark signal and therefore exposes a
zero-point offset of a few ADU between the ZE reference and the dark frames --
about {abs(_m15E)/float(E.methods['GainForElectrons']):.1f} e-, the same size as the
answer. Fixing the dark zero point is the prerequisite, and until it is fixed the
substitution would trade a large known bias for a smaller unknown one.""")
w()

# ================================================================== Part 7
w('## 7. The cross-die layer')
w()
w(f"""`desy_die_summary.py` takes the `methods.json`, `dark.json`, `light.json`,
`badcol.json`, `ptc.json` and window dumps of an arbitrary list of die-runs and
answers the questions no single die can. It reads the per-run
`PTC_Config.xlsx` itself, so when two runs differ it can say *what* differed
rather than quoting a literal written for whichever pair of runs the author had
in mind.

| section | question | what decides it |
|---|---|---|
| 1 | the datasheet, die by die | every stage's headline, with the out-of-family flags |
| 2 | does the dark deficit repeat? | the two gains with errors, the ensemble offset, and the mean per-pixel gain difference -- three estimators of one quantity, side by side to bound the method error |
| 3 | the dark-current gradient: silicon or setup? | the **amplitude**, not the shape |
| 4 | the four routes across dies, flavours and runs | the same die measured on two setups |
| 4b | every other datasheet quantity, die by die | |
| 4c | controlled comparisons | pairs of runs that differ in exactly one setting |
| 5 | the fit window each die was given | |

Three of those deserve their reasoning recorded.

**The out-of-family test is made against the lot itself**, not against a
specification: a quantity more than 5 MAD from the median over all die-runs, or a
threshold negative by more than its own error. The second half of that rule was
tightened after an earlier version flagged four dies for thresholds of -0.6 to
-2.8 e- with errors of 2.7 to 4.9 -- negative, but not significantly. Flagged
die-runs are **marked and kept**, never dropped, with the reason printed next to
them.

**Shape is a useless discriminator for the gradient and amplitude is not.** Every
dark-current profile along the readout direction is a monotonic ramp, so every
pair of dies correlates above 0.88 whatever the cause. What separates the
hypotheses is the amplitude: the ranking of the dies is identical on both setups
and the between-die spread is 5x the within-die scatter, yet each die's ratio
shrinks at the higher-dark-current run, which a gradient fixed in the silicon
cannot do -- that would give exactly 1.000, and the measurement gives 0.772 +-
0.021. **What is excluded is a silicon-fixed gradient**; what is *not* established
is that the cause is temperature, because the two setups also differ in three
bias voltages. The test that would settle it is the same dies at a different chuck
set-point with the bias held fixed, and the summary says so rather than implying
the answer.

**A controlled comparison requires exactly one setting to differ**, which is why
the config diff exists. Three runs of this campaign form a clean transfer-gate
scan -- identical in every recorded setting except `zVDD_TX` at 3.3, 3.5 and
3.7 V -- and that is the only reason their differences can be attributed. The
pair that differs in many settings at once is reported as a difference and not as
a cause.""")
w()

# ================================================================== Part 8
def shdoc(path):
    """the first block of comment lines of a driver, which is its documentation"""
    out = []
    with open(path) as fh:
        for i, L in enumerate(fh):
            L = L.rstrip()
            if i == 0 and L.startswith('#!'):
                continue
            if L.startswith('#'):
                out.append(L.lstrip('# ').rstrip())
            elif out:
                break
    return out

w('## 8. Running it')
w()
w('### 8.1 Environment')
w()
w("""| what | how |
|---|---|
| MATLAB | `matlab -batch` with AstroPack on the path. The stages are package-qualified **scripts**, so they run in the caller's workspace and leave their variables there -- convenient when one fails half way and `P` is wanted. Note that `exist('ultrasat.lab.scripts.x','file')` returns 0 for them even though `which` finds them: test with `which` |
| python | `numpy`, `matplotlib` for the plot and report layer; `scipy.special` in two places |
| markdown to HTML | `marked.js` from cdnjs, inlined by the report builder |
| HTML to PDF | `/snap/bin/chromium --headless --print-to-pdf` |
| proof-rendering a deck | `soffice --convert-to pdf`, only to look at it |

A stage is invoked by setting the selector and calling the script:""")
block(["matlab -batch \"DieSelect=struct('Run','%s','Folder','%s', ...\n"
       "                  'Die','%s','Gain','%s','Lot','%s'); DieWindowMode='%s'; \\\n"
       "       ultrasat.lab.scripts.desy_die_dark\""
       % (RUN, os.path.basename(E.chain['Dataset'].rsplit('/', 2)[-2]) if E.chain else 'FOLDER',
          DIE, E.ptc['GainHalf'], LOT, 'signal' if E.sig else 'chi2')])
w(f"""`desy_die_config.m` resolves that selector into every path and every setting
the stage needs, so it is the only file to edit when the dataset moves. It also
holds the analysis constants: `DieDarkTol` = 1.10, `DieLinLimit` =
{LINLIM:.0f} ADU, `DieSigLo` / `DieSigHi` = {E.fw['SigLo']:.0f} / {E.fw['SigHi']:.0f} ADU,
`DieGainRange`, `DieNBlock`, and the output tag -- which carries the lot only for
a non-default lot, and the suffix `_sig` in signal mode so the two conventions
cannot overwrite each other.""")
w()
w('### 8.2 The drivers')
w()
w(f"""The shell drivers live in `{A.batch}` and are **not** in the repository:
they hold machine-specific paths. Each takes `RUN FOLDER DIE GAIN [LOT]`. Their
own headers, read at build time:""")
w()
_sh = sorted(f for f in os.listdir(A.batch) if f.endswith('.sh'))
w('| driver | usage | what it does |')
w('|---|---|---|')
for _f in _sh:
    _doc = shdoc(os.path.join(A.batch, _f))
    _use = next((L[len('usage:'):].strip() for L in _doc if L.startswith('usage:')), '')
    _txt = ' '.join(L for L in _doc if not L.startswith('usage:')).strip()
    w(f"| `{_f}` | `{_use}` | {_txt} |")
w()
w("""The batch drivers read the die list on file descriptor 9 and give each child
`</dev/null`, which is not decoration: `matlab -batch` inherits stdin, and a
driver that fed the list on plain stdin had its children drain it. That was
diagnosed the hard way -- a second launcher was started on the belief that the
list had been consumed, two chains wrote one output directory, and the recovery
was to kill the duplicate, delete 258 MB of mixed output and re-run that die
clean. The original batch had simply been at its concurrency limit.""")
w()
w('### 8.3 What has to be re-run after what')
w()
w("""The single most useful table in this document when changing anything. A
change to a row re-runs the stages marked, and nothing else:""")
w()
w("""| what changed | re-run | why |
|---|---|---|
| report text, a plot script | nothing -- rebuild the report | the builders read only json |
| `DieNoiseSigma`, the bad-column rule | 4, then 6 | the mask feeds the budget |
| `DieGainRange` | 5, 6, 8, 9, 10, 10+ | stages 1, 2a, 2, 3, 4, 7 never read it |
| `DieLowMax` | 8 | nothing else uses it |
| `DieNBlock` | 10 | the block grid is local to it |
| the **fit window** (`DieSigLo/Hi`, `DieDarkTol`, the mode) | 2a and then everything from 2 on | every fit downstream takes its steps from the window |
| stage 7 added to a die that lacked it | 10+ (then its figures and report) | the export folds stage 7's `SpreadRel` into the plotted points |
| a new die | the whole chain | |
| the die list | the summary, the deck, the README | |

The second-to-last row is a real dependency and was missed once: two dies had
been exported before stage 7 existed for them, so `SpreadRel = 0` and the open
symbols of their photon-transfer figure carried no median-to-mean correction. The
corrected points were unaffected -- they are `ExcessMean`, which does not use it.""")
w()
w('### 8.4 Cost and layout')
w()
if E.chain:
    _tt = {s['Name']: float(s['Seconds']) for s in E.chain['Stages']}
    w(f"""Measured on the worked example: {int(E.chain['Nfiles'])} files,
{float(E.chain['Bytes'])/1e9:.1f} GB read, {float(E.chain['TotalSeconds'])/60:.0f} minutes
of wall clock for the whole die. The three expensive stages are the ones that
touch every frame of both ladders --
{', '.join('%s (%.0f s)' % (k.replace('desy_die_', ''), v) for k, v in sorted(_tt.items(), key=lambda kv: -kv[1])[:3])}
-- and they are the reason the streamed mode exists. Four dies in parallel is
free on one node; the limit is disk read, not CPU.""")
    w()
w(f"""| where | what |
|---|---|
| `{A.rnroot}/<tag>/` | stage 1, shared by both window conventions |
| `{A.root}/<tag>[_sig]/` | every other stage: json, binary maps, figures, `report.md/html` |
| `{A.root}/summary[_sig][16]/` | the cross-die summaries |
| `{A.root}/pipeline/` | this document |
| `{A.reports}/` | the PDFs, the deck and the README that go out |

A tag is `run<RUN>[_<LOT>]_<WAFER>_<DIE>_<GAIN>`, with the lot present only when
it is not the default one, and `_sig` appended in signal mode.""")
w()

# ================================================================== Part 9
w('## 9. What the chain does not determine')
w()
w("""Stated here so that it is not inferred from silence.

**It does not measure a temperature.** Nothing in the data is a thermometer. The
dark-current ratio between two setups was read as one in an early draft and that
reading was withdrawn, because the same two setups also differ in three bias
voltages. What survives is cause-independent and is reported as such.

**It does not measure an absolute flux.** The bright x axis is the illumination
value the tester recorded times 1000; the photo-response is in ADU per that unit,
not in electrons per photon. A quantum efficiency needs a calibrated source.

**It does not give a single charge threshold**, and Part 6 is the argument for
why asking for one is the wrong question. It gives four numbers, their errors,
and a recommendation.

**A pixel-level gain variation is not detected**, which is not the same as being
absent: stage 5 bounds it, and the bound is what the dumps carry.

**The dark zero point is unfixed.** The first dark step of a low-dark-current run
measures a few ADU *below* the bias reference, which is unphysical as a dark
signal. It is small -- a few electrons -- but it is the same size as the threshold
the chain is trying to measure, and it is the prerequisite for the estimator
change of section 6.4.

**Two estimator changes are proposed and not applied**, deliberately: the
measured dark pedestal for route b) (section 6.4), and widening the window
systematic from the variants of the chosen window to the full grid (sections 3.8
and 4.2). Both would move published numbers, so both are stated with the
measurement that motivates them and left for a decision.""")
w()

# ================================================================== Appendix
w('## Appendix A. The operational layer')
w()
w('### A.1 One die, end to end')
w()
block([
 "# the whole chain, signal-window convention",
 "cd %s" % A.batch,
 "./run_die_sig.sh %s %s %s %s %s" % (RUN, os.path.basename(E.chain['Dataset'].rsplit('/', 2)[-2])
                                      if E.chain else 'FOLDER', DIE, E.ptc['GainHalf'], LOT),
 "",
 "# a batch, four at a time, from a list of 'RUN FOLDER DIE GAIN [LOT]' lines",
 "./run_batch_sig.sh 4 dies_all26.txt",
 "",
 "# only the report and the PDF, after a change to the report text",
 "./rebuild_reports.sh 4",
 "",
 "# the cross-die summaries",
 "./run_summary.sh dies_all.txt            # default window, 16 die-runs",
 "./run_summary.sh dies_all26.txt _sig     # signal window, 26 die-runs",
 "",
 "# the deck, the README and this document",
 "SCR=%s" % HERE,
 "python3 $SCR/desy_slides_wis.py",
 "python3 $SCR/desy_reports_readme.py",
 "python3 $SCR/desy_pipeline_doc.py",
])
w('### A.2 The json key inventory')
w()
w("""Every key of every main dump of the example die, generated by walking the
files. `array[n]` is a per-step or per-column vector, `struct(n)` a sub-structure
not expanded here.""")
w()
for _nm in ('dark', 'light', 'badcol', 'ptc', 'budget', 'methods', 'both'):
    _d = getattr(E, _nm)
    if _d is None:
        continue
    _ks = jkeys(_d, depth=2)
    w(f"**`{Die.NAMES[_nm]}`** -- {len(_ks)} keys at depth 2")
    w()
    w('```')
    for _k, _t in _ks:
        w('%-54s %s' % (_k, _t))
    w('```')
    w()
w('### A.3 Reading a binary map back')
w()
block(["% MATLAB",
       "fid = fopen('dc.bin');  DC = fread(fid, [%d %d], 'single');  fclose(fid);" % (NY, NX),
       "",
       "# python",
       "import numpy as np",
       "DC = np.fromfile('dc.bin', dtype=np.float32).reshape(%d, %d, order='F')" % (NY, NX)])
w(f"""Column-major, no header, `single` unless the table of section 2.5 says
otherwise. The shape is in every json as `Size`.""")
w()
w('### A.4 Rebuilding this document')
w()
block(["python3 %s/desy_pipeline_doc.py \\" % HERE,
       "    --example %s --exdef %s \\" % (A.example, A.exdef),
       "    --contrast %s --conref %s" % (A.contrast, A.conref),
       "",
       "# -> %s/pipeline.md, pipeline.html" % A.out,
       "# -> %s" % A.pdf])
w(f"""Every number in it was read from the dumps at build time, so re-running it
after any change to the analysis updates it. Nothing in it is typed in by hand,
which is the only way a document this long stays true. Written by
`desy_pipeline_doc.py` ({srclines('desy_pipeline_doc.py')} lines).""")
w()

# ---------------------------------------------------------------- emit
md = '\n'.join(MD) + '\n'
with open(os.path.join(A.out, 'pipeline.md'), 'w') as fh:
    fh.write(md)

HTML = """<!DOCTYPE html>
<html><head><meta charset="utf-8"><title>__TITLE__</title>
<style>
body{max-width:1100px;margin:2rem auto;padding:0 1rem;font:15px/1.6 -apple-system,Segoe UI,Roboto,sans-serif;color:#222}
h1{border-bottom:2px solid #ddd;padding-bottom:.3rem}
h2{margin-top:2.4rem;border-bottom:1px solid #eee}
h3{margin-top:1.6rem}
table{border-collapse:collapse;font-size:13px;margin:1rem 0}
th,td{border:1px solid #ddd;padding:3px 8px;text-align:left;vertical-align:top}
th{background:#f5f5f5}
em{color:#666;font-size:13px}
table em,table strong{font-size:inherit;color:inherit}
code{background:#f5f5f5;padding:1px 4px}
pre{background:#f7f7f7;padding:.6rem;overflow-x:auto;font-size:12.5px}
@page{size:A4 portrait;margin:12mm 10mm}
@media print{
  body{max-width:none;margin:0;padding:0;font-size:11.5px}
  table{display:table;width:100%;overflow:visible;font-size:10px;margin:.5rem 0}
  th,td{white-space:normal;overflow-wrap:anywhere;padding:2px 4px;line-height:1.2}
  h1,h2,h3{page-break-after:avoid}
  p,li{orphans:2;widows:2}
  pre{font-size:9px;page-break-inside:avoid}
}
</style></head><body>
<div id="c"></div>
<script type="text/markdown" id="src">
__MD__
</script>
<script src="https://cdnjs.cloudflare.com/ajax/libs/marked/9.1.6/marked.min.js"></script>
<script>
document.getElementById('c').innerHTML =
  marked.parse(document.getElementById('src').textContent);
</script>
</body></html>"""
with open(os.path.join(A.out, 'pipeline.html'), 'w') as fh:
    fh.write(HTML.replace('__MD__', md.replace('</script', '<\\/script'))
                 .replace('__TITLE__', 'The DESY wafer-test analysis pipeline'))
print('pipeline.md / pipeline.html -> %s  (%d lines, %d words)'
      % (A.out, md.count('\n'), len(md.split())))

if not A.no_pdf:
    subprocess.run(['/snap/bin/chromium', '--headless', '--disable-gpu', '--no-sandbox',
                    '--virtual-time-budget=30000', '--print-to-pdf=' + A.pdf,
                    'file://' + os.path.join(A.out, 'pipeline.html')],
                   capture_output=True)
    if os.path.isfile(A.pdf) and os.path.getsize(A.pdf) > 0:
        print('PDF -> %s (%.1f MB)' % (A.pdf, os.path.getsize(A.pdf)/1e6))
    else:
        print('PDF FAILED')
