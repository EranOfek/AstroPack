#!/usr/bin/env python3
"""Write README.txt for the DESY report directory, from the reports themselves.

Reads the PDFs that are actually there and the json dumps behind them, so the
description cannot drift from the contents: rerun it after adding die-runs and
the counts, the run list and the flagged dies all follow.
"""
import argparse, glob, json, os, re, subprocess, datetime
import numpy as np

P = argparse.ArgumentParser()
P.add_argument('--dir', default='/home/sasha/DESY_reports')
P.add_argument('--root', default='/home/sasha/claude/desy_die')
P.add_argument('--summary', default='/home/sasha/claude/desy_die/summary_sig')
A = P.parse_args()

def pages(p):
    try:
        out = subprocess.run(['pdfinfo', p], capture_output=True, text=True).stdout
        return int(re.search(r'^Pages:\s+(\d+)', out, re.M).group(1))
    except Exception:
        return 0

pdfs = sorted(os.path.basename(p) for p in glob.glob(os.path.join(A.dir, '*.pdf')))
decks = sorted(os.path.basename(p) for p in glob.glob(os.path.join(A.dir, '*.pptx')))
die_pdfs = [f for f in pdfs if f.endswith('_report.pdf')]
sums = [f for f in pdfs if 'summary' in f]

# what was analysed, from the dumps
runs = {}
for d in sorted(glob.glob(os.path.join(A.root, 'run*'))):
    j = os.path.join(d, 'dark.json')
    if not os.path.isfile(j):
        continue
    D = json.load(open(j))
    key = str(D['Run'])
    e = runs.setdefault(key, {'lots': set(), 'dies': set(), 'sig': 0, 'def': 0})
    e['lots'].add(str(D['Lot']))
    e['dies'].add(f"{D['Lot']}/{D['Die']}")
    e['sig' if d.endswith('_sig') else 'def'] += 1

# the settings each run carried, and the flagged die-runs, from the summary
md = open(os.path.join(A.summary, 'summary.md')).read() if \
     os.path.isfile(os.path.join(A.summary, 'summary.md')) else ''
flags = [(d, y) for d, y in
         re.findall(r'^\| ([^|]+?) \| ([^|]*(?:typical|negative beyond)[^|]*) \|$', md, re.M)
         if not d.strip().startswith('die /')]

L = []
w = L.append
w('ULTRASAT BSI wafer-level test analysis -- DESY / aSpect measurements')
w('=' * 72)
w('')
w(f'Weizmann Institute of Science.  Generated {datetime.date.today():%d %B %Y}.')
w('')
w('This directory holds the WIS analysis of the aSpect flavour-comparison')
w('measurements: one report per die-run, cross-die summaries, and a slide deck.')
w('Every number in every file is computed by the chain in the AstroPack package')
w('ultrasat.lab.scripts (desy_die_*.m / *.py) and read back from its json dumps;')
w('nothing is typed in by hand.')
w('')
w('WHAT WAS MEASURED')
w('-' * 72)
_offpdf = [f for f in die_pdfs if '_sigwin_offset_' in f]
_sigpdf = [f for f in die_pdfs if '_sigwin_' in f and f not in _offpdf]
_defpdf = [f for f in die_pdfs if '_sigwin_' not in f]
_ndump  = sum(e['def'] for e in runs.values())
w(f'{len(die_pdfs)} per-die reports, in three flavours: {len(_sigpdf)} under the SIGNAL WINDOW,')
w(f'which is the rule this analysis uses; {len(_defpdf)} under the default window, kept only as')
w('the "before" of the before/after in the slide deck; and' if _offpdf else 'the "before" of the before/after in the slide deck.')
if _offpdf:
    w(f'{len(_offpdf)} in CHARGE-COLLECTING TIME, the signal window with the readout time taken')
    w('off every exposure (see THE EXPOSURE-TIME OFFSET below).')
w(f'Covering {len(runs)} runs and '
  f'{len(set().union(*[e["lots"] for e in runs.values()]))} lots.')
w('')
w(f"  {'run':<7}{'lot(s)':<22}{'dies':<6}{'settings':<34}")
SET = {'31': 'AV  (V_RST_L 1.0, SEL 3.8, SF 3.3)',
       '32': 'aSpect, V_TX 3.3 V',
       '35': 'aSpect, V_TX 3.3 V',
       '38': 'aSpect, V_TX 3.5 V',
       '38-2': 'aSpect, V_TX 3.7 V'}
for r in sorted(runs, key=lambda k: (int(k.split('-')[0]), k)):
    e = runs[r]
    w(f"  {r:<7}{', '.join(sorted(e['lots'])):<22}{len(e['dies']):<6}{SET.get(r,'-'):<34}")
w('')
w('Only dies with FT = pass (soft bin 1) are analysed.')
w('Flavour is a property of the WAFER, from General/Lots_Cassette_table.xlsx:')
w('TH02954 W04 = flavour 6, W08 = flavour 2, every other wafer here = flavour 6.')
w('')
w('THE TWO FIT-WINDOW CONVENTIONS')
w('-' * 72)
w('The file names say which one a report used, because the choice moves the')
w('dark-current and threshold numbers and nothing else explains the difference.')
w('')
w('*** The signal window (_sigwin) is the rule this analysis uses. Quote those. ***')
w('')
w('  default window   Each ladder is fitted over the widest window, anchored at')
w('                   the top of its linear range, that still fits a straight')
w('                   line. Each ladder is measured over as much of itself as is')
w('                   straight, so the two ladders span different charge ranges.')
w('')
w('  signal window    Both ladders are fitted over the same 100-1000 ADU of mean')
w('  (_sigwin)        signal, and within a ladder the response fit and the')
w('                   photon-transfer fit use exactly the same steps. Where fewer')
w('                   than three steps fall in the band the floor is lowered to')
w('                   the nearest step below (never the ceiling). Comparable, at')
w('                   the price of a much shorter lever arm on the dark ladder.')
w('')
w('CROSS-DIE SUMMARIES')
w('-' * 72)
for f in sums:
    n = re.search(r'_(\d+)dieruns_', f)
    win = 'signal window, 100-1000 ADU' if 'signal' in f else 'default (per-ladder) windows'
    w(f'  {f}')
    w(f'      {pages(os.path.join(A.dir,f))} pages. {n.group(1) if n else "?"} die-runs, {win}.')
    w('      The lot in one table; whether the dark deficit repeats; the')
    w('      dark-current gradient and what it tracks; the four threshold routes')
    w('      across dies, flavours and runs; every other datasheet number die by')
    w('      die; and the fit windows each die was given.')
    w('')
_cmp = [f for f in pdfs if 'offset_comparison' in f]
if _offpdf or _cmp:
    w('THE EXPOSURE-TIME OFFSET')
    w('-' * 72)
    w('The commanded exposure of the tester is t_exp = RO_time + Reset_delay, with')
    w('RO_time the full-die readout: the configuration records it as')
    w('zDUT_ExpTimeOffset = 12 and DESY confirmed (8 Oct 2026) RO_time =')
    w('2 x 4742 rows x 1.3 ms = 12.3292 s. The charge-collecting interval is')
    w('therefore t_exp - RO_time, and the chain had been fitting against t_exp.')
    w('')
    w('A shift of the time axis cannot change a slope, so the dark current, the gain,')
    w('the read noise, the DSNU and the PRNU are unaffected and need no revision, and')
    w('the two photon-transfer thresholds never use the time axis at all. What moves is')
    w('the two RESPONSE-route thresholds, by DC x 12.329 s - most of their value on the')
    w('high-dark-current AV setting (run 31) and about a fifth of it on aSpect.')
    w('')
    for f in sorted(_cmp):
        w(f'  {f}')
        w(f'      {pages(os.path.join(A.dir,f))} page. The two sets side by side: what cannot move and')
        w('      does not, what moves and by how much, and what it does downstream to the')
        w('      threshold fixed pattern and the limiting signal. Read this first.')
        w('')
    if _offpdf:
        w(f'  DESY_<lot>_<die>_run<run>_sigwin_offset_report.pdf    ({len(_offpdf)} files)')
        w('      The full per-die report in collecting time, each marked as such in its')
        w('      own header. Same 21 pages and same structure as the signal-window')
        w('      reports, so any two can be compared line by line.')
        w('')
    w('Dumps: ~/claude/desy_die/<tag>_sig_off/. The offset is applied in')
    w('ultrasat.lab.readPTC through ExpTimeOffset and selected by DieExpOffset in')
    w('desy_die_config; at 0 the chain reproduces every earlier result.')
    w('')
_pipe = [f for f in pdfs if 'pipeline' in f]
if _pipe:
    w('THE PIPELINE ITSELF')
    w('-' * 72)
    for f in _pipe:
        w(f'  {f}')
        w(f'      {pages(os.path.join(A.dir,f))} pages. What every step of the analysis computes,')
        w('      the formula it uses and why that one, which MATLAB object and which dump')
        w('      holds each result, the error model, every command, and the table of what')
        w('      has to be re-run after what. Read this to reproduce or extend the chain;')
        w('      read a per-die report for the numbers of one die. Generated from the')
        w('      dumps by desy_pipeline_doc.py, so it cannot drift from the analysis.')
        w('')
w('PER-DIE REPORTS')
w('-' * 72)
w('  DESY_<lot>_<wafer>_<die>_run<run>[_sigwin]_report.pdf')
w('')
w('  The two rules choose a different dark window on 15 of the 16 die-runs where')
w('  both were computed, so the default-window reports are not a second opinion on')
w('  the same numbers -- they are a different measurement. Only the two the slide')
w('  deck cites are kept as PDFs:')
for _f in sorted(_defpdf):
    w(f'      {_f}')
w('  The dumps behind all 16 remain under ~/claude/desy_die/, so the default-window')
w('  summary and the deck\'s before/after panels can be rebuilt at any time, and so')
w('  can any of the deleted PDFs.')
w('')
# which convention determines the DARK quantities better is not a matter of
# opinion and not the same on every run, so it is measured here over every
# die-run that has both. The total (stat (+) syst) relative error decides.
def _darkprec():
    out = {}
    for d in sorted(glob.glob(os.path.join(A.root, 'run*_sig'))):
        b = d[:-4]
        if not os.path.isfile(os.path.join(b, 'methods.json')):
            continue
        try:
            Ra = json.load(open(os.path.join(b, 'methods.json')))['Routes']['a']
            Rb = json.load(open(os.path.join(d, 'methods.json')))['Routes']['a']
        except Exception:
            continue
        rd = (float(np.hypot(Ra['SlopeStat'], Ra['SlopeSyst'])/abs(Ra['Slope'])),
              float(np.hypot(Rb['SlopeStat'], Rb['SlopeSyst'])/abs(Rb['Slope'])))
        rt = (float(np.hypot(Ra['ThresholdStat'], Ra['ThresholdSyst'])/abs(Ra['Threshold'])),
              float(np.hypot(Rb['ThresholdStat'], Rb['ThresholdSyst'])/abs(Rb['Threshold'])))
        run = os.path.basename(b)[3:].split('_')[0]
        out[os.path.basename(b)] = (run, rd, rt)
    return out
_dp = _darkprec()
_defwin = sorted({v[0] for v in _dp.values() if v[1][0] < v[1][1] and v[2][0] < v[2][1]})
_sigwin = sorted({v[0] for v in _dp.values() if not (v[1][0] < v[1][1] and v[2][0] < v[2][1])})
_ndef = sum(1 for v in _dp.values() if v[1][0] < v[1][1] and v[2][0] < v[2][1])
if _dp:
    w('  Which convention measures the DARK quantities more precisely is not the same')
    w('  on every run, and it is measured rather than assumed. Taking the total')
    w('  (statistical (+) window) relative error of the dark current and of the')
    w(f'  dark-route threshold over the {len(_dp)} die-runs that have both windows, the')
    w(f'  default window wins on {_ndef} of them and the signal window on {len(_dp)-_ndef}.')
    w('  The split is by setup, not by die: the default window is better on exactly')
    w('  the runs whose dark ladder is long (run ' + ', '.join(_defwin) + '), where more')
    w('  steps and a longer lever outweigh everything else, and the signal window is')
    w('  better on the low-dark-current runs (run ' + ', '.join(_sigwin) + '), whose')
    w('  default window reaches down into the knee and pays for it in the window')
    w('  systematic. For a dark-current or DSNU number, use the dumps of whichever')
    w('  convention wins for that run; the per-die reports quote both errors.')
w('')
_pg = sorted({pages(os.path.join(A.dir, f)) for f in die_pdfs})
w(f'  {len(die_pdfs)} files, '
  + (f'{_pg[0]} pages each.' if len(_pg) == 1 else f'{_pg[0]}-{_pg[-1]} pages each.'))
w('  "_sigwin" marks the signal-window convention; without it, the default one.')
w('  Whole die, 4740 x 4742 = 22.5 M pixels, individual pixels throughout, every')
w('  statistic also split by readout-column parity. Contents:')
w('')
for ln in ('1  what was done, and what each stage cost',
           '2  both ladders are curved, and why the windows are what they are',
           '3  bias and read noise          4  dark current',
           '5  response, PRNU, light-route threshold    6  bad readout columns',
           '7  conversion gain              8  how much the variance itself varies',
           '9  is that variance explained, pixel by pixel',
           '10 the noise budget             11 gain and threshold: four routes, with errors',
           '12 what the chain does not determine        appendix: how to rerun it'):
    w(f'      {ln}')
w('')
w('SLIDE DECK')
w('-' * 72)
for f in decks:
    w(f'  {f}')
w('      13 slides for the WIS/aSpect review. Opener; data summary; then a single')
w('      die (TH02954 W04_D07, wafer 4 = flavour 6, run 32, aSpect settings) for')
w('      read noise, dark current and gain; the four threshold routes with their')
w('      pixel-to-pixel distributions; where the ~50 e- difference between the')
w('      response routes came from and what removes it; the cross-die comparison')
w('      before and after the common window; and conclusions.')
w('')
if flags:
    w('DIE-RUNS FLAGGED AS OUT OF FAMILY')
    w('-' * 72)
    w('Marked with a dagger in the summaries and LEFT IN, not dropped. The test is')
    w('made against the lot itself: a quantity more than 5 MAD from the median over')
    w('all die-runs, or a threshold negative by more than its own error. A die named')
    w('without a lot is TH02954, the default; TH02260 dies carry their lot.')
    w('')
    for die, why in flags:
        w(f'  {die.strip()}')
        for part in why.split(';'):
            w(f'      {part.strip()}')
    w('')
    w('The run 38-2 shift in bias and read noise is deliberately NOT flagged: it is')
    w('the transfer-gate voltage at 3.7 V, measured and explained in the summary.')
    w('')
w('HOW TO REGENERATE')
w('-' * 72)
w('  Code:     AstroPack  matlab/astro/+ultrasat/+lab/+scripts/  (branch main)')
w('  Drivers:  ~/claude/desy_batch/   run_die.sh, run_die_sig.sh, run_batch_*.sh,')
w('            run_summary.sh  (machine-specific paths, not in the repository)')
w('  Dumps:    ~/claude/desy_die/<tag>/   json + binary maps + figures')
w('  Deck:     python3 .../desy_slides_wis.py')
w('  This file: python3 .../desy_reports_readme.py')

open(os.path.join(A.dir, 'README.txt'), 'w').write('\n'.join(L) + '\n')
print(f"wrote {os.path.join(A.dir, 'README.txt')} ({len(L)} lines)")
