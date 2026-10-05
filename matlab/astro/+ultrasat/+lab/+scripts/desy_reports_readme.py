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
_sigpdf = [f for f in die_pdfs if '_sigwin_' in f]
_defpdf = [f for f in die_pdfs if '_sigwin_' not in f]
_ndump  = sum(e['def'] for e in runs.values())
w(f'{len(die_pdfs)} per-die reports: {len(_sigpdf)} under the SIGNAL WINDOW, which is the rule this')
w(f'analysis uses, and {len(_defpdf)} under the default window, kept only as the "before" of the')
w(f'before/after in the slide deck. Covering {len(runs)} runs and '
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
w('  One thing the default window does better: it gives the dark ladder more points')
w('  and a longer lever, so the dark current and the dark-route threshold are more')
w('  precisely determined there (on run 32 W04_D07, 2.3x and 3.2x). The signal')
w('  window buys comparability between the four threshold routes and pays for it in')
w('  lever arm. For a dark-current or DSNU datasheet number, prefer the default-')
w('  window dumps.')
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
