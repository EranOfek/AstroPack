#!/usr/bin/env python3
"""Record what the single-die chain cost on this die.

The report quotes the size of the dataset and the time each stage took. Those
are properties of the run, not of the chain, so they are measured here and
written to chain.json rather than written into the report text, where they would
silently describe whichever die the text was first drafted for.
"""
import argparse, json, os

P = argparse.ArgumentParser()
P.add_argument('--indir',   required=True, help='the die output directory')
P.add_argument('--tag',     required=True)
P.add_argument('--dataset', required=True, help='the device directory that was read')
P.add_argument('--times',   required=True, help='file of "<stage name> <seconds>" lines')
P.add_argument('--scan',    default=None,
               help='";"-separated directories to measure; defaults to --dataset. The DESY '
                    'uc_st_measurement unit is a PAIR of run directories, and --dataset is '
                    'then a label rather than a path, so the two have to be given apart.')
P.add_argument('--source', default='aspect',
               help="'aspect' (the TIFF wafer tester) or 'ucfits' (the DESY "
                    "uc_st_measurement FITS, whose unit is a pair of runs and whose "
                    "light is a spot rather than a flat field)")
P.add_argument('--region', default='',
               help='CCDSEC of the analysed region, "[x1 x2 y1 y2]"; empty means the whole die')
A = P.parse_args()

# the aSpect tester writes TIFFs, the DESY uc_st_measurement pipeline writes FITS
nfile, nbyte = 0, 0
for d in (A.scan if A.scan is not None else A.dataset).split(';'):
    d = d.strip()
    if not os.path.isdir(d):
        continue
    for root, _, files in os.walk(d):
        for f in files:
            if f.lower().endswith(('.tif', '.tiff', '.fits')):
                nfile += 1
                nbyte += os.path.getsize(os.path.join(root, f))

# A stage can appear more than once when only the tail of the chain was re-run;
# the last time it took is the one that describes the dumps now on disk.
seen = {}
order = []
with open(A.times) as fh:
    for line in fh:
        parts = line.split()
        if len(parts) == 2:
            if parts[0] not in seen:
                order.append(parts[0])
            seen[parts[0]] = float(parts[1])
stages = [{'Name': k, 'Seconds': seen[k]} for k in order]

rec = {'Tag': A.tag, 'Dataset': A.dataset, 'Source': A.source, 'Region': A.region,
       'Nfiles': nfile, 'Bytes': nbyte,
       'Stages': stages, 'TotalSeconds': sum(d['Seconds'] for d in stages)}
with open(os.path.join(A.indir, 'chain.json'), 'w') as fh:
    json.dump(rec, fh)
print(f"chain.json: {nfile} frame files, {nbyte/1e9:.1f} GB, {len(stages)} stages, "
      f"{rec['TotalSeconds']/60:.0f} min")
