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
A = P.parse_args()

nfile, nbyte = 0, 0
for root, _, files in os.walk(A.dataset):
    for f in files:
        if f.lower().endswith(('.tif', '.tiff')):
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

rec = {'Tag': A.tag, 'Dataset': A.dataset, 'Nfiles': nfile, 'Bytes': nbyte,
       'Stages': stages, 'TotalSeconds': sum(d['Seconds'] for d in stages)}
with open(os.path.join(A.indir, 'chain.json'), 'w') as fh:
    json.dump(rec, fh)
print(f"chain.json: {nfile} TIFF files, {nbyte/1e9:.1f} GB, {len(stages)} stages, "
      f"{rec['TotalSeconds']/60:.0f} min")
