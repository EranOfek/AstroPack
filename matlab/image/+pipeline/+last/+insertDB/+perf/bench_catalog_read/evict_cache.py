#!/usr/bin/env python3
"""Drop the page-cache pages of files (posix_fadvise DONTNEED; no root needed).

usage: evict_cache.py DIR [GLOB]      default glob: LAST*proc_Cat_1.fits
Standalone (no imports from this folder) so MATLAB can call it via system().
"""
import glob
import os
import sys

d = sys.argv[1]
pat = sys.argv[2] if len(sys.argv) > 2 else "LAST*proc_Cat_1.fits"
n = 0
for p in glob.glob(os.path.join(d, pat)):
    fd = os.open(p, os.O_RDONLY)
    try:
        os.posix_fadvise(fd, 0, 0, os.POSIX_FADV_DONTNEED)
    finally:
        os.close(fd)
    n += 1
print(n)
