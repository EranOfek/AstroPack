"""Reader implementations + page-cache tools for LAST proc_Cat FITS benchmarks.

Every reader takes a list of paths and returns a list of C-contiguous native
float64 arrays shaped (Nrows, Ncols) -- one per file -- i.e. the same payload
production gets from ``AstroCatalog(..., 'UseMex', true)`` (a 514x43 double matrix
per file), minus MATLAB's column-major layout.
"""

from __future__ import annotations

import mmap
import os
from concurrent.futures import ThreadPoolExecutor
from typing import Callable, Dict, List, Sequence

import numpy as np

BLOCK = 2880


# --------------------------------------------------------------------------- page cache
def evict(paths: Sequence[str]) -> None:
    """Drop clean pages of each file from the page cache (no root needed)."""
    for p in paths:
        fd = os.open(p, os.O_RDONLY)
        try:
            os.posix_fadvise(fd, 0, 0, os.POSIX_FADV_DONTNEED)
        finally:
            os.close(fd)



# --------------------------------------------------------------------------- FITS parsing
def _card_int(hdr: bytes, key: str) -> int:
    """Integer value of an 80-byte FITS card (key padded to 8 chars, aligned to 80)."""
    pat = key.ljust(8).encode() + b"="
    i = hdr.find(pat)
    while i >= 0 and i % 80:
        i = hdr.find(pat, i + 1)
    if i < 0:
        raise KeyError(key)
    return int(hdr[i + 10:i + 80].split(b"/")[0])


def _header_end(buf, off: int) -> int:
    """Offset just past the header (END card) that starts at ``off``."""
    while True:
        blk = bytes(buf[off:off + BLOCK])
        if len(blk) < BLOCK:
            raise ValueError("truncated FITS header")
        off += BLOCK
        for i in range(0, BLOCK, 80):
            if blk[i:i + 8] == b"END     ":
                return off


def _data_size(hdr: bytes) -> int:
    """Bytes of data (padded to 2880) described by an HDU header."""
    nax = _card_int(hdr, "NAXIS")
    if nax == 0:
        return 0
    n = 1
    for i in range(1, nax + 1):
        n *= _card_int(hdr, f"NAXIS{i}")
    nbytes = n * abs(_card_int(hdr, "BITPIX")) // 8
    return -(-nbytes // BLOCK) * BLOCK


def locate_table(buf):
    """Return (data_offset, nrows, ncols) of the first BINTABLE with all-'1D' columns."""
    off = 0
    while off < len(buf):
        end = _header_end(buf, off)
        hdr = bytes(buf[off:end])
        if hdr.startswith(b"XTENSION= 'BINTABLE'"):
            nrows = _card_int(hdr, "NAXIS2")
            width = _card_int(hdr, "NAXIS1")
            ncols = _card_int(hdr, "TFIELDS")
            if width != 8 * ncols or hdr.count(b"'1D      '") != ncols:
                raise ValueError("table is not all-1D (fast reader only handles double columns)")
            return end, nrows, ncols
        off = end + _data_size(hdr)
    raise ValueError("no BINTABLE HDU")


# --------------------------------------------------------------------------- readers
def read_raw(path: str):
    """I/O ceiling: read all bytes, no parsing."""
    with open(path, "rb", buffering=0) as f:
        return f.read()


def read_numpy(path: str) -> np.ndarray:
    """Hand-rolled: one read(), parse 2 headers, big-endian frombuffer -> native copy."""
    with open(path, "rb", buffering=0) as f:
        buf = f.read()
    off, nr, nc = locate_table(buf)
    return np.frombuffer(buf, dtype=">f8", count=nr * nc, offset=off).reshape(nr, nc).astype(np.float64)


def read_numpy_mmap(path: str) -> np.ndarray:
    with open(path, "rb") as f:
        mm = mmap.mmap(f.fileno(), 0, prot=mmap.PROT_READ)
        try:
            off, nr, nc = locate_table(mm)
            return np.frombuffer(mm, dtype=">f8", count=nr * nc, offset=off).reshape(nr, nc).astype(np.float64)
        finally:
            # frombuffer(...).astype() is a copy, so releasing the map is safe
            mm.close()


def read_astropy(path: str) -> np.ndarray:
    from astropy.io import fits
    with fits.open(path, memmap=False) as hdul:
        d = hdul[1].data
        return np.ascontiguousarray(np.column_stack([d[n] for n in d.dtype.names]).astype(np.float64))


def read_fitsio(path: str) -> np.ndarray:
    import fitsio
    d = fitsio.read(path, ext=1)
    return np.ascontiguousarray(np.column_stack([d[n] for n in d.dtype.names]).astype(np.float64))


def read_fitsio_unstruct(path: str) -> np.ndarray:
    import fitsio
    from numpy.lib import recfunctions as rfn
    return np.ascontiguousarray(rfn.structured_to_unstructured(fitsio.read(path, ext=1), dtype=np.float64))


READERS: Dict[str, Callable[[str], object]] = {
    "raw": read_raw,
    "numpy": read_numpy,
    "numpy_mmap": read_numpy_mmap,
    "astropy": read_astropy,
    "fitsio": read_fitsio,
    "fitsio_unstruct": read_fitsio_unstruct,
}


# --------------------------------------------------------------------------- drivers
def run_serial(fn, paths: Sequence[str]) -> List:
    return [fn(p) for p in paths]


def run_threads(fn, paths: Sequence[str], nthreads: int) -> List:
    with ThreadPoolExecutor(max_workers=nthreads) as ex:
        return list(ex.map(fn, paths))


def prefetch(paths: Sequence[str]) -> None:
    """Queue async readahead for every file (kernel sorts/merges the requests)."""
    for p in paths:
        fd = os.open(p, os.O_RDONLY)
        try:
            os.posix_fadvise(fd, 0, 0, os.POSIX_FADV_WILLNEED)
        finally:
            os.close(fd)


def run_prefetch_threads(fn, paths: Sequence[str], nthreads: int) -> List:
    prefetch(paths)
    return run_threads(fn, paths, nthreads)
