# io.blosc2 - read Blosc2 image files

`io.blosc2.read1` reads a 2-D float32 image stored as a Blosc2 NDArray file (`.b2nd`), together with its FITS header.

Blosc2 packs an image losslessly, block by block: byte reordering ("shuffle"), then `zstd` or `lz4`. Blocks that are
all zero take almost no space, and unpacking is fast (about 5 ms for a 1726 x 1726 image).

```matlab
[Image, Header] = io.blosc2.read1('image.b2nd');               % Header: 3-column cell {key, value, comment}
[Image, Text]   = io.blosc2.read1('image.b2nd', 'HeaderType','char');
```

By default the whole file is read with `fread` and then unpacked (`'ReadWhole',true`). Over the network this is
faster than letting the library read the file in many small pieces; from a local disk both are about the same.

## Writing (Python)

MATLAB has no Blosc2 writer; files are written with python-blosc2 (`pip install blosc2`):

```python
import blosc2
from astropy.io import fits
Image = fits.getdata('image.fits').astype('<f4')
Header = fits.getheader('image.fits')
a = blosc2.asarray(Image, chunks=(256, Image.shape[1]), blocks=(32, Image.shape[1]), urlpath='image.b2nd', mode='w',
                   cparams=blosc2.CParams(codec=blosc2.Codec.ZSTD, clevel=1, filters=[blosc2.Filter.SHUFFLE]))
a.vlmeta['FITS_HEADER'] = Header.tostring()
```

## Building the MEX file

`+mex/mex_blosc2_read.mexa64` is linked statically with the C-Blosc2 library (BSD-3 license), so it needs nothing at
run time. To rebuild it, point `io.blosc2.mex.build` to a directory holding `include/b2nd.h`, `include/blosc2.h` and
`lib/libblosc2.a`; the python-blosc2 package has them:

```matlab
io.blosc2.mex.build('<venv>/lib/python3.12/site-packages/blosc2')
```

Notes:
- Only 2-D float32 arrays are supported.
- The MEX file locks itself in memory once used (`mexLock`): C-Blosc2 creates per-thread data whose destructor lives
  in the MEX file, and MATLAB crashed at exit when the file had been unloaded.
