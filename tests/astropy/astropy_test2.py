# https://docs.astropy.org/en/stable/index.html

import numpy as np
import astropy

import numpy as np
from astropy.time import Time
times = ['1999-01-01T00:00:00.123456789', '2010-01-01T00:00:00']
t = Time(times, format='isot', scale='utc')
t

t[1]


