import math
import numpy as np


MAB0 = -2.5 * np.log10(3631.e-23)  # AB Magnitude zero point
pc_cm = 3.08568025e18  # parsec in cm


def f_nu_from_magAB(magAB):
    f_nu = 10.**((magAB + MAB0)/(-2.5))
    return f_nu


def L_nu_from_magAB(magAB):
    const = 4. * math.pi * (10. * pc_cm) ** 2.
    L_nu = const * 10. ** ((magAB + MAB0) / (-2.5))
    return L_nu


# Based on http://roban.github.com/CosmoloPy/:
#
# The MIT License
#
# Copyright (c) 2009-2010 Roban Hultman Kramer and contributors
# http://roban.github.com/CosmoloPy/
#
# Permission is hereby granted, free of charge, to any person obtaining a copy
# of this software and associated documentation files (the "Software"), to deal
# in the Software without restriction, including without limitation the rights
# to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
# copies of the Software, and to permit persons to whom the Software is
# furnished to do so, subject to the following conditions:
#
# The above copyright notice and this permission notice shall be included in
# all copies or substantial portions of the Software.
#
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
# OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
# THE SOFTWARE.
