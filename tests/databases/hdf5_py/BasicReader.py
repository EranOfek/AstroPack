#!/usr/bin/env python
'''Reads NeXus HDF5 files using h5py and prints the contents'''

import h5py    # HDF5 support

fileName = "prj_test.nexus.hdf5"
f = h5py.File(fileName,  "r")
for item in f.attrs.keys():
    print(item + ":", f.attrs[item])
mr = f['/entry/mr_scan/mr']
i00 = f['/entry/mr_scan/I00']
print("%s\t%s\t%s" % ("#", "mr", "I00"))
for i in range(len(mr)):
    print("%d\t%g\t%d" % (i, mr[i], i00[i]))
f.close()
