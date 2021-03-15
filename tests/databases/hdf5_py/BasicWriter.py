#!/usr/bin/env python
'''Writes a NeXus HDF5 file using h5py and numpy'''

import h5py    # HDF5 support
import numpy
import six

print("Write a NeXus HDF5 file")
fileName = u"prj_test.nexus.hdf5"
timestamp = u"2010-10-18T17:17:04-0500"

# load data from two column format
data = numpy.loadtxt(u"input.dat").T
mr_arr = data[0]
i00_arr = numpy.asarray(data[1],'int32')

# create the HDF5 NeXus file
f = h5py.File(fileName, "w")
# point to the default data to be plotted
f.attrs[u'default']          = u'entry'
# give the HDF5 root some more attributes
f.attrs[u'file_name']        = fileName
f.attrs[u'file_time']        = timestamp
f.attrs[u'instrument']       = u'APS USAXS at 32ID-B'
f.attrs[u'creator']          = u'BasicWriter.py'
f.attrs[u'NeXus_version']    = u'4.3.0'
f.attrs[u'HDF5_Version']     = six.u(h5py.version.hdf5_version)
f.attrs[u'h5py_version']     = six.u(h5py.version.version)

# create the NXentry group
nxentry = f.create_group(u'entry')
nxentry.attrs[u'NX_class'] = u'NXentry'
nxentry.attrs[u'default'] = u'mr_scan'
nxentry.create_dataset(u'title', data=u'1-D scan of I00 v. mr')

# create the NXentry group
nxdata = nxentry.create_group(u'mr_scan')
nxdata.attrs[u'NX_class'] = u'NXdata'
nxdata.attrs[u'signal'] = u'I00'      # Y axis of default plot
nxdata.attrs[u'axes'] = u'mr'         # X axis of default plot
nxdata.attrs[u'mr_indices'] = [0,]   # use "mr" as the first dimension of I00

# X axis data
ds = nxdata.create_dataset(u'mr', data=mr_arr)
ds.attrs[u'units'] = u'degrees'
ds.attrs[u'long_name'] = u'USAXS mr (degrees)'    # suggested X axis plot label

# Y axis data
ds = nxdata.create_dataset(u'I00', data=i00_arr)
ds.attrs[u'units'] = u'counts'
ds.attrs[u'long_name'] = u'USAXS I00 (counts)'    # suggested Y axis plot label

f.close()	# be CERTAIN to close the file

print("wrote file:", fileName)
