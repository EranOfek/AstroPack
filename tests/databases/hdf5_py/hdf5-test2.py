# https://manual.nexusformat.org/examples/h5py/

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

f.close()   # be CERTAIN to close the file

print("wrote file:", fileName)




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




with h5py.File("prj_test.nexus.hdf5", "r") as nx:
    # find the default NXentry group
    nx_entry = nx[nx.attrs["default"]]
    # find the default NXdata group
    nx_data = nx_entry[nx_entry.attrs["default"]]
    # find the signal field
    signal = nx_data[nx_data.attrs["signal"]]
    # find the axes field(s)
    attr_axes = nx_data.attrs["axes"]
    if isinstance(attr_axes, (set, tuple, list)):
        #  but check that attr_axes only describes 1-D data
        if len(attr_axes) == 1:
            attr_axes = attr_axes[0]
        else:
            raise ValueError(f"expected 1-D data but @axes={attr_axes}")
    axes = nx_data[attr_axes]

    print(f"file: {nx.filename}")
    print(f"signal: {signal.name}")
    print(f"axes: {axes.name}")
    print(f"{axes.name} {signal.name}")
    for x, y in zip(axes, signal):
        print(x, y)


        