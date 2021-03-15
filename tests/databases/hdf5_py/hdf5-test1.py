# https://www.christopherlovell.co.uk/blog/2016/04/27/h5py-intro.html

import numpy as np
import h5py

d1 = np.random.random(size = (1000,20))
d2 = np.random.random(size = (1000,200))


# Creating HDF5 files
hf = h5py.File('data.h5', 'w')
hf.create_dataset('dataset_1', data=d1)
hf.create_dataset('dataset_2', data=d2)
hf.close()


# Reading HDF5 files
hf = h5py.File('data.h5', 'r')
print(hf.keys())
n1 = hf.get('dataset_1')
n1 = np.array(n1)
print(n1.shape)
hf.close()


# Groups
# Groups are the basic container mechanism in a HDF5 file, allowing hierarchical organisation of the data.
# Groups are created similarly to datasets, and datsets are then added using the group object.

d1 = np.random.random(size = (100,33))
d2 = np.random.random(size = (100,333))
d3 = np.random.random(size = (100,3333))

hf = h5py.File('data.h5', 'w')
g1 = hf.create_group('group1')
g1.create_dataset('data1',data=d1)
g1.create_dataset('data2',data=d1)

# We can also create subfolders. Just specify the group name as a directory format.
g2 = hf.create_group('group2/subfolder')
g2.create_dataset('data3',data=d3)

# As before, to read data in irectories and subdirectories use the get method with the full subdirectory path.
group2 = hf.get('group2/subfolder')
print(group2.items())
group1 = hf.get('group1')
print(group1.items())
n1 = group1.get('data1')
print(np.array(n1).shape)
hf.close()

# Compression

# To save on disk space, while sacrificing read speed, you can compress the data.
# Just add the compression argument, which can be either gzip, lzf or szip. gzip is the most portable,
# as it’s available with every HDF5 install, lzf is the fastest but doesn’t compress as effectively as gzip,
# and szip is a NASA format that is patented up; if you don’t know about it, chances are your organisation doesn’t
# have the patent, so avoid.
# For gzip you can also specify the additional compression_opts argument, which sets the compression level.
# The default is 4, but it can be an integer between 0 and 9.

hf = h5py.File('data.h5', 'w')

hf.create_dataset('dataset_1', data=d1, compression="gzip", compression_opts=9)
hf.create_dataset('dataset_2', data=d2, compression="gzip", compression_opts=9)

hf.close()

