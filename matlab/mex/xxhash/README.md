# xxHash

https://github.com/Cyan4973/xxHash

The xxHash algorithm is designed to provide a high level of performance on modern 
processors and can generate hash values at speeds of several GB/s per core. 
It also has a low memory footprint, which means that it can be used efficiently 
in applications that process large amounts of data.

The xxHash algorithm is suitable for detecting duplicates and can be used to 
compare files based on their content. However, it is not suitable for detecting 
data corruption or malicious tampering, as it is not a cryptographic hash function.


## Python

	import xxhash

	# generate a 32-bit hash of a string
	hash32 = xxhash.xxh32('hello world').hexdigest()

	# generate a 64-bit hash of a file
	with open('myfile.txt', 'rb') as f:
		hash64 = xxhash.xxh64(f.read()).hexdigest()

	print(hash32)
	print(hash64)


