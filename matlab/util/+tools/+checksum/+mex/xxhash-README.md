# xxHash - a super-fast hash algorithm in a single C++ header

The xxHash algorithm is designed to provide a high level of performance on modern 
processors and can generate hash values at speeds of several GB/s per core. 
It also has a low memory footprint, which means that it can be used efficiently 
in applications that process large amounts of data.

The xxHash algorithm is suitable for detecting duplicates and can be used to 
compare files based on their content. However, it is not suitable for detecting 
data corruption or malicious tampering, as it is not a cryptographic hash function.

See: https://github.com/Cyan4973/xxHash

## Simple implementation

https://create.stephan-brumme.com/xxhash/#git1


This is an implementation of Yann Collet's xxHash32 and xxHash64 algorithms (https://github.com/Cyan4973/xxHash).
Just include the short [xxhash32.h](xxhash32.h) or [xxhash64.h](xxhash64.h) header - that's it, no external dependencies !

Performance of my library is usually on par with his original code.
His code can be hard to understand due to the massive use of #ifdef, 
therefore I posted a detailled explanation of the algorithm on my website https://create.stephan-brumme.com/xxhash/
(which is also the main repository; GitHub serves as a mirror only).
