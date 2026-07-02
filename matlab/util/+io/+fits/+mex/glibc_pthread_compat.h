// glibc 2.34 merged libpthread into libc and re-versioned several pthread_*
// symbols under GLIBC_2.34. Statically linking libcfitsio.a built on a
// system with glibc >= 2.34 (e.g. Ubuntu 22.04) embeds that requirement
// directly into the mex file, so it fails to load on older systems where
// only the GLIBC_2.2.5-versioned symbols exist (e.g. Ubuntu 20.04, glibc
// 2.31):
//   "GLIBC_2.34 not found (required by .../read_image.mexa64)"
//
// Fix: define our own pthread_mutexattr_init/settype in this translation
// unit that forward to the older, ABI-identical GLIBC_2.2.5 symbol. Since
// these become strong definitions inside the mex .so itself, CFITSIO's
// statically-linked undefined references to them resolve locally at link
// time and never reach the dynamic symbol table, so no GLIBC_2.34
// requirement is recorded. Hidden visibility keeps them from leaking into
// (and interposing on) other libraries loaded in the same MATLAB process.
#pragma once

#if defined(__linux__) && defined(__GLIBC__)
#include <pthread.h>

extern "C" {

__asm__(".symver compat_pthread_mutexattr_init,pthread_mutexattr_init@GLIBC_2.2.5");
extern int compat_pthread_mutexattr_init(pthread_mutexattr_t *attr);

__asm__(".symver compat_pthread_mutexattr_settype,pthread_mutexattr_settype@GLIBC_2.2.5");
extern int compat_pthread_mutexattr_settype(pthread_mutexattr_t *attr, int type);

__attribute__((visibility("hidden")))
int pthread_mutexattr_init(pthread_mutexattr_t *attr) {
    return compat_pthread_mutexattr_init(attr);
}

__attribute__((visibility("hidden")))
int pthread_mutexattr_settype(pthread_mutexattr_t *attr, int type) {
    return compat_pthread_mutexattr_settype(attr, type);
}

}  // extern "C"
#endif  // __linux__ && __GLIBC__
