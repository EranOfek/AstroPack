#ifndef mf_defH
#define mf_defH

// Operating System and compiler defines:
//
//      _WIN32          Windows 32/64
//      _LINUX          Linux

#ifdef _WIN32
    // Windows

#else
    // Linux
    #define _LINUX

#endif

//---------------------------------------------------------------------------
// Standard headers
#include <stdlib.h>
#include <stdio.h>
#include <stdint.h>
#include <stdbool.h>
#include <assert.h>
#include <string>

// Alias
typedef unsigned char uchar;

// Assert: Throw if (x == false)
#define Assert(x) assert(x)

#define BZERO(x) { memset(x, 0, sizeof(x)); }

#endif
