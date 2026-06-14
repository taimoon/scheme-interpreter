#pragma once

#ifndef SCM_HOSTED
#error SCM_HOSTED is undefined
#endif

#ifndef SCM_UTF32
#error SCM_UTF32 is undefined
#endif

#ifndef SCM_HOSTED
#define SCM_HOSTED 1
#endif
#ifndef SCM_UTF32
#define SCM_UTF32 0
#endif

#define SCM_FREESTANDING    (!SCM_HOSTED)

#if SCM_HOSTED
    #define SCM_RAW_FFI 1
#else
    #define SCM_RAW_FFI 0
#endif

#ifdef PICO_BOARD
#if defined(__STDC_HOSTED__) && __STDC_HOSTED__ == 0
#error "PICO_BOARD requires a freestanding (non-hosted) C environment"
#endif
#endif

#include <stdint.h>
#if UINTPTR_MAX == UINT32_MAX
#define SCM_NATIVE_WIDTH    32
#elif UINTPTR_MAX == UINT64_MAX
#define SCM_NATIVE_WIDTH    64
#else
#error "Unknown native width"
#endif
