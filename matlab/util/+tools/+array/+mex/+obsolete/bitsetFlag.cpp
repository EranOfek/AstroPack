#include "mex.h"
#include <cstdint>
#include <cstring>
#include <cstddef>
#include <vector>

#if defined(_OPENMP)
  #include <omp.h>
#endif
#if defined(__AVX2__)
  #include <immintrin.h>
#endif

// ===== Branchless scalar kernel (all types) =====
// Out = A | (M & Sel)      when setOne
// Out = A & ~(M & Sel)     when clear
// where Sel = all-ones if F[i], else 0.
template<typename T>
static void bitset_scalar_branchless(const T* __restrict A,
                                     const mxLogical* __restrict F,
                                     T* __restrict Out,
                                     size_t N,
                                     unsigned bit0,  // 0-based
                                     bool setOne)
{
    const T M = T(1) << bit0;
    if (setOne) {
      #if defined(_OPENMP)
      #pragma omp parallel for schedule(static) if(N >= (1u<<18))
      #endif
      for (mwIndex i=0; i<(mwIndex)N; ++i) {
          const T sel = T(0) - T( (unsigned)(F[i]!=0) ); // 0x..FF.. or 0
          Out[i] = A[i] | (M & sel);
      }
    } else {
      const T ALL = ~T(0);
      #if defined(_OPENMP)
      #pragma omp parallel for schedule(static) if(N >= (1u<<18))
      #endif
      for (mwIndex i=0; i<(mwIndex)N; ++i) {
          const T sel = T(0) - T( (unsigned)(F[i]!=0) );
          Out[i] = A[i] & (ALL ^ (M & sel));
      }
    }
}

#if defined(__AVX2__)
// ===== Helpers for AVX2 paths =====
static inline bool is_aligned_32(const void* p){
    return (reinterpret_cast<std::uintptr_t>(p) & 31u) == 0u;
}

// Detect if logical flag array is all zeros or all ones.
// Returns true if uniform; sets all_one accordingly.
static bool flags_all_zero_or_one(const mxLogical* F, size_t N, bool& all_one){
    const __m256i vFF = _mm256_cmpeq_epi8(_mm256_setzero_si256(), _mm256_setzero_si256()); // all-ones
    __m256i vor  = _mm256_setzero_si256();
    __m256i vand = vFF;

    size_t i=0;
    for (; i+32<=N; i+=32){
        __m256i v = _mm256_loadu_si256((const __m256i*)(F+i));
        vor  = _mm256_or_si256(vor, v);
        vand = _mm256_and_si256(vand, v);
    }
    alignas(32) unsigned char tmp_or[32], tmp_and[32];
    _mm256_storeu_si256((__m256i*)tmp_or, vor);
    _mm256_storeu_si256((__m256i*)tmp_and, vand);
    unsigned char acc_or = 0;
    unsigned char acc_and = 0xFF;
    for (int k=0;k<32;++k){ acc_or |= tmp_or[k]; acc_and &= tmp_and[k]; }
    for (; i<N; ++i){ acc_or |= F[i]; acc_and &= F[i]; }

    if (acc_or==0){ all_one=false; return true; }      // all zeros
    if (acc_and==0xFF){ all_one=true;  return true; }  // all ones
    return false;
}

// AVX2 kernel for uint32 (8 lanes), unrolled, optional streaming stores
static void bitset_u32_avx2(const uint32_T* __restrict A,
                            const mxLogical* __restrict F,
                            uint32_T* __restrict Out,
                            size_t N,
                            unsigned bit0,
                            bool setOne)
{
    const __m256i vBit  = _mm256_set1_epi32( int32_T(uint32_T(1u) << bit0) );
    const __m256i vAll  = _mm256_set1_epi32(-1);
    const __m256i vZero = _mm256_setzero_si256();

    const bool big = (N >= (1u<<20)); // heuristic: ~1M elems
    const bool ok_stream = big && is_aligned_32(Out);

    size_t i = 0;
    for (; i + 16 <= N; i += 16)
    {
        // block 0
        __m256i va0 = _mm256_loadu_si256((const __m256i*)(A + i));
        __m128i vf8_0 = _mm_loadl_epi64((const __m128i*)(F + i));         // 8 bytes
        __m256i vf0 = _mm256_cvtepu8_epi32(vf8_0);                         // zero-extend
        __m256i vsel0 = _mm256_cmpgt_epi32(vf0, vZero);                    // 0xFFFFFFFF where flag!=0
        __m256i vmsk0 = _mm256_and_si256(vsel0, vBit);
        __m256i vr0 = setOne ? _mm256_or_si256(va0, vmsk0)
                             : _mm256_and_si256(va0, _mm256_xor_si256(vAll, vmsk0));

        // block 1
        __m256i va1 = _mm256_loadu_si256((const __m256i*)(A + i + 8));
        __m128i vf8_1 = _mm_loadl_epi64((const __m128i*)(F + i + 8));
        __m256i vf1 = _mm256_cvtepu8_epi32(vf8_1);
        __m256i vsel1 = _mm256_cmpgt_epi32(vf1, vZero);
        __m256i vmsk1 = _mm256_and_si256(vsel1, vBit);
        __m256i vr1 = setOne ? _mm256_or_si256(va1, vmsk1)
                             : _mm256_and_si256(va1, _mm256_xor_si256(vAll, vmsk1));

        if (ok_stream) {
            _mm256_stream_si256((__m256i*)(Out + i),     vr0);
            _mm256_stream_si256((__m256i*)(Out + i + 8), vr1);
        } else {
            _mm256_storeu_si256((__m256i*)(Out + i),     vr0);
            _mm256_storeu_si256((__m256i*)(Out + i + 8), vr1);
        }
    }
    if (i + 8 <= N){
        __m256i va = _mm256_loadu_si256((const __m256i*)(A + i));
        __m128i vf8 = _mm_loadl_epi64((const __m128i*)(F + i));
        __m256i vf = _mm256_cvtepu8_epi32(vf8);
        __m256i vsel = _mm256_cmpgt_epi32(vf, vZero);
        __m256i vmsk = _mm256_and_si256(vsel, vBit);
        __m256i vr = setOne ? _mm256_or_si256(va, vmsk)
                            : _mm256_and_si256(va, _mm256_xor_si256(vAll, vmsk));
        if (ok_stream) _mm256_stream_si256((__m256i*)(Out + i), vr);
        else           _mm256_storeu_si256((__m256i*)(Out + i), vr);
        i += 8;
    }
    const uint32_T M   = (uint32_T(1u) << bit0);
    const uint32_T ALL = ~uint32_T(0);
    if (setOne){
        for (; i < N; ++i){
            const uint32_T sel = uint32_T(0) - uint32_T(F[i]!=0);
            Out[i] = A[i] | (M & sel);
        }
    } else {
        for (; i < N; ++i){
            const uint32_T sel = uint32_T(0) - uint32_T(F[i]!=0);
            Out[i] = A[i] & (ALL ^ (M & sel));
        }
    }
    if (ok_stream) _mm_sfence();
}
#endif // __AVX2__

// ===== Dispatcher per class =====
static void run_for_class(const mxArray* Arr, const mxArray* Flag,
                          mxArray* Out, int bit1_based, bool setOne)
{
    const size_t N = mxGetNumberOfElements(Arr);
    if (N==0) return;

    if (bit1_based < 1) mexErrMsgIdAndTxt("bitsetFlag:bit","BitNumber must be >= 1.");
    const mxClassID id = mxGetClassID(Arr);
    const mxLogical* F = mxGetLogicals(Flag);

    switch (id) {
        case mxUINT8_CLASS: {
            if (bit1_based > 8)  mexErrMsgIdAndTxt("bitsetFlag:bit","BitNumber for uint8 in [1,8].");
            const uint8_T* a = (const uint8_T*)mxGetData(Arr);
            uint8_T*       o = (uint8_T*)mxGetData(Out);
            bitset_scalar_branchless<uint8_T>(a, F, o, N, (unsigned)(bit1_based-1), setOne);
        } break;

        case mxUINT16_CLASS: {
            if (bit1_based > 16) mexErrMsgIdAndTxt("bitsetFlag:bit","BitNumber for uint16 in [1,16].");
            const uint16_T* a = (const uint16_T*)mxGetData(Arr);
            uint16_T*       o = (uint16_T*)mxGetData(Out);
            bitset_scalar_branchless<uint16_T>(a, F, o, N, (unsigned)(bit1_based-1), setOne);
        } break;

        case mxUINT32_CLASS: {
            if (bit1_based > 32) mexErrMsgIdAndTxt("bitsetFlag:bit","BitNumber for uint32 in [1,32].");
            const uint32_T* a = (const uint32_T*)mxGetData(Arr);
            uint32_T*       o = (uint32_T*)mxGetData(Out);
            const unsigned bit0 = (unsigned)(bit1_based-1);

            // Uniform flags fast path (big arrays only to amortize scan)
            #if defined(__AVX2__)
            if (N >= (1u<<18)) {
                bool all_one=false;
                if (flags_all_zero_or_one(F, N, all_one)) {
                    if (!all_one){
                        // All flags false -> Out = A (memcpy)
                        std::memcpy(o, a, N*sizeof(uint32_T));
                        return;
                    } else {
                        // All flags true -> uniform OR/AND (no flag loads)
                        const __m256i vM   = _mm256_set1_epi32((int32_T)((uint32_T)1u << bit0));
                        const __m256i vAll = _mm256_set1_epi32(-1);
                        size_t i=0;
                        for (; i+8<=N; i+=8){
                            __m256i va = _mm256_loadu_si256((const __m256i*)(a+i));
                            __m256i vr = setOne ? _mm256_or_si256(va, vM)
                                                : _mm256_and_si256(va, _mm256_xor_si256(vAll, vM));
                            _mm256_storeu_si256((__m256i*)(o+i), vr);
                        }
                        const uint32_T M = (uint32_T)1u << bit0;
                        for (; i<N; ++i) o[i] = setOne ? (a[i] | M) : (a[i] & ~M);
                        return;
                    }
                }
            }
            // General AVX2 path reading flags
            bitset_u32_avx2(a, F, o, N, bit0, setOne);
            #else
            // Fallback scalar branchless
            bitset_scalar_branchless<uint32_T>(a, F, o, N, bit0, setOne);
            #endif
        } break;

        case mxUINT64_CLASS: {
            if (bit1_based > 64) mexErrMsgIdAndTxt("bitsetFlag:bit","BitNumber for uint64 in [1,64].");
            const uint64_T* a = (const uint64_T*)mxGetData(Arr);
            uint64_T*       o = (uint64_T*)mxGetData(Out);
            bitset_scalar_branchless<uint64_T>(a, F, o, N, (unsigned)(bit1_based-1), setOne);
        } break;

        default:
            mexErrMsgIdAndTxt("bitsetFlag:type","Array must be uint8/uint16/uint32/uint64.");
    }
}

// ===== MEX entry =====
void mexFunction(int nlhs, mxArray* plhs[], int nrhs, const mxArray* prhs[])
{
    if (nrhs != 4)
        mexErrMsgIdAndTxt("bitsetFlag:usage",
            "Usage: Result = bitsetFlag(Array, FlagArray, BitNumber, SetVal);");
    if (nlhs > 1)
        mexErrMsgIdAndTxt("bitsetFlag:usage","One output only.");

    const mxArray* Arr = prhs[0];
    const mxArray* Flg = prhs[1];
    const mxArray* Bit = prhs[2];
    const mxArray* Set = prhs[3];

    if (mxIsComplex(Arr))
        mexErrMsgIdAndTxt("bitsetFlag:type","Array must be real.");
    const mxClassID id = mxGetClassID(Arr);
    if (!(id==mxUINT8_CLASS||id==mxUINT16_CLASS||id==mxUINT32_CLASS||id==mxUINT64_CLASS))
        mexErrMsgIdAndTxt("bitsetFlag:type","Array must be uint8/uint16/uint32/uint64.");

    if (!mxIsLogical(Flg))
        mexErrMsgIdAndTxt("bitsetFlag:flag","FlagArray must be logical.");
    if (mxGetNumberOfDimensions(Arr) != mxGetNumberOfDimensions(Flg))
        mexErrMsgIdAndTxt("bitsetFlag:size","FlagArray must have the same size as Array.");
    {
        const mwSize nd = mxGetNumberOfDimensions(Arr);
        const mwSize* da = mxGetDimensions(Arr);
        const mwSize* df = mxGetDimensions(Flg);
        for (mwSize k=0;k<nd;++k)
            if (da[k]!=df[k]) mexErrMsgIdAndTxt("bitsetFlag:size","FlagArray must have the same size as Array.");
    }

    // BitNumber: real scalar, round-to-nearest, 1-based
    if (mxIsComplex(Bit) || mxGetNumberOfElements(Bit)!=1)
        mexErrMsgIdAndTxt("bitsetFlag:bit","BitNumber must be a real scalar.");
    const double b = mxGetScalar(Bit);
    if (!(b==b)) mexErrMsgIdAndTxt("bitsetFlag:bit","BitNumber must be finite.");
    const int bit1 = (int)((b>=0) ? (b+0.5) : (b-0.5));

    // SetVal → logical: nonzero or NaN → true
    if (mxIsComplex(Set) || mxGetNumberOfElements(Set)!=1)
        mexErrMsgIdAndTxt("bitsetFlag:set","SetVal must be a real or logical scalar.");
    const double sv = mxGetScalar(Set);
    const bool setOne = (sv != 0.0); // logical(NaN)->true

    // Create output (avoid zero-init if available)
    const mwSize nd = mxGetNumberOfDimensions(Arr);
    const mwSize* dims = mxGetDimensions(Arr);
    #if defined(mxCreateUninitNumericArray)
      std::vector<size_t> dimsCopy(nd);
      for (mwSize k=0; k<nd; ++k) dimsCopy[k] = static_cast<size_t>(dims[k]);
      plhs[0] = mxCreateUninitNumericArray(static_cast<size_t>(nd), dimsCopy.data(), id, mxREAL);
    #else
      plhs[0] = mxCreateNumericArray(nd, dims, id, mxREAL);
    #endif

    run_for_class(Arr, Flg, plhs[0], bit1, setOne);
}
