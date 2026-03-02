// mex -O CXXFLAGS="\$CXXFLAGS -O3 -fopenmp -mavx2 -mfma" LDFLAGS="\$LDFLAGS -fopenmp" interp2_bilinear_mex3.cpp
// interp2_bilinear_mex.cpp  (algorithmic optimizations + OpenMP + AVX2 SIMD)
//
// ✅ Supports Xin/Yin as vectors OR meshgrid matrices (rectilinear).
// ✅ Bilinear interpolation, single or double (all 5 inputs must match class).
// ✅ Meshgrid-output case (Xout 1xM, Yout Nx1) returns N x M.
// ✅ OpenMP: parallel over columns (j).
// ✅ SIMD (AVX2+FMA): vectorized inner loop over rows (i) in meshgrid-output case.
//    Uses AVX2 gather to load Z values for non-contiguous Iy indices.
//
// Notes:
// - Requires AVX2 for SIMD path; otherwise falls back to scalar.
// - For gather offsets we use int32 byte offsets. If array indices exceed int32 range,
//   we fall back to scalar for safety.
//
// Compile (Linux gcc/g++):
//   mex -O CXXFLAGS="\$CXXFLAGS -O3 -fopenmp -mavx2 -mfma" LDFLAGS="\$LDFLAGS -fopenmp" interp2_bilinear_mex.cpp
//
// Compile (Windows MSVC):
//   mex -O COMPFLAGS="$COMPFLAGS /openmp /O2 /arch:AVX2" interp2_bilinear_mex.cpp
//
// (macOS requires clang+libomp+AVX2-capable build; tell me your toolchain if needed.)

#include "mex.h"
#include <cmath>
#include <cstdint>
#include <algorithm>
#include <limits>

#ifdef _OPENMP
  #include <omp.h>
#endif

#include <immintrin.h>

// ------------------------ shape helpers ------------------------

static inline bool isVector(const mxArray* A) { return (mxGetM(A) == 1 || mxGetN(A) == 1); }
static inline bool isRowVector(const mxArray* A) { return (mxGetM(A) == 1 && mxGetN(A) >= 1); }
static inline bool isColVector(const mxArray* A) { return (mxGetN(A) == 1 && mxGetM(A) >= 1); }

// ------------------------ Axis view ------------------------

template<typename T>
struct AxisView {
    const T* p = nullptr;
    mwSize n = 0;
    mwSize stride = 1;
    inline T at(mwIndex i) const { return p[(mwSize)i * stride]; }
};

// ------------------------ monotonic checks ------------------------

template<typename T>
static inline bool isStrictIncreasing(const AxisView<T>& a) {
    if (a.n < 2) return true;
    T prev = a.at(0);
    for (mwIndex i = 1; i < (mwIndex)a.n; ++i) {
        T v = a.at(i);
        if (!(v > prev)) return false;
        prev = v;
    }
    return true;
}

template<typename T>
static inline bool isStrictDecreasing(const AxisView<T>& a) {
    if (a.n < 2) return true;
    T prev = a.at(0);
    for (mwIndex i = 1; i < (mwIndex)a.n; ++i) {
        T v = a.at(i);
        if (!(v < prev)) return false;
        prev = v;
    }
    return true;
}

template<typename T>
static inline bool isMonotoneNondecreasing(const T* v, mwSize n) {
    if (n < 2) return true;
    for (mwIndex i = 1; i < (mwIndex)n; ++i) if (v[i] < v[i-1]) return false;
    return true;
}

template<typename T>
static inline bool isMonotoneNonincreasing(const T* v, mwSize n) {
    if (n < 2) return true;
    for (mwIndex i = 1; i < (mwIndex)n; ++i) if (v[i] > v[i-1]) return false;
    return true;
}

// Uniform grid check
template<typename T>
static inline bool isUniformGrid(const AxisView<T>& a, T& dx, T& invDx) {
    if (a.n < 2) return false;
    dx = a.at(1) - a.at(0);
    if (dx == (T)0) return false;

    const T tol = (std::is_same<T,float>::value) ? (T)1e-4 : (T)1e-10;
    const T adx = (T)std::abs((double)dx);
    const T thr = tol * std::max((T)1, adx);

    T prev = a.at(1);
    for (mwIndex i = 2; i < (mwIndex)a.n; ++i) {
        T v = a.at(i);
        T d = v - prev;
        if ((T)std::abs((double)(d - dx)) > thr) return false;
        prev = v;
    }
    invDx = (T)1 / dx;
    return true;
}

// ------------------------ interval search ------------------------

template<typename T>
static inline mwIndex findIntervalBinary(const AxisView<T>& a, bool inc, T x) {
    const T a0 = a.at(0);
    const T aN = a.at((mwIndex)a.n - 1);

    if (inc) {
        if (x < a0 || x > aN) return (mwIndex)-1;
    } else {
        if (x > a0 || x < aN) return (mwIndex)-1;
    }

    mwIndex lo = 0;
    mwIndex hi = (mwIndex)a.n - 1;
    while (hi - lo > 1) {
        mwIndex mid = lo + (hi - lo) / 2;
        T am = a.at(mid);
        if (inc) {
            if (x >= am) lo = mid; else hi = mid;
        } else {
            if (x <= am) lo = mid; else hi = mid;
        }
    }
    if (lo >= (mwIndex)a.n - 1) lo = (mwIndex)a.n - 2;
    return lo;
}

template<typename T>
static inline mwIndex findIntervalWalkForward(const AxisView<T>& a, bool inc, T x, mwIndex& cur) {
    const mwIndex n2 = (mwIndex)a.n - 2;
    if (inc) {
        if (x < a.at(0) || x > a.at((mwIndex)a.n - 1)) return (mwIndex)-1;
        while (cur < n2 && x > a.at(cur + 1)) ++cur;
        while (cur > 0  && x < a.at(cur))     --cur;
    } else {
        if (x > a.at(0) || x < a.at((mwIndex)a.n - 1)) return (mwIndex)-1;
        while (cur < n2 && x < a.at(cur + 1)) ++cur;
        while (cur > 0  && x > a.at(cur))     --cur;
    }
    if (cur > n2) cur = n2;
    return cur;
}

template<typename T>
static inline mwIndex findIntervalWalkBackward(const AxisView<T>& a, bool inc, T x, mwIndex& cur) {
    const mwIndex n2 = (mwIndex)a.n - 2;
    if (inc) {
        if (x < a.at(0) || x > a.at((mwIndex)a.n - 1)) return (mwIndex)-1;
        while (cur > 0  && x < a.at(cur))     --cur;
        while (cur < n2 && x > a.at(cur + 1)) ++cur;
    } else {
        if (x > a.at(0) || x < a.at((mwIndex)a.n - 1)) return (mwIndex)-1;
        while (cur > 0  && x > a.at(cur))     --cur;
        while (cur < n2 && x < a.at(cur + 1)) ++cur;
    }
    if (cur > n2) cur = n2;
    return cur;
}

template<typename T>
static inline mwIndex findIntervalUniform(const AxisView<T>& a, T x, T x0, T dx, T invDx, T& tOut) {
    const T xN = a.at((mwIndex)a.n - 1);
    if (dx > (T)0) {
        if (x < x0 || x > xN) return (mwIndex)-1;
    } else {
        if (x > x0 || x < xN) return (mwIndex)-1;
    }

    T u = (x - x0) * invDx;
    if (u < (T)0) u = (T)0;

    mwIndex i = (mwIndex)std::floor((double)u);
    const mwIndex n2 = (mwIndex)a.n - 2;
    if (i > n2) i = n2;

    T xi = x0 + (T)i * dx;
    tOut = (x - xi) * invDx;
    tOut = (T)std::min<double>(1.0, std::max<double>(0.0, (double)tOut));
    return i;
}

// ------------------------ scalar bilinear ------------------------

template<typename T>
static inline T lerp(T a, T b, T t) { return a + (b - a) * t; }

template<typename T>
static inline T bilinearFromIndicesScalar(const T* Z, mwIndex Ny, mwIndex ix, mwIndex iy, T tx, T ty) {
    const mwIndex base0 = ix * Ny;
    const mwIndex base1 = (ix + 1) * Ny;

    const mwIndex idx00 = base0 + iy;
    const mwIndex idx10 = base1 + iy;
    const mwIndex idx01 = base0 + (iy + 1);
    const mwIndex idx11 = base1 + (iy + 1);

    T z00 = Z[idx00], z10 = Z[idx10], z01 = Z[idx01], z11 = Z[idx11];
    T z0 = lerp(z00, z10, tx);
    T z1 = lerp(z01, z11, tx);
    return lerp(z0, z1, ty);
}

// ------------------------ SIMD inner loop (meshgrid-output only) ------------------------

static inline bool indices_fit_int32_bytes(int64_t max_elem_index, int elem_size) {
    // gather uses int32 byte offsets, so need max_elem_index*elem_size <= INT32_MAX
    int64_t max_bytes = max_elem_index * (int64_t)elem_size;
    return (max_bytes <= (int64_t)std::numeric_limits<int32_t>::max());
}

static inline __m256 make_nan_ps() {
    const uint32_t nan_bits = 0x7FC00000u;
    return _mm256_castsi256_ps(_mm256_set1_epi32((int)nan_bits));
}
static inline __m256d make_nan_pd() {
    const uint64_t nan_bits = 0x7FF8000000000000ull;
    return _mm256_castsi256_pd(_mm256_set1_epi64x((long long)nan_bits));
}

static inline void column_interp_simd_float(
    const float* Z, int Ny,
    int ix, float tx,
    const int* Iy, const float* Ty, int N,
    float* OutCol
) {
#if defined(__AVX2__)
    const __m256 Tx = _mm256_set1_ps(tx);
    const __m256 one = _mm256_set1_ps(1.0f);
    const __m256 nanv = make_nan_ps();

    const int base0 = ix * Ny;
    const int base1 = (ix + 1) * Ny;

    int i = 0;
    for (; i + 8 <= N; i += 8) {
        __m256i iy = _mm256_loadu_si256((const __m256i*)(Iy + i));

        // mask: active lanes where iy >= 0
        __m256i zero = _mm256_setzero_si256();
        __m256i ge0 = _mm256_cmpgt_epi32(iy, _mm256_set1_epi32(-1)); // iy > -1  <=> iy>=0
        __m256 mask = _mm256_castsi256_ps(ge0);

        // idx00 = base0 + iy
        __m256i idx00 = _mm256_add_epi32(_mm256_set1_epi32(base0), iy);
        __m256i idx10 = _mm256_add_epi32(_mm256_set1_epi32(base1), iy);
        __m256i idx01 = _mm256_add_epi32(idx00, _mm256_set1_epi32(1));
        __m256i idx11 = _mm256_add_epi32(idx10, _mm256_set1_epi32(1));

        // byte offsets
        __m256i off00 = _mm256_slli_epi32(idx00, 2);
        __m256i off10 = _mm256_slli_epi32(idx10, 2);
        __m256i off01 = _mm256_slli_epi32(idx01, 2);
        __m256i off11 = _mm256_slli_epi32(idx11, 2);

        // masked gathers: src is NaN so inactive lanes remain NaN
        __m256 z00 = _mm256_mask_i32gather_ps(nanv, (const float*)((const char*)Z), off00, mask, 1);
        __m256 z10 = _mm256_mask_i32gather_ps(nanv, (const float*)((const char*)Z), off10, mask, 1);
        __m256 z01 = _mm256_mask_i32gather_ps(nanv, (const float*)((const char*)Z), off01, mask, 1);
        __m256 z11 = _mm256_mask_i32gather_ps(nanv, (const float*)((const char*)Z), off11, mask, 1);

        __m256 ty = _mm256_loadu_ps(Ty + i);

        // z0 = z00 + (z10-z00)*tx
        __m256 z0 = _mm256_fmadd_ps(_mm256_sub_ps(z10, z00), Tx, z00);
        __m256 z1 = _mm256_fmadd_ps(_mm256_sub_ps(z11, z01), Tx, z01);
        __m256 out = _mm256_fmadd_ps(_mm256_sub_ps(z1, z0), ty, z0);

        // If lane inactive, out already NaN because gathers returned NaN
        _mm256_storeu_ps(OutCol + i, out);
    }

    // tail
    for (; i < N; ++i) {
        int iy_s = Iy[i];
        if (iy_s < 0) OutCol[i] = std::numeric_limits<float>::quiet_NaN();
        else OutCol[i] = bilinearFromIndicesScalar<float>(Z, Ny, ix, iy_s, tx, Ty[i]);
    }
#else
    for (int i = 0; i < N; ++i) {
        int iy_s = Iy[i];
        if (iy_s < 0) OutCol[i] = std::numeric_limits<float>::quiet_NaN();
        else OutCol[i] = bilinearFromIndicesScalar<float>(Z, Ny, ix, iy_s, tx, Ty[i]);
    }
#endif
}

static inline void column_interp_simd_double(
    const double* Z, int Ny,
    int ix, double tx,
    const int* Iy, const double* Ty, int N,
    double* OutCol
) {
#if defined(__AVX2__)
    const __m256d Tx = _mm256_set1_pd(tx);
    const __m256d nanv = make_nan_pd();

    const int base0 = ix * Ny;
    const int base1 = (ix + 1) * Ny;

    int i = 0;
    for (; i + 4 <= N; i += 4) {
        __m128i iy = _mm_loadu_si128((const __m128i*)(Iy + i)); // 4x int32

        // mask lanes iy>=0
        __m128i ge0 = _mm_cmpgt_epi32(iy, _mm_set1_epi32(-1));
        __m256d mask = _mm256_castsi256_pd(_mm256_cvtepi32_epi64(ge0)); // not ideal; we’ll build mask differently below

        // Build proper gather mask: set sign bit for active lanes
        // Convert ge0 (0/-1) to int64 mask words with sign bit set if active.
        __m256i ge0_64 = _mm256_cvtepi32_epi64(ge0); // 4x int64 (0 or -1)
        __m256d gmask = _mm256_castsi256_pd(ge0_64);

        __m128i base0v = _mm_set1_epi32(base0);
        __m128i base1v = _mm_set1_epi32(base1);

        __m128i idx00_32 = _mm_add_epi32(base0v, iy);
        __m128i idx10_32 = _mm_add_epi32(base1v, iy);
        __m128i idx01_32 = _mm_add_epi32(idx00_32, _mm_set1_epi32(1));
        __m128i idx11_32 = _mm_add_epi32(idx10_32, _mm_set1_epi32(1));

        // byte offsets int32*8 -> bytes
        __m128i off00_32 = _mm_slli_epi32(idx00_32, 3);
        __m128i off10_32 = _mm_slli_epi32(idx10_32, 3);
        __m128i off01_32 = _mm_slli_epi32(idx01_32, 3);
        __m128i off11_32 = _mm_slli_epi32(idx11_32, 3);

        __m256d z00 = _mm256_mask_i32gather_pd(nanv, (const double*)((const char*)Z), off00_32, gmask, 1);
        __m256d z10 = _mm256_mask_i32gather_pd(nanv, (const double*)((const char*)Z), off10_32, gmask, 1);
        __m256d z01 = _mm256_mask_i32gather_pd(nanv, (const double*)((const char*)Z), off01_32, gmask, 1);
        __m256d z11 = _mm256_mask_i32gather_pd(nanv, (const double*)((const char*)Z), off11_32, gmask, 1);

        __m256d ty = _mm256_loadu_pd(Ty + i);

        __m256d z0 = _mm256_fmadd_pd(_mm256_sub_pd(z10, z00), Tx, z00);
        __m256d z1 = _mm256_fmadd_pd(_mm256_sub_pd(z11, z01), Tx, z01);
        __m256d out = _mm256_fmadd_pd(_mm256_sub_pd(z1, z0), ty, z0);

        _mm256_storeu_pd(OutCol + i, out);
    }

    for (; i < N; ++i) {
        int iy_s = Iy[i];
        if (iy_s < 0) OutCol[i] = std::numeric_limits<double>::quiet_NaN();
        else OutCol[i] = bilinearFromIndicesScalar<double>(Z, Ny, ix, iy_s, tx, Ty[i]);
    }
#else
    for (int i = 0; i < N; ++i) {
        int iy_s = Iy[i];
        if (iy_s < 0) OutCol[i] = std::numeric_limits<double>::quiet_NaN();
        else OutCol[i] = bilinearFromIndicesScalar<double>(Z, Ny, ix, iy_s, tx, Ty[i]);
    }
#endif
}

// ------------------------ main worker ------------------------

template<typename T>
static void runInterp(
    const mxArray* mxXin, const mxArray* mxYin, const mxArray* mxZ,
    const mxArray* mxXq,  const mxArray* mxYq,
    mxArray* mxOut
) {
    const bool returnNaNIfOOB = true;

    AxisView<T> Xaxis, Yaxis;

    const T* Z = (const T*)mxGetData(mxZ);
    mwSize Zm = mxGetM(mxZ);
    mwSize Zn = mxGetN(mxZ);

    // Grid input form
    if (isVector(mxXin) && isVector(mxYin)) {
        Xaxis.p = (const T*)mxGetData(mxXin); Xaxis.n = mxGetNumberOfElements(mxXin); Xaxis.stride = 1;
        Yaxis.p = (const T*)mxGetData(mxYin); Yaxis.n = mxGetNumberOfElements(mxYin); Yaxis.stride = 1;
        if (Zm != Yaxis.n || Zn != Xaxis.n) {
            mexErrMsgIdAndTxt("interp2_bilinear_mex:dim",
                              "For vector axes: Z must be size [numel(Yin) x numel(Xin)].");
        }
    } else {
        mwSize Xm = mxGetM(mxXin), Xn = mxGetN(mxXin);
        mwSize Ym = mxGetM(mxYin), Yn = mxGetN(mxYin);
        if (!(Xm == Zm && Xn == Zn && Ym == Zm && Yn == Zn)) {
            mexErrMsgIdAndTxt("interp2_bilinear_mex:grid",
                              "Xin/Yin must be vectors OR matrices the same size as Z (meshgrid form).");
        }
        const T* XinGrid = (const T*)mxGetData(mxXin);
        const T* YinGrid = (const T*)mxGetData(mxYin);

        Xaxis.p = XinGrid; Xaxis.n = Zn; Xaxis.stride = Zm; // first row
        Yaxis.p = YinGrid; Yaxis.n = Zm; Yaxis.stride = 1;  // first col
    }

    bool xInc = isStrictIncreasing(Xaxis), xDec = isStrictDecreasing(Xaxis);
    bool yInc = isStrictIncreasing(Yaxis), yDec = isStrictDecreasing(Yaxis);
    if (!xInc && !xDec) mexErrMsgIdAndTxt("interp2_bilinear_mex:grid", "Xin must be strictly monotonic.");
    if (!yInc && !yDec) mexErrMsgIdAndTxt("interp2_bilinear_mex:grid", "Yin must be strictly monotonic.");

    const bool xIncreasing = xInc;
    const bool yIncreasing = yInc;

    T xdx=0, xInvDx=0, ydx=0, yInvDx=0;
    bool xUniform = isUniformGrid(Xaxis, xdx, xInvDx);
    bool yUniform = isUniformGrid(Yaxis, ydx, yInvDx);

    const T x0 = Xaxis.at(0);
    const T y0 = Yaxis.at(0);

    const T* Xq = (const T*)mxGetData(mxXq);
    const T* Yq = (const T*)mxGetData(mxYq);
    T* Out      = (T*)mxGetData(mxOut);

    const mwSize outM = mxGetM(mxOut);
    const mwSize outN = mxGetN(mxOut);

    const bool meshgridOut = isRowVector(mxXq) && isColVector(mxYq);

    const mwIndex Ny = (mwIndex)Yaxis.n;

    if (meshgridOut) {
        const mwSize M = outN; // numel(Xout)
        const mwSize N = outM; // numel(Yout)

        // Precompute ix/tx for Xout
        mwIndex* Ix64 = (mwIndex*)mxMalloc(sizeof(mwIndex) * M);
        T* Tx         = (T*)mxMalloc(sizeof(T) * M);

        bool XqNondec = isMonotoneNondecreasing(Xq, M);
        bool XqNoninc = isMonotoneNonincreasing(Xq, M);
        bool useWalkForwardX=false, useWalkBackwardX=false;
        if (!xUniform) {
            if (xIncreasing && XqNondec) useWalkForwardX = true;
            else if (xIncreasing && XqNoninc) useWalkBackwardX = true;
            else if (!xIncreasing && XqNoninc) useWalkForwardX = true;
            else if (!xIncreasing && XqNondec) useWalkBackwardX = true;
        }

        mwIndex curIx = 0;
        for (mwSize j = 0; j < M; ++j) {
            T x = Xq[j];
            mwIndex ix;
            T tx;

            if (xUniform) ix = findIntervalUniform(Xaxis, x, x0, xdx, xInvDx, tx);
            else if (useWalkForwardX) {
                ix = findIntervalWalkForward(Xaxis, xIncreasing, x, curIx);
                if (ix != (mwIndex)-1) {
                    T xL=Xaxis.at(ix), xR=Xaxis.at(ix+1);
                    tx = (x-xL)/(xR-xL);
                    tx = (T)std::min<double>(1.0,std::max<double>(0.0,(double)tx));
                }
            } else if (useWalkBackwardX) {
                if (j==0) curIx = (mwIndex)Xaxis.n - 2;
                ix = findIntervalWalkBackward(Xaxis, xIncreasing, x, curIx);
                if (ix != (mwIndex)-1) {
                    T xL=Xaxis.at(ix), xR=Xaxis.at(ix+1);
                    tx = (x-xL)/(xR-xL);
                    tx = (T)std::min<double>(1.0,std::max<double>(0.0,(double)tx));
                }
            } else {
                ix = findIntervalBinary(Xaxis, xIncreasing, x);
                if (ix != (mwIndex)-1) {
                    T xL=Xaxis.at(ix), xR=Xaxis.at(ix+1);
                    tx = (x-xL)/(xR-xL);
                    tx = (T)std::min<double>(1.0,std::max<double>(0.0,(double)tx));
                }
            }

            Ix64[j] = ix;
            Tx[j]   = (ix==(mwIndex)-1) ? std::numeric_limits<T>::quiet_NaN() : tx;
        }

        // Precompute iy/ty for Yout
        mwIndex* Iy64 = (mwIndex*)mxMalloc(sizeof(mwIndex) * N);
        T* Ty         = (T*)mxMalloc(sizeof(T) * N);

        bool YqNondec = isMonotoneNondecreasing(Yq, N);
        bool YqNoninc = isMonotoneNonincreasing(Yq, N);
        bool useWalkForwardY=false, useWalkBackwardY=false;
        if (!yUniform) {
            if (yIncreasing && YqNondec) useWalkForwardY = true;
            else if (yIncreasing && YqNoninc) useWalkBackwardY = true;
            else if (!yIncreasing && YqNoninc) useWalkForwardY = true;
            else if (!yIncreasing && YqNondec) useWalkBackwardY = true;
        }

        mwIndex curIy = 0;
        for (mwSize i = 0; i < N; ++i) {
            T y = Yq[i];
            mwIndex iy;
            T ty;

            if (yUniform) iy = findIntervalUniform(Yaxis, y, y0, ydx, yInvDx, ty);
            else if (useWalkForwardY) {
                iy = findIntervalWalkForward(Yaxis, yIncreasing, y, curIy);
                if (iy != (mwIndex)-1) {
                    T yB=Yaxis.at(iy), yT=Yaxis.at(iy+1);
                    ty = (y-yB)/(yT-yB);
                    ty = (T)std::min<double>(1.0,std::max<double>(0.0,(double)ty));
                }
            } else if (useWalkBackwardY) {
                if (i==0) curIy = (mwIndex)Yaxis.n - 2;
                iy = findIntervalWalkBackward(Yaxis, yIncreasing, y, curIy);
                if (iy != (mwIndex)-1) {
                    T yB=Yaxis.at(iy), yT=Yaxis.at(iy+1);
                    ty = (y-yB)/(yT-yB);
                    ty = (T)std::min<double>(1.0,std::max<double>(0.0,(double)ty));
                }
            } else {
                iy = findIntervalBinary(Yaxis, yIncreasing, y);
                if (iy != (mwIndex)-1) {
                    T yB=Yaxis.at(iy), yT=Yaxis.at(iy+1);
                    ty = (y-yB)/(yT-yB);
                    ty = (T)std::min<double>(1.0,std::max<double>(0.0,(double)ty));
                }
            }

            Iy64[i] = iy;
            Ty[i]   = (iy==(mwIndex)-1) ? std::numeric_limits<T>::quiet_NaN() : ty;
        }

        // For SIMD gather we need int32 indices
        int* Ix = (int*)mxMalloc(sizeof(int) * M);
        int* Iy = (int*)mxMalloc(sizeof(int) * N);

        // Determine if safe to use int32 indexing for gathers
        // Max element index used: max((ix+1)*Ny + (iy+1)) <= (Nx*Ny-1) roughly.
        // We’ll be conservative: require (Ny*Zn) fits in int32 elements and bytes.
        const int elem_size = (int)sizeof(T);
        int64_t max_elem_index = (int64_t)Zm * (int64_t)Zn; // Zm=Ny, Zn=Nx
        bool gatherSafe = indices_fit_int32_bytes(max_elem_index, elem_size);

        for (mwSize j = 0; j < M; ++j) {
            mwIndex v = Ix64[j];
            Ix[j] = (v==(mwIndex)-1) ? -1 : (int)v;
        }
        for (mwSize i = 0; i < N; ++i) {
            mwIndex v = Iy64[i];
            Iy[i] = (v==(mwIndex)-1) ? -1 : (int)v;
        }

        // Parallel over columns
        #ifdef _OPENMP
        #pragma omp parallel for schedule(static)
        #endif
        for (mwIndex jj = 0; jj < (mwIndex)M; ++jj) {
            const mwIndex baseCol = jj * (mwIndex)outM;

            int ix = Ix[jj];
            T tx = Tx[jj];

            if (ix < 0) {
                for (mwIndex ii = 0; ii < (mwIndex)N; ++ii)
                    Out[baseCol + ii] = std::numeric_limits<T>::quiet_NaN();
                continue;
            }

            T* outCol = Out + baseCol;

            // SIMD only in meshgrid-output and only if gather is safe
            if (gatherSafe) {
                if constexpr (std::is_same<T,float>::value) {
                    column_interp_simd_float((const float*)Z, (int)Ny, ix, (float)tx,
                                            (const int*)Iy, (const float*)Ty, (int)N,
                                            (float*)outCol);
                } else {
                    column_interp_simd_double((const double*)Z, (int)Ny, ix, (double)tx,
                                             (const int*)Iy, (const double*)Ty, (int)N,
                                             (double*)outCol);
                }
            } else {
                // Scalar fallback (very large arrays)
                for (mwIndex ii = 0; ii < (mwIndex)N; ++ii) {
                    int iy = Iy[ii];
                    if (iy < 0) outCol[ii] = std::numeric_limits<T>::quiet_NaN();
                    else outCol[ii] = bilinearFromIndicesScalar<T>(Z, (mwIndex)Ny, (mwIndex)ix, (mwIndex)iy, tx, Ty[ii]);
                }
            }
        }

        mxFree(Ix64); mxFree(Tx);
        mxFree(Iy64); mxFree(Ty);
        mxFree(Ix);   mxFree(Iy);

    } else {
        // General case (same-size Xq/Yq): keep scalar (SIMD not worth it due to interval searches)
        const mwSize Ne = outM * outN;

        #ifdef _OPENMP
        #pragma omp parallel for schedule(static)
        #endif
        for (mwIndex kk = 0; kk < (mwIndex)Ne; ++kk) {
            const T x = Xq[kk];
            const T y = Yq[kk];

            mwIndex ix, iy;
            T tx = (T)0, ty = (T)0;

            if (xUniform) ix = findIntervalUniform(Xaxis, x, x0, xdx, xInvDx, tx);
            else {
                ix = findIntervalBinary(Xaxis, xIncreasing, x);
                if (ix != (mwIndex)-1) {
                    T xL = Xaxis.at(ix), xR = Xaxis.at(ix + 1);
                    tx = (x - xL) / (xR - xL);
                    tx = (T)std::min<double>(1.0, std::max<double>(0.0, (double)tx));
                }
            }

            if (yUniform) iy = findIntervalUniform(Yaxis, y, y0, ydx, yInvDx, ty);
            else {
                iy = findIntervalBinary(Yaxis, yIncreasing, y);
                if (iy != (mwIndex)-1) {
                    T yB = Yaxis.at(iy), yT = Yaxis.at(iy + 1);
                    ty = (y - yB) / (yT - yB);
                    ty = (T)std::min<double>(1.0, std::max<double>(0.0, (double)ty));
                }
            }

            if (ix == (mwIndex)-1 || iy == (mwIndex)-1) {
                Out[kk] = returnNaNIfOOB ? std::numeric_limits<T>::quiet_NaN() : (T)0;
            } else {
                Out[kk] = bilinearFromIndicesScalar<T>(Z, (mwIndex)Ny, ix, iy, tx, ty);
            }
        }
    }
}

// ------------------------ mex entry ------------------------

void mexFunction(int nlhs, mxArray* plhs[], int nrhs, const mxArray* prhs[]) {
    if (nrhs != 5) {
        mexErrMsgIdAndTxt("interp2_bilinear_mex:nrhs",
                          "Usage: Vq = interp2_bilinear_mex(Xin, Yin, Z, Xout, Yout)");
    }
    if (nlhs > 1) {
        mexErrMsgIdAndTxt("interp2_bilinear_mex:nlhs", "One output only.");
    }

    const mxArray* mxXin = prhs[0];
    const mxArray* mxYin = prhs[1];
    const mxArray* mxZ   = prhs[2];
    const mxArray* mxXq  = prhs[3];
    const mxArray* mxYq  = prhs[4];

    auto checkRealNumeric = [](const mxArray* A, const char* name) {
        if (!mxIsNumeric(A) || mxIsComplex(A)) {
            mexErrMsgIdAndTxt("interp2_bilinear_mex:type", "%s must be real numeric.", name);
        }
    };
    checkRealNumeric(mxXin, "Xin");
    checkRealNumeric(mxYin, "Yin");
    checkRealNumeric(mxZ,   "Z");
    checkRealNumeric(mxXq,  "Xout");
    checkRealNumeric(mxYq,  "Yout");

    mxClassID cls = mxGetClassID(mxZ);
    if (!(cls == mxSINGLE_CLASS || cls == mxDOUBLE_CLASS)) {
        mexErrMsgIdAndTxt("interp2_bilinear_mex:type", "Z must be single or double.");
    }
    if (mxGetClassID(mxXin) != cls || mxGetClassID(mxYin) != cls ||
        mxGetClassID(mxXq)  != cls || mxGetClassID(mxYq)  != cls) {
        mexErrMsgIdAndTxt("interp2_bilinear_mex:class",
                          "Xin, Yin, Z, Xout, Yout must all have the same class (single or double).");
    }

    if (mxGetNumberOfDimensions(mxZ) != 2) {
        mexErrMsgIdAndTxt("interp2_bilinear_mex:dim", "Z must be a 2D matrix.");
    }

    const bool meshgridOut = isRowVector(mxXq) && isColVector(mxYq);

    mwSize outM = 0, outN = 0;
    if (meshgridOut) {
        outM = mxGetM(mxYq);
        outN = mxGetN(mxXq);
    } else {
        if (mxGetM(mxXq) != mxGetM(mxYq) || mxGetN(mxXq) != mxGetN(mxYq)) {
            mexErrMsgIdAndTxt("interp2_bilinear_mex:querydim",
                              "Xout and Yout must be same size, unless Xout is 1xM and Yout is Nx1.");
        }
        outM = mxGetM(mxXq);
        outN = mxGetN(mxXq);
    }

    plhs[0] = mxCreateNumericMatrix(outM, outN, cls, mxREAL);

    if (cls == mxDOUBLE_CLASS) runInterp<double>(mxXin, mxYin, mxZ, mxXq, mxYq, plhs[0]);
    else                      runInterp<float >(mxXin, mxYin, mxZ, mxXq, mxYq, plhs[0]);
}
