// interp2_lanczos3_mex.cpp  (Lanczos-3 separable + OpenMP + AVX2 SIMD)
//
// Kernel: Lanczos with a=3, separable.
//   L(d) = sinc(d) * sinc(d/3)   for |d| < 3, else 0
//   sinc(x) = sin(pi*x)/(pi*x), sinc(0)=1
//
// I/O conventions (same as before):
//   Vq = interp2_lanczos3_mex(Xin, Yin, Z, Xout, Yout)
//   - Xin/Yin: vectors OR meshgrid matrices (rectilinear; Xin first row, Yin first col)
//   - If Xout is 1xM and Yout is Nx1 -> output is N x M
//   - Else Xout and Yout must be same size
//   - single or double (all 5 inputs same class)
//   - Z must be 2D
//   - Boundary handling: clamp indices (replicate edge)
//   - OOB query guard: returns NaN (safe)
//
// Performance:
//   - meshgrid-output: precompute per-column wx[6], per-row wy[6], OpenMP over columns.
//   - AVX2 SIMD path (meshgrid-output): vectorize over rows using gathers.
//     Note: Lanczos3 needs 6x6=36 taps -> SIMD gather path is heavy but often still faster.
//
// Compile (Linux gcc/g++):
// mex -O CXXFLAGS="\$CXXFLAGS -O3 -fopenmp -mavx2 -mfma" LDFLAGS="\$LDFLAGS -fopenmp" interp2_lanczos3_mex.cpp
//
// Compile (Windows MSVC):
// mex -O COMPFLAGS="$COMPFLAGS /openmp /O2 /arch:AVX2" interp2_lanczos3_mex.cpp

#include "mex.h"
#include <cmath>
#include <cstdint>
#include <algorithm>
#include <limits>

#ifdef _OPENMP
  #include <omp.h>
#endif

#if defined(__AVX2__)
  #include <immintrin.h>
#endif

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
    for (mwIndex i = 1; i < (mwIndex)a.n; ++i) { T v = a.at(i); if (!(v > prev)) return false; prev = v; }
    return true;
}
template<typename T>
static inline bool isStrictDecreasing(const AxisView<T>& a) {
    if (a.n < 2) return true;
    T prev = a.at(0);
    for (mwIndex i = 1; i < (mwIndex)a.n; ++i) { T v = a.at(i); if (!(v < prev)) return false; prev = v; }
    return true;
}

// ------------------------ uniform grid check ------------------------
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

// ------------------------ interval find ------------------------
template<typename T>
static inline mwIndex findIntervalBinary(const AxisView<T>& a, bool inc, T x) {
    const T a0 = a.at(0);
    const T aN = a.at((mwIndex)a.n - 1);
    if (inc) { if (x < a0 || x > aN) return (mwIndex)-1; }
    else     { if (x > a0 || x < aN) return (mwIndex)-1; }

    mwIndex lo = 0, hi = (mwIndex)a.n - 1;
    while (hi - lo > 1) {
        mwIndex mid = lo + (hi - lo) / 2;
        T am = a.at(mid);
        if (inc) { if (x >= am) lo = mid; else hi = mid; }
        else     { if (x <= am) lo = mid; else hi = mid; }
    }
    if (lo >= (mwIndex)a.n - 1) lo = (mwIndex)a.n - 2;
    return lo;
}

template<typename T>
static inline mwIndex findIntervalUniform(const AxisView<T>& a, T x, T x0, T dx, T invDx, T& tOut) {
    const T xN = a.at((mwIndex)a.n - 1);
    if (dx > (T)0) { if (x < x0 || x > xN) return (mwIndex)-1; }
    else           { if (x > x0 || x < xN) return (mwIndex)-1; }

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

template<typename T>
static inline void intervalAndFrac(const AxisView<T>& a, bool inc, bool uniform,
                                   T x, T x0, T dx, T invDx,
                                   mwIndex& ix, T& t) {
    if (uniform) {
        ix = findIntervalUniform(a, x, x0, dx, invDx, t);
        return;
    }
    ix = findIntervalBinary(a, inc, x);
    if (ix == (mwIndex)-1) { t = std::numeric_limits<T>::quiet_NaN(); return; }
    T xL = a.at(ix);
    T xR = a.at(ix + 1);
    t = (x - xL) / (xR - xL);
    t = (T)std::min<double>(1.0, std::max<double>(0.0, (double)t));
}

// ------------------------ clamp helper ------------------------
static inline mwIndex clampIndex(mwIndex v, mwIndex lo, mwIndex hi) {
    return (v < lo) ? lo : (v > hi ? hi : v);
}

// ------------------------ Lanczos3 weights (scalar) ------------------------
template<typename T>
static inline T sinc_pi(T x) {
    if (x == (T)0) return (T)1;
    T pix = (T)M_PI * x;
    return (T)std::sin((double)pix) / pix;
}

template<typename T>
static inline T lanczos3(T x) {
    x = (T)std::abs((double)x);
    if (x >= (T)3) return (T)0;
    return sinc_pi<T>(x) * sinc_pi<T>(x * (T)(1.0/3.0));
}

template<typename T>
static inline void lanczos3Weights01(T t, T w[6]) {
    // taps at ix-2, ix-1, ix, ix+1, ix+2, ix+3
    // distances from u=ix+t:
    // d0=t+2, d1=t+1, d2=t, d3=1-t, d4=2-t, d5=3-t
    w[0] = lanczos3<T>(t + (T)2);
    w[1] = lanczos3<T>(t + (T)1);
    w[2] = lanczos3<T>(t);
    w[3] = lanczos3<T>((T)1 - t);
    w[4] = lanczos3<T>((T)2 - t);
    w[5] = lanczos3<T>((T)3 - t);

    // Normalize to unity gain
    T s = w[0]+w[1]+w[2]+w[3]+w[4]+w[5];
    if (s != (T)0) { T inv=(T)1/s; for (int k=0;k<6;++k) w[k]*=inv; }
}

// ------------------------ scalar separable Lanczos3 at one point ------------------------
template<typename T>
static inline T lanczos3OneScalar(const T* Z, mwIndex Ny,
                                  mwIndex ix, mwIndex iy,
                                  T tx, T ty,
                                  mwIndex NxTot, mwIndex NyTot) {
    T wx[6], wy[6];
    lanczos3Weights01<T>(tx, wx);
    lanczos3Weights01<T>(ty, wy);

    const mwIndex xIdx[6] = {
        clampIndex(ix - 2, 0, NxTot - 1),
        clampIndex(ix - 1, 0, NxTot - 1),
        clampIndex(ix + 0, 0, NxTot - 1),
        clampIndex(ix + 1, 0, NxTot - 1),
        clampIndex(ix + 2, 0, NxTot - 1),
        clampIndex(ix + 3, 0, NxTot - 1)
    };
    const mwIndex yIdx[6] = {
        clampIndex(iy - 2, 0, NyTot - 1),
        clampIndex(iy - 1, 0, NyTot - 1),
        clampIndex(iy + 0, 0, NyTot - 1),
        clampIndex(iy + 1, 0, NyTot - 1),
        clampIndex(iy + 2, 0, NyTot - 1),
        clampIndex(iy + 3, 0, NyTot - 1)
    };

    mwIndex bx[6];
    for (int k=0;k<6;++k) bx[k] = xIdx[k]*Ny;

    // x-conv for each y tap
    T r[6];
    for (int yy=0; yy<6; ++yy) {
        mwIndex y = yIdx[yy];
        T acc = (T)0;
        for (int xx=0; xx<6; ++xx) {
            acc += wx[xx] * Z[bx[xx] + y];
        }
        r[yy] = acc;
    }

    // y-conv
    T out = (T)0;
    for (int yy=0; yy<6; ++yy) out += wy[yy]*r[yy];
    return out;
}

#if defined(__AVX2__)
// ------------------------ SIMD helpers ------------------------
static inline __m256 make_nan_ps() { return _mm256_castsi256_ps(_mm256_set1_epi32((int)0x7FC00000u)); }
static inline __m256d make_nan_pd() { return _mm256_castsi256_pd(_mm256_set1_epi64x((long long)0x7FF8000000000000ull)); }

static inline bool gather_safe_bytes(int64_t max_elem_index, int elem_size) {
    const int64_t max_bytes = max_elem_index * (int64_t)elem_size; // gather uses int32 byte offsets
    return (max_bytes <= (int64_t)std::numeric_limits<int32_t>::max());
}

// SIMD column eval float (8 lanes). Uses Wy SoA arrays; Wx scalar per column.
static inline void lanczos3_column_simd_ps(
    const float* Z, int Ny, int NxTot,
    int ix, const float wx[6],
    const int* Iy,
    const float* Wy0, const float* Wy1, const float* Wy2, const float* Wy3, const float* Wy4, const float* Wy5,
    int N,
    float* outCol
) {
    // clamped x indices and bases
    int xIdx[6] = {
        std::max(0, std::min(NxTot-1, ix-2)),
        std::max(0, std::min(NxTot-1, ix-1)),
        std::max(0, std::min(NxTot-1, ix)),
        std::max(0, std::min(NxTot-1, ix+1)),
        std::max(0, std::min(NxTot-1, ix+2)),
        std::max(0, std::min(NxTot-1, ix+3))
    };
    int bx[6];
    for (int k=0;k<6;++k) bx[k]=xIdx[k]*Ny;

    __m256 wxv[6] = {
        _mm256_set1_ps(wx[0]), _mm256_set1_ps(wx[1]), _mm256_set1_ps(wx[2]),
        _mm256_set1_ps(wx[3]), _mm256_set1_ps(wx[4]), _mm256_set1_ps(wx[5])
    };

    const __m256 nanv = make_nan_ps();

    int i=0;
    for (; i+8<=N; i+=8) {
        __m256i iy = _mm256_loadu_si256((const __m256i*)(Iy+i));
        __m256i valid = _mm256_cmpgt_epi32(iy, _mm256_set1_epi32(-1));
        __m256 mask = _mm256_castsi256_ps(valid);

        // build y taps (clamped): iy-2..iy+3
        __m256i y0 = _mm256_sub_epi32(iy, _mm256_set1_epi32(2));
        __m256i y1 = _mm256_sub_epi32(iy, _mm256_set1_epi32(1));
        __m256i y2 = iy;
        __m256i y3 = _mm256_add_epi32(iy, _mm256_set1_epi32(1));
        __m256i y4 = _mm256_add_epi32(iy, _mm256_set1_epi32(2));
        __m256i y5 = _mm256_add_epi32(iy, _mm256_set1_epi32(3));

        __m256i zero = _mm256_setzero_si256();
        __m256i maxy = _mm256_set1_epi32(Ny-1);
        auto clampV = [&]( __m256i v )->__m256i {
            v = _mm256_max_epi32(v, zero);
            v = _mm256_min_epi32(v, maxy);
            return v;
        };
        y0=clampV(y0); y1=clampV(y1); y2=clampV(y2); y3=clampV(y3); y4=clampV(y4); y5=clampV(y5);

        auto gatherZ = [&](int b, __m256i y)->__m256 {
            __m256i idx = _mm256_add_epi32(_mm256_set1_epi32(b), y);
            __m256i off = _mm256_slli_epi32(idx, 2);
            return _mm256_mask_i32gather_ps(nanv, (const float*)((const char*)Z), off, mask, 1);
        };

        // r(yTap) = sum_x wx[x]*Z[bx[x]+yTap]
        auto rowX = [&]( __m256i y )->__m256 {
            __m256 acc = _mm256_setzero_ps();
            for (int k=0;k<6;++k) {
                __m256 zk = gatherZ(bx[k], y);
                acc = _mm256_fmadd_ps(wxv[k], zk, acc);
            }
            return acc;
        };

        __m256 r0=rowX(y0), r1=rowX(y1), r2=rowX(y2), r3=rowX(y3), r4=rowX(y4), r5=rowX(y5);

        __m256 wy0=_mm256_loadu_ps(Wy0+i);
        __m256 wy1=_mm256_loadu_ps(Wy1+i);
        __m256 wy2=_mm256_loadu_ps(Wy2+i);
        __m256 wy3=_mm256_loadu_ps(Wy3+i);
        __m256 wy4=_mm256_loadu_ps(Wy4+i);
        __m256 wy5=_mm256_loadu_ps(Wy5+i);

        __m256 out = _mm256_mul_ps(wy0, r0);
        out = _mm256_fmadd_ps(wy1, r1, out);
        out = _mm256_fmadd_ps(wy2, r2, out);
        out = _mm256_fmadd_ps(wy3, r3, out);
        out = _mm256_fmadd_ps(wy4, r4, out);
        out = _mm256_fmadd_ps(wy5, r5, out);

        _mm256_storeu_ps(outCol+i, out);
    }

    for (; i<N; ++i) {
        int iy_s = Iy[i];
        if (iy_s < 0) outCol[i] = std::numeric_limits<float>::quiet_NaN();
        else {
            // scalar using precomputed weights (avoid recomputing sinc)
            int yIdx[6] = {
                std::max(0, std::min(Ny-1, iy_s-2)),
                std::max(0, std::min(Ny-1, iy_s-1)),
                std::max(0, std::min(Ny-1, iy_s)),
                std::max(0, std::min(Ny-1, iy_s+1)),
                std::max(0, std::min(Ny-1, iy_s+2)),
                std::max(0, std::min(Ny-1, iy_s+3))
            };
            float r[6];
            for (int yy=0;yy<6;++yy) {
                int y = yIdx[yy];
                float acc=0;
                for (int k=0;k<6;++k) acc += wx[k]*Z[bx[k]+y];
                r[yy]=acc;
            }
            outCol[i] = Wy0[i]*r[0]+Wy1[i]*r[1]+Wy2[i]*r[2]+Wy3[i]*r[3]+Wy4[i]*r[4]+Wy5[i]*r[5];
        }
    }
}

// SIMD column eval double (4 lanes)
static inline void lanczos3_column_simd_pd(
    const double* Z, int Ny, int NxTot,
    int ix, const double wx[6],
    const int* Iy,
    const double* Wy0, const double* Wy1, const double* Wy2, const double* Wy3, const double* Wy4, const double* Wy5,
    int N,
    double* outCol
) {
    int xIdx[6] = {
        std::max(0, std::min(NxTot-1, ix-2)),
        std::max(0, std::min(NxTot-1, ix-1)),
        std::max(0, std::min(NxTot-1, ix)),
        std::max(0, std::min(NxTot-1, ix+1)),
        std::max(0, std::min(NxTot-1, ix+2)),
        std::max(0, std::min(NxTot-1, ix+3))
    };
    int bx[6];
    for (int k=0;k<6;++k) bx[k]=xIdx[k]*Ny;

    __m256d wxv[6] = {
        _mm256_set1_pd(wx[0]), _mm256_set1_pd(wx[1]), _mm256_set1_pd(wx[2]),
        _mm256_set1_pd(wx[3]), _mm256_set1_pd(wx[4]), _mm256_set1_pd(wx[5])
    };

    const __m256d nanv = make_nan_pd();

    int i=0;
    for (; i+4<=N; i+=4) {
        __m128i iy = _mm_loadu_si128((const __m128i*)(Iy+i));           // 4x int32
        __m128i valid32 = _mm_cmpgt_epi32(iy, _mm_set1_epi32(-1));      // 0 or -1
        __m256i valid64 = _mm256_cvtepi32_epi64(valid32);               // 0 or -1 int64
        __m256d mask = _mm256_castsi256_pd(valid64);

        __m128i y0 = _mm_sub_epi32(iy, _mm_set1_epi32(2));
        __m128i y1 = _mm_sub_epi32(iy, _mm_set1_epi32(1));
        __m128i y2 = iy;
        __m128i y3 = _mm_add_epi32(iy, _mm_set1_epi32(1));
        __m128i y4 = _mm_add_epi32(iy, _mm_set1_epi32(2));
        __m128i y5 = _mm_add_epi32(iy, _mm_set1_epi32(3));

        __m128i zero = _mm_setzero_si128();
        __m128i maxy = _mm_set1_epi32(Ny-1);
        auto clamp128 = [&]( __m128i v )->__m128i {
            v = _mm_max_epi32(v, zero);
            v = _mm_min_epi32(v, maxy);
            return v;
        };
        y0=clamp128(y0); y1=clamp128(y1); y2=clamp128(y2); y3=clamp128(y3); y4=clamp128(y4); y5=clamp128(y5);

        auto gatherZ = [&](int b, __m128i y)->__m256d {
            __m128i idx = _mm_add_epi32(_mm_set1_epi32(b), y);
            __m128i off = _mm_slli_epi32(idx, 3);
            return _mm256_mask_i32gather_pd(nanv, (const double*)((const char*)Z), off, mask, 1);
        };

        auto rowX = [&]( __m128i y )->__m256d {
            __m256d acc = _mm256_setzero_pd();
            for (int k=0;k<6;++k) {
                __m256d zk = gatherZ(bx[k], y);
                acc = _mm256_fmadd_pd(wxv[k], zk, acc);
            }
            return acc;
        };

        __m256d r0=rowX(y0), r1=rowX(y1), r2=rowX(y2), r3=rowX(y3), r4=rowX(y4), r5=rowX(y5);

        __m256d wy0=_mm256_loadu_pd(Wy0+i);
        __m256d wy1=_mm256_loadu_pd(Wy1+i);
        __m256d wy2=_mm256_loadu_pd(Wy2+i);
        __m256d wy3=_mm256_loadu_pd(Wy3+i);
        __m256d wy4=_mm256_loadu_pd(Wy4+i);
        __m256d wy5=_mm256_loadu_pd(Wy5+i);

        __m256d out = _mm256_mul_pd(wy0, r0);
        out = _mm256_fmadd_pd(wy1, r1, out);
        out = _mm256_fmadd_pd(wy2, r2, out);
        out = _mm256_fmadd_pd(wy3, r3, out);
        out = _mm256_fmadd_pd(wy4, r4, out);
        out = _mm256_fmadd_pd(wy5, r5, out);

        _mm256_storeu_pd(outCol+i, out);
    }

    for (; i<N; ++i) {
        int iy_s = Iy[i];
        if (iy_s < 0) outCol[i] = std::numeric_limits<double>::quiet_NaN();
        else {
            int yIdx[6] = {
                std::max(0, std::min(Ny-1, iy_s-2)),
                std::max(0, std::min(Ny-1, iy_s-1)),
                std::max(0, std::min(Ny-1, iy_s)),
                std::max(0, std::min(Ny-1, iy_s+1)),
                std::max(0, std::min(Ny-1, iy_s+2)),
                std::max(0, std::min(Ny-1, iy_s+3))
            };
            double r[6];
            for (int yy=0;yy<6;++yy) {
                int y = yIdx[yy];
                double acc=0;
                for (int k=0;k<6;++k) acc += wx[k]*Z[bx[k]+y];
                r[yy]=acc;
            }
            outCol[i] = Wy0[i]*r[0]+Wy1[i]*r[1]+Wy2[i]*r[2]+Wy3[i]*r[3]+Wy4[i]*r[4]+Wy5[i]*r[5];
        }
    }
}
#endif // __AVX2__

// ------------------------ main worker ------------------------
template<typename T>
static void runInterpLanczos3(
    const mxArray* mxXin, const mxArray* mxYin, const mxArray* mxZ,
    const mxArray* mxXq,  const mxArray* mxYq,
    mxArray* mxOut
) {
    AxisView<T> Xaxis, Yaxis;

    const T* Z = (const T*)mxGetData(mxZ);
    const mwSize Zm = mxGetM(mxZ);
    const mwSize Zn = mxGetN(mxZ);

    // Grid input form
    if (isVector(mxXin) && isVector(mxYin)) {
        Xaxis.p = (const T*)mxGetData(mxXin); Xaxis.n = mxGetNumberOfElements(mxXin); Xaxis.stride = 1;
        Yaxis.p = (const T*)mxGetData(mxYin); Yaxis.n = mxGetNumberOfElements(mxYin); Yaxis.stride = 1;
        if (Zm != Yaxis.n || Zn != Xaxis.n) {
            mexErrMsgIdAndTxt("interp2_lanczos3_mex:dim",
                              "For vector axes: Z must be size [numel(Yin) x numel(Xin)].");
        }
    } else {
        mwSize Xm = mxGetM(mxXin), Xn = mxGetN(mxXin);
        mwSize Ym = mxGetM(mxYin), Yn = mxGetN(mxYin);
        if (!(Xm == Zm && Xn == Zn && Ym == Zm && Yn == Zn)) {
            mexErrMsgIdAndTxt("interp2_lanczos3_mex:grid",
                              "Xin/Yin must be vectors OR matrices the same size as Z (meshgrid form).");
        }
        const T* XinGrid = (const T*)mxGetData(mxXin);
        const T* YinGrid = (const T*)mxGetData(mxYin);
        Xaxis.p = XinGrid; Xaxis.n = Zn; Xaxis.stride = Zm; // first row
        Yaxis.p = YinGrid; Yaxis.n = Zm; Yaxis.stride = 1;  // first col
    }

    if (Xaxis.n < 2 || Yaxis.n < 2) {
        mexErrMsgIdAndTxt("interp2_lanczos3_mex:grid", "Grid must have at least 2 points in each dimension.");
    }

    const bool xInc = isStrictIncreasing(Xaxis);
    const bool xDec = isStrictDecreasing(Xaxis);
    const bool yInc = isStrictIncreasing(Yaxis);
    const bool yDec = isStrictDecreasing(Yaxis);
    if (!xInc && !xDec) mexErrMsgIdAndTxt("interp2_lanczos3_mex:grid", "Xin must be strictly monotonic.");
    if (!yInc && !yDec) mexErrMsgIdAndTxt("interp2_lanczos3_mex:grid", "Yin must be strictly monotonic.");

    const bool xIncreasing = xInc;
    const bool yIncreasing = yInc;

    T xdx=0, xInvDx=0, ydx=0, yInvDx=0;
    const bool xUniform = isUniformGrid(Xaxis, xdx, xInvDx);
    const bool yUniform = isUniformGrid(Yaxis, ydx, yInvDx);
    const T x0 = Xaxis.at(0);
    const T y0 = Yaxis.at(0);

    const T* Xq = (const T*)mxGetData(mxXq);
    const T* Yq = (const T*)mxGetData(mxYq);
    T* Out      = (T*)mxGetData(mxOut);

    const mwSize outM = mxGetM(mxOut);
    const mwSize outN = mxGetN(mxOut);
    const bool meshgridOut = isRowVector(mxXq) && isColVector(mxYq);

    const mwIndex NyTot = (mwIndex)Yaxis.n;
    const mwIndex NxTot = (mwIndex)Xaxis.n;

    const bool returnNaNIfOOB = true;

    if (meshgridOut) {
        const mwSize M = outN; // numel(Xout)
        const mwSize N = outM; // numel(Yout)

        // Precompute per-column: Ix and Wx[6]
        mwIndex* Ix64 = (mwIndex*)mxMalloc(sizeof(mwIndex) * M);
        T* Wx = (T*)mxMalloc(sizeof(T) * (6 * M));

        for (mwSize j=0; j<M; ++j) {
            mwIndex ix; T tx;
            intervalAndFrac<T>(Xaxis, xIncreasing, xUniform, Xq[j], x0, xdx, xInvDx, ix, tx);
            Ix64[j] = ix;
            if (ix == (mwIndex)-1) {
                for (int k=0;k<6;++k) Wx[6*j+k] = std::numeric_limits<T>::quiet_NaN();
            } else {
                T w[6]; lanczos3Weights01<T>(tx, w);
                for (int k=0;k<6;++k) Wx[6*j+k]=w[k];
            }
        }

        // Precompute per-row: Iy and Wy SoA (Wy0..Wy5)
        mwIndex* Iy64 = (mwIndex*)mxMalloc(sizeof(mwIndex) * N);
        T* Wy0 = (T*)mxMalloc(sizeof(T) * N);
        T* Wy1 = (T*)mxMalloc(sizeof(T) * N);
        T* Wy2 = (T*)mxMalloc(sizeof(T) * N);
        T* Wy3 = (T*)mxMalloc(sizeof(T) * N);
        T* Wy4 = (T*)mxMalloc(sizeof(T) * N);
        T* Wy5 = (T*)mxMalloc(sizeof(T) * N);

        for (mwSize i=0; i<N; ++i) {
            mwIndex iy; T ty;
            intervalAndFrac<T>(Yaxis, yIncreasing, yUniform, Yq[i], y0, ydx, yInvDx, iy, ty);
            Iy64[i] = iy;
            if (iy == (mwIndex)-1) {
                Wy0[i]=Wy1[i]=Wy2[i]=Wy3[i]=Wy4[i]=Wy5[i]=std::numeric_limits<T>::quiet_NaN();
            } else {
                T w[6]; lanczos3Weights01<T>(ty, w);
                Wy0[i]=w[0]; Wy1[i]=w[1]; Wy2[i]=w[2]; Wy3[i]=w[3]; Wy4[i]=w[4]; Wy5[i]=w[5];
            }
        }

        // Convert indices to int32 for SIMD paths
        int* Ix = (int*)mxMalloc(sizeof(int) * M);
        int* Iy = (int*)mxMalloc(sizeof(int) * N);
        for (mwSize j=0; j<M; ++j) Ix[j] = (Ix64[j]==(mwIndex)-1) ? -1 : (int)Ix64[j];
        for (mwSize i=0; i<N; ++i) Iy[i] = (Iy64[i]==(mwIndex)-1) ? -1 : (int)Iy64[i];

#if defined(__AVX2__)
        const bool gatherSafe = gather_safe_bytes((int64_t)NyTot*(int64_t)NxTot, (int)sizeof(T));
#else
        const bool gatherSafe = false;
#endif

        // Parallel over columns
#ifdef _OPENMP
#pragma omp parallel for schedule(static)
#endif
        for (mwIndex jj=0; jj<(mwIndex)M; ++jj) {
            const mwIndex baseCol = jj*(mwIndex)outM;
            T* outCol = Out + baseCol;

            const int ix = Ix[jj];
            if (ix < 0) {
                for (mwIndex ii=0; ii<(mwIndex)N; ++ii) outCol[ii] = std::numeric_limits<T>::quiet_NaN();
                continue;
            }

            T wxsT[6];
            for (int k=0;k<6;++k) wxsT[k] = Wx[6*jj+k];

#if defined(__AVX2__)
            if (gatherSafe) {
                if constexpr (std::is_same<T,float>::value) {
                    float wxs[6]; for (int k=0;k<6;++k) wxs[k]=(float)wxsT[k];
                    lanczos3_column_simd_ps((const float*)Z, (int)NyTot, (int)NxTot,
                                            ix, wxs,
                                            (const int*)Iy,
                                            (const float*)Wy0,(const float*)Wy1,(const float*)Wy2,(const float*)Wy3,(const float*)Wy4,(const float*)Wy5,
                                            (int)N,
                                            (float*)outCol);
                } else {
                    double wxs[6]; for (int k=0;k<6;++k) wxs[k]=(double)wxsT[k];
                    lanczos3_column_simd_pd((const double*)Z, (int)NyTot, (int)NxTot,
                                            ix, wxs,
                                            (const int*)Iy,
                                            (const double*)Wy0,(const double*)Wy1,(const double*)Wy2,(const double*)Wy3,(const double*)Wy4,(const double*)Wy5,
                                            (int)N,
                                            (double*)outCol);
                }
            } else
#endif
            {
                // Scalar fallback column (still uses precomputed weights)
                const mwIndex NxT=NxTot, NyT=NyTot;
                const mwIndex xIdx[6] = {
                    clampIndex((mwIndex)ix-2, 0, NxT-1),
                    clampIndex((mwIndex)ix-1, 0, NxT-1),
                    clampIndex((mwIndex)ix+0, 0, NxT-1),
                    clampIndex((mwIndex)ix+1, 0, NxT-1),
                    clampIndex((mwIndex)ix+2, 0, NxT-1),
                    clampIndex((mwIndex)ix+3, 0, NxT-1)
                };
                mwIndex bx[6];
                for (int k=0;k<6;++k) bx[k]=xIdx[k]*NyT;

                for (mwIndex ii=0; ii<(mwIndex)N; ++ii) {
                    int iy = Iy[ii];
                    if (iy < 0) { outCol[ii]=std::numeric_limits<T>::quiet_NaN(); continue; }

                    const mwIndex yIdx[6] = {
                        clampIndex((mwIndex)iy-2, 0, NyT-1),
                        clampIndex((mwIndex)iy-1, 0, NyT-1),
                        clampIndex((mwIndex)iy+0, 0, NyT-1),
                        clampIndex((mwIndex)iy+1, 0, NyT-1),
                        clampIndex((mwIndex)iy+2, 0, NyT-1),
                        clampIndex((mwIndex)iy+3, 0, NyT-1)
                    };

                    T r[6];
                    for (int yy=0;yy<6;++yy) {
                        mwIndex y=yIdx[yy];
                        T acc=(T)0;
                        for (int xx=0;xx<6;++xx) acc += wxsT[xx]*Z[bx[xx]+y];
                        r[yy]=acc;
                    }

                    outCol[ii] = Wy0[ii]*r[0]+Wy1[ii]*r[1]+Wy2[ii]*r[2]+Wy3[ii]*r[3]+Wy4[ii]*r[4]+Wy5[ii]*r[5];
                }
            }
        }

        mxFree(Ix64); mxFree(Wx);
        mxFree(Iy64); mxFree(Wy0); mxFree(Wy1); mxFree(Wy2); mxFree(Wy3); mxFree(Wy4); mxFree(Wy5);
        mxFree(Ix);   mxFree(Iy);

    } else {
        // General case: scalar per-point with OpenMP
        const mwSize Ne = outM*outN;

#ifdef _OPENMP
#pragma omp parallel for schedule(static)
#endif
        for (mwIndex k=0; k<(mwIndex)Ne; ++k) {
            mwIndex ix; T tx;
            intervalAndFrac<T>(Xaxis, xIncreasing, xUniform, Xq[k], x0, xdx, xInvDx, ix, tx);
            mwIndex iy; T ty;
            intervalAndFrac<T>(Yaxis, yIncreasing, yUniform, Yq[k], y0, ydx, yInvDx, iy, ty);

            if (ix==(mwIndex)-1 || iy==(mwIndex)-1) {
                Out[k] = returnNaNIfOOB ? std::numeric_limits<T>::quiet_NaN() : (T)0;
            } else {
                Out[k] = lanczos3OneScalar<T>(Z, NyTot, ix, iy, tx, ty, NxTot, NyTot);
            }
        }
    }
}

// ------------------------ mex entry ------------------------
void mexFunction(int nlhs, mxArray* plhs[], int nrhs, const mxArray* prhs[]) {
    if (nrhs != 5) {
        mexErrMsgIdAndTxt("interp2_lanczos3_mex:nrhs",
                          "Usage: Vq = interp2_lanczos3_mex(Xin, Yin, Z, Xout, Yout)");
    }
    if (nlhs > 1) mexErrMsgIdAndTxt("interp2_lanczos3_mex:nlhs", "One output only.");

    const mxArray* mxXin = prhs[0];
    const mxArray* mxYin = prhs[1];
    const mxArray* mxZ   = prhs[2];
    const mxArray* mxXq  = prhs[3];
    const mxArray* mxYq  = prhs[4];

    auto checkRealNumeric = [](const mxArray* A, const char* name) {
        if (!mxIsNumeric(A) || mxIsComplex(A)) {
            mexErrMsgIdAndTxt("interp2_lanczos3_mex:type", "%s must be real numeric.", name);
        }
    };
    checkRealNumeric(mxXin, "Xin");
    checkRealNumeric(mxYin, "Yin");
    checkRealNumeric(mxZ,   "Z");
    checkRealNumeric(mxXq,  "Xout");
    checkRealNumeric(mxYq,  "Yout");

    mxClassID cls = mxGetClassID(mxZ);
    if (!(cls == mxSINGLE_CLASS || cls == mxDOUBLE_CLASS)) {
        mexErrMsgIdAndTxt("interp2_lanczos3_mex:type", "Z must be single or double.");
    }
    if (mxGetClassID(mxXin) != cls || mxGetClassID(mxYin) != cls ||
        mxGetClassID(mxXq)  != cls || mxGetClassID(mxYq)  != cls) {
        mexErrMsgIdAndTxt("interp2_lanczos3_mex:class",
                          "Xin, Yin, Z, Xout, Yout must all have the same class (single or double).");
    }
    if (mxGetNumberOfDimensions(mxZ) != 2) {
        mexErrMsgIdAndTxt("interp2_lanczos3_mex:dim", "Z must be a 2D matrix.");
    }

    // Output sizing
    const bool meshgridOut = isRowVector(mxXq) && isColVector(mxYq);

    mwSize outM=0, outN=0;
    if (meshgridOut) {
        outM = mxGetM(mxYq);
        outN = mxGetN(mxXq);
    } else {
        if (mxGetM(mxXq) != mxGetM(mxYq) || mxGetN(mxXq) != mxGetN(mxYq)) {
            mexErrMsgIdAndTxt("interp2_lanczos3_mex:querydim",
                              "Xout and Yout must be same size, unless Xout is 1xM and Yout is Nx1.");
        }
        outM = mxGetM(mxXq);
        outN = mxGetN(mxXq);
    }

    plhs[0] = mxCreateNumericMatrix(outM, outN, cls, mxREAL);

    if (cls == mxDOUBLE_CLASS) runInterpLanczos3<double>(mxXin, mxYin, mxZ, mxXq, mxYq, plhs[0]);
    else                      runInterpLanczos3<float >(mxXin, mxYin, mxZ, mxXq, mxYq, plhs[0]);
}
