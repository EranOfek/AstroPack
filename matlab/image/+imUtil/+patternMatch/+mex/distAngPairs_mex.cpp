// catPairsWithin_mex.cpp — grid-accelerated; r^2 cutoff; compile-time skip of flip/copy via templates
// Usage:
//   [CatDist, CatTan] = catPairsWithin_mex(CatX, CatY, MaxDist [, Unique=true [, FlipX=1 [, FlipY=1]]])
//
// Semantics:
//   - Prefilter: |dx| < MaxDist && |dy| < MaxDist   (strict '<')
//   - Exact cutoff: dx^2 + dy^2 < MaxDist^2         (strict '<')
//   - sqrt() only after passing r^2 test
//   - Unique=true  -> triangle (i<j), no self
//     Unique=false -> ordered full matrix (includes self)
//   - FlipX/FlipY applied once up-front only when needed; otherwise zero-copy alias.
//
// Compile (OpenMP optional):
//   mex -O CXXFLAGS="$CXXFLAGS -march=native -fopenmp" LDFLAGS="$LDFLAGS -fopenmp" catPairsWithin_mex.cpp

#include "mex.h"
#include <cmath>
#include <vector>
#include <type_traits>
#include <cstring>
#include <limits>
#include <algorithm>

#if defined(_OPENMP)
  #include <omp.h>
#endif

// ---------- Basics ----------
static bool isVector(const mxArray* A){
    const mwSize nd = mxGetNumberOfDimensions(A);
    const mwSize* d = mxGetDimensions(A);
    return (nd==2 && (d[0]==1 || d[1]==1));
}
static bool parseBoolOptional(const mxArray* A, bool defv){
    if (!A || mxIsEmpty(A)) return defv;
    if (mxIsLogicalScalar(A)) return mxIsLogicalScalarTrue(A);
    if (mxGetNumberOfElements(A)!=1) mexErrMsgIdAndTxt("catPairsWithin:arg","Scalar expected.");
    return (mxGetScalar(A)!=0.0);
}
static double parseDoubleOptional(const mxArray* A, double defv){
    if (!A || mxIsEmpty(A)) return defv;
    if (mxGetNumberOfElements(A)!=1) mexErrMsgIdAndTxt("catPairsWithin:arg","Scalar expected.");
    return mxGetScalar(A);
}

// ---------- Template loaders (compile-time control over copy/flip) ----------
template<typename T, bool COPY, bool FLIP>
inline void load_vec(const void* src, bool srcIsDouble, mwSize n, T factor, std::vector<T>& buf, const T*& out){
    if constexpr (!COPY){
        // Zero-copy alias (src already of type T and no flip)
        out = reinterpret_cast<const T*>(src);
    }else{
        buf.resize(n);
        if (srcIsDouble){
            const double* p = static_cast<const double*>(src);
            if constexpr (FLIP){
                const T f = factor;
                #pragma omp simd
                for (mwSize k=0;k<n;++k) buf[k] = (T)p[k] * f;
            }else{
                #pragma omp simd
                for (mwSize k=0;k<n;++k) buf[k] = (T)p[k];
            }
        }else{ // single
            const float* p = static_cast<const float*>(src);
            if constexpr (FLIP){
                const T f = factor;
                #pragma omp simd
                for (mwSize k=0;k<n;++k) buf[k] = (T)p[k] * f;
            }else{
                #pragma omp simd
                for (mwSize k=0;k<n;++k) buf[k] = (T)p[k];
            }
        }
        out = buf.data();
    }
}

template<typename T, bool COPYX, bool FLIPX, bool COPYY, bool FLIPY>
inline void load_inputs_dispatch(const mxArray* Ax, const mxArray* Ay,
                                 double flipX, double flipY,
                                 std::vector<T>& xbuf, std::vector<T>& ybuf,
                                 const T*& x, const T*& y)
{
    const bool AxDouble = mxIsDouble(Ax), AyDouble = mxIsDouble(Ay);
    const mwSize n = mxGetNumberOfElements(Ax);
    // Ax
    if constexpr (!COPYX){
        // alias (compute type == input type & no flip): pick correct data pointer without copying
        if constexpr (std::is_same<T,double>::value) load_vec<T,false,false>(mxGetPr(Ax), true,  n, (T)1, xbuf, x);
        else                                         load_vec<T,false,false>(mxGetData(Ax), false, n, (T)1, xbuf, x);
    }else{
        // copy/convert ± flip in one pass
        if constexpr (std::is_same<T,double>::value) load_vec<T,true,FLIPX>( mxGetPr(Ax),    true,  n, (T)flipX, xbuf, x);
        else                                         load_vec<T,true,FLIPX>( mxGetData(Ax),  false, n, (T)flipX, xbuf, x);
    }
    // Ay
    if constexpr (!COPYY){
        if constexpr (std::is_same<T,double>::value) load_vec<T,false,false>(mxGetPr(Ay), true,  n, (T)1, ybuf, y);
        else                                         load_vec<T,false,false>(mxGetData(Ay), false, n, (T)1, ybuf, y);
    }else{
        if constexpr (std::is_same<T,double>::value) load_vec<T,true,FLIPY>( mxGetPr(Ay),    true,  n, (T)flipY, ybuf, y);
        else                                         load_vec<T,true,FLIPY>( mxGetData(Ay),  false, n, (T)flipY, ybuf, y);
    }
}

// ---------- Grid structs ----------
template<typename T>
struct Grid {
    T xmin, ymin, cell;
    int nx, ny;
    std::vector<int> head, next;
};

template<typename T>
static bool build_grid(const T* x, const T* y, int n, T cell, Grid<T>& G)
{
    if (n<=0 || !(cell>(T)0)) return false;
    T xmin=x[0], xmax=x[0], ymin=y[0], ymax=y[0];
    for (int i=1;i<n;++i){ if (x[i]<xmin) xmin=x[i]; if (x[i]>xmax) xmax=x[i];
                           if (y[i]<ymin) ymin=y[i]; if (y[i]>ymax) ymax=y[i]; }
    const T dx = xmax-xmin, dy = ymax-ymin;
    int nx = (int)std::max<T>((T)1, std::floor(dx/cell)+1);
    int ny = (int)std::max<T>((T)1, std::floor(dy/cell)+1);
    if (1LL*nx*ny > 50'000'000LL) return false;

    G.xmin=xmin; G.ymin=ymin; G.cell=cell; G.nx=nx; G.ny=ny;
    G.head.assign(nx*ny, -1); G.next.assign(n, -1);
    for (int i=0;i<n;++i){
        int cx = (int)std::floor((x[i]-xmin)/cell); if (cx<0) cx=0; else if (cx>=nx) cx=nx-1;
        int cy = (int)std::floor((y[i]-ymin)/cell); if (cy<0) cy=0; else if (cy>=ny) cy=ny-1;
        const int c = cy*nx + cx;
        G.next[i] = G.head[c];
        G.head[c] = i;
    }
    return true;
}

template<typename T>
static inline void cell_range_for_point(const Grid<T>& G, T xj, T yj, int& cx0, int& cx1, int& cy0, int& cy1)
{
    const int cx = (int)std::floor((xj - G.xmin)/G.cell);
    const int cy = (int)std::floor((yj - G.ymin)/G.cell);
    cx0 = std::max(0,cx-1); cx1 = std::min(G.nx-1,cx+1);
    cy0 = std::max(0,cy-1); cy1 = std::min(G.ny-1,cy+1);
}

// Reserve hint
template<typename T>
static double estimate_keep_ratio(const T* x, const T* y, int n, T r){
    if (n<=1) return 0.0;
    T xmin=x[0], xmax=x[0], ymin=y[0], ymax=y[0];
    for (int i=1;i<n;++i){ if (x[i]<xmin) xmin=x[i]; if (x[i]>xmax) xmax=x[i];
                           if (y[i]<ymin) ymin=y[i]; if (y[i]>ymax) ymax=y[i]; }
    const double Rx = std::max<double>(1e-12,(double)(xmax-xmin));
    const double Ry = std::max<double>(1e-12,(double)(ymax-ymin));
    const double px = std::min(1.0, 2.0*(double)r / Rx);
    const double py = std::min(1.0, 2.0*(double)r / Ry);
    return px*py;
}

// ---------- Core kernel ----------
template<typename T>
static void run_impl_core(const T* x, const T* y, int n, T md, bool unique,
                          mxArray*& oD, mxArray*& oA, mxClassID outClass)
{
    const T md2 = md*md;

    // Reserve output chunks
    int nthreads = 1;
    #if defined(_OPENMP)
    nthreads = omp_get_max_threads();
    #endif
    std::vector<std::vector<T>> Dloc(nthreads), Aloc(nthreads);

    const double keep_ratio = estimate_keep_ratio(x, y, n, md);
    double expected_pairs = unique ? 0.5 * (double)n * (double)(n-1) * keep_ratio
                                   :        (double)n * (double)n     * keep_ratio;
    for (int t=0;t<nthreads;++t){
        size_t hint = (size_t)std::min<double>(2e6, expected_pairs/nthreads + 1024.0);
        Dloc[t].reserve(hint);
        Aloc[t].reserve(hint);
    }

    // Grid
    Grid<T> G;
    const bool grid_ok = build_grid(x, y, n, md, G);

    #if defined(_OPENMP)
    #pragma omp parallel
    #endif
    {
        const int tid =
        #if defined(_OPENMP)
            omp_get_thread_num();
        #else
            0;
        #endif
        auto& dvec = Dloc[tid];
        auto& avec = Aloc[tid];

        if (grid_ok){
            #if defined(_OPENMP)
            #pragma omp for schedule(static)
            #endif
            for (int j=0;j<n;++j){
                const T xj=x[j], yj=y[j];
                int cx0,cx1,cy0,cy1; cell_range_for_point(G,xj,yj,cx0,cx1,cy0,cy1);
                for (int cy=cy0; cy<=cy1; ++cy){
                    for (int cx=cx0; cx<=cx1; ++cx){
                        const int cell = cy*G.nx + cx;
                        for (int i=G.head[cell]; i!=-1; i=G.next[i]){
                            if (unique){ if (i<=j) continue; }
                            const T dx = x[i]-xj; const T adx = dx>=0?dx:-dx; if (adx>=md) continue;
                            const T dy = y[i]-yj; const T ady = dy>=0?dy:-dy; if (ady>=md) continue;
                            const T r2 = dx*dx + dy*dy; if (r2>=md2) continue;
                            const T dist = std::sqrt(r2);
                            const T ang  = (!unique && dx==(T)0 && dy==(T)0)
                                            ? std::numeric_limits<T>::quiet_NaN()
                                            : (T)std::atan2((double)dy,(double)dx);
                            dvec.push_back(dist); avec.push_back(ang);
                        }
                    }
                }
            }
        } else {
            #if defined(_OPENMP)
            #pragma omp for schedule(static)
            #endif
            for (int j=0;j<n;++j){
                const T xj=x[j], yj=y[j];
                if (unique){
                    for (int i=j+1;i<n;++i){
                        const T dx=x[i]-xj; const T adx=dx>=0?dx:-dx; if (adx>=md) continue;
                        const T dy=y[i]-yj; const T ady=dy>=0?dy:-dy; if (ady>=md) continue;
                        const T r2=dx*dx+dy*dy; if (r2>=md2) continue;
                        const T dist=std::sqrt(r2);
                        const T ang=(T)std::atan2((double)dy,(double)dx);
                        dvec.push_back(dist); avec.push_back(ang);
                    }
                }else{
                    for (int i=0;i<n;++i){
                        const T dx=x[i]-xj; const T adx=dx>=0?dx:-dx; if (adx>=md) continue;
                        const T dy=y[i]-yj; const T ady=dy>=0?dy:-dy; if (ady>=md) continue;
                        const T r2=dx*dx+dy*dy; if (r2>=md2) continue;
                        const T dist=std::sqrt(r2);
                        const T ang=(dx==(T)0 && dy==(T)0) ? std::numeric_limits<T>::quiet_NaN()
                                                           : (T)std::atan2((double)dy,(double)dx);
                        dvec.push_back(dist); avec.push_back(ang);
                    }
                }
            }
        }
    } // parallel

    // Concatenate
    size_t total=0; for (auto& v:Dloc) total += v.size();
    oD = mxCreateNumericMatrix((mwSize)total, 1, outClass, mxREAL);
    oA = mxCreateNumericMatrix((mwSize)total, 1, outClass, mxREAL);
    T* Do = reinterpret_cast<T*>(mxGetData(oD));
    T* Ao = reinterpret_cast<T*>(mxGetData(oA));
    size_t pos=0;
    for (auto& v:Dloc){
        const size_t len=v.size();
        if (len){
            std::memcpy(Do+pos, v.data(), len*sizeof(T));
        }
        pos += len;
    }
    pos=0;
    for (auto& v:Aloc){
        const size_t len=v.size();
        if (len){
            std::memcpy(Ao+pos, v.data(), len*sizeof(T));
        }
        pos += len;
    }
}

// ---------- Top-level run_impl: decide template path for flip/copy ----------
template<typename T>
static void run_impl(const mxArray* Ax, const mxArray* Ay, const mxArray* Amax,
                     bool unique, double flipX, double flipY,
                     mxArray*& oD, mxArray*& oA)
{
    const int n = (int)mxGetNumberOfElements(Ax);
    const mxClassID outClass = std::is_same<T,double>::value ? mxDOUBLE_CLASS : mxSINGLE_CLASS;

    if (n==0){
        oD = mxCreateNumericMatrix(0,1, outClass, mxREAL);
        oA = mxCreateNumericMatrix(0,1, outClass, mxREAL);
        return;
    }

    // MaxDist
    T md;
    if (mxIsDouble(Amax)) md = (T)(*mxGetPr(Amax));
    else if (mxIsSingle(Amax)) md = (T)(*reinterpret_cast<const float*>(mxGetData(Amax)));
    else mexErrMsgIdAndTxt("catPairsWithin:maxd","MaxDist must be single/double.");
    if (!(md>(T)0)){
        oD = mxCreateNumericMatrix(0,1, outClass, mxREAL);
        oA = mxCreateNumericMatrix(0,1, outClass, mxREAL);
        return;
    }

    // Decide whether we can alias or must copy/convert/flip
    const bool AxMatchesT = ( std::is_same<T,double>::value ? mxIsDouble(Ax) : mxIsSingle(Ax) );
    const bool AyMatchesT = ( std::is_same<T,double>::value ? mxIsDouble(Ay) : mxIsSingle(Ay) );
    const bool flipX1 = (flipX==1.0);
    const bool flipY1 = (flipY==1.0);

    const bool COPYX = !(AxMatchesT && flipX1);
    const bool COPYY = !(AyMatchesT && flipY1);
    const bool FLIPX = !flipX1;
    const bool FLIPY = !flipY1;

    // Storage for copies (only used if COPYX/COPYY are true)
    std::vector<T> xbuf, ybuf;
    const T* x=nullptr; const T* y=nullptr;

    // Static dispatch over the 16 possible combinations; the compiler prunes unused code inside
    // the chosen instantiation (no runtime branches inside the tight copy loop).
    if (COPYX){
        if (FLIPX){
            if (COPYY){
                if (FLIPY)      load_inputs_dispatch<T,true, true, true, true >(Ax,Ay,flipX,flipY,xbuf,ybuf,x,y);
                else            load_inputs_dispatch<T,true, true, true, false>(Ax,Ay,flipX,flipY,xbuf,ybuf,x,y);
            }else{
                if (FLIPY)      load_inputs_dispatch<T,true, true, false,true >(Ax,Ay,flipX,flipY,xbuf,ybuf,x,y);
                else            load_inputs_dispatch<T,true, true, false,false>(Ax,Ay,flipX,flipY,xbuf,ybuf,x,y);
            }
        }else{
            if (COPYY){
                if (FLIPY)      load_inputs_dispatch<T,true, false, true, true >(Ax,Ay,flipX,flipY,xbuf,ybuf,x,y);
                else            load_inputs_dispatch<T,true, false, true, false>(Ax,Ay,flipX,flipY,xbuf,ybuf,x,y);
            }else{
                if (FLIPY)      load_inputs_dispatch<T,true, false, false,true >(Ax,Ay,flipX,flipY,xbuf,ybuf,x,y);
                else            load_inputs_dispatch<T,true, false, false,false>(Ax,Ay,flipX,flipY,xbuf,ybuf,x,y);
            }
        }
    }else{ // !COPYX
        if (COPYY){
            if (FLIPY)          load_inputs_dispatch<T,false,false, true, true >(Ax,Ay,flipX,flipY,xbuf,ybuf,x,y);
            else                load_inputs_dispatch<T,false,false, true, false>(Ax,Ay,flipX,flipY,xbuf,ybuf,x,y);
        }else{ // !COPYX && !COPYY  => full zero-copy alias on both
                                load_inputs_dispatch<T,false,false, false,false>(Ax,Ay,flipX,flipY,xbuf,ybuf,x,y);
        }
    }

    // Launch core
    run_impl_core<T>(x, y, n, md, unique, oD, oA, outClass);
}

// ---------- Gateway ----------
void mexFunction(int nlhs, mxArray* plhs[], int nrhs, const mxArray* prhs[])
{
    if (nrhs < 3)
        mexErrMsgIdAndTxt("catPairsWithin:args",
            "Usage: [CatDist, CatTan] = catPairsWithin_mex(CatX, CatY, MaxDist [, Unique=true [, FlipX=1 [, FlipY=1]]])");

    const mxArray* Ax   = prhs[0];
    const mxArray* Ay   = prhs[1];
    const mxArray* Amax = prhs[2];
    const mxArray* Auni = (nrhs >= 4) ? prhs[3] : nullptr;
    const mxArray* Afx  = (nrhs >= 5) ? prhs[4] : nullptr;
    const mxArray* Afy  = (nrhs >= 6) ? prhs[5] : nullptr;

    if (mxIsComplex(Ax) || mxIsComplex(Ay) || mxIsComplex(Amax) ||
        (Auni && mxIsComplex(Auni)) || (Afx && mxIsComplex(Afx)) || (Afy && mxIsComplex(Afy)))
        mexErrMsgIdAndTxt("catPairsWithin:complex","Inputs must be real.");
    if (!isVector(Ax) || !isVector(Ay))
        mexErrMsgIdAndTxt("catPairsWithin:shape","CatX and CatY must be 1-D vectors.");
    if (mxGetNumberOfElements(Ax) != mxGetNumberOfElements(Ay))
        mexErrMsgIdAndTxt("catPairsWithin:nmatch","CatX and CatY must have same length.");
    if (mxGetNumberOfElements(Amax) != 1 || !(mxIsDouble(Amax) || mxIsSingle(Amax)))
        mexErrMsgIdAndTxt("catPairsWithin:maxd","MaxDist must be a real scalar (single/double).");

    const bool   unique = parseBoolOptional(Auni, true);
    const double flipX  = parseDoubleOptional(Afx, 1.0);
    const double flipY  = parseDoubleOptional(Afy, 1.0);

    const bool useDouble = (mxGetClassID(Ax)==mxDOUBLE_CLASS) || (mxGetClassID(Ay)==mxDOUBLE_CLASS);

    mxArray *oD=nullptr, *oA=nullptr;
    if (useDouble){
        run_impl<double>(Ax, Ay, Amax, unique, flipX, flipY, oD, oA);
    } else {
        if (!(mxIsSingle(Ax) && mxIsSingle(Ay)))
            mexErrMsgIdAndTxt("catPairsWithin:type","If not double, CatX and CatY must be single.");
        run_impl<float>(Ax, Ay, Amax, unique, flipX, flipY, oD, oA);
    }
    plhs[0]=oD; plhs[1]=oA;
}
