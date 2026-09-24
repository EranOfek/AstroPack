// smallestRadiusBoundingCircle.cpp
// [Xcirc, Ycirc, Rcirc] = smallestRadiusBoundingCircle(X, Y)
// Exact smallest enclosing circle (SEC) via Welzl's randomized incremental algorithm.
// - No recursion, no convex hull, no rotating calipers (nothing to hang).
// - Supports single or double inputs; outputs are in the same class (single iff both inputs are single).
//
// Compile:
// mex -O CXXFLAGS="$CXXFLAGS -std=c++17 -Ofast -march=native -DNDEBUG" smallestRadiusBoundingCircle.cpp

#include "mex.h"
#include <vector>
#include <algorithm>
#include <random>
#include <cmath>
#include <cstdint>
#include <limits>

static inline bool isRealFloatOrDoubleVector(const mxArray* a){
    if (mxIsComplex(a)) return false;
    if (!(mxIsSingle(a) || mxIsDouble(a))) return false;
    const mwSize nd = mxGetNumberOfDimensions(a);
    if (nd != 2) return false;
    const mwSize* d = mxGetDimensions(a);
    return (d[0]==1 || d[1]==1);
}

template<typename T>
struct Circle { T cx{0}, cy{0}, r2{T(-1)}; }; // r2<0 => empty

template<typename T> static inline T dist2(T x1,T y1,T x2,T y2){ T dx=x1-x2, dy=y1-y2; return dx*dx+dy*dy; }

template<typename T> static inline T epsInside();
template<> inline float  epsInside<float>()  { return 1.0e-6f;  }
template<> inline double epsInside<double>() { return 1.0e-12; }

template<typename T>
static inline bool inside(const Circle<T>& c, T x, T y){
    if (c.r2 < T(0)) return false;
    return dist2(c.cx,c.cy,x,y) <= c.r2 * (T(1)+epsInside<T>());
}

template<typename T>
static inline Circle<T> circleFrom1(T x, T y){
    Circle<T> c; c.cx=x; c.cy=y; c.r2=T(0); return c;
}

template<typename T>
static inline Circle<T> circleFrom2(T x1,T y1,T x2,T y2){
    Circle<T> c; c.cx=(x1+x2)/T(2); c.cy=(y1+y2)/T(2); c.r2=dist2(c.cx,c.cy,x1,y1); return c;
}

// Robust circumcircle; fallback to 2-point cover if nearly collinear
template<typename T>
static inline Circle<T> circleFrom3(T x1,T y1,T x2,T y2,T x3,T y3){
    T a=x2-x1, b=y2-y1, c=x3-x1, d=y3-y1;
    T e=a*(x1+x2)+b*(y1+y2);
    T f=c*(x1+x3)+d*(y1+y3);
    T g=T(2)*(a*(y3-y2)-b*(x3-x2)); // 2*det
    Circle<T> C;
    const T tiny = std::numeric_limits<T>::epsilon()*T(32);
    if (std::abs(g) <= tiny){
        Circle<T> c12=circleFrom2<T>(x1,y1,x2,y2);
        Circle<T> c13=circleFrom2<T>(x1,y1,x3,y3);
        Circle<T> c23=circleFrom2<T>(x2,y2,x3,y3);
        auto coverAll=[&](const Circle<T>& cc)->bool{
            T r2e=cc.r2*(T(1)+epsInside<T>());
            return dist2(cc.cx,cc.cy,x1,y1)<=r2e &&
                   dist2(cc.cx,cc.cy,x2,y2)<=r2e &&
                   dist2(cc.cx,cc.cy,x3,y3)<=r2e;
        };
        Circle<T> best=c12;
        if (!coverAll(best) || c13.r2>best.r2) best=c13;
        if (!coverAll(best) || c23.r2>best.r2) best=c23;
        return best;
    }
    T cx=(d*e - b*f)/g;
    T cy=(a*f - c*e)/g;
    C.cx=cx; C.cy=cy; C.r2=dist2(cx,cy,x1,y1);
    return C;
}

// Welzl randomized incremental (iterative), points accessed by index order
template<typename T>
static Circle<T> smallestEnclosingCircle(const T* X, const T* Y, size_t n){
    // Trivial cases
    if (n==0){ Circle<T> z; z.r2=T(-1); return z; }
    if (n==1){ return circleFrom1<T>(X[0],Y[0]); }

    // Order: randomized for expected O(n). Fixed seed for determinism.
    std::vector<size_t> order(n);
    for (size_t i=0;i<n;++i) order[i]=i;
    std::mt19937_64 rng(0x9E3779B97F4A7C15ull);
    std::shuffle(order.begin(), order.end(), rng);

    Circle<T> c; c.r2=T(-1); // empty
    for (size_t ii=0; ii<n; ++ii){
        size_t i = order[ii];
        T xi=X[i], yi=Y[i];
        if (inside<T>(c, xi, yi)) continue;

        c = circleFrom1<T>(xi, yi);
        for (size_t jj=0; jj<ii; ++jj){
            size_t j = order[jj];
            T xj=X[j], yj=Y[j];
            if (inside<T>(c, xj, yj)) continue;

            c = circleFrom2<T>(xi, yi, xj, yj);
            for (size_t kk=0; kk<jj; ++kk){
                size_t k = order[kk];
                T xk=X[k], yk=Y[k];
                if (inside<T>(c, xk, yk)) continue;

                c = circleFrom3<T>(xi, yi, xj, yj, xk, yk);
            }
        }
    }
    return c;
}

// ---------------- MEX entry ----------------
void mexFunction(int nlhs, mxArray* plhs[], int nrhs, const mxArray* prhs[]){
    if (nrhs != 2)
        mexErrMsgIdAndTxt("smallestRadiusBoundingCircle:nrhs",
                          "Usage: [Xcirc, Ycirc, Rcirc] = smallestRadiusBoundingCircle(X, Y)");
    const mxArray* X = prhs[0];
    const mxArray* Y = prhs[1];
    if (!isRealFloatOrDoubleVector(X) || !isRealFloatOrDoubleVector(Y))
        mexErrMsgIdAndTxt("smallestRadiusBoundingCircle:type",
                          "X and Y must be real vectors (single or double).");
    const mwSize n = mxGetNumberOfElements(X);
    if (n != mxGetNumberOfElements(Y))
        mexErrMsgIdAndTxt("smallestRadiusBoundingCircle:len","X and Y must have the same length.");

    // Quick NaN/Inf guard: if any invalid input, return NaNs (keeps algorithm simple and safe)
    auto hasBad = [](const mxArray* A)->bool{
        if (mxIsSingle(A)){
            const float* v = reinterpret_cast<const float*>(mxGetData(A));
            for (mwSize i=0;i<mxGetNumberOfElements(A);++i) if (!std::isfinite(v[i])) return true;
        } else {
            const double* v = mxGetPr(A);
            for (mwSize i=0;i<mxGetNumberOfElements(A);++i) if (!std::isfinite(v[i])) return true;
        }
        return false;
    };
    if (hasBad(X) || hasBad(Y)){
        // Return NaNs of matching class
        const bool wantSingle = mxIsSingle(X) && mxIsSingle(Y);
        if (wantSingle){
            plhs[0]=mxCreateNumericMatrix(1,1,mxSINGLE_CLASS,mxREAL);
            plhs[1]=mxCreateNumericMatrix(1,1,mxSINGLE_CLASS,mxREAL);
            plhs[2]=mxCreateNumericMatrix(1,1,mxSINGLE_CLASS,mxREAL);
            *reinterpret_cast<float*>(mxGetData(plhs[0])) = NAN;
            *reinterpret_cast<float*>(mxGetData(plhs[1])) = NAN;
            *reinterpret_cast<float*>(mxGetData(plhs[2])) = NAN;
        } else {
            plhs[0]=mxCreateDoubleMatrix(1,1,mxREAL);
            plhs[1]=mxCreateDoubleMatrix(1,1,mxREAL);
            plhs[2]=mxCreateDoubleMatrix(1,1,mxREAL);
            *mxGetPr(plhs[0]) = mxGetNaN();
            *mxGetPr(plhs[1]) = mxGetNaN();
            *mxGetPr(plhs[2]) = mxGetNaN();
        }
        return;
    }

    const bool wantSingle = mxIsSingle(X) && mxIsSingle(Y);

    if (wantSingle){
        const float* x = reinterpret_cast<const float*>(mxGetData(X));
        const float* y = reinterpret_cast<const float*>(mxGetData(Y));
        Circle<float> c = smallestEnclosingCircle<float>(x,y,(size_t)n);

        plhs[0]=mxCreateNumericMatrix(1,1,mxSINGLE_CLASS,mxREAL);
        plhs[1]=mxCreateNumericMatrix(1,1,mxSINGLE_CLASS,mxREAL);
        plhs[2]=mxCreateNumericMatrix(1,1,mxSINGLE_CLASS,mxREAL);
        *reinterpret_cast<float*>(mxGetData(plhs[0])) = (c.r2<0 ? NAN : c.cx);
        *reinterpret_cast<float*>(mxGetData(plhs[1])) = (c.r2<0 ? NAN : c.cy);
        *reinterpret_cast<float*>(mxGetData(plhs[2])) = (c.r2<0 ? NAN : std::sqrt(std::max<float>(0.0f,c.r2)));
    } else {
        const double* x = mxGetPr(X);
        const double* y = mxGetPr(Y);
        Circle<double> c = smallestEnclosingCircle<double>(x,y,(size_t)n);

        plhs[0]=mxCreateDoubleMatrix(1,1,mxREAL);
        plhs[1]=mxCreateDoubleMatrix(1,1,mxREAL);
        plhs[2]=mxCreateDoubleMatrix(1,1,mxREAL);
        *mxGetPr(plhs[0]) = (c.r2<0 ? mxGetNaN() : c.cx);
        *mxGetPr(plhs[1]) = (c.r2<0 ? mxGetNaN() : c.cy);
        *mxGetPr(plhs[2]) = (c.r2<0 ? mxGetNaN() : std::sqrt(std::max<double>(0.0,c.r2)));
    }
}
