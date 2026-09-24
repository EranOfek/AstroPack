// boundingCircleSpherical_mex.cpp
// [LonC, LatC, Radius] = boundingCircleSpherical_mex(Lon,Lat)
// [LonC, LatC, Radius] = boundingCircleSpherical_mex(X,Y,Z)
// Exact smallest spherical cap (Welzl randomized incremental, iterative).
// - Inputs: either lon/lat (radians) OR direction cosines (X,Y,Z)
// - Class: outputs match input class (single iff all inputs are single)
// - Robust to antipodal/degenerate cases; no recursion; no while-loops.
//
// Compile:
// mex -O CXXFLAGS="$CXXFLAGS -std=c++17 -Ofast -march=native -DNDEBUG" boundingCircleSpherical_mex.cpp

#include "mex.h"
#include <vector>
#include <algorithm>
#include <random>
#include <cmath>
#include <cstdint>
#include <limits>

static inline bool isVectorRealFloatOrDouble(const mxArray* a){
    if (mxIsComplex(a)) return false;
    if (!(mxIsSingle(a) || mxIsDouble(a))) return false;
    if (mxGetNumberOfDimensions(a)!=2) return false;
    const mwSize* d = mxGetDimensions(a);
    return (d[0]==1 || d[1]==1);
}

template<typename T>
struct Cap { T cx{0}, cy{0}, cz{0}, t{T(-2)}; }; // t = cos(radius); t<-1 => invalid

template<typename T> static inline T dot3(T ax,T ay,T az,T bx,T by,T bz){ return ax*bx + ay*by + az*bz; }
template<typename T> static inline T norm3(T x,T y,T z){ return std::sqrt(x*x+y*y+z*z); }
template<typename T> static inline void normalize3(T& x,T& y,T& z){
    T n = norm3(x,y,z); if (n>0) { x/=n; y/=n; z/=n; }
}
template<typename T> static inline void cross3(T ax,T ay,T az,T bx,T by,T bz, T& rx,T& ry,T& rz){
    rx = ay*bz - az*by;
    ry = az*bx - ax*bz;
    rz = ax*by - ay*bx;
}
template<typename T> static inline T epsInside();
template<> inline float  epsInside<float>()  { return 1.0e-6f;  }
template<> inline double epsInside<double>() { return 1.0e-12; }

// inside test: dot >= t - tol  (additive tolerance is safer when t≈0 or negative)
template<typename T>
static inline bool inside(const Cap<T>& c, T x, T y, T z){
    if (c.t < T(-1)) return false;
    const T tol = epsInside<T>();
    return dot3(c.cx,c.cy,c.cz, x,y,z) >= c.t - tol;
}

// constructors
template<typename T>
static inline Cap<T> capFrom1(T x,T y,T z){
    Cap<T> c; c.cx=x; c.cy=y; c.cz=z; normalize3(c.cx,c.cy,c.cz); c.t = T(1); return c;
}

template<typename T>
static inline Cap<T> capFrom2(const T ax,const T ay,const T az,
                              const T bx,const T by,const T bz){
    Cap<T> c;
    T d = dot3(ax,ay,az, bx,by,bz);
    // identical or nearly: return point cap
    if (d > T(1)-T(1e-12)){
        return capFrom1<T>(ax,ay,az);
    }
    // non-antipodal: center ∝ a+b, radius = theta/2
    if (d > T(-1)+T(1e-12)){
        c.cx = ax + bx; c.cy = ay + by; c.cz = az + bz;
        normalize3(c.cx,c.cy,c.cz);
        c.t = dot3(c.cx,c.cy,c.cz, ax,ay,az); // cos(theta/2)
        return c;
    }
    // antipodal: minimal cap for both has R=pi/2 (t=0); pick any center ⟂ a,b
    T nx,ny,nz; cross3(ax,ay,az, bx,by,bz, nx,ny,nz);
    // If cross is tiny (numerical), pick any orthonormal
    if (norm3(nx,ny,nz) < T(1e-18)){
        // Choose a vector perpendicular to a: e.g., cross with an axis
        T ux,uy,uz;
        if (std::abs(ax) < std::abs(ay)){
            cross3(ax,ay,az, T(1),T(0),T(0), ux,uy,uz);
        } else {
            cross3(ax,ay,az, T(0),T(1),T(0), ux,uy,uz);
        }
        nx=ux; ny=uy; nz=uz;
    }
    normalize3(nx,ny,nz);
    c.cx = nx; c.cy = ny; c.cz = nz; c.t = T(0);
    return c;
}

template<typename T>
static inline Cap<T> capFrom3(const T ax,const T ay,const T az,
                              const T bx,const T by,const T bz,
                              const T cx,const T cy,const T cz){
    // u = (a-b) × (a-c)
    T abx=ax-bx, aby=ay-by, abz=az-bz;
    T acx=ax-cx, acy=ay-cy, acz=az-cz;
    T ux,uy,uz; cross3(abx,aby,abz, acx,acy,acz, ux,uy,uz);
    T un = norm3(ux,uy,uz);
    // nearly collinear on great circle -> fallback to best 2-point cap covering all three
    if (un < T(1e-18)){
        Cap<T> c12 = capFrom2(ax,ay,az, bx,by,bz);
        Cap<T> c13 = capFrom2(ax,ay,az, cx,cy,cz);
        Cap<T> c23 = capFrom2(bx,by,bz, cx,cy,cz);
        auto coverAll = [&](const Cap<T>& c)->bool{
            return inside(c, ax,ay,az) && inside(c, bx,by,bz) && inside(c, cx,cy,cz);
        };
        Cap<T> best = c12;
        if (!coverAll(best) || c13.t > best.t) best = c13; // larger t = smaller radius
        if (!coverAll(best) || c23.t > best.t) best = c23;
        return best;
    }
    // circumcap center is ±normalize(u); pick sign so it faces the points
    ux/=un; uy/=un; uz/=un;
    T s = dot3(ux,uy,uz, ax+bx+cx, ay+by+cy, az+bz+cz);
    if (s < T(0)){ ux=-ux; uy=-uy; uz=-uz; }
    Cap<T> C; C.cx=ux; C.cy=uy; C.cz=uz; C.t = dot3(ux,uy,uz, ax,ay,az);
    return C;
}

// Welzl randomized incremental (iterative). V has size n, each row is unit {x,y,z}
template<typename T>
static Cap<T> smallestCapWelzl(const T* VX, const T* VY, const T* VZ, size_t n){
    if (n==0){ Cap<T> z; z.t=T(-2); return z; }
    if (n==1){ return capFrom1<T>(VX[0],VY[0],VZ[0]); }

    std::vector<size_t> order(n);
    for (size_t i=0;i<n;++i) order[i]=i;
    std::mt19937_64 rng(0x9E3779B97F4A7C15ull);
    std::shuffle(order.begin(), order.end(), rng);

    Cap<T> C; C.t=T(-2);
    for (size_t ii=0; ii<n; ++ii){
        size_t i = order[ii];
        T px=VX[i], py=VY[i], pz=VZ[i];
        if (inside(C, px,py,pz)) continue;

        C = capFrom1<T>(px,py,pz);
        for (size_t jj=0; jj<ii; ++jj){
            size_t j = order[jj];
            T qx=VX[j], qy=VY[j], qz=VZ[j];
            if (inside(C, qx,qy,qz)) continue;

            C = capFrom2<T>(px,py,pz, qx,qy,qz);
            for (size_t kk=0; kk<jj; ++kk){
                size_t k = order[kk];
                T rx=VX[k], ry=VY[k], rz=VZ[k];
                if (inside(C, rx,ry,rz)) continue;

                C = capFrom3<T>(px,py,pz, qx,qy,qz, rx,ry,rz);
            }
        }
    }
    return C;
}

template<typename T>
static void lonlatFromDir(T cx,T cy,T cz, T& lon, T& lat){
    lon = std::atan2(cy, cx);
    lat = std::asin(std::max<T>(-1, std::min<T>(1, cz)));
}

// ---------------- MEX entry ----------------
void mexFunction(int nlhs, mxArray* plhs[], int nrhs, const mxArray* prhs[]){
    if (!(nrhs==2 || nrhs==3))
        mexErrMsgIdAndTxt("boundingCircleSpherical_mex:nrhs",
            "Usage: [LonC,LatC,Radius] = boundingCircleSpherical_mex(Lon,Lat) OR boundingCircleSpherical_mex(X,Y,Z)");

    // Class decision: single only if ALL inputs are single
    bool wantSingle = true;
    for (int a=0;a<nrhs;++a) wantSingle = wantSingle && mxIsSingle(prhs[a]);

    // Read inputs -> unit directions (same type T)
    if (wantSingle){
        std::vector<float> VX, VY, VZ;
        if (nrhs==2){
            const float* Lon = reinterpret_cast<const float*>(mxGetData(prhs[0]));
            const float* Lat = reinterpret_cast<const float*>(mxGetData(prhs[1]));
            const mwSize N = mxGetNumberOfElements(prhs[0]);
            if (N != mxGetNumberOfElements(prhs[1]))
                mexErrMsgIdAndTxt("boundingCircleSpherical_mex:len","Lon and Lat must have same length.");
            VX.resize(N); VY.resize(N); VZ.resize(N);
            for (mwSize i=0;i<N;++i){
                float cphi = std::cos(Lat[i]);
                VX[i] = cphi*std::cos(Lon[i]);
                VY[i] = cphi*std::sin(Lon[i]);
                VZ[i] = std::sin(Lat[i]);
            }
        } else { // X,Y,Z
            const float* X = reinterpret_cast<const float*>(mxGetData(prhs[0]));
            const float* Y = reinterpret_cast<const float*>(mxGetData(prhs[1]));
            const float* Z = reinterpret_cast<const float*>(mxGetData(prhs[2]));
            const mwSize N = mxGetNumberOfElements(prhs[0]);
            if (N!=mxGetNumberOfElements(prhs[1]) || N!=mxGetNumberOfElements(prhs[2]))
                mexErrMsgIdAndTxt("boundingCircleSpherical_mex:len","X,Y,Z must have same length.");
            VX.assign(X, X+N); VY.assign(Y, Y+N); VZ.assign(Z, Z+N);
            for (mwSize i=0;i<N;++i){
                float n = std::sqrt(VX[i]*VX[i]+VY[i]*VY[i]+VZ[i]*VZ[i]);
                if (n>0){ VX[i]/=n; VY[i]/=n; VZ[i]/=n; }
            }
        }
        const mwSize N = VX.size();
        Cap<float> C = smallestCapWelzl<float>(VX.data(), VY.data(), VZ.data(), (size_t)N);

        plhs[0]=mxCreateNumericMatrix(1,1,mxSINGLE_CLASS,mxREAL);
        plhs[1]=mxCreateNumericMatrix(1,1,mxSINGLE_CLASS,mxREAL);
        plhs[2]=mxCreateNumericMatrix(1,1,mxSINGLE_CLASS,mxREAL);
        float LonC, LatC;
        lonlatFromDir<float>(C.cx,C.cy,C.cz, LonC,LatC);
        *reinterpret_cast<float*>(mxGetData(plhs[0])) = LonC;
        *reinterpret_cast<float*>(mxGetData(plhs[1])) = LatC;
        float t = std::max<float>(-1.f, std::min<float>(1.f, C.t));
        *reinterpret_cast<float*>(mxGetData(plhs[2])) = std::acos(t);
    } else {
        std::vector<double> VX, VY, VZ;
        if (nrhs==2){
            const double* Lon = mxGetPr(prhs[0]);
            const double* Lat = mxGetPr(prhs[1]);
            const mwSize N = mxGetNumberOfElements(prhs[0]);
            if (N != mxGetNumberOfElements(prhs[1]))
                mexErrMsgIdAndTxt("boundingCircleSpherical_mex:len","Lon and Lat must have same length.");
            VX.resize(N); VY.resize(N); VZ.resize(N);
            for (mwSize i=0;i<N;++i){
                double cphi = std::cos(Lat[i]);
                VX[i] = cphi*std::cos(Lon[i]);
                VY[i] = cphi*std::sin(Lon[i]);
                VZ[i] = std::sin(Lat[i]);
            }
        } else {
            const double* X = mxGetPr(prhs[0]);
            const double* Y = mxGetPr(prhs[1]);
            const double* Z = mxGetPr(prhs[2]);
            const mwSize N = mxGetNumberOfElements(prhs[0]);
            if (N!=mxGetNumberOfElements(prhs[1]) || N!=mxGetNumberOfElements(prhs[2]))
                mexErrMsgIdAndTxt("boundingCircleSpherical_mex:len","X,Y,Z must have same length.");
            VX.assign(X, X+N); VY.assign(Y, Y+N); VZ.assign(Z, Z+N);
            for (mwSize i=0;i<N;++i){
                double n = std::sqrt(VX[i]*VX[i]+VY[i]*VY[i]+VZ[i]*VZ[i]);
                if (n>0){ VX[i]/=n; VY[i]/=n; VZ[i]/=n; }
            }
        }
        const mwSize N = VX.size();
        Cap<double> C = smallestCapWelzl<double>(VX.data(), VY.data(), VZ.data(), (size_t)N);

        plhs[0]=mxCreateDoubleMatrix(1,1,mxREAL);
        plhs[1]=mxCreateDoubleMatrix(1,1,mxREAL);
        plhs[2]=mxCreateDoubleMatrix(1,1,mxREAL);
        double LonC, LatC;
        lonlatFromDir<double>(C.cx,C.cy,C.cz, LonC,LatC);
        *mxGetPr(plhs[0]) = LonC;
        *mxGetPr(plhs[1]) = LatC;
        double t = std::max<double>(-1.0, std::min<double>(1.0, C.t));
        *mxGetPr(plhs[2]) = std::acos(t);
    }
}
