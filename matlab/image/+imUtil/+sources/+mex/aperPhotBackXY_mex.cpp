// aperPhotBackXY_mex.cpp  (fast version)
// [AperFlux, Back, BackStd, NpixAper, NpixBack] = ...
//   aperPhotBackXY_mex(Cube, X, Y, AperRadii, AnnulusRadii, SubBack)
//
// Faster hot-paths:
//  - Per-row circular clipping via dxmax=floor(sqrt(Rscan^2 - dy^2))
//  - Incremental r2 update across X (r2 += 2*dx + 1)
//  - Unrolled comparator chain for K<=8; binary search otherwise
//  - Gated aperture accumulation by largest aperture only
//  - Fixed pixel-center bounds for exact equality vs fixed-center version
//
// Build:
//   mex -O CXXFLAGS="$CXXFLAGS -std=c++17 -fopenmp -march=native -Ofast -ffast-math" \
//       LDFLAGS="$LDFLAGS -fopenmp" aperPhotBackXY_mex.cpp

#include "mex.h"
#include <cmath>
#include <vector>
#include <algorithm>
#include <cstdint>
#include <cstring>
#include <type_traits>

#ifdef _OPENMP
  #include <omp.h>
#endif

// ---------- helpers ----------
static inline bool isRealFloatOrDouble3D(const mxArray* a){
    return !mxIsComplex(a) && (mxIsSingle(a) || mxIsDouble(a)) && mxGetNumberOfDimensions(a)==3;
}
static inline bool isRealFloatOrDoubleVec(const mxArray* a){
    return !mxIsComplex(a) && (mxIsSingle(a) || mxIsDouble(a)) &&
           mxGetNumberOfDimensions(a)==2 && (mxGetM(a)==1 || mxGetN(a)==1);
}
static inline mwIndex sub2lin(mwIndex iy, mwIndex ix, mwSize My){
    return iy + ix*My;  // column-major plane index
}
static inline double median_inplace(double* a, mwIndex n){
    if (n==0) return mxGetNaN();
    mwIndex mid = n/2;
    std::nth_element(a, a+mid, a+n);
    if (n & 1) return a[mid];
    double m1 = a[mid];
    std::nth_element(a, a+mid-1, a+n);
    double m0 = a[mid-1];
    return 0.5*(m0+m1);
}

// Manual lower_bound on small K with branch-lean binary search
static inline int lb_index(const double* R2, int K, double r2){
    int lo = 0, hi = K;
    // invariant: R2[lo..hi-1] is search range; want first idx with R2[idx] >= r2
    while (lo < hi){
        int mid = (lo + hi) >> 1;
        if (R2[mid] < r2) lo = mid + 1;
        else              hi = mid;
    }
    return lo; // in [0..K]
}

// Unrolled chain for K<=8: return first idx with R2[idx] >= r2, else K
static inline int lb_smallK(const double* R2, int K, double r2){
    // K up to 8
    if (K >= 1 && r2 <= R2[0]) return 0;
    if (K >= 2 && r2 <= R2[1]) return 1;
    if (K >= 3 && r2 <= R2[2]) return 2;
    if (K >= 4 && r2 <= R2[3]) return 3;
    if (K >= 5 && r2 <= R2[4]) return 4;
    if (K >= 6 && r2 <= R2[5]) return 5;
    if (K >= 7 && r2 <= R2[6]) return 6;
    if (K >= 8 && r2 <= R2[7]) return 7;
    return K;
}

// ---------- core ----------
template<typename Tin, typename Tout>
void run_core(const mxArray* Cube_mx,
              const double* Xc, const double* Yc, mwSize N,
              const double* AperR, mwSize K,
              double Rin, double Rout,
              bool SubBack,
              mxArray*& oAperFlux, mxArray*& oBack, mxArray*& oBackStd,
              mxArray*& oNpixAper, mxArray*& oNpixBack)
{
    const mwSize* dims = mxGetDimensions(Cube_mx);
    const mwSize My = dims[0], Mx = dims[1], Ns = dims[2];
    if (Ns != N) mexErrMsgIdAndTxt("aperPhotBackXY_mex:len","numel(X)=numel(Y)=size(Cube,3) required.");

    if (Rout < Rin) std::swap(Rout, Rin);
    const double Rin2  = Rin*Rin;
    const double Rout2 = Rout*Rout;

    // Precompute R^2 (ascending) and bounds
    std::vector<double> R2v(K);
    double Amax = 0.0;
    for (mwSize k=0;k<K;++k){ R2v[k] = AperR[k]*AperR[k]; Amax = std::max(Amax, AperR[k]); }
    const double Amax2 = (K? Amax*Amax : 0.0);
    const double Rscan = std::max(Amax, Rout);
    const double Rscan2 = Rscan*Rscan;
    const double* R2 = R2v.data();
    const int Ki = (int)K;

    // Accumulator type: float if Tin is single, double otherwise
    using AccumT = typename std::conditional<std::is_same<Tin,float>::value, float, double>::type;

    const Tin* Cube = reinterpret_cast<const Tin*>(mxGetData(Cube_mx));
    const mwSize plane = My*Mx;

    // outputs
    {
        mwSize odims[2] = {N, K};
        oAperFlux = mxCreateNumericArray(2, odims, std::is_same<Tout,double>::value ? mxDOUBLE_CLASS : mxSINGLE_CLASS, mxREAL);
        oNpixAper = mxCreateDoubleMatrix(N, K, mxREAL);
    }
    oBack     = mxCreateNumericMatrix(N, 1, std::is_same<Tout,double>::value ? mxDOUBLE_CLASS : mxSINGLE_CLASS, mxREAL);
    oBackStd  = mxCreateNumericMatrix(N, 1, std::is_same<Tout,double>::value ? mxDOUBLE_CLASS : mxSINGLE_CLASS, mxREAL);
    oNpixBack = mxCreateDoubleMatrix(N, 1, mxREAL);

    Tout*   AperFlux = reinterpret_cast<Tout*>(mxGetData(oAperFlux));
    Tout*   Back     = reinterpret_cast<Tout*>(mxGetData(oBack));
    Tout*   BackStd  = reinterpret_cast<Tout*>(mxGetData(oBackStd));
    double* NpixAper = mxGetPr(oNpixAper);
    double* NpixBack = mxGetPr(oNpixBack);

    // parallel over stamps
    #pragma omp parallel if (N>8)
    {
        // difference arrays (K+1 sentinels)
        std::vector<AccumT> diffSum(K+1, (AccumT)0);
        std::vector<double> diffCnt(K+1, 0.0);
        std::vector<double> ann_vals; ann_vals.reserve(256);

        #pragma omp for schedule(static)
        for (mwIndex s=0; s<(mwIndex)N; ++s)
        {
            const mwIndex base = s * (mwIndex)plane;
            const double xc = Xc[s];
            const double yc = Yc[s];

            // reset per-stamp buffers
            std::fill(diffSum.begin(), diffSum.end(), (AccumT)0);
            std::fill(diffCnt.begin(), diffCnt.end(), 0.0);
            ann_vals.clear();

            // Y range (pixel centers at iy+1) within Rscan
            const int iymin = std::max(0,           (int)std::ceil (yc - Rscan) - 1);
            const int iymax = std::min((int)My - 1, (int)std::floor(yc + Rscan) - 1);

            for (int iy = iymin; iy <= iymax; ++iy){
                const double y = (double)(iy+1);
                const double dy = y - yc;
                const double dy2 = dy*dy;

                // Clip row to disk: dxmax = floor(sqrt(Rscan^2 - dy^2))
                double rem = Rscan2 - dy2;
                if (rem < 0.0) continue;
                int dxmax = (int)std::floor(std::sqrt(rem));
                // X range (pixel centers) for this row
                const int ixmin = std::max(0,           (int)std::ceil (xc - dxmax) - 1);
                const int ixmax = std::min((int)Mx - 1, (int)std::floor(xc + dxmax) - 1);
                if (ixmin > ixmax) continue;

                // Starting x and r2 for ix=ixmin
                double x = (double)(ixmin+1) - xc;
                double r2 = dy2 + x*x;

                // We iterate ix increasing by 1: r2' = r2 + 2*x + 1
                for (int ix = ixmin; ix <= ixmax; ++ix){
                    const double v = (double)Cube[base + sub2lin((mwIndex)iy, (mwIndex)ix, My)];

                    // background annulus
                    if (r2 > Rin2 && r2 <= Rout2){
                        ann_vals.push_back(v);
                    }

                    // only add to apertures if within largest aperture
                    if (Ki && r2 <= Amax2){
                        // map r2 to first radius with R2[idx] >= r2
                        int j;
                        if (Ki <= 8) j = lb_smallK(R2, Ki, r2);
                        else         j = lb_index  (R2, Ki, r2);
                        if (j < Ki){
                            diffSum[j] += (AccumT)v;
                            diffCnt[j] += 1.0;
                            diffSum[Ki] -= (AccumT)v;  // sentinel
                            diffCnt[Ki] -= 1.0;
                        }
                    }

                    // advance to next ix
                    r2 += 2.0*x + 1.0;
                    x  += 1.0;
                }
            }

            // prefix-sum to get sums/counts per radius
            AccumT runS = (AccumT)0;
            double runC = 0.0;
            for (int k=0;k<Ki;++k){
                runS += diffSum[k];
                runC += diffCnt[k];
                AperFlux[s + (mwIndex)k*(mwIndex)N] = (Tout)runS;
                NpixAper[s + (mwIndex)k*(mwIndex)N] = runC;
            }

            // background stats (median + pop. std about median)
            double med = mxGetNaN(), stdp = mxGetNaN();
            if (!ann_vals.empty()){
                med = median_inplace(ann_vals.data(), (mwIndex)ann_vals.size());
                long double acc = 0.0L;
                for (double a : ann_vals){ long double d=(long double)a - (long double)med; acc += d*d; }
                stdp = std::sqrt( (double)(acc / (long double)ann_vals.size()) );
            }
            Back[s]     = (Tout)med;
            BackStd[s]  = (Tout)stdp;
            NpixBack[s] = (double)ann_vals.size();

            // optional background subtraction
            if (SubBack && !std::isnan(med)){
                const double b = med;
                for (int k=0;k<Ki;++k){
                    const mwIndex idx = s + (mwIndex)k*(mwIndex)N;
                    const double flux = (double)AperFlux[idx] - b * NpixAper[idx];
                    AperFlux[idx] = (Tout)flux;
                }
            }
        }
    }
}

// ---------- mex entry ----------
void mexFunction(int nlhs, mxArray* plhs[], int nrhs, const mxArray* prhs[])
{
    if (nrhs < 5 || nrhs > 6)
        mexErrMsgIdAndTxt("aperPhotBackXY_mex:args",
            "Usage: [AperFlux,Back,BackStd,NpixAper,NpixBack] = aperPhotBackXY_mex(Cube, X, Y, AperRadii, AnnulusRadii [, SubBack])");

    const mxArray* Cube_mx = prhs[0];
    const mxArray* X_mx    = prhs[1];
    const mxArray* Y_mx    = prhs[2];
    const mxArray* R_mx    = prhs[3];
    const mxArray* Ann_mx  = prhs[4];

    if (!isRealFloatOrDouble3D(Cube_mx))
        mexErrMsgIdAndTxt("aperPhotBackXY_mex:type","Cube must be real single/double My x Mx x N.");
    if (!isRealFloatOrDoubleVec(X_mx) || !isRealFloatOrDoubleVec(Y_mx))
        mexErrMsgIdAndTxt("aperPhotBackXY_mex:type","X and Y must be real vectors (single/double).");
    if (!isRealFloatOrDoubleVec(R_mx))
        mexErrMsgIdAndTxt("aperPhotBackXY_mex:type","AperRadii must be a real vector (ascending).");
    if (!isRealFloatOrDoubleVec(Ann_mx) || mxGetNumberOfElements(Ann_mx)!=2)
        mexErrMsgIdAndTxt("aperPhotBackXY_mex:type","AnnulusRadii must be [Rin Rout].");

    const mwSize N  = mxGetNumberOfElements(X_mx);
    const mwSize N2 = mxGetNumberOfElements(Y_mx);
    const mwSize Ns = mxGetDimensions(Cube_mx)[2];
    if (N != N2 || N != Ns)
        mexErrMsgIdAndTxt("aperPhotBackXY_mex:len","numel(X)=numel(Y)=size(Cube,3) required.");

    // SubBack default true
    bool SubBack = true;
    if (nrhs == 6){
        const mxArray* S = prhs[5];
        if (mxIsLogicalScalar(S)) SubBack = mxIsLogicalScalarTrue(S);
        else if (!mxIsEmpty(S) && mxGetNumberOfElements(S)==1 && !mxIsComplex(S)) SubBack = (mxGetScalar(S) != 0.0);
        else mexErrMsgIdAndTxt("aperPhotBackXY_mex:SubBack","SubBack must be a scalar logical/numeric.");
    }

    // Load X, Y as double
    std::vector<double> Xc(N), Yc(N);
    if (mxIsDouble(X_mx)) std::memcpy(Xc.data(), mxGetPr(X_mx), N*sizeof(double));
    else { const float* p=(const float*)mxGetData(X_mx); for (mwSize i=0;i<N;++i) Xc[i]=p[i]; }
    if (mxIsDouble(Y_mx)) std::memcpy(Yc.data(), mxGetPr(Y_mx), N*sizeof(double));
    else { const float* p=(const float*)mxGetData(Y_mx); for (mwSize i=0;i<N;++i) Yc[i]=p[i]; }

    // Radii (double)
    const mwSize K = mxGetNumberOfElements(R_mx);
    std::vector<double> AperR(K);
    if (mxIsDouble(R_mx)) std::memcpy(AperR.data(), mxGetPr(R_mx), K*sizeof(double));
    else { const float* p=(const float*)mxGetData(R_mx); for (mwSize i=0;i<K;++i) AperR[i]=p[i]; }

    // Annulus
    double Rin, Rout;
    if (mxIsDouble(Ann_mx)){ const double* a=mxGetPr(Ann_mx); Rin=a[0]; Rout=a[1]; }
    else { const float* a=(const float*)mxGetData(Ann_mx); Rin=a[0]; Rout=a[1]; }

    // Outputs
    mxArray *oAperFlux=nullptr, *oBack=nullptr, *oBackStd=nullptr, *oNpixAper=nullptr, *oNpixBack=nullptr;

    // Dispatch by Cube type; OUT class matches IN class (counts are double)
    if (mxIsSingle(Cube_mx)){
        run_core<float,float>(Cube_mx, Xc.data(), Yc.data(), N, AperR.data(), K, Rin, Rout, SubBack,
                              oAperFlux, oBack, oBackStd, oNpixAper, oNpixBack);
    } else {
        run_core<double,double>(Cube_mx, Xc.data(), Yc.data(), N, AperR.data(), K, Rin, Rout, SubBack,
                                oAperFlux, oBack, oBackStd, oNpixAper, oNpixBack);
    }

    plhs[0]=oAperFlux;
    if (nlhs>1) plhs[1]=oBack;
    if (nlhs>2) plhs[2]=oBackStd;
    if (nlhs>3) plhs[3]=oNpixAper;
    if (nlhs>4) plhs[4]=oNpixBack;
}
