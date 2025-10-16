// aperPhotBack_mex.cpp
// [AperFlux,Back,BackStd,NpixAper,NpixBack] = aperPhotBack_mex(Cube, AperRadii, AnnulusRadii [, SubBack])
//
// Spec:
// - Cube: MxMxN (each MxM stamp centered on the source; center at (M+1)/2).
// - AperRadii: K radii (ascending), in pixels.
// - AnnulusRadii: [Rin Rout] (pixels), background annulus.
// - Pixels are included by pixel-center (hard edge).
// - Background "Back" is the *median* of annulus pixels; BackStd is the *population std*
//   around that median: sqrt(mean((x - median)^2)).
// - SubBack (optional, default=true): if true/1, return background-subtracted
//   aperture flux: sum(aperture) - Back .* NpixAper; otherwise return raw sums.
//
// Outputs:
// - AperFlux  (NxK): per-star flux per radius; raw sums if SubBack=false, otherwise background-subtracted.
// - Back      (Nx1): median of annulus pixels (scalar background per star).
// - BackStd   (Nx1): std (population) of annulus pixels around the median.
// - NpixAper  (1xK): number of pixels in each aperture (geometry only).
// - NpixBack  (1x1): number of pixels in the background annulus (geometry only).
//
// Build:
// mex -O CXXFLAGS="$CXXFLAGS -std=c++17 -fopenmp -march=native" LDFLAGS="$LDFLAGS -fopenmp" aperPhotBack_mex.cpp

#include "mex.h"
#include <cmath>
#include <vector>
#include <algorithm>
#include <cstdint>
#include <cstring>

#ifdef _OPENMP
  #include <omp.h>
#endif

// --------- helpers ----------
static inline bool isRealFloatOrDouble3D(const mxArray* a){
    return !mxIsComplex(a) && (mxIsSingle(a) || mxIsDouble(a)) && mxGetNumberOfDimensions(a)==3;
}
static inline bool isRealFloatOrDoubleVec(const mxArray* a){
    return !mxIsComplex(a) && (mxIsSingle(a) || mxIsDouble(a)) &&
           mxGetNumberOfDimensions(a)==2 && (mxGetM(a)==1 || mxGetN(a)==1);
}

// MATLAB column-major linear index for MxM plane; i,j are 0-based
static inline mwIndex ij_to_lin(mwIndex i, mwIndex j, mwSize M){
    return i + j*M;
}

// Median via nth_element (no full sort). Returns median for buffer a[0..n-1].
static inline double median_inplace(double* a, mwIndex n){
    if (n==0) return mxGetNaN();
    mwIndex mid = n/2;
    std::nth_element(a, a+mid, a+n);
    if ((n & 1)==1) return a[mid];
    // even: need neighbor from lower half
    double m1 = a[mid];
    std::nth_element(a, a+mid-1, a+n);
    double m0 = a[mid-1];
    return 0.5*(m0+m1);
}

// --------- core templated compute ----------
template<typename Tcube, typename Tout>
void run_core(const mxArray* Cube_mx,
              const double* AperR, mwSize K,
              double Rin, double Rout,
              bool SubBack,
              mxArray*& oAperFlux, mxArray*& oBack, mxArray*& oBackStd,
              mxArray*& oNpixAper, mxArray*& oNpixBack)
{
    // Dimensions
    const mwSize* dims = mxGetDimensions(Cube_mx);
    const mwSize M  = dims[0];
    const mwSize M2 = dims[1];
    const mwSize N  = dims[2];
    if (M != M2) mexErrMsgIdAndTxt("aperPhotBack_mex:shape","Cube must be MxMxN.");

    const mwSize MM = static_cast<mwSize>(M)*static_cast<mwSize>(M);
    const Tcube* Cube = reinterpret_cast<const Tcube*>(mxGetData(Cube_mx));

    // Center (MATLAB coords): (M+1)/2
    const double cx = (static_cast<double>(M) + 1.0) * 0.5;
    const double cy = (static_cast<double>(M) + 1.0) * 0.5;

    // Precompute r^2 per pixel and order ascending by radius
    std::vector<double> r2(MM);
    std::vector<mwIndex> order(MM);
    {
        mwIndex p = 0;
        for (mwIndex j=0;j<static_cast<mwIndex>(M);++j){
            const double y = (j+1) - cy;
            for (mwIndex i=0;i<static_cast<mwIndex>(M);++i,++p){
                const double x = (i+1) - cx;
                r2[p] = x*x + y*y;
                order[p] = ij_to_lin(i,j,M);
            }
        }
        // sort by r2
        std::vector<mwIndex> idx(MM);
        for (mwIndex u=0; u<static_cast<mwIndex>(MM); ++u) idx[u]=u;
        std::sort(idx.begin(), idx.end(), [&](mwIndex a, mwIndex b){ return r2[a] < r2[b]; });
        std::vector<double>  r2s(MM);
        std::vector<mwIndex> os(MM);
        for (mwIndex u=0; u<static_cast<mwIndex>(MM); ++u){ r2s[u]=r2[idx[u]]; os[u]=order[idx[u]]; }
        r2.swap(r2s); order.swap(os);
    }

    // For each aperture: count of pixels with r^2 <= R^2 (AperR sorted ascending)
    std::vector<mwIndex> nk(K);
    for (mwSize k=0;k<K;++k){
        const double thr = AperR[k]*AperR[k];
        nk[k] = static_cast<mwIndex>(std::upper_bound(r2.begin(), r2.end(), thr) - r2.begin());
    }
    const mwIndex nk_max = (K>0) ? nk.back() : 0;   // since AperR is ascending

    // Annulus index list
    if (Rout < Rin) std::swap(Rin, Rout);
    const double Rin2 = Rin*Rin, Rout2 = Rout*Rout;
    std::vector<mwIndex> ann_idx; ann_idx.reserve(MM/3);
    for (mwIndex p=0;p<static_cast<mwIndex>(MM);++p){
        const double rr = r2[p];
        if (rr > Rin2 && rr <= Rout2)
            ann_idx.push_back(order[p]);
    }
    const mwIndex NpixBack_ct = static_cast<mwIndex>(ann_idx.size());

    // Create outputs: class matches Cube for values; counts are double
    const bool outIsDouble = std::is_same<Tout,double>::value;

    // AperFlux NxK
    {
        mwSize odims[2] = {N, K};
        oAperFlux = mxCreateNumericArray(2, odims, outIsDouble ? mxDOUBLE_CLASS : mxSINGLE_CLASS, mxREAL);
    }
    oBack     = mxCreateNumericMatrix(N,1, outIsDouble ? mxDOUBLE_CLASS : mxSINGLE_CLASS, mxREAL);
    oBackStd  = mxCreateNumericMatrix(N,1, outIsDouble ? mxDOUBLE_CLASS : mxSINGLE_CLASS, mxREAL);
    oNpixAper = mxCreateDoubleMatrix(1, K, mxREAL);
    oNpixBack = mxCreateDoubleMatrix(1, 1, mxREAL);

    // Fill geometry counts
    {
        double* NpixAper = mxGetPr(oNpixAper);
        for (mwSize k=0;k<K;++k) NpixAper[k] = static_cast<double>(nk[k]);
        *mxGetPr(oNpixBack) = static_cast<double>(NpixBack_ct);
    }

    // Raw pointers
    Tout* AperFlux = reinterpret_cast<Tout*>(mxGetData(oAperFlux));
    Tout* Back     = reinterpret_cast<Tout*>(mxGetData(oBack));
    Tout* BackStd  = reinterpret_cast<Tout*>(mxGetData(oBackStd));

    // Temporary buffer per-thread for annulus values and aperture partials
    #pragma omp parallel if (N>8)
    {
        std::vector<double> ann_vals;
        ann_vals.reserve(NpixBack_ct > 0 ? NpixBack_ct : 1);
        std::vector<double> aper_raw; aper_raw.resize(K, 0.0);

        #pragma omp for schedule(static)
        for (mwIndex s=0; s<static_cast<mwIndex>(N); ++s)
        {
            const mwIndex base = s * static_cast<mwIndex>(MM);

            // ---- Background: median and std around median
            double med = mxGetNaN(), st = mxGetNaN();
            if (NpixBack_ct > 0){
                ann_vals.resize(NpixBack_ct);
                for (mwIndex u=0; u<NpixBack_ct; ++u){
                    ann_vals[u] = static_cast<double>(Cube[base + ann_idx[u]]);
                }
                med = median_inplace(ann_vals.data(), static_cast<mwIndex>(ann_vals.size()));

                long double acc = 0.0L;
                for (mwIndex u=0; u<NpixBack_ct; ++u){
                    long double d = (long double)ann_vals[u] - (long double)med;
                    acc += d*d;
                }
                st = (NpixBack_ct>0) ? std::sqrt( (double)(acc / (long double)NpixBack_ct) ) : mxGetNaN();
            }
            Back[s]    = static_cast<Tout>(med);
            BackStd[s] = static_cast<Tout>(st);

            // ---- Aperture cumulative sums once up to nk_max; capture at each radius
            std::fill(aper_raw.begin(), aper_raw.end(), 0.0);
            if (K>0 && nk_max>0){
                double run = 0.0;
                mwIndex next_k = 0;
                for (mwIndex p=0; p<nk_max; ++p){
                    run += static_cast<double>(Cube[base + order[p]]);
                    while (next_k < static_cast<mwIndex>(K) && (p+1) == nk[next_k]){
                        aper_raw[next_k] = run;
                        ++next_k;
                    }
                }
                // nk==0 => leave as 0
            }

            // ---- Finalize output per aperture: raw or background-subtracted
            if (SubBack){
                const double b = static_cast<double>(Back[s]);
                for (mwSize k=0;k<K;++k){
                    const double flux = aper_raw[k] - b * static_cast<double>(nk[k]);
                    AperFlux[s + k*static_cast<mwIndex>(N)] = static_cast<Tout>(flux);
                }
            } else {
                for (mwSize k=0;k<K;++k){
                    AperFlux[s + k*static_cast<mwIndex>(N)] = static_cast<Tout>(aper_raw[k]);
                }
            }
        }
    }
}

// --------- MEX entry ----------
void mexFunction(int nlhs, mxArray* plhs[], int nrhs, const mxArray* prhs[])
{
    if (nrhs < 3 || nrhs > 4)
        mexErrMsgIdAndTxt("aperPhotBack_mex:args",
            "Usage: [AperFlux,Back,BackStd,NpixAper,NpixBack]=aperPhotBack_mex(Cube, AperRadii, AnnulusRadii [, SubBack])");

    const mxArray* Cube_mx = prhs[0];
    const mxArray* Aper_mx = prhs[1];
    const mxArray* Ann_mx  = prhs[2];

    if (!isRealFloatOrDouble3D(Cube_mx))
        mexErrMsgIdAndTxt("aperPhotBack_mex:type","Cube must be real single/double MxMxN.");
    if (!isRealFloatOrDoubleVec(Aper_mx))
        mexErrMsgIdAndTxt("aperPhotBack_mex:type","AperRadii must be a real vector (single/double), ascending.");
    if (!isRealFloatOrDoubleVec(Ann_mx) || mxGetNumberOfElements(Ann_mx)!=2)
        mexErrMsgIdAndTxt("aperPhotBack_mex:type","AnnulusRadii must be a real vector with 2 elements [Rin Rout].");

    // Optional SubBack (default true)
    bool SubBack = true;
    if (nrhs == 4){
        const mxArray* S = prhs[3];
        if (mxIsLogicalScalar(S)) SubBack = mxIsLogicalScalarTrue(S);
        else if (!mxIsEmpty(S) && mxGetNumberOfElements(S)==1 && !mxIsComplex(S) && (mxIsDouble(S)||mxIsSingle(S)||mxIsInt32(S)||mxIsInt64(S))){
            double v = mxGetScalar(S);
            SubBack = (v != 0.0);
        } else {
            mexErrMsgIdAndTxt("aperPhotBack_mex:SubBack","SubBack must be a scalar logical/numeric.");
        }
    }

    // Read radii as double (geometry)
    const mwSize K = mxGetNumberOfElements(Aper_mx);
    std::vector<double> AperR(K);
    if (mxIsDouble(Aper_mx)){
        std::memcpy(AperR.data(), mxGetPr(Aper_mx), K*sizeof(double));
    } else {
        const float* p = reinterpret_cast<const float*>(mxGetData(Aper_mx));
        for (mwSize i=0;i<K;++i) AperR[i] = static_cast<double>(p[i]);
    }
    double Rin, Rout;
    if (mxIsDouble(Ann_mx)){
        const double* a = mxGetPr(Ann_mx);
        Rin = a[0]; Rout = a[1];
    } else {
        const float* a = reinterpret_cast<const float*>(mxGetData(Ann_mx));
        Rin = a[0]; Rout = a[1];
    }

    // Outputs
    mxArray *oAperFlux=nullptr, *oBack=nullptr, *oBackStd=nullptr, *oNpixAper=nullptr, *oNpixBack=nullptr;

    // Dispatch by Cube class; outputs match Cube class (counts are double)
    if (mxIsSingle(Cube_mx)){
        run_core<float,float>(Cube_mx, AperR.data(), K, Rin, Rout, SubBack,
                              oAperFlux, oBack, oBackStd, oNpixAper, oNpixBack);
    } else {
        run_core<double,double>(Cube_mx, AperR.data(), K, Rin, Rout, SubBack,
                                oAperFlux, oBack, oBackStd, oNpixAper, oNpixBack);
    }

    // Set plhs
    plhs[0] = oAperFlux;
    if (nlhs>1) plhs[1] = oBack;
    if (nlhs>2) plhs[2] = oBackStd;
    if (nlhs>3) plhs[3] = oNpixAper;
    if (nlhs>4) plhs[4] = oNpixBack;
}
