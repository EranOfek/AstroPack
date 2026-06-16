/*
 * cosbellTaper.cpp
 *
 * Usage:
 *   NewCube = cosbellTaper(Cube, Annulii)
 *
 * Inputs:
 *   Cube    - single or double, 2D image or 3D cube.
 *             If 3D, third dimension is image slice index.
 *   Annulii - [InnerRadius, OuterRadius]
 *
 * Output:
 *   NewCube - same size and class as Cube.
 *
 * Compile in MATLAB:
 *
 *   mex CXXFLAGS="$CXXFLAGS -O3 -march=native" cosbellTaper.cpp
 *
 * With OpenMP on Linux:
 *
 *   mex CXXFLAGS="$CXXFLAGS -O3 -march=native -fopenmp" ...
 *       LDFLAGS="$LDFLAGS -fopenmp" cosbellTaper.cpp
 */

#include "mex.h"
#include <cmath>
#include <vector>
#include <algorithm>

#ifdef _OPENMP
#include <omp.h>
#endif

template<typename T>
void applyCosbellTaper(
    const T* In,
    T* Out,
    const double* Weight,
    mwSize Npix,
    mwSize Nslices
) {
    const mwSize Ntot = Npix * Nslices;

    /*
     * MATLAB stores arrays column-major and contiguous.
     * Weight repeats every Npix pixels for each image slice.
     */
    #ifdef _OPENMP
    #pragma omp parallel for schedule(static)
    #endif
    for (mwSignedIndex I = 0; I < static_cast<mwSignedIndex>(Ntot); ++I) {
        mwSize Pix = static_cast<mwSize>(I) % Npix;
        Out[I] = static_cast<T>(static_cast<double>(In[I]) * Weight[Pix]);
    }
}


static void buildWeightImage(
    double* Weight,
    mwSize Ny,
    mwSize Nx,
    double Inner,
    double Outer
) {
    const mwSize Npix = Ny * Nx;

    if (Outer <= 0.0) {
        std::fill(Weight, Weight + Npix, 0.0);
        return;
    }

    if (Inner < 0.0) {
        Inner = 0.0;
    }

    const double Cy = 0.5 * (static_cast<double>(Ny) + 1.0);
    const double Cx = 0.5 * (static_cast<double>(Nx) + 1.0);

    /*
     * MATLAB-like pixel coordinates:
     *   Y = 1,...,Ny
     *   X = 1,...,Nx
     * center:
     *   Cy = (Ny + 1)/2
     *   Cx = (Nx + 1)/2
     */

    if (Outer <= Inner) {
        /*
         * Degenerate case:
         * no taper region. Keep R <= Inner, zero outside.
         */
        const double Inner2 = Inner * Inner;

        #ifdef _OPENMP
        #pragma omp parallel for schedule(static)
        #endif
        for (mwSignedIndex X = 0; X < static_cast<mwSignedIndex>(Nx); ++X) {
            const double Dx = static_cast<double>(X + 1) - Cx;
            const double Dx2 = Dx * Dx;

            for (mwSize Y = 0; Y < Ny; ++Y) {
                const double Dy = static_cast<double>(Y + 1) - Cy;
                const double R2 = Dx2 + Dy * Dy;

                const mwSize Ind = static_cast<mwSize>(X) * Ny + Y;
                Weight[Ind] = (R2 <= Inner2) ? 1.0 : 0.0;
            }
        }

        return;
    }

    const double Inner2 = Inner * Inner;
    const double Outer2 = Outer * Outer;
    const double InvWidth = 1.0 / (Outer - Inner);
    const double Pi = 3.141592653589793238462643383279502884;

    /*
     * Optimization:
     * - Compare R^2 to Inner^2 and Outer^2.
     * - Compute sqrt and cos only inside the annulus.
     */
    #ifdef _OPENMP
    #pragma omp parallel for schedule(static)
    #endif
    for (mwSignedIndex X = 0; X < static_cast<mwSignedIndex>(Nx); ++X) {
        const double Dx = static_cast<double>(X + 1) - Cx;
        const double Dx2 = Dx * Dx;

        for (mwSize Y = 0; Y < Ny; ++Y) {
            const double Dy = static_cast<double>(Y + 1) - Cy;
            const double R2 = Dx2 + Dy * Dy;

            const mwSize Ind = static_cast<mwSize>(X) * Ny + Y;

            if (R2 <= Inner2) {
                Weight[Ind] = 1.0;
            } else if (R2 >= Outer2) {
                Weight[Ind] = 0.0;
            } else {
                const double R = std::sqrt(R2);
                const double T = (R - Inner) * InvWidth;

                /*
                 * T = 0 at Inner -> Weight = 1
                 * T = 1 at Outer -> Weight = 0
                 */
                Weight[Ind] = 0.5 * (1.0 + std::cos(Pi * T));
            }
        }
    }
}


void mexFunction(
    int Nlhs,
    mxArray* Plhs[],
    int Nrhs,
    const mxArray* Prhs[]
) {
    if (Nrhs != 2) {
        mexErrMsgIdAndTxt(
            "cosbellTaper:BadInput",
            "Usage: NewCube = cosbellTaper(Cube, Annulii)"
        );
    }

    if (Nlhs > 1) {
        mexErrMsgIdAndTxt(
            "cosbellTaper:BadOutput",
            "Only one output argument is allowed."
        );
    }

    const mxArray* Cube = Prhs[0];
    const mxArray* Annulii = Prhs[1];

    if (mxIsComplex(Cube)) {
        mexErrMsgIdAndTxt(
            "cosbellTaper:ComplexInput",
            "Cube must be real, not complex."
        );
    }

    if (!(mxIsSingle(Cube) || mxIsDouble(Cube))) {
        mexErrMsgIdAndTxt(
            "cosbellTaper:BadClass",
            "Cube must be single or double."
        );
    }

    const mwSize Ndims = mxGetNumberOfDimensions(Cube);

    if (Ndims < 2 || Ndims > 3) {
        mexErrMsgIdAndTxt(
            "cosbellTaper:BadDim",
            "Cube must be a 2D image or a 3D cube."
        );
    }

    if (!mxIsDouble(Annulii) || mxIsComplex(Annulii) || mxGetNumberOfElements(Annulii) != 2) {
        mexErrMsgIdAndTxt(
            "cosbellTaper:BadAnnulii",
            "Annulii must be a real double vector [InnerRadius, OuterRadius]."
        );
    }

    const double* Ann = static_cast<const double*>(mxGetData(Annulii));

    double Inner = Ann[0];
    double Outer = Ann[1];

    if (!std::isfinite(Inner) || !std::isfinite(Outer)) {
        mexErrMsgIdAndTxt(
            "cosbellTaper:BadAnnulii",
            "Annulii values must be finite."
        );
    }

    if (Inner < 0.0 || Outer < 0.0) {
        mexErrMsgIdAndTxt(
            "cosbellTaper:BadAnnulii",
            "Radii must be non-negative."
        );
    }

    const mwSize* Dims = mxGetDimensions(Cube);

    const mwSize Ny = Dims[0];
    const mwSize Nx = Dims[1];
    const mwSize Nslices = (Ndims == 3) ? Dims[2] : 1;
    const mwSize Npix = Ny * Nx;

    Plhs[0] = mxCreateNumericArray(
        Ndims,
        Dims,
        mxGetClassID(Cube),
        mxREAL
    );

    /*
     * Build the 2D taper image once.
     * This is the main optimization for 3D cubes.
     */
    std::vector<double> Weight(Npix);
    buildWeightImage(
        Weight.data(),
        Ny,
        Nx,
        Inner,
        Outer
    );

    if (mxIsDouble(Cube)) {
        const double* In = static_cast<const double*>(mxGetData(Cube));
        double* Out = static_cast<double*>(mxGetData(Plhs[0]));

        applyCosbellTaper<double>(
            In,
            Out,
            Weight.data(),
            Npix,
            Nslices
        );

    } else {
        const float* In = static_cast<const float*>(mxGetData(Cube));
        float* Out = static_cast<float*>(mxGetData(Plhs[0]));

        applyCosbellTaper<float>(
            In,
            Out,
            Weight.data(),
            Npix,
            Nslices
        );
    }
}
