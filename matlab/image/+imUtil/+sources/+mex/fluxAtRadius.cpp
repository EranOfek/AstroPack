/* fluxAtRadius.cpp
 *
 * Fast median annulus flux around many [X,Y] positions.
 *
 * Usage:
 *   MedAnnulusFlux = fluxAtRadius(Image, XY)
 *   MedAnnulusFlux = fluxAtRadius(Image, XY, Radii)
 *
 * Input:
 *   Image : 2D single or double image, MATLAB column-major.
 *   XY    : N x 2 double matrix, [X,Y], where X=column, Y=row.
 *   Radii : optional [InnerRadius OuterRadius], default [10 12].
 *
 * Output:
 *   MedAnnulusFlux : N x 1 single or double, same class as Image.
 *
 * Notes:
 *   - XY positions are rounded to the nearest pixel.
 *   - NaN and Inf image pixels are ignored.
 *   - Pixels with InnerRadius <= r <= OuterRadius are used.
 *
 * Compile:
 *   mex CXXFLAGS="$CXXFLAGS -O3 -march=native" fluxAtRadius.cpp
 *
 * With OpenMP on Linux:
 *   mex CXXFLAGS="$CXXFLAGS -O3 -march=native -fopenmp" ...
 *       LDFLAGS="$LDFLAGS -fopenmp" fluxAtRadius.cpp
 */

#include "mex.h"
#include <vector>
#include <algorithm>
#include <cmath>
#include <limits>
#include <cstdint>

#ifdef _OPENMP
#include <omp.h>
#endif

struct Offset {
    int Dx;
    int Dy;
};

static inline bool isFiniteDouble(double X)
{
    return std::isfinite(X);
}

template <typename T>
static inline bool isFiniteValue(T X)
{
    return std::isfinite(static_cast<double>(X));
}

template <typename T>
static inline T quietNaN()
{
    return std::numeric_limits<T>::quiet_NaN();
}

template <typename T>
static T medianInPlace(std::vector<T>& Values)
{
    const size_t N = Values.size();

    if (N == 0) {
        return quietNaN<T>();
    }

    const size_t K = N / 2;

    std::nth_element(Values.begin(), Values.begin() + K, Values.end());
    T Upper = Values[K];

    if (N % 2 == 1) {
        return Upper;
    } else {
        T Lower = *std::max_element(Values.begin(), Values.begin() + K);
        return static_cast<T>(0.5 * (static_cast<double>(Lower) + static_cast<double>(Upper)));
    }
}

static std::vector<Offset> buildAnnulusOffsets(double InnerRadius, double OuterRadius)
{
    const int BoxRadius = static_cast<int>(std::ceil(OuterRadius));

    const double InnerR2 = InnerRadius * InnerRadius;
    const double OuterR2 = OuterRadius * OuterRadius;

    std::vector<Offset> Offsets;
    Offsets.reserve((2 * BoxRadius + 1) * (2 * BoxRadius + 1));

    for (int Dy = -BoxRadius; Dy <= BoxRadius; ++Dy) {
        for (int Dx = -BoxRadius; Dx <= BoxRadius; ++Dx) {
            const double R2 = static_cast<double>(Dx * Dx + Dy * Dy);

            if (R2 >= InnerR2 && R2 <= OuterR2) {
                Offset O;
                O.Dx = Dx;
                O.Dy = Dy;
                Offsets.push_back(O);
            }
        }
    }

    return Offsets;
}

template <typename T>
void computeFluxAtRadius(const mxArray* ImageArray,
                         const mxArray* XYArray,
                         mxArray* OutputArray,
                         const std::vector<Offset>& Offsets)
{
    const T* Image = static_cast<const T*>(mxGetData(ImageArray));
    const double* XY = static_cast<const double*>(mxGetData(XYArray));
    T* Out = static_cast<T*>(mxGetData(OutputArray));

    const mwSize Ny = mxGetM(ImageArray);
    const mwSize Nx = mxGetN(ImageArray);

    const mwSize Npos = mxGetM(XYArray);

    const size_t Noffset = Offsets.size();

    #pragma omp parallel
    {
        std::vector<T> Values;
        Values.reserve(Noffset);

        #pragma omp for
        for (mwSignedIndex Ipos = 0; Ipos < static_cast<mwSignedIndex>(Npos); ++Ipos) {

            const double Xd = XY[Ipos];
            const double Yd = XY[Ipos + Npos];

            if (!isFiniteDouble(Xd) || !isFiniteDouble(Yd)) {
                Out[Ipos] = quietNaN<T>();
                continue;
            }

            const mwSignedIndex X0 = static_cast<mwSignedIndex>(std::llround(Xd)) - 1;
            const mwSignedIndex Y0 = static_cast<mwSignedIndex>(std::llround(Yd)) - 1;

            Values.clear();

            for (size_t Io = 0; Io < Noffset; ++Io) {
                const mwSignedIndex X = X0 + static_cast<mwSignedIndex>(Offsets[Io].Dx);
                const mwSignedIndex Y = Y0 + static_cast<mwSignedIndex>(Offsets[Io].Dy);

                if (X >= 0 && X < static_cast<mwSignedIndex>(Nx) &&
                    Y >= 0 && Y < static_cast<mwSignedIndex>(Ny)) {

                    const mwSize Ind = static_cast<mwSize>(Y) +
                                       static_cast<mwSize>(X) * Ny;

                    const T Val = Image[Ind];

                    if (isFiniteValue<T>(Val)) {
                        Values.push_back(Val);
                    }
                }
            }

            Out[Ipos] = medianInPlace<T>(Values);
        }
    }
}

void mexFunction(int Nout, mxArray* Out[],
                 int Nin, const mxArray* In[])
{
    if (Nin < 2 || Nin > 3) {
        mexErrMsgIdAndTxt("fluxAtRadius:BadNin",
                          "Usage: MedAnnulusFlux = fluxAtRadius(Image, XY, Radii)");
    }

    if (Nout > 1) {
        mexErrMsgIdAndTxt("fluxAtRadius:BadNout",
                          "Only one output argument is allowed.");
    }

    const mxArray* ImageArray = In[0];
    const mxArray* XYArray = In[1];

    if (!mxIsSingle(ImageArray) && !mxIsDouble(ImageArray)) {
        mexErrMsgIdAndTxt("fluxAtRadius:BadImageClass",
                          "Image must be single or double.");
    }

    if (mxIsComplex(ImageArray)) {
        mexErrMsgIdAndTxt("fluxAtRadius:ComplexImage",
                          "Image must be real.");
    }

    if (mxGetNumberOfDimensions(ImageArray) != 2) {
        mexErrMsgIdAndTxt("fluxAtRadius:BadImageDim",
                          "Image must be a 2D matrix.");
    }

    if (!mxIsDouble(XYArray) || mxIsComplex(XYArray)) {
        mexErrMsgIdAndTxt("fluxAtRadius:BadXY",
                          "XY must be a real double matrix.");
    }

    if (mxGetN(XYArray) != 2) {
        mexErrMsgIdAndTxt("fluxAtRadius:BadXYSize",
                          "XY must be an N x 2 matrix of [X,Y] positions.");
    }

    double InnerRadius = 10.0;
    double OuterRadius = 12.0;

    if (Nin >= 3) {
        const mxArray* RadiiArray = In[2];

        if (!mxIsDouble(RadiiArray) || mxIsComplex(RadiiArray) ||
            mxGetNumberOfElements(RadiiArray) != 2) {
            mexErrMsgIdAndTxt("fluxAtRadius:BadRadii",
                              "Radii must be a real double two-element vector [Inner Outer].");
        }

        const double* Radii = static_cast<const double*>(mxGetData(RadiiArray));
        InnerRadius = Radii[0];
        OuterRadius = Radii[1];
    }

    if (!std::isfinite(InnerRadius) || !std::isfinite(OuterRadius) ||
        InnerRadius < 0.0 || OuterRadius <= InnerRadius) {
        mexErrMsgIdAndTxt("fluxAtRadius:BadRadiusValues",
                          "Radii must satisfy 0 <= InnerRadius < OuterRadius.");
    }

    const std::vector<Offset> Offsets = buildAnnulusOffsets(InnerRadius, OuterRadius);

    if (Offsets.empty()) {
        mexErrMsgIdAndTxt("fluxAtRadius:EmptyAnnulus",
                          "No integer pixels found inside the requested annulus.");
    }

    const mwSize Npos = mxGetM(XYArray);

    if (mxIsSingle(ImageArray)) {
        Out[0] = mxCreateNumericMatrix(Npos, 1, mxSINGLE_CLASS, mxREAL);
        computeFluxAtRadius<float>(ImageArray, XYArray, Out[0], Offsets);
    } else {
        Out[0] = mxCreateNumericMatrix(Npos, 1, mxDOUBLE_CLASS, mxREAL);
        computeFluxAtRadius<double>(ImageArray, XYArray, Out[0], Offsets);
    }
}
