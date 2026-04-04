/* imrotate_sinc.cpp

   NewImage = imrotate_sinc(Image, Rotation)

   Image    : single or double, real, 2-D
   Rotation : scalar, degrees, counter-clockwise

   Output:
     - same size as Image
     - same class as Image

   Interpolation:
     - full separable sinc interpolation
     - zero outside the image (implicit zero extension)
     - inverse mapping
     - rotation about image center: ((Nx+1)/2, (Ny+1)/2)

   Warning:
     This is computationally expensive for large images.
*/

#include "mex.h"
#include <cmath>
#include <vector>

#ifdef _OPENMP
#include <omp.h>
#endif

#ifndef M_PI
#define M_PI 3.141592653589793238462643383279502884
#endif

inline double sinc1(double X)
{
    if (std::abs(X) < 1e-12) {
        return 1.0;
    }
    double Pix = M_PI * X;
    return std::sin(Pix) / Pix;
}

template <typename T>
void rotateSinc(const T* Image,
                T* NewImage,
                mwSize Ny,
                mwSize Nx,
                double RotationDeg)
{
    const double Theta = RotationDeg * (M_PI / 180.0);
    const double CosT  = std::cos(Theta);
    const double SinT  = std::sin(Theta);

    const double Cx = (static_cast<double>(Nx) + 1.0) * 0.5;
    const double Cy = (static_cast<double>(Ny) + 1.0) * 0.5;

    #ifdef _OPENMP
    #pragma omp parallel
    #endif
    {
        std::vector<double> Wx(Nx);
        std::vector<double> Wy(Ny);
        std::vector<double> RowSum(Ny);

        #ifdef _OPENMP
        #pragma omp for
        #endif
        for (mwSignedIndex Xo0 = 0; Xo0 < static_cast<mwSignedIndex>(Nx); ++Xo0) {

            const double Xo = static_cast<double>(Xo0) + 1.0;
            const double Dx = Xo - Cx;

            double Dy = 1.0 - Cy;
            double Xi = Cx + CosT * Dx + SinT * Dy;
            double Yi = Cy - SinT * Dx + CosT * Dy;

            T* OutCol = NewImage + static_cast<mwSize>(Xo0) * Ny;

            for (mwSize Yo0 = 0; Yo0 < Ny; ++Yo0) {

                for (mwSize X = 0; X < Nx; ++X) {
                    Wx[X] = sinc1(Xi - (static_cast<double>(X) + 1.0));
                }

                for (mwSize Y = 0; Y < Ny; ++Y) {
                    Wy[Y] = sinc1(Yi - (static_cast<double>(Y) + 1.0));
                }

                for (mwSize Y = 0; Y < Ny; ++Y) {
                    const T* ColPtr = Image + Y;
                    double SumX = 0.0;

                    for (mwSize X = 0; X < Nx; ++X) {
                        SumX += static_cast<double>(ColPtr[X * Ny]) * Wx[X];
                    }

                    RowSum[Y] = SumX;
                }

                double Value = 0.0;
                for (mwSize Y = 0; Y < Ny; ++Y) {
                    Value += RowSum[Y] * Wy[Y];
                }

                OutCol[Yo0] = static_cast<T>(Value);

                Xi += SinT;
                Yi += CosT;
            }
        }
    }
}

void mexFunction(int nlhs, mxArray* plhs[], int nrhs, const mxArray* prhs[])
{
    if (nrhs != 2) {
        mexErrMsgIdAndTxt("imrotate_sinc:InputCount",
                          "Usage: NewImage = imrotate_sinc(Image, Rotation)");
    }

    if (nlhs > 1) {
        mexErrMsgIdAndTxt("imrotate_sinc:OutputCount",
                          "One output only.");
    }

    const mxArray* Image = prhs[0];
    const mxArray* Rotation = prhs[1];

    if (mxIsComplex(Image) || mxGetNumberOfDimensions(Image) != 2) {
        mexErrMsgIdAndTxt("imrotate_sinc:InvalidImage",
                          "Image must be a real 2-D array.");
    }

    mxClassID ClassID = mxGetClassID(Image);
    if (!(ClassID == mxSINGLE_CLASS || ClassID == mxDOUBLE_CLASS)) {
        mexErrMsgIdAndTxt("imrotate_sinc:InvalidClass",
                          "Image must be single or double.");
    }

    if (!mxIsNumeric(Rotation) || mxIsComplex(Rotation) || mxGetNumberOfElements(Rotation) != 1) {
        mexErrMsgIdAndTxt("imrotate_sinc:InvalidRotation",
                          "Rotation must be a real numeric scalar.");
    }

    double RotationDeg = 0.0;
    if (mxIsSingle(Rotation)) {
        RotationDeg = static_cast<double>(*static_cast<const float*>(mxGetData(Rotation)));
    } else if (mxIsDouble(Rotation)) {
        RotationDeg = *static_cast<const double*>(mxGetData(Rotation));
    } else {
        RotationDeg = mxGetScalar(Rotation);
    }

    const mwSize* Size = mxGetDimensions(Image);
    const mwSize Ny = Size[0];
    const mwSize Nx = Size[1];

    plhs[0] = mxCreateNumericMatrix(Ny, Nx, ClassID, mxREAL);

    if (ClassID == mxSINGLE_CLASS) {
        const float* ImagePtr = static_cast<const float*>(mxGetData(Image));
        float* OutPtr = static_cast<float*>(mxGetData(plhs[0]));
        rotateSinc<float>(ImagePtr, OutPtr, Ny, Nx, RotationDeg);
    } else {
        const double* ImagePtr = static_cast<const double*>(mxGetData(Image));
        double* OutPtr = static_cast<double*>(mxGetData(plhs[0]));
        rotateSinc<double>(ImagePtr, OutPtr, Ny, Nx, RotationDeg);
    }
}
