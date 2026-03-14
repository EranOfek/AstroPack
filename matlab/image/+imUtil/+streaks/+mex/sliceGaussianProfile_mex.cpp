#include <mex.h>
#include <cmath>
#include <algorithm>
#include <cstring>
#include <vector>

/*
 * MEX function for fast Gaussian profile fitting
 * Input: X1, X2, x, y, W, slice_width, rthreshold, medianclip
 * Output: [C, goodindices]
 */

// Gaussian model: A * exp(-((D - mu)^2) / (2 * sigma^2))
inline double gaussianModel(double A, double mu, double sigma, double D) {
    double diff = D - mu;
    return A * std::exp(-(diff * diff) / (2.0 * sigma * sigma));
}

// Levenberg-Marquardt style least squares for 3-parameter Gaussian
void fitGaussian3Param(const double* D, const double* W, int n,
                       double slice_width,
                       double& A, double& mu, double& sigma, double& rsquare) {
    if (n < 3) {
        rsquare = 0.0;
        A = std::nan("");
        mu = std::nan("");
        sigma = std::nan("");
        return;
    }
    
    // Initial parameters
    double W_max = *std::max_element(W, W + n);
    double W_sum = 0.0;
    double D_sum = 0.0;
    double D2_sum = 0.0;
    
    for (int i = 0; i < n; i++) {
        W_sum += W[i];
        D_sum += D[i] * W[i];
        D2_sum += D[i] * D[i] * W[i];
    }
    
    if (W_sum < 1e-10) {
        rsquare = 0.0;
        A = std::nan("");
        mu = std::nan("");
        sigma = std::nan("");
        return;
    }
    
    mu = D_sum / W_sum;
    double variance = (D2_sum / W_sum) - (mu * mu);
    sigma = std::sqrt(std::abs(variance) + 1e-6);
    A = W_max;
    
    // Enforce bounds during initialization
    mu = std::max(-slice_width, std::min(slice_width, mu));
    sigma = std::max(1e-6, std::min(slice_width, sigma));
    A = std::max(0.0, A);
    
    // Levenberg-Marquardt iterations
    double lambda = 0.1;
    
    for (int iter = 0; iter < 10; iter++) {
        // Compute residuals and Jacobian
        std::vector<double> residuals(n);
        std::vector<double> jac_A(n), jac_mu(n), jac_sigma(n);
        
        double SS_res = 0.0;
        
        for (int i = 0; i < n; i++) {
            double diff = D[i] - mu;
            double sigma2 = sigma * sigma;
            double exp_term = std::exp(-(diff * diff) / (2.0 * sigma2));
            double model = A * exp_term;
            
            residuals[i] = W[i] - model;
            SS_res += residuals[i] * residuals[i];
            
            // Jacobian entries (derivatives of model w.r.t. parameters)
            jac_A[i] = exp_term;
            jac_mu[i] = A * (diff / sigma2) * exp_term;
            jac_sigma[i] = A * (diff * diff / (sigma2 * sigma)) * exp_term;
        }
        
        // Compute Hessian approximation: J^T * J
        double H_AA = 0, H_mm = 0, H_ss = 0;
        double H_Am = 0, H_As = 0, H_ms = 0;
        double g_A = 0, g_mu = 0, g_sigma = 0;
        
        for (int i = 0; i < n; i++) {
            H_AA += jac_A[i] * jac_A[i];
            H_mm += jac_mu[i] * jac_mu[i];
            H_ss += jac_sigma[i] * jac_sigma[i];
            H_Am += jac_A[i] * jac_mu[i];
            H_As += jac_A[i] * jac_sigma[i];
            H_ms += jac_mu[i] * jac_sigma[i];
            
            g_A += jac_A[i] * residuals[i];
            g_mu += jac_mu[i] * residuals[i];
            g_sigma += jac_sigma[i] * residuals[i];
        }
        
        // Levenberg-Marquardt damping
        H_AA += lambda;
        H_mm += lambda;
        H_ss += lambda;
        
        // Solve 3x3 system using Cramer's rule or simple elimination
        double det = H_AA * (H_mm * H_ss - H_ms * H_ms) - 
                     H_Am * (H_Am * H_ss - H_ms * H_As) + 
                     H_As * (H_Am * H_ms - H_mm * H_As);
        
        if (std::abs(det) < 1e-15) break;  // Singular, stop iteration
        
        double dA = (g_A * (H_mm * H_ss - H_ms * H_ms) - 
                     g_mu * (H_Am * H_ss - H_As * H_ms) + 
                     g_sigma * (H_Am * H_ms - H_mm * H_As)) / det;
        
        double dmu = (H_AA * (g_mu * H_ss - g_sigma * H_ms) - 
                      H_Am * (g_A * H_ss - g_sigma * H_As) + 
                      H_As * (g_A * H_ms - g_mu * H_As)) / det;
        
        double dsigma = (H_AA * (H_mm * g_sigma - H_ms * g_mu) - 
                         H_Am * (H_Am * g_sigma - H_ms * g_A) + 
                         H_As * (H_Am * g_mu - H_mm * g_A)) / det;
        
        // Update with bounds
        double A_new = A + dA;
        double mu_new = mu + dmu;
        double sigma_new = sigma + dsigma;
        
        A_new = std::max(0.0, A_new);
        mu_new = std::max(-slice_width, std::min(slice_width, mu_new));
        sigma_new = std::max(1e-6, std::min(slice_width, sigma_new));
        
        // Check for convergence
        if (std::abs(dA) < 1e-8 && std::abs(dmu) < 1e-8 && std::abs(dsigma) < 1e-8) {
            A = A_new;
            mu = mu_new;
            sigma = sigma_new;
            break;
        }
        
        A = A_new;
        mu = mu_new;
        sigma = sigma_new;
        
        // Adjust lambda
        if (iter < 5) lambda *= 0.1;
    }
    
    // Compute final R-squared
    double W_mean = 0.0;
    for (int i = 0; i < n; i++) W_mean += W[i];
    W_mean /= n;
    
    double SS_res_final = 0.0;
    double SS_tot = 0.0;
    
    for (int i = 0; i < n; i++) {
        double W_fit = gaussianModel(A, mu, sigma, D[i]);
        double residual = W[i] - W_fit;
        SS_res_final += residual * residual;
        SS_tot += (W[i] - W_mean) * (W[i] - W_mean);
    }
    
    if (SS_tot > 1e-10) {
        rsquare = 1.0 - (SS_res_final / SS_tot);
    } else {
        rsquare = 0.0;
    }
}

// Compute median of array (for medianclip)
double computeMedian(double* arr, int n) {
    if (n == 0) return 0.0;
    if (n == 1) return arr[0];
    
    // Simple partition for median (not fully sorted, just find median element)
    double* temp = new double[n];
    std::copy(arr, arr + n, temp);
    std::nth_element(temp, temp + n/2, temp + n);
    double median = temp[n/2];
    delete[] temp;
    return median;
}


void mexFunction(int nlhs, mxArray* plhs[], 
                 int nrhs, const mxArray* prhs[]) {
    
    if (nrhs != 8) {
        mexErrMsgIdAndTxt("sliceGaussianProfile_mex:nrhs", 
                         "Exactly 8 inputs required");
    }
    if (nlhs != 2) {
        mexErrMsgIdAndTxt("sliceGaussianProfile_mex:nlhs", 
                         "Exactly 2 outputs required");
    }
    
    // Input: X1, X2
    double* X1 = (double*)mxGetPr(prhs[0]);
    double* X2 = (double*)mxGetPr(prhs[1]);
    
    // Input: x, y, W
    double* x = (double*)mxGetPr(prhs[2]);
    mwSize n = std::max(mxGetM(prhs[2]), mxGetN(prhs[2]));
    double* y = (double*)mxGetPr(prhs[3]);
    double* W = (double*)mxGetPr(prhs[4]);
    
    // Input: slice_width, rthreshold, medianclip
    double slice_width = mxGetScalar(prhs[5]);
    double rthreshold = mxGetScalar(prhs[6]);
    double medianclip = mxGetScalar(prhs[7]);
    
    // Compute L and coordinate transforms
    double L = std::sqrt((X2[0] - X1[0]) * (X2[0] - X1[0]) + 
                        (X2[1] - X1[1]) * (X2[1] - X1[1]));
    
    double L2 = L * L;
    double dX = X2[0] - X1[0];
    double dY = X2[1] - X1[1];
    
    // Allocate T and D
    std::vector<double> T(n), D(n);
    
    for (mwSize i = 0; i < n; i++) {
        T[i] = ((dX * (x[i] - X1[0]) + dY * (y[i] - X1[1])) / L2);
        D[i] = ((dX * (y[i] - X1[1]) - dY * (x[i] - X1[0])) / L);
    }
    
    // Number of slices
    int M = (int)std::ceil(L / slice_width);
    
    double* A_series = new double[M];
    double* sigma_series = new double[M];
    
    // Output 1: C (4xM)
    plhs[0] = mxCreateDoubleMatrix(4, M, mxREAL);
    double* C = (double*)mxGetPr(plhs[0]);
    
    // Initialize C with NaN
    double nan_val = std::nan("");
    for (int i = 0; i < 4 * M; i++) {
        C[i] = nan_val;
    }
    
    // Output 2: goodindices (1xN logical)
    plhs[1] = mxCreateLogicalMatrix(1, n);
    mxLogical* goodindices = (mxLogical*)mxGetData(plhs[1]);
    
    for (mwSize i = 0; i < n; i++) {
        goodindices[i] = true;
    }
    
    // Main fitting loop
    for (int slice = 0; slice < M; slice++) {
        double t_lower = (double)slice / M;
        double t_upper = (double)(slice + 1) / M;
        
        // Extract slice data
        std::vector<double> D_slice, W_slice;
        std::vector<int> indices;
        
        for (mwSize i = 0; i < n; i++) {
            if (T[i] > t_lower && T[i] <= t_upper) {
                D_slice.push_back(D[i]);
                W_slice.push_back(W[i]);
                indices.push_back(i);
            }
        }
        
        int count = D_slice.size();
        
        // Fit Gaussian
        double A, mu, sigma, rsquare;
        fitGaussian3Param(D_slice.data(), W_slice.data(), count, slice_width, 
                         A, mu, sigma, rsquare);
        
        // Store results
        C[slice * 4 + 0] = A;
        C[slice * 4 + 1] = mu;
        C[slice * 4 + 2] = sigma;
        C[slice * 4 + 3] = rsquare;
        
        A_series[slice] = A;
        sigma_series[slice] = sigma;
        }
        
    // median clipping and discarding low R loop
    
    double A_median = computeMedian(A_series, M);
    double sigma_median = computeMedian(sigma_series, M);
    
    double A, mu, sigma, rsquare;

    for (int slice = 0; slice < M; slice++) {

        double t_lower = (double)slice / M;
        double t_upper = (double)(slice + 1) / M;
        
        // Extract slice data
        std::vector<int> indices;
        
        for (mwSize i = 0; i < n; i++) {
            if (T[i] > t_lower && T[i] <= t_upper) {
                 indices.push_back(i);
            }
        }

        A = C[slice * 4 + 0];
        mu = C[slice * 4 + 1];
        sigma = C[slice * 4 + 2];
        rsquare = C[slice * 4 + 3];

        // Set goodindices
        if (rsquare < rthreshold || A > medianclip*A_median || sigma > medianclip*sigma_median) {
            for (int idx : indices) {
                goodindices[idx] = false;
            }
            C[slice*4] = nan_val;
            C[slice*4 + 1] = nan_val;
            C[slice*4 + 2] = nan_val;
        }
    }
}
