#include "cuda_runtime.h"
#include "device_launch_parameters.h"
#include <iostream>
#include <cmath>
#include <chrono>
#include <fstream>
#include <sstream>
#include <stdio.h>

#ifndef M_PI
#define M_PI 3.14159265358979323846
#endif

using namespace std;

__global__ void cuda_NDFT_gpu_2(const double* t, const double* m, const double* f,
    double* freq_result_real, double* freq_result_imag, int i, int N) {
    int tid = blockIdx.x * blockDim.x + threadIdx.x;

    if (tid < N) {
        freq_result_real[tid] = m[tid] * cos(-2 * M_PI * f[i] * t[tid]);
        freq_result_imag[tid] = m[tid] * sin(-2 * M_PI * f[i] * t[tid]);
    }
}

int main() {

    int NN[4] = { 10, 100, 1000, 10000 };
    int iters = 10;

    for (int iii = 0; iii < 4; ++iii) {
        double time_cuda = 0.0;

        int N = NN[iii];

        double* t = new double[N];
        double* m = new double[N];
        double* f = new double[N];

        double* result_real = new double[N];
        double* result_imag = new double[N];

        // Generate file name dynamically
        std::stringstream filename;
        filename << "ndft_input_" << N << ".csv";
        std::ifstream file(filename.str());

        if (!file.is_open()) {
            cout << "Error opening file: " << filename.str() << endl;
            return 1; // exit with an error code
        }

        std::string line;

        for (int i = 0; i < 3; ++i) {
            std::getline(file, line);
            std::istringstream iss(line);

            for (int j = 0; j < N; ++j) {
                std::string valueStr;
                std::getline(iss, valueStr, ',');

                // Convert the string to a double
                double value = std::stod(valueStr);

                if (i == 0) {
                    t[j] = value;
                }
                else if (i == 1) {
                    m[j] = value;
                }
                else if (i == 2) {
                    f[j] = value;
                }
            }
        }

        // Define grid and block sizes
        int blockSize = 256;
        int numBlocks = (N + blockSize - 1) / blockSize;

        for (int iter = 0; iter < iters; ++iter) {

            auto ttic = std::chrono::high_resolution_clock::now();

            double* iter_result_real = new double[N];
            double* iter_result_imag = new double[N];

            // Allocate memory on the GPU
            double* d_t, * d_m, * d_f;
            cudaMalloc(&d_t, N * sizeof(double));
            cudaMalloc(&d_m, N * sizeof(double));
            cudaMalloc(&d_f, N * sizeof(double));

            // Copy data from host to device
            cudaMemcpy(d_t, t, N * sizeof(double), cudaMemcpyHostToDevice);
            cudaMemcpy(d_m, m, N * sizeof(double), cudaMemcpyHostToDevice);
            cudaMemcpy(d_f, f, N * sizeof(double), cudaMemcpyHostToDevice);
            
            for (int i = 0; i < N; ++i) {

                double* freq_result_real = new double[N];
                double* freq_result_imag = new double[N];

                double* d_freq_result_real, * d_freq_result_imag;

                cudaMalloc(&d_freq_result_real, N * sizeof(double));
                cudaMalloc(&d_freq_result_imag, N * sizeof(double));

                double sum_real = 0.0;
                double sum_imag = 0.0;

                cuda_NDFT_gpu_2 << <numBlocks, blockSize >> > (d_t, d_m, d_f, d_freq_result_real, d_freq_result_imag, i, N);
                cudaDeviceSynchronize();

                cudaMemcpy(freq_result_real, d_freq_result_real, N * sizeof(double), cudaMemcpyDeviceToHost);
                cudaMemcpy(freq_result_imag, d_freq_result_imag, N * sizeof(double), cudaMemcpyDeviceToHost);

                for (int j = 0; j < N; ++j) {
                    sum_real += freq_result_real[j];
                    sum_imag += freq_result_imag[j];
                }

                iter_result_real[i] = sum_real / N;
                iter_result_imag[i] = sum_imag / N;

                auto elapsed_time = std::chrono::high_resolution_clock::now() - ttic;
                time_cuda += std::chrono::duration_cast<std::chrono::microseconds>(elapsed_time).count() / 1e6;

                delete[] freq_result_real;
                delete[] freq_result_imag;

                cudaFree(d_freq_result_real);
                cudaFree(d_freq_result_imag);
            }

            cudaFree(d_t);
            cudaFree(d_m);
            cudaFree(d_f);

            // Copy values to result arrays
            std::copy(iter_result_real, iter_result_real + N, result_real);
            std::copy(iter_result_imag, iter_result_imag + N, result_imag);

            delete[] iter_result_real;
            delete[] iter_result_imag;

        }

        double time_cuda_avg = time_cuda / iters;

        cout << "GPU :: For N=" << N << ", CUDA time=" << time_cuda_avg << endl;

        // Clean up
        delete[] t;
        delete[] m;
        delete[] f;

        // Write result to CSV file
        std::stringstream outputFilename;
        outputFilename << "ndft_cpp_cpu_output_" << N << ".csv";
        std::ofstream outputFile(outputFilename.str());

        for (int i = 0; i < N; ++i) {
            outputFile << result_real[i] << " + " << result_imag[i] << "i \n";
        }

        delete[] result_real;
        delete[] result_imag;

    }

    return 0;
}

