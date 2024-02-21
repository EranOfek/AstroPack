#include "cuda_runtime.h"
#include "device_launch_parameters.h"
#include <iostream>
#include <cmath>
#include <chrono>
#include <fstream>
#include <sstream>
#include <stdio.h>
#include <iomanip>

#ifndef M_PI
#define M_PI 3.14159265358979323846
#endif

using namespace std;

__global__ void cuda_NDFT_gpu(const double* t, const double* m, const double* f,
    double* iter_result_real, double* iter_result_imag, int N) {
    int tid = blockIdx.x * blockDim.x + threadIdx.x;

    if (tid < N) {
        double sum_real = 0.0;
        double sum_imag = 0.0;

        for (int j = 0; j < N; ++j) {
            sum_real += m[j] * cos(-2 * M_PI * f[tid] * t[j]);
            sum_imag += m[j] * sin(-2 * M_PI * f[tid] * t[j]);
        }

        iter_result_real[tid] = sum_real / N;
        iter_result_imag[tid] = sum_imag / N;
    }
}

int main() {

    int NN[4] = { 10, 100, 1000, 10000 };
    int iters = 100;

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
        filename << "../inputs/ndft_input_" << N << ".csv";
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
        int blockSize = 16;
        int numBlocks = (N + blockSize - 1) / blockSize;

        for (int iter = 0; iter < iters; ++iter) {

            auto ttic = std::chrono::high_resolution_clock::now();

            double* iter_result_real = new double[N];
            double* iter_result_imag = new double[N];

            // Allocate memory on the GPU
            double* d_t, * d_m, * d_f, * d_iter_result_real, * d_iter_result_imag;
            cudaMalloc(&d_t, N * sizeof(double));
            cudaMalloc(&d_m, N * sizeof(double));
            cudaMalloc(&d_f, N * sizeof(double));
            cudaMalloc(&d_iter_result_real, N * sizeof(double));
            cudaMalloc(&d_iter_result_imag, N * sizeof(double));

            // Copy data from host to device
            cudaMemcpy(d_t, t, N * sizeof(double), cudaMemcpyHostToDevice);
            cudaMemcpy(d_m, m, N * sizeof(double), cudaMemcpyHostToDevice);
            cudaMemcpy(d_f, f, N * sizeof(double), cudaMemcpyHostToDevice);

            cuda_NDFT_gpu << <numBlocks, blockSize >> > (d_t, d_m, d_f, d_iter_result_real, d_iter_result_imag, N);
            cudaDeviceSynchronize();

            cudaMemcpy(iter_result_real, d_iter_result_real, N * sizeof(double), cudaMemcpyDeviceToHost);
            cudaMemcpy(iter_result_imag, d_iter_result_imag, N * sizeof(double), cudaMemcpyDeviceToHost);

            auto elapsed_time = std::chrono::high_resolution_clock::now() - ttic;
            time_cuda += std::chrono::duration_cast<std::chrono::microseconds>(elapsed_time).count() / 1e6;

            cudaFree(d_t);
            cudaFree(d_m);
            cudaFree(d_f);
            cudaFree(d_iter_result_real);
            cudaFree(d_iter_result_imag);

            // Copy values to result arrays
            std::copy(iter_result_real, iter_result_real + N, result_real);
            std::copy(iter_result_imag, iter_result_imag + N, result_imag);

            delete[] iter_result_real;
            delete[] iter_result_imag;
        }

        double time_cuda_avg = time_cuda / iters;

        cout << "GPU :: For N=" << N << ", CUDA time=" << scientific << setprecision(6) <<  time_cuda_avg << endl;

        // Clean up
        delete[] t;
        delete[] m;
        delete[] f;

        // Write result to CSV file
        std::stringstream outputFilename;
        outputFilename << "ndft_cpp_gpu_output_" << N << ".csv";
        std::ofstream outputFile(outputFilename.str());


	if (outputFile.is_open()) {
        // Write complex numbers to the file
	        for (int i = 0; i < N; ++i) {
        	    outputFile << result_real[i];
		    
		    if (result_imag[i] >= 0) {
		    	outputFile << "+";		
		    } else {
			outputFile << "-";
		    }

		   outputFile << std::abs(result_imag[i]) << "i\n";
		
	        }

        	// Close the file
        	outputFile.close();
        }

        delete[] result_real;
        delete[] result_imag;


        std::string tfilename = "output_times.csv";

        if (iii == 0) {

                std::ofstream outFile(tfilename);
                if (!outFile.is_open()) {
                        std::cerr << "Error opening file for writing headers" << std::endl;
                        return 1; // Return an error code
                }

                outFile << "N, time" << std::endl;

        }

        std::ofstream outFile(tfilename, std::ios_base::app);
        if (!outFile.is_open()) {
                std::cerr << "Error opening file for appending values" << std::endl;
                return 1; // Return an error code
        }

        outFile << N << ", " << std::scientific << std::setprecision(6) << time_cuda_avg << std::endl;


    }

    return 0;
}

