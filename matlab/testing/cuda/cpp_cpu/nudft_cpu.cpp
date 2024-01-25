#include <iostream>
#include <cmath>
#include <chrono>
#include <fstream>
#include <sstream>

#ifndef M_PI
#define M_PI 3.14159265358979323846
#endif

using namespace std;

int main() {

    int NN[4] = { 10, 100, 1000, 10000 };
    int iters = 10;

    for (int iii = 0; iii < 4; ++iii) {
        double time_cpp = 0.0;

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

        for (int iter = 0; iter < iters; ++iter) {

            auto ttic = chrono::high_resolution_clock::now();

            double* iter_result_real = new double[N];
            double* iter_result_imag = new double[N];
            
            for (int i = 0; i < N; ++i) {
                double sum_real = 0.0;
                double sum_imag = 0.0;

                for (int j = 0; j < N; ++j) {
                    sum_real += m[j] * cos(-2 * M_PI * f[i] * t[j]);
                    sum_imag += m[j] * sin(-2 * M_PI * f[i] * t[j]);
                }

                iter_result_real[i] = sum_real / N;
                iter_result_imag[i] = sum_imag / N;
            }

            auto elapsed_time = chrono::high_resolution_clock::now() - ttic;
            time_cpp += chrono::duration_cast<chrono::microseconds>(elapsed_time).count() / 1e6;

            // Copy values to result arrays
            std::copy(iter_result_real, iter_result_real + N, result_real);
            std::copy(iter_result_imag, iter_result_imag + N, result_imag);

            delete[] iter_result_real;
            delete[] iter_result_imag;
        }

        double time_cpp_avg = time_cpp / iters;

        cout << "CPU :: For N=" << N << ", regular time=" << time_cpp_avg << endl;

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
