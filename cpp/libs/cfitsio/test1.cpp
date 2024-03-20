
#include "fitsio.h"
#include <iostream>
#include <vector>
#include <chrono>
#include <string>

void createAndWriteFits(const char* filename) {
    fitsfile* fptr;
    int status = 0; // CFITSIO status value must be initialized to zero.
    long naxes[2] = {1700, 1700}; // Image size

    // Create new FITS file
    if (fits_create_file(&fptr, filename, &status)) {
        fits_report_error(stderr, status);
        return;
    }

    // Create the primary array image (2D image)
    if (fits_create_img(fptr, SHORT_IMG, 2, naxes, &status)) {
        fits_report_error(stderr, status);
        fits_close_file(fptr, &status);
        return;
    }

    // Add 200 arbitrary header fields
    for (int i = 1; i <= 200; ++i) {
        std::string key = "KEY" + std::to_string(i);
        std::string value = "Value" + std::to_string(i);
        if (fits_write_key(fptr, TSTRING, key.c_str(), (void*)value.c_str(), nullptr, &status)) {
            fits_report_error(stderr, status);
            fits_close_file(fptr, &status);
            return;
        }
    }

    // Generate and write image data
    long nelements = naxes[0] * naxes[1];
    std::vector<short> array(nelements, 0); // Example data

    // Fill the array with some data
    for (long i = 0; i < nelements; ++i) {
        array[i] = static_cast<short>(i % 32768); // Example pattern
    }

    // Write the array to the FITS file
    if (fits_write_img(fptr, TSHORT, 1, nelements, array.data(), &status)) {
        fits_report_error(stderr, status);
        fits_close_file(fptr, &status);
        return;
    }

    // Close the FITS file
    if (fits_close_file(fptr, &status)) {
        fits_report_error(stderr, status);
        return;
    }
}

int main() {
    const int iterations = 10;
    std::vector<double> times;

    for (int i = 0; i < iterations; ++i) {
        auto start = std::chrono::high_resolution_clock::now();

        char filename[50];
        sprintf(filename, "!temp_fits_file_%d.fits", i); // Overwrite if exists
        createAndWriteFits(filename);

        auto end = std::chrono::high_resolution_clock::now();
        std::chrono::duration<double> diff = end - start;
        times.push_back(diff.count());
    }

    double total = 0;
    for (double time : times) {
        total += time;
    }

    std::cout << "Average write time over " << iterations << " iterations: "
              << total / iterations << " seconds\n";

    return 0;
}


