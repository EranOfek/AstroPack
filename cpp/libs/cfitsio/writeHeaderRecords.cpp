#include "fitsio.h"
#include <stdio.h>

void writeHeaderRecords(fitsfile *fptr, char headerRecords[][81], int numRecords) {
    int status = 0;
    for (int i = 0; i < numRecords; ++i) {
        if (fits_write_record(fptr, headerRecords[i], &status)) {
            fits_report_error(stderr, status); // Handle error appropriately
            break;
        }
    }
}

int main() {
    fitsfile *fptr;
    int status = 0;

    // Your FITS file path
    char fileName[] = "!newfits.fits"; 

    // Open or create a FITS file
    if (fits_create_file(&fptr, fileName, &status)) {
        fits_report_error(stderr, status); // Handle error appropriately
        return status;
    }

    // Create an example image
    long naxes[2] = {300, 200};
    if (fits_create_img(fptr, SHORT_IMG, 2, naxes, &status)) {
        fits_report_error(stderr, status); // Handle error appropriately
        fits_close_file(fptr, &status);
        return status;
    }

    // Example header records
    char headerRecords[][81] = {
        "SIMPLE  =                    T / file does conform to FITS standard",
        "BITPIX  =                   16 / number of bits per data pixel",
        "NAXIS   =                    2 / number of data axes",
        "NAXIS1  =                  300 / length of data axis 1",
        "NAXIS2  =                  200 / length of data axis 2",
        // Add more header records as needed
    };
    int numRecords = sizeof(headerRecords) / sizeof(headerRecords[0]);

    // Write header records
    writeHeaderRecords(fptr, headerRecords, numRecords);

    // Close the FITS file
    if (fits_close_file(fptr, &status)) {
        fits_report_error(stderr, status); // Handle error appropriately
        return status;
    }

    printf("FITS file '%s' created and header records written.\n", fileName);

    return 0;
}


