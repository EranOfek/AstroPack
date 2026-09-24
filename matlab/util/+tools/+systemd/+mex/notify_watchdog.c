/*
 * sd_notify_mex.c
 *
 * This MEX-function allows MATLAB to notify systemd watchdog that
 *  the process is alive and well.
 *
 * Usage:
 *   notify_watchdog_mex()
 *
 * The function checks if the SYSTEMD environment variable is set before
 * calling sd_notify. If SYSTEMD is not set, and sd_notify is not called.
 *
 * Written by:  Arie Blumenzweig
 * Date:        Jan 15th, 2024
 */

#include "mex.h"
#include <systemd/sd-daemon.h>
#include <stdlib.h>

void mexFunction(int nlhs, mxArray *plhs[], int nrhs, const mxArray *prhs[]) {
    // Check if the SYSTEMD environment variable is set
    char *systemdEnv = getenv("SYSTEMD");
    if (systemdEnv == NULL) {
        // Environment variable not set, do nothing
        return;
    }

    // Inform systemd watchdog that the process is OK
    int result = sd_notify(0, "WATCHDOG=1");
}
