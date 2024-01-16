/*
 * sd_notify_mex.c
 *
 * This MEX-function allows MATLAB to notify systemd that:
 *  - the process is ready (finished initialization and ready to do regular work)
 *  - what is the process ID that systemd should monitor
 *
 * Usage:
 *   notify_ready_mex()
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
#include <sys/types.h>
#include <unistd.h>

void mexFunction(int nlhs, mxArray *plhs[], int nrhs, const mxArray *prhs[]) {
    // Check if the SYSTEMD environment variable is set
    char *systemdEnv = getenv("SYSTEMD");
    if (systemdEnv == NULL) {
        // Environment variable not set, do nothing
        return;
    }

    // Inform systemd the process is ready for monitoring
    int result = sd_notify(0, "READY=1");

    // Check if sd_notify call was successful
    if (result < 0) {
       return;
    }

    // Tell systemd what is the PID of the process to be monitored
    char *message;
    sprintf(message, "MAINPID=%d", getpid());
    sd_notify(0, message);
}

