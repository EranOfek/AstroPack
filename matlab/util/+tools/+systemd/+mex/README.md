Enables the MATLAB process to comunicate with the system daemon

* `notify_ready`: informs the system daemon that
  * the process is ready for monitoring 
  * what the process ID should be monitored

* `notify_watchdog`: should be called periodically to inform the system daemon that the process is alive

NOTES:
* To compile use: `mex <file> -lsystemd`

