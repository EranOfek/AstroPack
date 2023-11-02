"""
Python Watchdog Script Overview:

The watchdog script monitors a specified file for timestamp updates
written by a MATLAB process. If the timestamp in the file is not updated
within a specified timeout, the watchdog script terminates the MATLAB
process. The script logs its activities to both the console and a text
log file. Functions: 1. log(message)

Purpose: Logs a message to both the console and a designated log file
(watchdog_logs.txt).

Parameters: message (type: str): The message to be logged.

Usage:

python

log("This is a sample log message.")

2. watchdog(watchdog_filename, watchdog_timeout, grace_time)

Purpose: Monitors a file for timestamp updates. If the timestamp is not
updated within a given timeout, the associated MATLAB process is
terminated.

Parameters: watchdog_filename (type: str): Path to the file where the
MATLAB process writes its timestamp. watchdog_timeout (type: int): Time
in seconds after which, if the timestamp is not updated, the MATLAB
process is terminated. grace_time (type: int): Initial waiting period in
seconds before the watchdog starts monitoring the file.

Usage:

python

watchdog('watchdog_file.txt', 60, 10)

Constants:

LOG_FILENAME: Specifies the name of the file where log messages are
written. Default value is 'watchdog_logs.txt'.

Execution:

To run the script, execute it as a regular Python script. Ensure the
required parameters (watchdog_filename, watchdog_timeout, and
grace_time) are set appropriately. Notes:

The script assumes the MATLAB process writes its process ID (PID) as the
first line in the watchdog_filename file, followed by the timestamp.
The log file (watchdog_logs.txt) will be created in the same directory
as the script. Ensure you have write permissions in this directory.
Log messages in the log file are prefixed with a timestamp for easy
tracking.



"""
import time
import datetime
import os
import signal
import psutil

LOG_FILENAME = 'watchdog_monitor.log'


def log(message):
    # Print to console
    print(message)

    # Write to log file
    with open(LOG_FILENAME, 'a') as log_file:
        log_file.write(f"{datetime.datetime.now()} - {message}\n")


def get_matlab_pid():
    """
    Retrieve the process ID (PID) of the MATLAB process.

    Returns:
        int: PID of the MATLAB process or None if not found.
    """
    for process in psutil.process_iter(attrs=["pid", "name"]):
        if process.info["name"] and "MATLAB" in process.info["name"]:
            return process.info["pid"]
    return None


def is_process_running(pid):
    """Check if a process with the given PID is running."""
    try:
        process = psutil.Process(pid)
        return True
    except psutil.NoSuchProcess:
        return False


def watchdog_monitor(watchdog_filename, watchdog_timeout, grace_time):
    """
    Watchdog monitor - Blocking function!

    :param watchdog_filename:
    :param watchdog_timeout:
    :param grace_time:
    :return:
    """
    log(f'watchdog_monitor started: {watchdog_filename}')

    # Initial grace period
    time.sleep(grace_time)

    while True:
        try:
            las_time = None
            with open(watchdog_filename, 'r') as f:
                last_time_str = f.readline().strip()
                last_time = datetime.datetime.strptime(last_time_str, '%Y-%m-%d %H:%M:%S')

                # Get the PID from the file
                pid = int(f.readline().strip())

            if last_time:
                elapsed_time = (datetime.datetime.utcnow() - last_time).total_seconds()

                if elapsed_time > watchdog_timeout:
                    if is_process_running(pid):
                        log(f"Killing process with PID {pid} due to timeout")
                        os.kill(pid, signal.SIGTERM)
                        time.sleep(1)
                        if is_process_running(pid):
                            log(f"Process was NOT killed: {pid}")
                        else:
                            log(f"Killing killed ok: {pid}")
                    else:
                        log(f"Process with PID {pid} is not running")

                    # Remove watchdog file, to make sure that we will not try to kill it again before it is reloaded
                    os.remove(watchdog_filename)

        except Exception as ex:
            log(f'{ex}')

        # Sleep for a short duration before checking again
        time.sleep(5)


if __name__ == '__main__':
    # Example usage
    watchdog_monitor('c:/soc/snr/snr_matlab_watchdog.txt', 120, 60)
