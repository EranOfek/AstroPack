# https://gist.github.com/wolever/e894d3a956c15044b2e4708f5e9d204d

"""
A simple watchdog for long running processes which may stall for some reason or
another.
If the main thread hasn't logged progress (by updating
``self.last_progress_time``) in WATCHDOG_HARD_KILL_TIMEOUT, the watchdog
thread will log an error containing the stack trace of all currently running
threads then use ``kill -9`` to kill the main process.
Assumes that a process monitor like supervisor or systemd will then restart
the process.
"""

import os
import sys
import time
import logging
import threading

WATCHDOG_HARD_KILL_TIMEOUT = 90

log = logging.getLogger(__name__)

class MyLongRunningProcess(object):
    def run(self):
        self.shutdown = threading.Event()
        watchdog_thread = threading.Thread(target=self._watchdog, name="watchdog")
        try:
            watchdog_thread.start()
            self._run()
        finally:
            self.shutdown.set()
            watchdog_thread.join()
        return 0

    def _watchdog(self):
        self.last_progress_time = time.time()
        while True:
            if self.shutdown.wait(timeout=5):
                return
            last_progress_delay = time.time() - self.last_progress_time
            if last_progress_delay < WATCHDOG_HARD_KILL_TIMEOUT:
                continue
            try:
                stacks = self._get_thread_stack_traces()
                log.error(
                    "no progress in %0.01f seconds\n"
                    "kill -9 time...\n\n%s",
                    last_progress_delay, self.last_message, "\n\n".join(stacks),
                    extra={"thread_stacks": stacks},
                )
            except:
                pass
            # Hopefully give logs some time to flush
            time.sleep(1)
            os.kill(0, 9)

    def _run(self):
        while True:
            self.last_progress_time = time.time()
            do_some_thing()


if __name__ == "__main__":
    sys.exit(MyLongRunningProcess().run())