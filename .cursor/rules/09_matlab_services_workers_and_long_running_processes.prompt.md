# 03_matlab_services_workers_and_long_running_processes.prompt.md

ROLE AND CONTEXT
This file defines how long-running MATLAB services and workers
are structured inside AstroPack.

These are not simple scripts, but operational components.

WHAT IS A MATLAB SERVICE
A MATLAB service or worker is a long-running process that:
- Waits for input (often via files)
- Performs computation
- Produces output
- Continues running

Examples:
- snr_service
- slew_service
- too_service
- incoming_alerts_filter

SERVICE STRUCTURE
A service is typically composed of:
- A manager or worker class
- Supporting helper functions
- Configuration handling
- Explicit start and stop logic

SERVICES ARE NOT
- GUI applications
- One-off scripts
- Interactive tools

STATE AND LIFECYCLE
- Services may hold state in memory
- State must be explicit and controlled
- Services must survive bad inputs

ERROR HANDLING
- Never crash on a single bad request
- Log or record errors explicitly
- Continue running after recoverable failures

FILE AND DATA FLOW
- Services often interact via file-based IPC
- File handling must be robust and defensive
- Never assume ordering or completeness

TESTING AND DEBUGGING
- Core logic must be testable without running the service
- Service loops should be thin wrappers around testable logic

WHAT NOT TO DO
- No UI code in services
- No blocking forever without escape
- No hidden infinite loops
- No hardcoded paths or credentials

OUTPUT EXPECTATION FROM THE LLM
When writing or modifying services:
- Separate loop control from computation
- Favor boring, explicit code
- Assume the service will run for months

END OF FILE
