# 10_matlab_file_based_ipc_with_python.prompt.md

ROLE AND CONTEXT
This file defines how MATLAB communicates with Python FastAPI services using file-based IPC.

This pattern is heavily used in AstroPack for:
- SNR calculation
- Slew calculation
- TOO planner
- Other heavy numerical workers

COMMUNICATION MODEL
MATLAB and Python never communicate directly via sockets or REST.

Flow:
1. Python writes request files to a shared folder
2. MATLAB worker detects new files
3. MATLAB processes the request
4. MATLAB writes a response file
5. Python consumes the response

FOLDER STRUCTURE
Each service owns a dedicated directory:
shared/<service>/requests
shared/<service>/responses

MATLAB WORKER DESIGN
- Implemented as a handle class
- Long-running process
- Polls or watches a folder
- Processes files one by one or in batches

FILE HANDLING RULES
- Never modify request files in place
- Mark processed files by rename or move
- Assume files may arrive incomplete
- Never assume ordering

FILE FORMATS
- JSON for control and metadata
- MAT files allowed for large numeric payloads
- Filenames must be unique and deterministic

ERROR HANDLING
- Never crash the worker on bad input
- Write an error response file on failure
- Include error message and timestamp

SEPARATION OF CONCERNS
- File I O logic is separate from computation
- Core algorithms must be callable without file system

WHAT NOT TO DO
- No blocking forever on a single file
- No hardcoded paths
- No GUI code in workers

OUTPUT EXPECTATION FROM THE LLM
When writing IPC-related code:
- Separate file handling from math
- Keep workers robust and boring
- Prefer explicit state transitions

END OF FILE
