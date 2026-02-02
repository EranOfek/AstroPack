# 05_matlab_mex_optimization.prompt.md

ROLE AND CONTEXT
This file defines how MEX files are used for performance optimization.

MEX files are a controlled escape hatch, not a default approach.

WHEN TO USE MEX
Use MEX only if:
- A correct MATLAB implementation exists
- Profiling shows a real bottleneck
- Vectorization is insufficient

WHAT BELONGS IN MEX
- Pure numeric computation
- Tight loops
- Deterministic kernels

WHAT DOES NOT BELONG IN MEX
- File I O
- Logging
- Configuration
- GUI logic
- System orchestration

ARCHITECTURE MODEL
- MATLAB orchestrates
- MEX computes

MATLAB calls a wrapper function, not the MEX directly.

FOLDER STRUCTURE
MEX sources live under:
astropack/MatlabMex

MATLAB wrappers live under:
astropack/Matlab

ERROR HANDLING
- Validate inputs defensively
- Use mexErrMsgIdAndTxt on error
- Never crash MATLAB

TESTING
- MATLAB implementation is the reference
- Compare outputs with defined tolerances

OUTPUT EXPECTATION FROM THE LLM
When writing MEX-related code:
- Always preserve the MATLAB reference path
- Document why MEX is justified

END OF FILE
