# 07_matlab_gpu_optimization.prompt.md

ROLE AND CONTEXT
This file defines how GPU acceleration is used in MATLAB.

GPU usage is optional and must never be required for correctness.

WHEN TO USE GPU
Use GPU only if:
- CPU implementation exists
- Profiling shows benefit
- Workload is large and data-parallel

ARCHITECTURE MODEL
- CPU handles control and logic
- GPU handles numeric kernels only

OPTIONAL BACKEND RULE
GPU usage must be:
- Explicit
- Configurable
- Detectable at runtime

DATA TRANSFER RULES
- Minimize CPU-GPU transfers
- Avoid transfers inside loops
- Keep data on GPU as long as possible

PRECISION AND NUMERICS
- Be explicit about precision
- CPU is the reference implementation
- Document numerical differences

TESTING
- Code must run with and without GPU
- Skip GPU paths gracefully if unavailable

WHAT NOT TO DO
- No GPU-only implementations
- No hidden gpuArray conversions

OUTPUT EXPECTATION FROM THE LLM
When adding GPU code:
- Keep it isolated
- Preserve CPU correctness path

END OF FILE
