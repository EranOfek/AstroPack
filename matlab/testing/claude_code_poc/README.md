
You are an expert C++ MATLAB MEX developer. I'm on Windows with MATLAB R2023a.
My tools/compilers may be in non-standard locations — you must DISCOVER them, not assume paths.

## GOAL
Build a POC MEX function + MATLAB wrapper + test script, developing iteratively with
compile → test → fix cycles so I can see exactly how you work through problems.

---

## PHASE 0 — Environment Discovery (do this first, before writing any code)

Run these MATLAB commands via the system shell (use `matlab -batch "..."`) to discover
the environment. Show me the full output of each:

1. `matlab -batch "mex -setup C++; disp(mex.getCompilerConfigurations('C++','Selected'))"` 
   — find the active C++ compiler
2. `matlab -batch "disp(mexext)"` 
   — confirm MEX extension (should be mexw64)
3. `matlab -batch "disp(matlabroot)"` 
   — find MATLAB root
4. `matlab -batch "ver"` 
   — confirm version
5. Check for compilers in common Windows locations:
   - `where cl.exe` (MSVC)
   - `where g++.exe` (MinGW)
   - Check `C:\mingw64`, `C:\mingw-w64`, `C:\TDM-GCC*` directories
   - Check Visual Studio: `C:\Program Files\Microsoft Visual Studio\*\*\VC\Tools\MSVC`
6. Report exactly which compiler MATLAB will use and its full path.

If no compiler is configured, walk me through the fix BEFORE proceeding.

---

## PHASE 1 — Write the POC MEX + Wrapper + Tests

After environment is confirmed, create this file structure in a new folder `mex_poc\`:


mex_poc/
src/
array_stats.cpp      ← the MEX C++ source
matlab/
array_stats_wrapper.m  ← MATLAB wrapper function
test_array_stats.m     ← test script
build/
build_mex.m          ← build script


### What the MEX function does (array_stats):
- Input: a 1D or 2D double array
- Outputs: [mean, std_dev, min_val, max_val, element_count]
- Must validate inputs with proper mexErrMsgIdAndTxt error messages
- Must handle edge cases: empty array, single element, NaN values

### array_stats.cpp requirements:
- Use mex.h (not matrix.h directly)
- Input validation: check nrhs==1, input is double, input is not sparse, not complex
- Use typed mxGetDoubles() (R2018a+ API — R2023a supports this)
- Compute stats in a separate C++ function (not inside mexFunction) to show good structure
- Include comments explaining MEX API choices

### array_stats_wrapper.m requirements:
- Full input validation with descriptive error messages
- Call the MEX binary
- Return a struct with named fields (not raw outputs)
- Include a docstring with examples

### test_array_stats.m requirements:
- Use a simple pass/fail test framework (no Toolbox required)
- Test cases MUST include:
  1. Normal 1D vector
  2. Normal 2D matrix  
  3. Single element
  4. Large random array (verify against MATLAB built-ins)
  5. Error case: wrong input type (should throw)
  6. Error case: empty array (should throw)
  7. Edge case: array with NaN (document expected behavior)
- Print PASS/FAIL for each test with the actual vs expected values
- Print a final summary: "X/Y tests passed"

### build_mex.m requirements:
- Auto-detect matlabroot
- Add -v flag for verbose compile output
- Add -g flag as a commented option for debug builds
- Show the exact mex command being run before running it
- On success, print "BUILD SUCCESS: array_stats.mexw64"
- On failure, print the error and suggest common fixes

---

## PHASE 2 — Iterative Build & Fix Loop

Now do this loop (show me every step):

**Step 1:** Run build_mex.m. Show the FULL compiler output.

**Step 2:** If build fails:
- Read the error carefully
- Explain what the error means
- Fix the source code
- Show a diff of what changed and WHY
- Rebuild. Repeat until build succeeds.

**Step 3:** Once built, run test_array_stats.m. Show full output.

**Step 4:** If any tests fail:
- Diagnose whether the bug is in the C++ MEX code or the MATLAB wrapper
- Fix it, explain the fix
- Rebuild if C++ changed
- Re-run tests
- Repeat until all tests pass

**Step 5:** After all tests pass, run this final validation:
```matlab
matlab -batch "cd mex_poc/matlab; build_mex; test_array_stats"
```
Show the clean passing output.

---

## PHASE 3 — Teach Me What Happened

After everything passes, give me:

1. **Annotated array_stats.cpp** — inline comments on every MEX API call explaining
   what it does and why (e.g., why mxGetDoubles vs mxGetPr, what plhs/prhs are, etc.)

2. **Common MEX gotchas on Windows** you encountered or avoided:
   - Memory ownership rules
   - MATLAB vs C++ data layout (column-major)
   - mexErrMsgIdAndTxt vs mexErrMsgTxt
   - When to use -largeArrayDims
   - Debug build tips

3. **How to extend this pattern** — what to change to add a second MEX function
   that calls into a real C++ class

---

## CONSTRAINTS & RULES
- Never assume a path — always discover it with shell commands first
- If MATLAB batch mode is unavailable in the shell, tell me and use an alternative approach
- Show EVERY compiler warning, not just errors
- If you need to choose between MSVC and MinGW, explain the tradeoff for MEX on Windows
- All C++ must compile cleanly at warning level /W3 (MSVC) or -Wall (GCC)
- Do not use any MATLAB Toolboxes in the test code


