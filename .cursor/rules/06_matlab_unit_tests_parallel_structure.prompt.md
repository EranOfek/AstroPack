# 06_matlab_unit_tests_parallel_structure.prompt.md

ROLE AND CONTEXT
This file defines how MATLAB unit tests are structured in AstroPack.

Tests are treated as first-class code and must not pollute source folders.

FOLDER STRUCTURE
Source code:
astropack/Matlab

Tests:
astropack/MatlabTests

The folder hierarchy under MatlabTests mirrors Matlab exactly.

NAMING RULES
- Test files start with Test
- One test class per source file
- Test class inherits from matlab.unittest.TestCase

TEST SCOPE
Unit tests focus on:
- Deterministic functions
- Core algorithms
- Data transformations
- Validation logic

Unit tests do not test:
- App Designer GUIs
- Long-running workers
- External services

FILE I O IN TESTS
- Use temporary folders only
- Never depend on production paths
- Clean up after execution

TIME AND RANDOMNESS
- No wall-clock dependencies
- Fix random seeds explicitly

RUNNING TESTS
All tests must run via:
runtests('MatlabTests')

OUTPUT EXPECTATION FROM THE LLM
When writing tests:
- Prefer clarity over coverage tricks
- Make failures easy to diagnose

END OF FILE
