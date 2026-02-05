# 12_matlab_ultrasat_planner_structure.prompt.md

ROLE AND CONTEXT
This file defines the internal structure of the ULTRASAT Planner MATLAB code.

The planner is mission-critical and must be cleanly layered.

HIGH-LEVEL COMPONENTS
- uplanner core class
- Helper classes
- DataModule class
- API client
- App Designer GUI

UPLANNER CORE CLASS
- Contains planning and feasibility logic
- No UI code
- No backend communication
- Deterministic and testable

HELPER CLASSES
- Small focused responsibilities
- Support planner or GUI
- No UI and no API logic

DATAMODULE
- Owns planner instance
- Holds application state
- Mediates between GUI, planner, and API

API CLIENT
- Handles backend communication only
- No planning logic
- Replaceable and mockable

APP DESIGNER GUI
- Thin UI shell
- Short callbacks
- Delegates to DataModule

DATA FLOW
GUI -> DataModule -> uplanner
GUI -> DataModule -> API client

WHAT NOT TO DO
- GUI calling planner directly
- GUI calling API directly
- Circular dependencies
- Global state

OUTPUT EXPECTATION FROM THE LLM
When modifying planner code:
- Preserve separation of roles
- Keep uplanner reusable and testable

END OF FILE
