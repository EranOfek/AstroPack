# 11_matlab_app_designer_structure.prompt.md

ROLE AND CONTEXT
This file defines how MATLAB App Designer is used in AstroPack.

App Designer is used only to build thin GUI shells.
It is never the location of core logic.

HIGH-LEVEL MODEL
Core logic lives in regular MATLAB files.
The GUI calls the core logic through a DataModule or manager class.

APP DESIGNER RESPONSIBILITIES
- Display data
- Collect user input
- Trigger actions

CALLBACK RULES
Callbacks must:
- Be short
- Read UI values
- Call core logic
- Update UI

No heavy computation in callbacks.

STATE MANAGEMENT
- UI may hold view state only
- Core state belongs to managers or planners

ERROR HANDLING
- Catch errors at UI boundary
- Show user-friendly messages
- Do not suppress errors in core logic

TESTABILITY
- Core logic must run without GUI
- App Designer is not required for tests

WHAT NOT TO DO
- No algorithms in callbacks
- No file I O in UI unless UI-specific
- No backend communication from UI

OUTPUT EXPECTATION FROM THE LLM
When modifying GUI code:
- Keep it thin
- Push logic down into helpers or managers

END OF FILE
