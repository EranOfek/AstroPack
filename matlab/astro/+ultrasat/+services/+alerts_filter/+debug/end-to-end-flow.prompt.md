You are an expert MATLAB software architect and ULTRASAT SOC developer.

Goal:
Debug and complete the full end-to-end flow of the alerts_filter MATLAB service.

IMPORTANT:
- Follow the existing architecture and design decisions already present in the codebase.
- Do NOT redesign the architecture.
- Do NOT introduce unnecessary abstractions.
- If you are unsure about a design decision or missing information, STOP and ask me before proceeding.

==============================================================================
HIGH LEVEL ARCHITECTURE
==============================================================================

The architecture is intentionally separated into:

1. Service/runtime infrastructure:
   ultrasat.services.alerts_filter

2. Scientific filtering logic:
   ultrasat.alerts_filters.lvc

3. Scientific filter implementations:
   ultrasat.alerts_filters.lvc.filters

Scientists should ONLY modify:
    +alerts_filters/+lvc/+filters

Scientists should NEVER touch:
    +services
    IPC
    runtime loop
    watchdog
    service infrastructure

The MATLAB service receives a SMALL JSON request file.
The request references an alert JSON file on disk.
The actual alert is loaded into:
    ultrasat.alerts_filters.lvc.models.LvcParsedAlert

The service then calls:
    ultrasat.alerts_filters.lvc.filters.lvc_filter()

which dispatches internally to a concrete filter implementation such as:
    lvc_filter_simple
    lvc_filter_with_criteria

==============================================================================
IMPORTANT DESIGN RULES
==============================================================================

1. KEEP INPUT REQUEST JSON SMALL

The request JSON should contain only:
- action
- alert_file
- filter
- optional future fields

Do NOT embed huge alert blobs into the IPC request JSON.

2. FILTERS WORK ON MODELS

The scientific filters should work on:
    LvcParsedAlert
NOT on raw JSON.

3. SERVICE LAYER LOADS FILES

processFilterLvc() should:
- validate request
- load LvcParsedAlert from alert_file
- attach alert model into Input.alert
- call lvc_filter()

4. FILTER ENTRY POINT

lvc_filter() is the scientific dispatcher/orchestrator.
It chooses which actual filter implementation to run.

5. KEEP EVERYTHING SIMPLE

Do NOT add:
- plugin registries
- dynamic discovery
- complicated inheritance
- common base frameworks

==============================================================================
WHAT TO IMPLEMENT / FIX
==============================================================================

Fix and complete the full end-to-end cycle.

Current goals:

1. Create a complete debug scenario folder:

    C:\Ultrasat\AstroPack\matlab\astro\+ultrasat\+services\+alerts_filter\+debug\end_to_end_lvc_simple\

(If you think a slightly better name is more consistent, you may improve it.)

2. Create ALL required sample files on disk.

3. Do NOT delete ANY files after processing.
All files must remain for inspection and debugging.

4. Create:
- input request JSON
- sample LVC alert JSON
- output result JSON

5. Fix all MATLAB code as needed.

6. Make the full flow work:
- request file loaded
- alert file loaded
- LvcParsedAlert created
- lvc_filter called
- lvc_filter_simple called
- LvcFilterResult returned
- output JSON written

==============================================================================
EXPECTED REQUEST JSON FORMAT
==============================================================================

The request JSON should be minimal and contain references only.

Example concept:

{
  "action": "filter_lvc",
  "filter": "simple",
  "alert_file": "C:/.../sample_alert.json"
}

==============================================================================
EXPECTED FLOW
==============================================================================

processRequest()
    ->
processFilterLvc()
    ->
LvcParsedAlert.loadFromJsonFile()
    ->
lvc_filter()
    ->
lvc_filter_simple()
    ->
LvcFilterResult
    ->
output JSON written

==============================================================================
IMPORTANT CODE REQUIREMENTS
==============================================================================

1. Fix lvc_filter.m

It should:
- receive Input struct
- extract Input.alert
- dispatch to:
    lvc_filter_simple
    lvc_filter_with_criteria

2. Fully migrate filters to use:
    LvcFilterResult

Remove remaining old struct-based logic.

3. Ensure result object serializes correctly to JSON.

4. Ensure all paths are valid and portable.

5. Ensure debug scenario can be rerun multiple times.

6. Add clear embedded comments where needed.

==============================================================================
IMPORTANT VALIDATION
==============================================================================

At the end verify:
- request JSON exists
- alert JSON exists
- output JSON exists
- output JSON contains expected filter result
- no crashes
- no undefined variables
- no stale old TooPlanner references remain

==============================================================================
IMPORTANT
==============================================================================

If you encounter ambiguity about:
- request format
- output format
- filter semantics
- file organization
- result structure

STOP and ask me before proceeding.
