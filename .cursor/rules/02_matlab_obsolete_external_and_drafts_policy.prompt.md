# 02_matlab_obsolete_external_and_drafts_policy.prompt.md

ROLE AND CONTEXT
This file defines how obsolete, draft, backup, and external code
must be treated inside the AstroPack MATLAB repository.

WHY THIS EXISTS
AstroPack has decades of accumulated knowledge.
Old code is preserved intentionally, not forgotten.

However, old code is not a template for new code.

CATEGORIES AND MEANING
OBSOLETE
Folders named obsolete contain deprecated implementations.
They exist for reference only.

DRAFTS
Folders named Drafts-* or draft contain unfinished or failed attempts.
They document ideas, not standards.

BACKUPS
Folders named backup-* or dated backups are historical snapshots.
They must not be modified.

EXTERNAL
Folders under external/ contain third-party code.
They follow their own rules and licenses.

STRICT RULES
- Do not copy patterns from obsolete or draft code
- Do not extend obsolete classes
- Do not add new code into obsolete folders
- Do not refactor external code unless explicitly instructed

ALLOWED ACTIONS
- Read obsolete code to understand history
- Compare old and new approaches conceptually
- Leave comments referencing obsolete behavior if needed

DISALLOWED ACTIONS
- Reviving obsolete APIs silently
- Mixing new code into legacy folders
- Treating draft code as production-ready

OUTPUT EXPECTATION FROM THE LLM
When encountering obsolete or external code:
- Treat it as read-only
- Do not infer architectural rules from it
- Ask before touching it

END OF FILE
