# 01_matlab_repository_domain_map_and_intent.prompt.md

ROLE AND CONTEXT
This file explains the high-level domain map of the AstroPack MATLAB repository.
Its purpose is to help the LLM understand intent, history, and boundaries
that cannot be reliably inferred from code structure alone.

HIGH-LEVEL REPOSITORY MODEL
AstroPack is a large, long-lived scientific codebase that evolved over many years.
It contains:
- Active production code
- Legacy code kept for reference
- Experimental and draft work
- External third-party packages
- Tests, benchmarks, and learning sandboxes

Not all folders are equal in importance or maturity.

PRIMARY MATLAB ROOTS
The main MATLAB roots are:
- matlab/
- tests/

The matlab/ tree contains production and near-production code.
The tests/ tree contains experiments, learning code, and validation utilities.

CORE SCIENTIFIC DOMAINS
The following domains represent mature scientific functionality:
- astro/
- image/
- pipeline/
- celestial/
- timeSeries/
- telescope/
- VO/

These domains contain reusable scientific algorithms and data structures.
They are not ULTRASAT-specific by default.

ULTRASAT-SPECIFIC DOMAINS
The following domains are ULTRASAT-focused:
- ultrasat/
- pipeline/+ultrasat
- util/+db related to PlannerDb and IncomingAlertsDb
- services under ultrasat

These domains encode mission-specific assumptions and workflows.

BASE AND INFRASTRUCTURE DOMAINS
The following domains provide infrastructure and shared services:
- base/
- util/
- io/
- db/
- tools/

These are cross-cutting and should remain generic.

EXTERNAL AND THIRD-PARTY CODE
Folders under:
- external/
- external/+yaml
- external toolboxes

Contain third-party or imported code.
They must not be modified unless explicitly intended.

OBSOLETE AND LEGACY CODE
Folders named:
- obsolete
- Drafts-*
- backup-*
- old
- *_obsolete

Are kept for reference and archaeology.
They are not authoritative and must not be extended.

WHAT NOT TO DO
- Do not refactor legacy or obsolete code unless explicitly requested
- Do not assume all folders follow the same standards
- Do not modernize external code silently

OUTPUT EXPECTATION FROM THE LLM
When working in the repository:
- Identify which domain you are touching
- Respect its maturity and intent
- Ask before crossing domain boundaries

END OF FILE
