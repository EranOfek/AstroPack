# 15_matlab_clickhouse_pipeline_usage.prompt.md

ROLE AND CONTEXT
This file defines the general principles for using ClickHouse from MATLAB
inside the AstroPack repository.

ClickHouse is used as a high-performance analytical database
for large-scale astronomical pipeline data such as images, sources,
detections, and derived products.

This file intentionally defines only general rules.
Specific schemas, tables, and workflows will be added later.

HIGH-LEVEL SYSTEM ROLE
ClickHouse is an analytics backend, not a transactional database.

It is used for:
- Fast querying of large datasets
- Aggregations over images, sources, detections
- Time-series and spatial analysis
- Pipeline monitoring and performance analysis

ClickHouse is NOT used for:
- Transactional state
- User management
- Small configuration data
- Frequent row-by-row updates

MATLAB ROLE WITH CLICKHOUSE
MATLAB acts as:
- A producer of pipeline results
- A consumer of analytical query results
- A scientific analysis client

MATLAB does NOT:
- Own ClickHouse schema lifecycle
- Perform schema migrations
- Act as a database server

DATA FLOW MODEL
Typical flow:
- Pipeline produces results (images, catalogs, measurements)
- Results are written or bulk-inserted into ClickHouse
- MATLAB queries ClickHouse for analysis, validation, or visualization

ClickHouse is optimized for:
- Append-heavy workloads
- Read-mostly analytics
- Large batch inserts

INTERACTION PATTERNS
Allowed MATLAB interactions:
- Bulk insert of pipeline results
- Read-only analytical queries
- Aggregation and filtering queries

Disallowed patterns:
- Row-by-row inserts in loops
- Frequent small updates
- Emulating relational transactions

QUERY DESIGN PRINCIPLES
- Prefer batch queries over many small queries
- Push filtering and aggregation into ClickHouse
- Avoid pulling large raw tables into MATLAB memory unnecessarily
- Assume datasets are large by default

SCHEMA ASSUMPTIONS
- Tables are designed for analytical access
- Denormalization is acceptable and expected
- Time and spatial columns are first-class dimensions
- Schema evolution is controlled externally

MATLAB CODE STRUCTURE
ClickHouse access in MATLAB must be:
- Isolated in dedicated helper or service classes
- Configurable (host, database, credentials)
- Replaceable or mockable for testing

Core scientific logic must not depend directly on ClickHouse APIs.

ERROR HANDLING
- Treat ClickHouse as an external dependency
- Handle connection failures gracefully
- Do not assume database availability

PERFORMANCE CONSIDERATIONS
- Minimize data transfer volume
- Avoid unnecessary full-table scans
- Be explicit about limits and filters

TESTING AND DEVELOPMENT
- Core pipeline logic must work without ClickHouse
- ClickHouse-dependent code should be separable
- Use small test tables or subsets for development

WHAT NOT TO DO
- No hardcoded credentials
- No schema creation or modification from MATLAB
- No implicit assumptions about table size
- No silent query failures

OUTPUT EXPECTATION FROM THE LLM
When generating MATLAB code involving ClickHouse:
- Keep database access isolated
- Assume large data volumes
- Prefer explicit, readable query construction
- Ask for schema details if missing

END OF FILE
