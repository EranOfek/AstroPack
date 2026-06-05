# soc.monitor — Record Write Flow

## Sequence: one monitoring call

```mermaid
sequenceDiagram
    participant Pipeline
    participant PackageFn as soc.monitor.stage_done
    participant getClient as get_client
    participant makeRecord as make_record
    participant Client as MonitorClient
    participant JSONL as jsonl_file

    Pipeline->>PackageFn: stage_done(imageId, stage, info)
    PackageFn->>getClient: get singleton
    getClient-->>PackageFn: MonitorClient
    PackageFn->>makeRecord: build record struct
    makeRecord-->>PackageFn: Record
    PackageFn->>Client: writeRecord(Record)
    Client->>Client: validateRecord
    Client->>JSONL: fopen append
    Client->>JSONL: jsonencode + fprintf
    Client->>JSONL: fclose
```

## Record build steps

```mermaid
flowchart LR
    A[Package function called] --> B[normalize_info]
    B --> C[make_record with MonitorConst fields]
    C --> D[utc_now_str for dt]
    C --> E[Client Config for pipeline_id / instance_id]
    D --> F[Record struct]
    E --> F
    F --> G[validateRecord]
    G --> H[jsonencode]
    H --> I[Append one line to JSONL]
```

## End-to-end data path (future)

MATLAB writes JSONL locally. Downstream Python forwarder and backend are separate tasks (specs 04–06).

```mermaid
flowchart LR
    MATLAB["MATLAB pipeline\nsoc.monitor.*"] --> JSONL["Local JSONL files"]
    JSONL --> PythonFwd["Python forwarder\n(future)"]
    PythonFwd --> Backend["FastAPI backend\n(future)"]
    Backend --> ExtMon["External monitor\n(future)"]
```
