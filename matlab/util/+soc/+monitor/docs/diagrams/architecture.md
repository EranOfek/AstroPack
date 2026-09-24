# soc.monitor — Package Architecture

## Overview

The `soc.monitor` package provides a thin function API for pipeline developers. Internally, a singleton `MonitorClient` writes JSON lines to daily JSONL files.

## Component diagram

```mermaid
flowchart TD
    subgraph pipeline [PipelineCode]
        P1["soc.monitor.init(cfg)"]
        P2["soc.monitor.heartbeat()"]
        P3["soc.monitor.image_started(...)"]
        P4["soc.monitor.stage_done(...)"]
        P5["soc.monitor.fault(...)"]
    end

    subgraph pkg [PackageFunctions]
        getClient["get_client()"]
        makeRecord["make_record()"]
        utcNow["utc_now_str()"]
    end

    subgraph core [MonitorClient]
        cfg["Config: MonitorConfig"]
        write["writeRecord(Record)"]
    end

    subgraph const [MonitorConst]
        C1["SchemaVersion"]
        C2["Kinds / Severities / Statuses"]
        C3["Event codes"]
    end

    subgraph output [Output]
        JSONL["pipeline_monitor_*.jsonl"]
    end

    pipeline --> pkg
    pkg --> getClient
    getClient -->|"persistent"| core
    pkg --> makeRecord
    makeRecord --> utcNow
    makeRecord --> const
    core --> write
    write --> JSONL
```

## File layout

```text
matlab/util/+soc/+monitor/
├── MonitorConst.m
├── MonitorConfig.m
├── MonitorClient.m
├── init.m / get_client.m / reset.m
├── make_record.m / utc_now_str.m
├── heartbeat.m, image_*.m, stage_*.m, ...
├── +debug/
│   ├── debug_monitor.m
│   └── debug_schema.m
└── docs/
    ├── README.md
    └── diagrams/
```

## Singleton lifecycle

```mermaid
stateDiagram-v2
    [*] --> Uninitialized
    Uninitialized --> Ready: init(configFile)
    Uninitialized --> Ready: get_client() creates default
    Ready --> Ready: heartbeat / image_* / stage_* / ...
    Ready --> Uninitialized: reset()
    Ready --> Ready: init(configFile) replaces client
```
