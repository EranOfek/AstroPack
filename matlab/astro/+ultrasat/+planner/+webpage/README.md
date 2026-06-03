# Plan and Target Web Page Export

POC module for exporting observation plans and targets as structured HTML pages with embedded figures and tables. Still under development — see ChatGPT reference in `WebPageExporter.m`.

https://chatgpt.com/c/67f645e1-0d68-8012-88b2-a74bff4d8e0e

---

## Class hierarchy

```
WebPageExporter           Base — templates, placeholders, ZIP export
├── PlanWebPageExporter   Plan-specific HTML
└── TargetWebPageExporter Target-specific HTML
```

| Class | File | Role |
|-------|------|------|
| `WebPageExporter` | `WebPageExporter.m` | Template loading, placeholder replacement, PNG figures, HTML tables, ZIP |
| `PlanWebPageExporter` | `PlanWebPageExporter.m` | Plan pages (`Prefix = 'plan'`) |
| `TargetWebPageExporter` | `TargetWebPageExporter.m` | Target pages (`Prefix = 'target'`) |

Legacy versions: `v0/` (older exporter API — reference only)

---

## Templates

HTML templates in `templates/` use placeholder tags:

| Placeholder type | Examples |
|------------------|----------|
| Images | `{{img_plan_params}}`, `{{img_unique_targets}}` |
| Tables | `{{table_targets}}`, `{{table_parameters}}` |

Available templates:

- `templates/plan_template_01.html`, `plan_template_02.html`
- `templates/target_template_01.html`
- `templates/v0/` — older template versions

---

## Workflow

1. Construct exporter with plan/target ID, output folder, template path
2. Set values, add figures (`addFigure`), add tables (`addTable`)
3. Call `generateHtmlFromTemplate()`
4. Optionally create ZIP for upload (`UploaderUrl` for S3 uploader service)

Example debug:

```matlab
ultrasat.planner.webpage.debug_PlanPageExporter()
ultrasat.planner.webpage.debug_TargetPageExporter()
```

---

## Sample output

Pre-generated examples in `export_output/`:

- `plan_2025_04_09_001/` — `index.html`, `data.json`
- `plan_2025_04_09_002/`
- `target_2025_04_09_42_12_00_40_001/`

Open `index.html` in a browser to preview layout.

---

## Planned extensions

From source comments:

- Persistent tags
- Notes fields
- JSON data blocks (single or multiple objects)

---

## Related

- Planner GUI: [[../+gui/README|+gui README]]
- Python file upload (if used): `python/prj/nova/soc/infra/file_service/`

---

_Last updated: 2026-06_
