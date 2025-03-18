# ULTRASAT Simulation Data (sim/ Folder)

This folder contains simulation data and test artifacts for the ULTRASAT planner. It includes JSON and MATLAB `.mat` files for storing plans, validation results, and user data.

## Folder Structure

```
sim/
│── plans/                    # Contains stored observation plans
│   ├── 001.json              # Plan metadata (JSON)
│   ├── 001.mat               # Corresponding MATLAB object (planner)
│   ├── 002.json              # Another plan
│   ├── 002.mat               # MATLAB object for the plan
│── approved_targets.json      # List of approved targets
│── current_user.json          # Active user session data
│── key_value_db.json          # Key-value database for various settings
│── sky_exposure.json          # Exposure time calculations
│── unique_targets.csv         # CSV list of unique targets
│── users.json                 # User authentication and role management
│── validator.json             # Validation history and status tracking
│── README.md                  # This file
```

## Plan Storage Format

Each **observation plan** is stored using two files:

1. **JSON (**``**)** – Contains all plan metadata **except the **``** field**.
2. **MATLAB (**``**)** – Stores the actual **planner** object, which is an instance of `ultrasat.uplanner`.

### Why two files?

- JSON is lightweight and can be easily read and edited.
- The `planner` object may contain complex MATLAB structures that are better stored in `.mat` format.

### Example: Saving a Plan

```matlab
% Save metadata in JSON (excluding 'planner')
jsonFile = fullfile(plansFolder, sprintf('%03d.json', obj.PlanData.pk));
planStruct = obj.PlanData.toStruct();
planStruct = rmfield(planStruct, 'planner'); % Remove 'planner' field
planStruct = api.ModelBase.convertDatetimeToString(planStruct); % Convert datetimes
fid = fopen(jsonFile, 'w');
fwrite(fid, jsonencode(planStruct, 'PrettyPrint', true), 'char');
fclose(fid);

% Save 'planner' separately in .mat file
matFile = fullfile(plansFolder, sprintf('%03d.mat', obj.PlanData.pk));
planner = obj.PlanData.planner; % Instance of ultrasat.uplanner
save(matFile, 'planner');
```

## Validation Data (validator.json)

- Stores all validation responses.
- Includes **history** of past validation results.

## Usage Guidelines

- **New observation plans** should be stored in `plans/` as `.json` + `.mat` pairs.
- **User data** is stored in `users.json`.
- **Validation logs** are kept in `validator.json`.
- **Target lists** are in `approved_targets.json` and `unique_targets.csv`.

## Planned Improvements

- Automate the cleanup of old validation results.
- Implement versioning for observation plans.
- Improve the structure of key-value storage.

**Maintained by:** ULTRASAT Planner Team\
**Last Updated:** 2025-03-18

