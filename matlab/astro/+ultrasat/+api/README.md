# SOC API

https://chatgpt.com/c/6756dedd-4c2c-8012-adad-4772c6780623


# ULTRASAT API Module

This folder contains MATLAB source files related to the ULTRASAT API, including mission client implementations, validation simulators, and tracking utilities.

## Folder Structure
```
+ultrasat/
   +api/                # API-related scripts and simulations
   ├── sim/             # Contains simulation data
   ├── .gitignore       # Git ignore file for excluding unnecessary files
   ├── allFunList.m     # Lists all available functions in the module
   ├── debug_Mission.m  # Debug script for mission-related operations
   ├── debug_MissionApiModels.m  # Debug script for mission API models
   ├── debug_MissionBase.m  # Debug script for mission base functionality
   ├── debug_MissionClientSim.m  # Debug script for simulated mission client
   ├── debug_MissionModels.m  # Debugging script for mission models
   ├── debug_SkyExposureTracker.m  # Debugging tool for sky exposure tracking
   ├── debug_ValidatorSim.m  # Debugging tool for validation simulator
   ├── debug_VirtualTime.m  # Debugging tool for virtual time client
   ├── MissionApiModels.m  # Defines API models for mission operations
   ├── MissionClient.m  # Client implementation for interacting with mission API
   ├── MissionClientBase.m  # Base class for mission client functionality
   ├── MissionClientSim.m  # Simulated mission client for offline testing
   ├── MissionModels.m  # Defines data structures for mission processing
   ├── MyRestClient.m  # Example REST client for API interaction
   ├── MyRestServer.m  # Example REST server implementation
   ├── PlanData.m  # Stores and manages plan-related data
   ├── README.md  # Documentation file
   ├── RestServer1.m  # Placeholder/rest server example
   ├── SkyExposureTrackerClient.m  # Client for sky exposure tracking
   ├── SkyExposureTrackerModels.m  # Models for sky exposure tracking
   ├── ValidatorSim.m  # Validation simulator for testing plan validation
   ├── VirtualTimeClient.m  # Client for virtual time operations
   ├── VirtualTimeModels.m  # Models for virtual time management
```

## Description
This directory contains all necessary MATLAB scripts related to 
mission operations, validation, sky exposure tracking, and virtual time 
handling for the ULTRASAT Observation Planner GUI.

### Key Components
- **Mission Client & API** (`MissionClient.m`, `MissionApiModels.m`, etc.)
- **Validation Simulator** (`ValidatorSim.m`, `debug_ValidatorSim.m`)
- **Sky Exposure Tracking** (`SkyExposureTrackerClient.m`, `SkyExposureTrackerModels.m`)
- **Virtual Time Management** (`VirtualTimeClient.m`, `VirtualTimeModels.m`)
- **Debugging & Testing** (`debug_*.m` files for various components)

## Usage
- Run `MissionClient.m` to interact with the mission API.
- Use `ValidatorSim.m` to simulate validation processes.
- Utilize `SkyExposureTrackerClient.m` to track exposure times.
- Refer to debug scripts for testing and troubleshooting.

_Last Updated: 2025-03-18_

