
### IAI GCS – SOC  Interfaces

The IAI GCS – SOC will have two physical interfaces. The main data elements are:
- SOC-GCS: Observation Request
- GCS-SOC: Satellite scheduled operations (for example scheduled maintenance procedures)
- Bi-directional interface: The SCC will get commands for SOC in real-time, and update the SOC with relevant Satellite constrains, Telemetry & operational messages. This interface will also support managing the Imagery workflow.
- GCS-SOC: Image data
- GCS-SOC: Tracking Telemetry Data, Payload Data and Control.


These interfaces must be secured and highly available.
The IAI-GCS will store all planned observations received from SOC. 
Note: The parameters and logics of these interfaces TBD 


