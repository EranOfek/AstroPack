function runTESSwidepipe(WorkDir)
    %{
    Runs the wide-field TESS FFI pipeline using a JSON configuration file located
    in a working directory. This function is a thin wrapper that:
      (1) reads WorkDir/config.json,
      (2) decodes the JSON into a MATLAB struct,
      (3) separates required positional arguments from optional name-value
          arguments,
      (4) calls pipeline.tess.TESSwidepipe with the configured parameters.
    
    The JSON file is expected to contain the required positional keys:
      - FFIDataPath
      - SavePath
    All remaining keys in the JSON are interpreted as optional name-value
    arguments supported by pipeline.tess.TESSwidepipe.
    
    Input   : - WorkDir. Path to a working directory containing a configuration
                file named 'config.json'.
    
    Output  : - None. All outputs are produced by pipeline.tess.TESSwidepipe
                (products written to disk and logging to file, depending on the
                configuration).
    
    Notes   : - Key names in config.json are case-sensitive and must match the
                parameter names expected by pipeline.tess.TESSwidepipe.
              - This wrapper converts JSON fields to MATLAB name-value pairs
                using fieldnames/struct2cell. If a JSON field is present that is
                not a valid name-value option for TESSwidepipe, MATLAB will error
                at call time.
              - The configuration file must be valid JSON as accepted by
                jsondecode and readable by fileread.
              - This function does not create WorkDir or validate that the paths
                referenced by the configuration exist; such validation is
                performed inside TESSwidepipe where relevant.
    
    Author  : Ruslan Konno (Jan 2026)
    Example : % Directory contains WorkDir/config.json:
              WorkDir = '/marvin/TESS/GRBs/GRB251013C';
              pipeline.tess.runTESSwidepipe(WorkDir);
    
              % Minimal config.json example:
              %{
              % {
              %   "FFIDataPath": "/path/to/FFIs",
              %   "SavePath": "/path/to/proc",
              %   "LogFile": "/path/to/status/tess_widepipe.log",
              %   "runSubtraction": true,
              %   "RefPath": "/path/to/ref",
              %   "FilterConfigFile": "/path/to/TESS.FilterParameters.json"
              % }
              %}
    %}

    ConfigFile = strcat(WorkDir,'/config.json');
    
    Config = jsondecode(fileread(ConfigFile));

    Args = rmfield(Config, {'FFIDataPath', 'SavePath'});

    ArgsNameVal = [fieldnames(Args), struct2cell(Args)]';
    ArgsNameVal = ArgsNameVal(:)';

    pipeline.tess.TESSwidepipe(Config.FFIDataPath, Config.SavePath, ...
        ArgsNameVal{:});
end