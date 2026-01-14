function runTESSpointpipe(WorkDir)
    %{
    Runs the TESS point-source pipeline using a JSON configuration file located
    in a working directory. This function is a thin wrapper that:
      (1) reads WorkDir/config.json,
      (2) decodes the JSON into a MATLAB struct,
      (3) separates positional arguments from optional name-value arguments,
      (4) calls pipeline.tess.TESSpointpipe with the configured parameters.
    
    The JSON file is expected to contain the required positional keys:
      - FFIDataPath
      - RA
      - Dec
      - SavePath
    All remaining keys in the JSON are interpreted as optional name-value
    arguments supported by pipeline.tess.TESSpointpipe.
    
    Input   : - WorkDir. Path to a working directory containing a configuration
                file named 'config.json'.
    
    Output  : - None. All outputs are produced by pipeline.tess.TESSpointpipe
                (products saved to disk and logging to file, depending on the
                configuration).
    
    Notes   : - Key names in config.json are case-sensitive and must match the
                parameter names expected by pipeline.tess.TESSpointpipe.
              - This wrapper converts JSON fields to MATLAB name-value pairs
                using fieldnames/struct2cell. If a JSON field is present that
                is not a valid name-value option for TESSpointpipe, MATLAB will
                error at call time.
              - The configuration file must be readable by fileread and valid
                JSON as accepted by jsondecode.
    
    Author  : Ruslan Konno (Jan 2026)
    Example : % Directory contains WorkDir/config.json:
              WorkDir = '/marvin/TESS/SNe/SN2025cnu';
              pipeline.tess.runTESSpointpipe(WorkDir);
    
              % Minimal config.json example:
              %{
              % {
              %   "FFIDataPath": "/path/to/FFIs",
              %   "RA": 159.277033151,
              %   "Dec": -7.46790523632,
              %   "SavePath": "/path/to/proc",
              %   "LogFile": "/path/to/status/tess_pointpipe.log",
              %   "runSubtraction": true,
              %   "RefPath": "/path/to/Ref"
              % }
              %}
    %}

    ConfigFile = strcat(WorkDir,'/config.json');
    
    Config = jsondecode(fileread(ConfigFile));

    Args = rmfield(Config, {'FFIDataPath', 'RA', 'Dec', 'SavePath'});

    ArgsNameVal = [fieldnames(Args), struct2cell(Args)]';
    ArgsNameVal = ArgsNameVal(:)';

    pipeline.tess.TESSpointpipe(Config.FFIDataPath, Config.RA, Config.Dec, ...
        Config.SavePath, ArgsNameVal{:});
end