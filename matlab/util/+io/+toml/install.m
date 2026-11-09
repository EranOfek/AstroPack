function install
% install MathWorks matlab-toml-yaml toolbox from the bundled .mltbx
% Run once per MATLAB user profile, or when upgrading the bundled installer.
% After install, verify with which readtoml and io.toml.demo.

scriptDirectory = fileparts(mfilename("fullpath"));
toolboxFile = fullfile(scriptDirectory, "MATLAB_Toolbox_for_TOML_and_YAML.mltbx");

if ~isfile(toolboxFile)
    error("io.toml:MissingToolboxFile", ...
        "Bundled toolbox not found: %s", toolboxFile);
end

fprintf("Installing TOML/YAML toolkit from:\n  %s\n", toolboxFile);
matlab.addons.toolbox.installToolbox(toolboxFile);

readtomlPath = which("readtoml");
if isempty(readtomlPath)
    error("io.toml:InstallFailed", ...
        "installToolbox completed but readtoml was not found on the path.");
end

fprintf("readtoml: %s\n", readtomlPath);

installed = matlab.addons.toolbox.installedToolboxes();
if ~isempty(installed)
    names = string({installed.Name});
    mask = contains(names, "TOML", "IgnoreCase", true) | ...
        contains(names, "YAML", "IgnoreCase", true);
    if any(mask)
        fprintf("Installed toolbox entries:\n");
        disp(installed(mask));
    end
end

fprintf("Install complete. Run io.toml.demo to verify.\n");
end
