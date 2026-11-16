function demo
% demo TOML reader smoke test for AstroPack io.toml package
% Reads the TOML files next to this script, verifies representative values,
% and prints both selected values and the complete parsed data.

scriptDirectory = fileparts(mfilename("fullpath"));

if isempty(which("readtoml"))
    error("io.toml:MissingToolbox", ...
        "readtoml was not found. Run io.toml.install first; see README.md.");
end

appFile = fullfile(scriptDirectory, "app_config.toml");
pipelineFile = fullfile(scriptDirectory, "pipeline.toml");

fprintf("Reading %s\n", appFile);
app = readtoml(appFile);

% Basic smoke tests: scalar, nested, array, datetime, and hyphenated keys.
assert(app.enabled == true);
assert(app.max_retries == 3);
assert(app.server.port == 8080);
assert(isequal(string(app.tags(:)), ["matlab"; "toml"; "poc"]));
assert(app.("build-system").("build-backend") == "readtoml");
assert(isdatetime(app.created_at));

fprintf("  title:         %s\n", app.title);
fprintf("  server:        %s:%d\n", app.server.host, app.server.port);
fprintf("  tags:          %s\n", strjoin(string(app.tags), ", "));
fprintf("  build backend: %s\n", ...
    app.("build-system").("build-backend"));
fprintf("  created class: %s\n", class(app.created_at));

fprintf("\nComplete app_config.toml data:\n");
disp(app);

fprintf("\nReading %s\n", pipelineFile);
pipeline = readtoml(pipelineFile);

% More smoke tests: literal Windows paths, nested tables, and table arrays.
assert(pipeline.name == "Ultrasat sample pipeline");
assert(isequal(pipeline.channels(:), [1; 2; 4]));
assert(pipeline.paths.input == "C:\data\ultrasat\input");
assert(pipeline.processing.options.reject_outliers == true);
assert(numel(pipeline.targets) == 2);

fprintf("  pipeline:      %s\n", pipeline.name);
fprintf("  channels:      %s\n", strjoin(string(pipeline.channels), ", "));
fprintf("  input path:    %s\n", pipeline.paths.input);
fprintf("  threshold:     %.2f\n", pipeline.processing.threshold);
fprintf("  target count:  %d\n", numel(pipeline.targets));

fprintf("\nComplete pipeline.toml data:\n");
disp(pipeline);

fprintf("\nStruct conversion (for APIs that do not accept TOMLData):\n");
pipelineStruct = struct(pipeline);
disp(pipelineStruct);

fprintf("\nPASS: both TOML files were read and all smoke tests passed.\n");
end
