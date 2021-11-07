function publishToolbox(v)
% Create release and publish toolbox
arguments
    v (1,:) char = '' % Release version
end
proj = currentProject();
cd(proj.RootFolder);
% Gnerate documentation
genDoc();
% Set version
tbxProj = fullfile(proj.RootFolder, 'main/LiveScriptToMarkdownConverterToolbox.prj');
if isempty(v)
    vCurrent = matlab.addons.toolbox.toolboxVersion(tbxProj);
    v = input(sprintf('<strong>Enter release version (%s): </strong>', vCurrent), 's');
    if isempty(v)
        v = vCurrent;
    end
end
% Create toolbox
mltbxFile = packToolbox(v);
% Publish toolbox
fprintf('Built: %s\n', mltbxFile);
vString = "v" + v;
system(sprintf('git add . && git commit -m %s', vString));
system(sprintf('git tag -a %s -m %s', vString, vString));
system('git push && git push --tags');
clipboard('copy', mltbxFile);
fprintf('Path to mtlbx-file was copied to clipboard\n');
disp("<strong>Don't forget to attach binary from clipboard</strong>");
pause(1)
repoUrl = erase(proj.RepositoryLocation, ".git" + lineBoundary('end'));
web(sprintf("%s/releases/edit/%s", repoUrl, vString), '-browser');
