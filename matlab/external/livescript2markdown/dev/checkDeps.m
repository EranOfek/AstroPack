function checkDeps()
% Check project dependencies are installed
proj = currentProject();
tbxProjFile = fullfile(proj.RootFolder, 'main/LiveScriptToMarkdownConverterToolbox.prj');
s = readstruct(tbxProjFile, 'FileType', 'xml');
reqs = s.configuration.param_required_addons.requiredaddons.requiredAddOn;
addons = matlab.addons.installedAddons;
if ~isempty(reqs)
    for i = 1 : length(reqs)
        if ~ismember(reqs(i).idAttribute, addons.Identifier)
            warning('<strong>Please install "%s" addon to work with project</strong>\n', reqs(i).Text);
        end
    end
end
end
