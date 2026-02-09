function runDeployedBootstrap(RelativePath)
% Run deployed bootstrap: set SOC_PATH and load FileMap when isdeployed.
% Use from MATLAB services (slew_calc, snr, too_planner) that run as MCC EXE.
%
% Input : RelativePath - path relative to SOC_PATH for FileMap .mat file
%                         e.g. 'slew/slew_matlab/AstroPackFileMap_1.mat'
% Output: None
%
% When ~isdeployed, returns immediately after setting global SOC_PATH.
% When isdeployed, loads FileMap from fullfile(SOC_PATH, RelativePath),
% then addPathFolders() and scanFolders().
%
% Author: Refactored from slew/snr/too_planner services (2026)

    global SOC_PATH;

    SOC_PATH = getenv('SOC_PATH');
    if isempty(SOC_PATH)
        if ispc
            SOC_PATH = 'c:/soc';
        else
            SOC_PATH = '/home/soc/soc';
        end
    end

    if ~isdeployed
        return;
    end

    FMap = FileMap.getSingleton();
    FMap.StorageFileName = fullfile(SOC_PATH, RelativePath);
    FMap.loadMap();
    FMap.addPathFolders();
    FMap.scanFolders();
end
