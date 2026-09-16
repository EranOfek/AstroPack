function [Files, Frames] = writeFITS(DeviceDir, OutDir, Args)
    % Export the DESY lab TIFF frames of one device as FITS files, one per gain.
    %   Each TIFF holds the low-gain and the high-gain readout of the same
    %   pixels (see ultrasat.lab.readPTC). Every frame is written as two
    %   FITS files, <name>_hg.fits and <name>_lg.fits, where <name> is the
    %   TIFF name with '#' replaced by 'S'. The header carries all keys
    %   collected by readPTC (TIFF keys, GAINSEL, ORIENT, RAWSEC, lot /
    %   wafer / device, exposure and intensity, DATE-OBS, biases, ADC
    %   registers, Calib best matches, PASS / SOFTBIN) plus ORIGFILE,
    %   BUNIT and SATURATE.
    % Input  : - Device directory (e.g., .../LOT_TH02954_W04_D07).
    %          - Output directory. Default is fullfile(DeviceDir, 'FITS').
    %          * ...,key,val,...
    %            'Test' - Test sub-directory. Default is 'PTC_int_hr'.
    %            'Gain' - Gains to write: 'high', 'low', or 'both'.
    %                   Default is 'both'.
    %            'Orient' - 'desy' (default) or 'tiff'; see readPTC.
    %            'FrameType', 'Step', 'FrameIndex' - frame filters as in
    %                   readPTC. Default is 'all', [], [].
    %            'Saturate' - ADC saturation level [ADU] written as SATURATE.
    %                   Default is 16383.
    %            'OverWrite' - Default is false (existing files are skipped).
    %            'Verbosity' - 0 silent, 1 report progress. Default is 0.
    % Output : - Cell array of the FITS files written (or found existing).
    %          - The frame table of readPTC.
    % Author : Sasha Krassilchtchikov (Sep 2026)
    % Example: Files = ultrasat.lab.writeFITS('LOT_TH02954_W04_D07', '/data/fits');
    %          ultrasat.lab.writeFITS(Dev, [], 'Gain','high', 'FrameType','D');

    arguments
        DeviceDir
        OutDir                       = [];
        Args.Test                    = 'PTC_int_hr';
        Args.Gain                    = 'both';
        Args.Orient                  = 'desy';
        Args.FrameType               = 'all';
        Args.Step                    = [];
        Args.FrameIndex              = [];
        Args.Saturate(1,1) double    = 16383;
        Args.OverWrite(1,1) logical  = false;
        Args.Verbosity(1,1) double   = 0;
    end

    if isempty(OutDir)
        OutDir = fullfile(DeviceDir, 'FITS');
    end
    if ~isfolder(OutDir)
        mkdir(OutDir);
    end
    switch lower(Args.Gain)
        case 'both'
            Gains = {'high', 'low'};
        case {'high', 'low'}
            Gains = {lower(Args.Gain)};
        otherwise
            error('ultrasat:lab:writeFITS:gain', 'Unknown Gain %s (high|low|both)', Args.Gain);
    end
    Suffix = struct('high','_hg', 'low','_lg');

    % inventory (headers only) to know the files and skip existing outputs
    [~, Frames] = ultrasat.lab.readPTC(DeviceDir, 'Test',Args.Test, 'FrameType',Args.FrameType, ...
                                       'Step',Args.Step, 'FrameIndex',Args.FrameIndex, 'ReadImage',false);
    Nf    = height(Frames);
    Files = cell(Nf, numel(Gains));
    for Ig=1:1:numel(Gains)
        for If=1:1:Nf
            [~, Name] = fileparts(Frames.FileName{If});
            Files{If,Ig} = fullfile(OutDir, [strrep(Name, '#', 'S'), Suffix.(Gains{Ig}), '.fits']);
        end
        ToDo = find(Args.OverWrite | ~cellfun(@isfile, Files(:,Ig)));
        for It=1:1:numel(ToDo)
            If = ToDo(It);
            if Args.Verbosity>0
                fprintf('writeFITS: %s %d/%d %s\n', Gains{Ig}, It, numel(ToDo), Files{If,Ig});
            end
            try
                AI = ultrasat.lab.readPTC(DeviceDir, 'Test',Args.Test, 'FrameType',Frames.FrameType{If}, ...
                                          'Step',Frames.Step(If), 'FrameIndex',Frames.FrameIndex(If), ...
                                          'Gain',Gains{Ig}, 'Orient',Args.Orient);
                AI = AI(1);
                AI.HeaderData.Data = [AI.HeaderData.Data;
                                      {'ORIGFILE', Frames.FileName{If}, 'Source TIFF file';
                                       'BUNIT',    'ADU',               'Pixel unit';
                                       'SATURATE', Args.Saturate,       'ADC saturation level [ADU]'}];
                AI.write1(Files{If,Ig}, 'Image', 'OverWrite',true);
            catch ME
                warning('ultrasat:lab:writeFITS:write', 'Failed to write %s (%s)', Files{If,Ig}, ME.message);
            end
        end
    end
    Files = Files(:);
end
