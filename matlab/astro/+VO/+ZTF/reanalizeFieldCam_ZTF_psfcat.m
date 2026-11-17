function Result = reanalizeFieldCam_ZTF_psfcat(Args)
    % Recalibrate a field/CCD with per-file checkpoints and explicit failures.
    % Result is a per-file status array (also saved as Status in Status.mat).
    % Successful, existing products are reused; failed/missing products retried.
    % Delete defaults false. Explicit Delete removes only checkpoint-listed
    % recalibrated products and this wrapper's checkpoint/merged products.
    arguments
        Args.Path = []
        Args.BasePath = []
        Args.FieldID = []
        Args.CCDID = []
        Args.FileTemp = 'ztf_*_psfcat.fits'
        Args.Delete logical = false
        Args.OnlyDelete logical = false
        Args.OrbEl = []
        Args.INPOP = []
        Args.GeoPos = 'p48'
        Args.Verbose logical = false
        Args.RunPhotCalib logical = true
        Args.RunSolarSystem logical = true
        Args.OutputDir = 'recalibrated'
    end
    if isempty(Args.FieldID) || isempty(Args.CCDID)
        error('VO:ZTF:MissingFieldCCD','FieldID and CCDID must be supplied');
    end
    FieldID = char(string(Args.FieldID));
    CCDID = char(string(Args.CCDID));
    if ~isempty(Args.BasePath)
        Args.Path = fullfile(Args.BasePath,FieldID,CCDID);
    end
    PWD = pwd;
    RestoreDir = onCleanup(@() cd(PWD));
    if ~isempty(Args.Path)
        cd(Args.Path);
    end
    if ischar(Args.GeoPos) || isstring(Args.GeoPos)
        if strcmpi(Args.GeoPos,'p48')
            Geo = celestial.earth.observatoryCoo('Name','Palomar48');
            Args.GeoPos = [Geo.Lon,Geo.Lat].*(pi/180);
            Args.GeoPos(3) = Geo.Height;
        else
            Args.GeoPos = telescope.obs.observatory_coo(Args.GeoPos);
        end
    end
    OldStatus = [];
    if isfile('Status.mat')
        Previous = load('Status.mat');
        if isfield(Previous,'Status')
            OldStatus = Previous.Status;
        end
    end
    if Args.Delete || Args.OnlyDelete
        if isstruct(OldStatus) && isfield(OldStatus,'OutputFile')
            for I=1:numel(OldStatus)
                Product = OldStatus(I).OutputFile;
                if endsWith(Product,'_recalibrated.mat') && isfile(Product)
                    delete(Product);
                end
            end
        end
        for Name = {'Status.mat','MergedOnlyMP.mat','Skip.mat'}
            if isfile(Name{1}), delete(Name{1}); end
        end
        OldStatus = [];
    end
    Result = repmat(ztfReductionStatus(),0,1);
    if Args.OnlyDelete, return; end
    Files = dir(Args.FileTemp);
    Nf = numel(Files);
    Status = repmat(ztfReductionStatus(),Nf,1);
    if Args.RunSolarSystem
        if isempty(Args.OrbEl), Args.OrbEl = celestial.OrbitalEl.loadSolarSystem('merge'); end
        if isempty(Args.INPOP), Args.INPOP = celestial.INPOP.init; end
    end
    for If=1:Nf
        Status(If).File = Files(If).name;
        Status(If).Path = pwd;
        PreviousIndex = [];
        if isstruct(OldStatus) && all(isfield(OldStatus,fieldnames(Status)))
            PreviousIndex = find(strcmp({OldStatus.File},Files(If).name),1);
        end
        if ~isempty(PreviousIndex) && OldStatus(PreviousIndex).Success && ...
                isfile(OldStatus(PreviousIndex).OutputFile)
            Status(If) = OldStatus(PreviousIndex);
            continue;
        end
        StartTime = tic;
        try
            [~,~,~,~,Status(If)] = VO.ZTF.reanalize_ZTF_psfcat( ...
                Files(If).name,'OrbEl',Args.OrbEl,'INPOP',Args.INPOP, ...
                'GeoPos',Args.GeoPos,'WriteProd',true, ...
                'FieldID',FieldID,'CCDID',CCDID, ...
                'RunPhotCalib',Args.RunPhotCalib, ...
                'RunSolarSystem',Args.RunSolarSystem, ...
                'OutputDir',Args.OutputDir,'OverWrite',true);
        catch Exception
            Status(If).Error = getReport(Exception,'basic','hyperlinks','off');
            Status(If).Success = false;
            warning('VO:ZTF:ReductionFailed','%s: %s',Files(If).name,Exception.message);
        end
        Status(If).RunTime = toc(StartTime);
        localCheckpoint(Status);
        if Args.Verbose
            fprintf('%d/%d %s success=%d (%.1f s)\n', ...
                If,Nf,Files(If).name,Status(If).Success,Status(If).RunTime);
        end
    end
    localCheckpoint(Status);
    if Args.RunSolarSystem
        OnlyMP = AstroCatalog;
        Nmp = 0;
        for If=1:Nf
            if Status(If).Success && isfile(Status(If).OutputFile)
                Product = load(Status(If).OutputFile,'OnlyMP');
                if Product.OnlyMP.sizeCatalog>0
                    Nmp = Nmp+1;
                    OnlyMP(Nmp) = Product.OnlyMP;
                end
            end
        end
        if Nmp>0
            MergedOnlyMP = merge(OnlyMP,'IsTable',true);
        else
            MergedOnlyMP = AstroCatalog;
        end
        save('MergedOnlyMP.mat','MergedOnlyMP','-v7.3');
    end
    Result = Status;
end

function localCheckpoint(Status)
    TempFile = [tempname(pwd) '.mat'];
    Cleanup = onCleanup(@() localRemoveTemp(TempFile));
    save(TempFile,'Status','-v7.3');
    movefile(TempFile,'Status.mat','f');
end

function localRemoveTemp(File)
    if isfile(File), delete(File); end
end
