function [Result] = reanalizeFieldCam_ZTF_psfcat(Args)
    % For all images in Field/camera - Given ZTF psfcat apply  photometric ZP, astrometry and match to known asteroids
    % Input  : - 
    %          - 
    %          * ...,key,val,... 
    % Output : - 
    % Author : Eran Ofek (2024 Apr) 
    % Example:
    % VO.ZTF.reanalizeFieldCam_ZTF_psfcat('BasePath','/raid/eran/projects/telescopes/ZTF/sourceCatalogs/DAO','FieldID',600,'CCDID',1);

    arguments
        Args.Path               = [];  % e.g., /raid/eran/projects/telescopes/ZTF/sourceCatalogs/DAO/600/1
        Args.BasePath           = [];  % e.g., /raid/eran/projects/telescopes/ZTF/sourceCatalogs/DAO
        Args.FieldID            = [];  % char
        Args.CCDID              = [];  % char
        Args.FileTemp           = 'ztf_*_psfcat.fits';
        Args.Delete logical     = true;
        Args.OnlyDelete logical = false;
         
        Args.OrbEl  = celestial.OrbitalEl.loadSolarSystem('merge');
        Args.INPOP  = celestial.INPOP.init;
        Args.GeoPos = 'p48';
        Args.Verbose logical     = false;

    end
    RAD = 180./pi;


    if isempty(Args.FieldID) || isempty(Args.CCDID)
        error('FieldID and CCDID must be supplied');
    end

    if isnumeric(Args.FieldID)
        Args.FieldID = sprintf('%d', Args.FieldID);
    end
    if isnumeric(Args.CCDID)
        Args.CCDID = sprintf('%d', Args.CCDID);
    end

    if ~isempty(Args.BasePath)
        % use RelPath
        Args.Path = sprintf('%s%s%s%s%s', Args.BasePath, filesep, Args.FieldID, filesep, Args.CCDID);
    end
        
      

    if ischar(Args.GeoPos)
        switch lower(Args.GeoPos)
            case 'p48'
                GeoPosSt = celestial.earth.observatoryCoo('Name','Palomar48');
                Args.GeoPos = [GeoPosSt.Lon./RAD, GeoPosSt.Lat./RAD, GeoPosSt.Height];
            otherwise
                error('Unknown observatory name');
        end
    end

    PWD = pwd;
    if ~isempty(Args.Path)
        cd(Args.Path);
    end

    if isfile('Status.mat')
        % skip
        fprintf('Skip %s\n',Args.Path);
    else
        fprintf('Working on %s\n',Args.Path);

        Files = dir(Args.FileTemp);
    
        Nf    = numel(Files);
        OnlyMP = AstroCatalog;
        %SkipAll = struct('File',{},'Path',{},'ZP',{});
    
        if Args.Delete
            system('rm ZTF*_1.fits');
            system('rm Status.mat');
            system('rm Skip.mat');
            system('rm MergedOnlyMP.mat');
        end
    
        Status = struct('ZP',cell(Nf,1), 'File',cell(Nf,1), 'Path',cell(Nf,1), 'AstNgood',cell(Nf,1), 'AstAssymRMS',cell(Nf,1));
        if ~Args.OnlyDelete
            for If=1:1:Nf
                % 
                [If Nf]
                
                % re use of the AstrometricCat
                tic;
                try
                    [UpdatedCat, OnlyMP(If), AstrometricCat, Status(If), Path] = VO.ZTF.reanalize_ZTF_psfcat(Files(If).name, 'Path',[], 'OrbEl',Args.OrbEl, 'INPOP',Args.INPOP, 'GeoPos',Args.GeoPos,...
                                                                            'WriteProd',true,...
                                                                            'FieldID',Args.FieldID,...
                                                                            'CCDID',Args.CCDID);
                catch
                    'Failed'
                end
                RunTime = toc;
                if Args.Verbose
                    %[Idir1, Idir2, If, Nf]
                    RunTime
                end
            end
    
            io.files.save('-v7.3','Status.mat',Status)
            MergedOnlyMP = merge(OnlyMP, 'IsTable',true);
            MergedOnlyMP.sizeCatalog
    
            io.files.save('-v7.3','MergedOnlyMP.mat',MergedOnlyMP)
        end

        cd(PWD);
    end



end
