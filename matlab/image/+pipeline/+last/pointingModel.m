function [AllResult,PM] = pointingModel(Files, Args)
    % Calculate pointing model from a lsit of images and write it to a configuration file.
    % Input  : - File name template to analyze.
    %            Default is 'LAST*PointingModel*sci*.fits'.
    %          * ...,key,val,...
    %            see code.
    % Example: R = pipeline.last.pointingModel([],'StartDate',[08 06 2022 17 54 00],'EndDate',[08 06 2022 18 06 00]);
    
    arguments
        Files                             = 'LAST*PointingModel*sci*.fits';
        Args.Dirs                         = 'ALL'; %{};
        Args.StartDate                    = [];
        Args.EndDate                      = [];
        Args.Nfiles                       = Inf;  % use only last N files
        %Args.Dir                          = pwd;
        Args.astrometryCroppedArgs cell   = {};
        %Args.backgroundArgs cell          = {};
        %Args.findMeasureSourcesArgs cell  = {};
        
        Args.PrepPointingModel logical    = true;
        Args.Nha                          = 20; %30
        Args.Ndec                         = 10; %15
        Args.MinAlt                       = 25; % [deg]
        Args.ObsCoo                       = [35 30];  % [deg]
        Args.ConfigFile                   = '';
    end
    RAD = 180./pi;
    
    if isempty(Files)
        Files = 'LAST*sci*.fits';
    end
    
    PWD = pwd;
    
    if ischar(Args.Dirs)
        if strcmp(Args.Dirs, 'ALL')
            % find all 4 dirs of data
            for Icam=1:1:4
                Dirs{Icam} = pipeline.last.constructCamDir(Icam);
            end
        else
            Dirs{1} = Args.Dirs;
        end
    elseif iscell(Args.Dirs)
        % assuming already a cell array
        Dirs = Args.Dirs;
    else
        error('Dirs must be a char array or cell array');
    end
    
    Ndirs = numel(Dirs);
    for Idirs=1:1:Ndirs
            
    
        cd(Dirs{Idirs});

        List = ImagePath.selectByDate(Files, Args.StartDate, Args.EndDate);
        if numel(List)>Args.Nfiles
            List = List(end-Args.Nfiles+1:end);
        end

        Nlist = numel(List);
        for Ilist=1:1:Nlist
            Ilist
            AI = AstroImage(List{Ilist});
            Keys = AI.getStructKey({'RA','DEC','HA','M_JRA','M_JDEC','M_JHA','JD','LST'});
            try
                [R, CAI, S] = imProc.astrometry.astrometryCropped(List{Ilist}, 'RA',Keys.RA, 'Dec',Keys.DEC, 'CropSize',[],Args.astrometryCroppedArgs{:});
            catch ME
                ME
                
                fprintf('Failed on image %d\n',Ilist);

                S.CenterRA = NaN;
                S.CenterDec = NaN;
                S.Scale = NaN;
                S.Rotation = NaN;
                S.Ngood = 0;
                S.AssymRMS = NaN;
            end
            if Ilist==1
                Head   = {'RA','Dec','HA','M_JRA','M_JDEC','M_JHA','JD','LST','CenterRA','CenterDec','Scale','Rotation','Ngood','AssymRMS'};
                Nhead  = numel(Head);
                Table  = zeros(Nlist,Nhead);
            end
            Table(Ilist,:) = [Keys.RA, Keys.DEC, Keys.HA, Keys.M_JRA, Keys.M_JDEC, Keys.M_JHA, Keys.JD, Keys.LST, ...
                              S.CenterRA, S.CenterDec, S.Scale, S.Rotation, S.Ngood, S.AssymRMS];

        end

        Result = array2table(Table);
        Result.Properties.VariableNames = Head;

        cd(PWD);

        TableDiff = array2table([-(Result.RA-Result.CenterRA).*cosd(Result.CenterDec), -(Result.Dec-Result.CenterDec)]);
        TableDiff.Properties.VariableNames = {'DiffHA','DiffDec'};

        Result = [Result, TableDiff];

        AllResult(Idirs).Result = Result;
        
        % generate scattered interpolanets
        AllResult(Idirs).Fha  = scatteredInterpolant(Result.HA, Result.Dec, (Result.CenterRA-Result.RA).*cosd(Result.CenterDec),'linear','nearest');
        AllResult(Idirs).Fdec = scatteredInterpolant(Result.HA, Result.Dec, (Result.CenterDec-Result.Dec),'linear','nearest');
        
    end
    
    if Args.PrepPointingModel
        [TileList,TileArea] = celestial.coo.tile_the_sky(Args.Nha, Args.Ndec);
        HADec = TileList(:,1:2);

        [Az, Alt] = celestial.coo.hadec2azalt(HADec(:,1), HADec(:,2), Args.ObsCoo(2)./RAD);

        % convert everything to degrees
        Az = Az*RAD;
        Alt = Alt*RAD;
        HADec = HADec*RAD;
        % convert to -pi to pi
        F180 = HADec(:,1)>180;
        HADec(F180,1) = HADec(F180,1) - 360;
        

        Flag = Alt>(Args.MinAlt);
        HADec = HADec(Flag,:);
        Ntarget = sum(Flag);
       
        ResidHA  = zeros(Ntarget,Ndirs);
        ResidDec = zeros(Ntarget,Ndirs);
        for Idirs=1:1:Ndirs
                        
            ResidHA(:,Idirs)  = AllResult(Idirs).Fha(HADec(:,1),HADec(:,2));
            ResidDec(:,Idirs) = AllResult(Idirs).Fha(HADec(:,1),HADec(:,2));
        end
        
        MeanResidHA  = mean(ResidHA,2,'omitnan');
        MeanResidDec = mean(ResidDec,2,'omitnan');
        PM = [HADec, MeanResidHA, MeanResidDec];
        Flag = any(isnan(PM),2);
        PM   = PM(~Flag,:);
        
        % add these values to avoid extrapolation at dec 90 deg
        AtPole = [-135 90 0 0; -90 90 0 0; -45 90 0 0; 0 90 0 0; 45 90 0 0; 90 90 0 0; 135 90 0 0];
        PM = [PM; AtPole];
        
        if ~isempty(Args.ConfigFile)
            % write config file
            FID = fopen(Args.ConfigFile,'w');
            fprintf(FID,'# pointing model interpolation data\n');
            fprintf(FID,'# Generated on: %s\n',date);
            fprintf(FID,'# format:       [M_JHA,  M_JDec,  offsetHA,  offsetDec]\n');
            
            fprintf(FID,'PointingData : [\n');
            Npm = size(PM,1);
            for Ipm=1:1:Npm
                fprintf(FID,'         [%11.6f, %11.6f, %11.6f, %11.6f],\n',PM(Ipm,:));
            end
            fprintf(FID,'     ]\n');
            fclose(FID);
        end
    end
    
end