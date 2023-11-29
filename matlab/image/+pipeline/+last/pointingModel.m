function [AllResult,PM, Report] = pointingModel(Files, Args)
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
        Args.ConfigFile                   = '/home/ocs/pointingModel.txt';

        Args.RemoveNaN logical            = false;
        Args.Plot logical                 = true;
    end
    
    RAD = 180./pi;
    
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
        % For each camera (4 cameras on a LAST mount)  
        
        cd(Dirs{Idirs});

        [List,~,FN] = ImagePath.selectByDate(Files, Args.StartDate, Args.EndDate);
        if numel(List)>Args.Nfiles
            List = List(end-Args.Nfiles+1:end);
        end

        fprintf('Number of images:')
        Nlist = numel(List);
        %List
        % Solve astrometry for all the pointing model images obtained by
        % one camera.
        for Ilist=1:1:Nlist
            Ilist
            List{Ilist}
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
                
                Keys.RA = NaN;
                Keys.DEC = NaN;
                Keys.HA = NaN;
                Keys.M_JRA = NaN;
                Keys.M_JDEC = NaN;
                Keys.M_JHA = NaN;
                
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
        
        if Args.RemoveNaN
            MaskNaN = isnan(Result.RA);
            Result = Result(~MaskNaN,:);
        end
        
        cd(PWD);

        % There was a sign bug here - fixed 15-Nov-2023
        TableDiff = array2table([-1.*(Result.CenterRA-Result.RA).*cosd(Result.CenterDec), -1.*(Result.CenterDec-Result.Dec)]);
        TableDiff.Properties.VariableNames = {'DiffHA','DiffDec'};

        Result = [Result, TableDiff];

        AllResult(Idirs).Result = Result;
        
        
        % generate scattered interpolanets
        %AllResult(Idirs).Fha  = scatteredInterpolant(Result.HA, Result.Dec, Result.DiffHA,'linear','nearest');
        %AllResult(Idirs).Fdec = scatteredInterpolant(Result.HA, Result.Dec, Result.DiffDec,'linear','nearest');
        
    end
    
    %writetable(AllResult(1).Result,'~/Desktop/nora/data/pm_rawdata.csv','Delimiter',',') 
    
    if Args.PrepPointingModel
        % construct a grid
        [TileList,~] = celestial.coo.tile_the_sky(Args.Nha, Args.Ndec);
        HADec = TileList(:,1:2);

        [Az, Alt] = celestial.coo.hadec2azalt(HADec(:,1), HADec(:,2), Args.ObsCoo(2)./RAD);

        % convert everything to degrees
        Az = Az*RAD;
        Alt = Alt*RAD;
        HADec = HADec*RAD;
        % convert to -180 to 180
        F180 = HADec(:,1)>180;
        HADec(F180,1) = HADec(F180,1) - 360;
        

        Flag = Alt>(Args.MinAlt);
        HADec = HADec(Flag,:);
        Ntarget = sum(Flag);
       
        ResidHA  = zeros(Ntarget,Ndirs);
        ResidDec = zeros(Ntarget,Ndirs);

        % Interpolate the results over the grid
        for Idirs=1:1:Ndirs
                        
            %ResidHA(:,Idirs)  = AllResult(Idirs).Fha(HADec(:,1),HADec(:,2));
            %ResidDec(:,Idirs) = AllResult(Idirs).Fdec(HADec(:,1),HADec(:,2));
            %[-1.*(Result.CenterRA-Result.RA).*cosd(Result.CenterDec), -1.*(Result.CenterDec-Result.Dec)]
            
            % HA diff - Mount - Astrometry
            DiffHA_Mnt_Ast(:,Idirs)  = AllResult(Idirs).Result.M_JRA  - AllResult(Idirs).Result.CenterRA;
            DiffDec_Mnt_Ast(:,Idirs) = AllResult(Idirs).Result.M_JDEC - AllResult(Idirs).Result.CenterDec;
            
        end

        % Make sure that all the differences are around zero (not 360)
        Flag = DiffHA_Mnt_Ast>180;
        DiffHA_Mnt_Ast(Flag) = DiffHA_Mnt_Ast(Flag) - 360;
        Flag = DiffHA_Mnt_Ast<-180;
        DiffHA_Mnt_Ast(Flag) = DiffHA_Mnt_Ast(Flag) + 360;

        
        % Distortions as a function of J2000 HA and Dec
        MeanDiffHA  = mean(DiffHA_Mnt_Ast,2);
        MeanDiffDec = mean(DiffDec_Mnt_Ast,2);

        PM = [AllResult(1).Result.M_JHA(Fd), AllResult(1).Result.M_JHA(Fd), MeanDiffHA(Fd), MeanDiffDec(Fd)];


        
        %MeanResidHA  = mean(ResidHA,2,'omitnan');
        %MeanResidDec = mean(ResidDec,2,'omitnan');
        %PM = [HADec, MeanResidHA, MeanResidDec];
        
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
        
        
        if nargout>2
            % Generate report

            % % plot pointing model
            % factor = 15; % increase shifts for visibility
            % 
            % f = figure('Position',[100,100,600,600]);
            % hold on
            % 
            % Npoints = length(Result.HA);
            % for i=1:1:Npoints
            %     plot([Result.HA(i),Result.HA(i)+Result.DiffHA(i)*factor], [Result.Dec(i), Result.Dec(i)+Result.DiffDec(i)*factor], '-b','linewidth',3)
            % end
            % plot(Result.HA, Result.Dec, 'xb','MarkerSize',8)
            % 
            % Npoints_inter = length(PM(:,1));
            % for i=1:1:Npoints_inter
            %     plot([PM(i,1), PM(i,1)+PM(i,3)*factor], [PM(i,2), PM(i,2)+PM(i,4)*factor], '-r')
            % end
            % 
            % 
            % xlabel('HA (deg)')
            % ylabel('Dec (deg)')
            % title('Pointing Model (shifts increased by x15)')
            % 
            % 
            % exportgraphics(f,'~/log/pointing_model.png','Resolution',300)

            Report.M_JHA           = AllResult(1).Result.M_JHA;
            Report.M_JDEC          = AllResult(1).Result.M_JDEC;
            Report.M_JRA           = AllResult(1).Result.M_JDEC;
            Report.DiffHA_Mnt_Ast  = DiffHA_Mnt_Ast;   % Mount - Astrometry [HA] (deg)
            Report.MeanDiffHA      = MeanDiffHA;       % mean of 4 cameras
            Report.DiffDec_Mnt_Ast = DiffDec_Mnt_Ast;   % Mount - Astrometry [Dec] (deg)
            Report.MeanDiffDec     = MeanDiffDec;       % mean of 4 cameras
            

            if Args.Plot
           
                Fd = AllResult(1).Result.M_JDEC<90;

                % DiffHA vs HA/Dec
                figure(1);
                scatter(Report.M_JHA(Fd), Report.M_JDEC(Fd), [], Report.MeanDiffHA(Fd), 'filled');
                Hc = colorbar;
                H = xlabel('HA [deg]');
                H.FontSize = 18;
                H.Interpreter = 'latex';
                H = ylabel('Dec [deg]');
                H.FontSize = 18;
                H.Interpreter = 'latex';
                Hc.Label.String='HA [Mnt-Ast] (deg)';

                % DiffDec vs HA/Dec
                figure(2);
                scatter(Report.M_JHA(Fd), Report.M_JDEC(Fd), [], Report.MeanDiffDec(Fd).*cosd(Report.M_JDEC(Fd)), 'filled');
                Hc = colorbar;
                H = xlabel('HA [deg]');
                H.FontSize = 18;
                H.Interpreter = 'latex';
                H = ylabel('Dec [deg]');
                H.FontSize = 18;
                H.Interpreter = 'latex';
                Hc.Label.String='HA [Mnt-Ast] (deg)';


                % plot differences between cameras on the same mount                
                DiffH = (DiffHA_Mnt_Ast(:,1) - DiffHA_Mnt_Ast(:,4)).*cosd(AllResult(1).Result.M_JDEC);
                figure(3); scatter(AllResult(1).Result.M_JHA(Fd), AllResult(1).Result.M_JDEC(Fd), [], DiffH(Fd), 'filled'); colorbar
                DiffH = (DiffHA_Mnt_Ast(:,2) - DiffHA_Mnt_Ast(:,3)).*cosd(AllResult(1).Result.M_JDEC);
                figure(4); scatter(AllResult(1).Result.M_JHA(Fd), AllResult(1).Result.M_JDEC(Fd), [], DiffH(Fd), 'filled'); colorbar
        
                DiffH = (DiffDec_Mnt_Ast(:,1) - DiffDec_Mnt_Ast(:,2));
                figure(5); scatter(AllResult(1).Result.M_JHA(Fd), AllResult(1).Result.M_JDEC(Fd), [], DiffH(Fd), 'filled'); colorbar
                DiffH = (DiffDec_Mnt_Ast(:,4) - DiffDec_Mnt_Ast(:,3));
                figure(6);scatter(AllResult(1).Result.M_JHA(Fd), AllResult(1).Result.M_JDEC(Fd), [], DiffH(Fd), 'filled'); colorbar

            end
        end



    else
        PM = [];
    end

    Date = convert.time(FN(1).Time,'JD','StrDateO');
    PM_FileName = sprintf('PointingModel_%s_%s', FN(1).ProjName, Date{1});
    save('-v7.3', PM_FileName, 'Report');

end