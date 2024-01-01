function [AllResult,PM, Report] = pointingModel(Files, Args)
    % Calculate pointing model from a lsit of images and write it to a configuration file.
    % Input  : - File name template to analyze.
    %            Default is 'LAST*PointingModel*sci*.fits'.
    %          * ...,key,val,...
    %            see code.
    % Example: [R,PM,Report] = pipeline.last.pointingModel('LAST*_PointingModel*sci*.fits','StartDate',[08 06 2022 17 54 00],'EndDate',[08 06 2022 18 06 00]);
    
    arguments
        Files                             = 'LAST*_PointingModel*sci*.fits';
        Args.Dirs                         = 'ALL'; %{};
        Args.StartDate                    = [];
        Args.EndDate                      = [];
        Args.Nfiles                       = Inf;  % use only last N files
        Args.astrometryCroppedArgs cell   = {};
        
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

        Nlist = numel(List);
        fprintf('\n\nNumber of images: %i\n\n', Nlist)

        % Solve astrometry for all the pointing model images obtained by
        % one camera.
        for Ilist=1:1:Nlist
            fprintf('%i %s\n', Ilist, List{Ilist}(42:end));
            AI = AstroImage(List{Ilist});
            Keys = AI.getStructKey({'RA','DEC','HA','M_JRA','M_JDEC','M_JHA','JD','LST', 'M_RA', 'M_DEC', 'M_HA'});
            try
                [R, CAI, S] = imProc.astrometry.astrometryCropped(List{Ilist}, ...
                    'RA',Keys.RA, 'Dec',Keys.DEC, 'CropSize',[],Args.astrometryCroppedArgs{:});
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

                Keys.M_RA = NaN;
                Keys.M_DEC = NaN;
                Keys.M_HA = NaN;

            end
            if Ilist==1

                Head   = {'RA','Dec','HA','M_JRA','M_JDEC','M_JHA','M_RA','M_DEC','M_HA','JD','LST','CenterRA','CenterDec','Scale','Rotation','Ngood','AssymRMS'};

                Nhead  = numel(Head);
                Table  = zeros(Nlist,Nhead);
            end
            Table(Ilist,:) = [Keys.RA, Keys.DEC, Keys.HA, Keys.M_JRA, Keys.M_JDEC, Keys.M_JHA, ...
                              Keys.M_RA, Keys.M_DEC, Keys.M_HA,...
                              Keys.JD, Keys.LST, ...
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
        %TableDiff = array2table([-1.*(Result.CenterRA-Result.RA).*cosd(Result.CenterDec), -1.*(Result.CenterDec-Result.Dec)]);
        %TableDiff.Properties.VariableNames = {'DiffHA','DiffDec'};

        %Result = [Result, TableDiff];

        AllResult(Idirs).Result = Result;
        
        
        % generate scattered interpolanets
        %AllResult(Idirs).Fha  = scatteredInterpolant(Result.HA, Result.Dec, Result.DiffHA,'linear','nearest');
        %AllResult(Idirs).Fdec = scatteredInterpolant(Result.HA, Result.Dec, Result.DiffDec,'linear','nearest');
        
    end
    
    %writetable(AllResult(1).Result,'~/Desktop/nora/data/pm_rawdata.csv','Delimiter',',') 
    
    if Args.PrepPointingModel
        % construct a grid
        [TileList,~] = celestial.grid.tile_the_sky(Args.Nha, Args.Ndec);
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
                                    
            % HA diff - Mount - Astrometry
            [Dists, Angles] = celestial.coo.sphere_dist(AllResult(Idirs).Result.M_JRA, AllResult(Idirs).Result.M_JDEC,...
                AllResult(Idirs).Result.CenterRA, AllResult(Idirs).Result.CenterDec, 'deg');
           
            DiffHA_Mnt_Ast(:,Idirs) = Dists.*sin(Angles)*RAD;
            DiffDec_Mnt_Ast(:,Idirs) = Dists.*cos(Angles)*RAD;


            % HA diff - Mount - Astrometry
            %DiffHA_Mnt_Ast(:,Idirs)  = AllResult(Idirs).Result.M_RA  - AllResult(Idirs).Result.CenterRA;
            %DiffDec_Mnt_Ast(:,Idirs) = AllResult(Idirs).Result.M_DEC - AllResult(Idirs).Result.CenterDec;
            
        end

        % Make sure that all the differences are around zero (not 360)
        Flag = DiffHA_Mnt_Ast>180;
        DiffHA_Mnt_Ast(Flag) = DiffHA_Mnt_Ast(Flag) - 360;
        Flag = DiffHA_Mnt_Ast<-180;
        DiffHA_Mnt_Ast(Flag) = DiffHA_Mnt_Ast(Flag) + 360;

        
        % Distortions as a function of J2000 HA and Dec
        MeanDiffHA  = mean(DiffHA_Mnt_Ast,2);
        MeanDiffDec = mean(DiffDec_Mnt_Ast,2);


        PM = [AllResult(1).Result.M_JHA, AllResult(1).Result.M_JHA, ...
            MeanDiffHA, MeanDiffDec];

        
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
                fig1 = figure(1);
                colormap(fig1,jet);
                scatter(Report.M_JHA(Fd), Report.M_JDEC(Fd), [], Report.MeanDiffHA(Fd), 'filled');
                Hc = colorbar;
                H = xlabel('HA [deg]');
                H.FontSize = 18;
                H.Interpreter = 'latex';
                H = ylabel('Dec [deg]');
                H.FontSize = 18;
                H.Interpreter = 'latex';
                Hc.Label.String='HA [Mnt-Ast] (deg)';
                exportgraphics(fig1,'~/log/PM_offsets_HA.png','Resolution',300)

                
                
                % DiffDec vs HA/Dec
                fig2 = figure(2);
                colormap(fig2,jet);
                scatter(Report.M_JHA(Fd), Report.M_JDEC(Fd), [], Report.MeanDiffDec(Fd).*cosd(Report.M_JDEC(Fd)), 'filled');
                Hc = colorbar;
                H = xlabel('HA [deg]');
                H.FontSize = 18;
                H.Interpreter = 'latex';
                H = ylabel('Dec [deg]');
                H.FontSize = 18;
                H.Interpreter = 'latex';
                Hc.Label.String='Dec [Mnt-Ast] (deg)';
                exportgraphics(fig2,'~/log/PM_offsets_Dec.png','Resolution',300)



                % plot offset for individual cameras                
                
                fig3 = figure(3); 
                colormap(fig3,jet);
                scatter(AllResult(1).Result.M_JHA(Fd), AllResult(1).Result.M_JDEC(Fd), [], DiffHA_Mnt_Ast(:,1), 'filled'); colorbar
                title('HA offset camera 1');
                xlabel('HA');
                ylabel('Dec');
                colorbar;
                exportgraphics(fig3,'~/log/PM_offsets_HA_cam1.png','Resolution',300)
                
                fig4 = figure(4); 
                colormap(fig4,jet);
                scatter(AllResult(1).Result.M_JHA(Fd), AllResult(1).Result.M_JDEC(Fd), [], DiffDec_Mnt_Ast(:,1), 'filled'); colorbar
                title('Dec offset Camera 1');
                xlabel('HA');
                ylabel('Dec');
                colorbar;
                exportgraphics(fig4,'~/log/PM_offsets_Dec_cam1.png','Resolution',300)
                   
                fig5 = figure(5); 
                colormap(fig5,jet);
                scatter(AllResult(1).Result.M_JHA(Fd), AllResult(1).Result.M_JDEC(Fd), [], DiffHA_Mnt_Ast(:,2), 'filled'); colorbar
                title('HA offset camera 2');
                xlabel('HA');
                ylabel('Dec');
                colorbar;
                exportgraphics(fig5,'~/log/PM_offsets_HA_cam2.png','Resolution',300)
                
                fig6 = figure(6); 
                colormap(fig6,jet);
                scatter(AllResult(1).Result.M_JHA(Fd), AllResult(1).Result.M_JDEC(Fd), [], DiffDec_Mnt_Ast(:,2), 'filled'); colorbar
                title('Dec offset Camera 2');
                xlabel('HA');
                ylabel('Dec');
                colorbar;
                exportgraphics(fig6,'~/log/PM_offsets_Dec_cam2.png','Resolution',300)

            end
        end

                 
                fig7 = figure(7); 
                colormap(fig7,jet);
                scatter(AllResult(1).Result.M_JHA(Fd), AllResult(1).Result.M_JDEC(Fd), [], DiffHA_Mnt_Ast(:,3), 'filled'); colorbar
                title('HA offset camera 3');
                xlabel('HA');
                ylabel('Dec');
                colorbar;
                exportgraphics(fig7,'~/log/PM_offsets_HA_cam3.png','Resolution',300)
                
                fig8 = figure(8); 
                colormap(fig8,jet);
                scatter(AllResult(1).Result.M_JHA(Fd), AllResult(1).Result.M_JDEC(Fd), [], DiffDec_Mnt_Ast(:,3), 'filled'); colorbar
                title('Dec offset Camera 3');
                xlabel('HA');
                ylabel('Dec');
                colorbar;
                exportgraphics(fig8,'~/log/PM_offsets_Dec_cam3.png','Resolution',300)


                   
                fig9 = figure(9); 
                colormap(fig9,jet);
                scatter(AllResult(1).Result.M_JHA(Fd), AllResult(1).Result.M_JDEC(Fd), [], DiffHA_Mnt_Ast(:,4), 'filled'); colorbar
                title('HA offset camera 4');
                xlabel('HA');
                ylabel('Dec');
                colorbar;
                exportgraphics(fig9,'~/log/PM_offsets_HA_cam4.png','Resolution',300)
                
                fig10 = figure(10);
                colormap(fig10,jet);
                scatter(AllResult(1).Result.M_JHA(Fd), AllResult(1).Result.M_JDEC(Fd), [], DiffDec_Mnt_Ast(:,4), 'filled'); colorbar
                title('Dec offset Camera 4');
                xlabel('HA');
                ylabel('Dec');
                colorbar;
                exportgraphics(fig10,'~/log/PM_offsets_Dec_cam4.png','Resolution',300)

 
            end
        end


    else
        PM = [];
    end

    Date = convert.time(FN(1).Time,'JD','StrDateO');
    PM_FileName = sprintf('PointingModel_%s_%s', FN(1).ProjName, Date{1});
    save('-v7.3', PM_FileName, 'Report');

end