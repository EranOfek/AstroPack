function [AllResult,PM, Report] = pointingModel_plots(Files, Args)
    % Calculate pointing model from a lsit of images and write it to a configuration file.
    % Input  : - File name template to analyze.
    %            Default is 'LAST*PointingModel*sci*.fits'.
    %          * ...,key,val,...
    %            see code.
    % Example: [R,PM,Report] = pipeline.last.pointingModel_plots('LAST*_PointingModel*sci*.fits','StartDate',[08 06 2022 17 54 00],'EndDate',[08 06 2022 18 06 00]);
    
    arguments
        Files                             = 'LAST*_PointingModel*sci*.fits';
        Args.Dirs                         = 'ALL';
        Args.StartDate                    = [];
        Args.EndDate                      = [];
        Args.Nfiles                       = Inf;  % use only last N files
        Args.astrometryCroppedArgs cell   = {};
        
        Args.ObsCoo                       = [35 30];  % [deg]
        Args.ConfigFile                   = '/home/ocs/pointingModel.txt';

        Args.Plot logical                 = true;
    end
    
    RAD = 180./pi;
    
    Dirs = getImageDirs(Args.Dirs);    
    Ndirs = numel(Dirs);
    
    % For each camera
    for Idirs=1:1:Ndirs
               
        cd(Dirs{Idirs});

        List = FileNames.selectByDate(Files, Args.StartDate, Args.EndDate);
        if numel(List)>Args.Nfiles
            List = List(end-Args.Nfiles+1:end);
        end

        Nfiles = numel(List);
        fprintf('\n\n%i images in dir %s\n\n', Nfiles, Dirs{Idirs})

        
        Head   = {'RA','Dec','HA','M_JRA','M_JDEC','M_JHA', ...
            'M_RA','M_DEC','M_HA', 'JD','LST', ...
            'CenterRA','CenterDec','Scale','Rotation','Ngood',...
            'AssymRMS', 'M_RA', 'M_DEC', 'M_HA'};
        Nhead  = numel(Head);
        Table  = zeros(Nfiles,Nhead);
        
        
        % Solve astrometry for all the pointing model images obtained by
        % one camera.
        for Ifile=1:1:Nfiles
            
            fprintf('%i %s\n', Ifile, List{Ifile}(42:end));
            
            AI = AstroImage(List{Ifile});
            Keys = AI.getStructKey({'RA','DEC','HA','M_JRA','M_JDEC', ...
                'M_JHA','M_RA','M_DEC','M_HA','JD','LST'});
            try
                [~, ~, S] = imProc.astrometry.astrometryCropped(...
                    List{Ifile}, 'RA',Keys.RA, 'Dec',Keys.DEC, ...
                    'CropSize',[], Args.astrometryCroppedArgs{:});
                
                Table(Ifile,:) = [Keys.RA, Keys.DEC, Keys.HA, ...
                    Keys.M_JRA, Keys.M_JDEC, Keys.M_JHA, ...
                    Keys.M_RA, Keys.M_DEC, Keys.M_HA, Keys.JD, Keys.LST, ...
                    S.CenterRA, S.CenterDec, S.Scale, S.Rotation, ...
                    S.Ngood, S.AssymRMS];
                
            catch ME
                %ME
                
                fprintf('Astrometry failed for image %d\n',Ifile);
                Table(Ifile,:) = ones(1, Nhead)*NaN;
            end
                          
        end

        Result = array2table(Table);
        Result.Properties.VariableNames = Head;
        

        % There was a sign bug here - fixed 15-Nov-2023
        %TableDiff = array2table([-1.*(Result.CenterRA-Result.RA).*cosd(Result.CenterDec), -1.*(Result.CenterDec-Result.Dec)]);
        %TableDiff.Properties.VariableNames = {'DiffHA','DiffDec'};
        %Result = [Result, TableDiff];

        AllResult(Idirs).Result = Result;
        
    end
    
    if nargout==1
        return
    end

    
    % Calculate offset for each PM image
    for Idirs=1:1:Ndirs
                                    
        % HA diff - Mount - Astrometry
        [Dists, Angles] = celestial.coo.sphere_dist( ...
            AllResult(Idirs).Result.M_RA, AllResult(Idirs).Result.M_DEC,...
            AllResult(Idirs).Result.CenterRA, ...
            AllResult(Idirs).Result.CenterDec, 'deg');
           
        DiffHA_Mnt_Ast(:,Idirs) = Dists.*sin(Angles)*RAD;
        DiffDec_Mnt_Ast(:,Idirs) = Dists.*cos(Angles)*RAD;

    end

    % Make sure that all the differences are around zero (not 360)
    Flag = DiffHA_Mnt_Ast>180;
    DiffHA_Mnt_Ast(Flag) = DiffHA_Mnt_Ast(Flag) - 360;
    Flag = DiffHA_Mnt_Ast<-180;
    DiffHA_Mnt_Ast(Flag) = DiffHA_Mnt_Ast(Flag) + 360;

        
    % Mean distortion for all 4 cameras as a function of J2000 HA and Dec
    MeanDiffHA  = mean(DiffHA_Mnt_Ast,2);
    MeanDiffDec = mean(DiffDec_Mnt_Ast,2);

    M_JRA = AllResult(1).Result.M_JRA;
    M_JHA = AllResult(1).Result.M_JHA;
    M_JDEC = AllResult(1).Result.M_JDEC;
    
    PM = [M_JHA, M_JDEC, MeanDiffHA, MeanDiffDec];


    Flag = any(isnan(PM),2);
    PM   = PM(~Flag,:);
        
    % add these values to avoid extrapolation at dec 90 deg
    AtPole = [-135 90 0 0; -90 90 0 0; -45 90 0 0; 0 90 0 0; 45 90 0 0; ...
        90 90 0 0; 135 90 0 0];
    PM = [PM; AtPole];
        
        
    if ~isempty(Args.ConfigFile)
        writePMFile(Args.ConfigFile, date, PM);
    end
        
        
    if nargout<3
        return
    end
        
    % Generate report

    Report.M_JHA           = M_JHA;
    Report.M_JDEC          = M_JDEC;
    Report.M_JRA           = M_JRA;
    Report.DiffHA_Mnt_Ast  = DiffHA_Mnt_Ast;   % Mount - Astrometry [HA] (deg)
    Report.MeanDiffHA      = MeanDiffHA;       % mean of 4 cameras
    Report.DiffDec_Mnt_Ast = DiffDec_Mnt_Ast;   % Mount - Astrometry [Dec] (deg)
    Report.MeanDiffDec     = MeanDiffDec;       % mean of 4 cameras
            

    
    FN = FileNames(List{1});
    Date = convert.time(FN.julday,'JD','StrDateO');
    ReportFileName = sprintf('PointingModel_%s_%s', FN.ProjName{1}, Date{1});
    save('-v7.3', ReportFileName, 'Report');


    if ~Args.Plot
        return
    end
           
    
    
    Fd = M_JDEC<90;
           
    
    Flag = M_JRA>180;
    M_JRA(Flag) = M_JRA(Flag)-360;

    
    fig = figure('Position',[100 100 900 600]);
    hold on

    mount = scatter(M_JRA, M_JDEC, 40, 'xb');

    CenterRA = zeros(Nfiles, 4);
    CenterDec = zeros(Nfiles, 4);
    for Idir=1:1:4
        
        Flag = AllResult(:,Idir).Result.RA>180;
        AllResult(:,Idir).Result.RA(Flag) = AllResult(Idir).Result.RA(Flag)-360;
        
        Flag = AllResult(:,Idir).Result.CenterRA>180;
        AllResult(:,Idir).Result.CenterRA(Flag) = AllResult(Idir).Result.CenterRA(Flag)-360;

        
        CenterRA(:,Idir) = AllResult(Idir).Result.CenterRA;
        CenterDec(:,Idir) = AllResult(Idir).Result.CenterDec;
        
        
        scatter(AllResult(Idir).Result.CenterRA, ...
            AllResult(Idir).Result.CenterDec, 20, '+r')
            
        scatter(AllResult(Idir).Result.RA, ...
            AllResult(Idir).Result.Dec, 20, 'xb')

    end
    
    reco = scatter(mean(CenterRA,2), mean(CenterDec,2),40, '+r');
    
    hold off
            
    legend([mount, reco], {'Mount', 'Reco'}, 'Location', 'SouthEast');
    xlabel('HA [deg]');
    ylabel('Dec [deg]');
    exportgraphics(fig,'~/log/PM.png','Resolution',300)
        

    % plot mean HA offset
    plotOffsets(AllResult(1).Result.M_JHA(Fd), ...
        AllResult(1).Result.M_JDEC(Fd), Report.MeanDiffHA(Fd), ...
        'HA offset (mean for 4 cameras)', ...
        '~/log/PM_offsets_HA.png')

    % plot mean Dec offset
    plotOffsets(AllResult(1).Result.M_JHA(Fd), ...
        AllResult(1).Result.M_JDEC(Fd), ...
        Report.MeanDiffDec(Fd).*cosd(Report.M_JDEC(Fd)), ...
        'Dec offset (mean for 4 cameras)', ...
        '~/log/PM_offsets_Dec.png')
       

    for Icam = 1:1:4
        
        % plot HA offset
        plotOffsets(AllResult(1).Result.M_JHA(Fd), ...
            AllResult(1).Result.M_JDEC(Fd), DiffHA_Mnt_Ast(Fd,Icam), ...
            'Dec offset Camera '+string(Icam), ...
            '~/log/PM_offsets_Dec_cam'+string(Icam)+'.png')

        % plot Dec offset
        plotOffsets(AllResult(1).Result.M_JHA(Fd), ...
            AllResult(1).Result.M_JDEC(Fd), DiffDec_Mnt_Ast(Fd,Icam), ...
            'Dec offset Camera '+string(Icam), ...
            '~/log/PM_offsets_Dec_cam'+string(Icam)+'.png')
    end

end



function ImgDirs = getImageDirs(Dirs)

    if ischar(Dirs)
        if strcmp(Dirs, 'ALL')
            % find all 4 dirs of data
            for Icam=1:1:4
                ImgDirs{Icam} = pipeline.last.constructCamDir(Icam);
            end
        else
            ImgDirs{1} = Dirs;
            fprintf('\n\nNeed images of all four cameras.\n\n')
        end
    elseif iscell(Dirs)
        % assuming already a cell array
        ImgDirs = Dirs;
        fprintf('\n\nNeed images of all four cameras.\n\n')

    else
        error('Dirs must be a char array or cell array');
    end

end


function writePMFile(ConfigFile, date, PM)

    % write config file
    FID = fopen(ConfigFile,'w');
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



function plotOffsets(HA, Dec, Offset, Title, SaveName)

    fig = figure;
    colormap(fig,jet);
    scatter(HA, Dec, [], Offset, 'filled'); 
    colorbar;
    title(Title);
    xlabel('HA');
    ylabel('Dec');
    exportgraphics(fig,SaveName,'Resolution',300)

end