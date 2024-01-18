function [AllResult,PM, Report] = pointingModel(Files, Args)
    % Calculate pointing model from a list of images and write it to a configuration file.
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
        %Args.astrometryCroppedArgs cell   = {};
        
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

        %%% TODO remove dependency on deprecated ImagePath and use
        %%% FileNames instead
        %List = FileNames.selectByDate(Files, Args.StartDate, Args.EndDate);
        List = ImagePath.selectByDate(Files, Args.StartDate, Args.EndDate);
        
        if numel(List)>Args.Nfiles
            List = List(end-Args.Nfiles+1:end);
        end

        Nfiles = numel(List);
        fprintf('\n\n%i images in dir %s\n\n', Nfiles, Dirs{Idirs})

        
        
        % Solve astrometry for all the pointing model images obtained by
        % one camera.
        for Ifile=1:1:Nfiles
            
            fprintf('%i %s\n', Ifile, List{Ifile}(42:end));
            TableRow = getAstrometricSolution(List{Ifile});
            
            if Ifile==1
                Table = TableRow
            else
                Table = [Table; TableRow]
            end
        end

        Result = array2table(Table);
        Result.Properties.VariableNames = Head;
        

        AllResult(Idirs).Result = Result;
        
    end
    
    if nargout==1
        return
    end

    
    % Calculate offset for each PM image
    for Idirs=1:1:Ndirs
                                    
        % HA diff - Mount - Astrometry
        [Dists, Angles] = celestial.coo.sphere_dist( ...
            AllResult(Idirs).Result.CenterHA, ...
            AllResult(Idirs).Result.CenterDec, ...
            AllResult(Idirs).Result.M_HA, AllResult(Idirs).Result.M_DEC, ...
            'deg');
           
        DiffHA(:,Idirs) = Dists.*sin(Angles)*RAD;
        DiffDec(:,Idirs) = Dists.*cos(Angles)*RAD;

    end

    % Make sure that all the differences are around zero (not 360)
    Flag = DiffHA>180;
    DiffHA(Flag) = DiffHA(Flag) - 360;
    Flag = DiffHA<-180;
    DiffHA(Flag) = DiffHA(Flag) + 360;

        
    % Mean distortion for all 4 cameras as a function of J2000 HA and Dec
    MeanDiffHA  = mean(DiffHA,2);
    MeanDiffDec = mean(DiffDec,2);

    M_RA = AllResult(1).Result.M_RA;
    M_HA = AllResult(1).Result.M_HA;
    M_DEC = AllResult(1).Result.M_DEC;
    
    PM = [M_HA, M_DEC, MeanDiffHA, MeanDiffDec];


    Flag = any(isnan(PM),2);
    PM   = PM(~Flag,:);
        
    % add these values to avoid interpolation at dec 90 deg
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
    Report.M_HA           = M_HA;
    Report.M_DEC          = M_DEC;
    Report.M_RA           = M_RA;
    Report.DiffHA         = DiffHA;   % Mount - Astrometry [HA] (deg)
    Report.MeanDiffHA     = MeanDiffHA;       % mean of 4 cameras
    Report.DiffDec        = DiffDec;   % Mount - Astrometry [Dec] (deg)
    Report.MeanDiffDec    = MeanDiffDec;       % mean of 4 cameras
            

    
    FN = FileNames(List{1});
    Date = convert.time(FN.julday,'JD','StrDateO');
    ReportFileName = sprintf('PointingModel_%s_%s', FN.ProjName{1}, Date{1});
    save('-v7.3', ReportFileName, 'Report');


    if ~Args.Plot
        return
    end
           
    
    Flag = M_RA>180;
    M_RA(Flag) = M_RA(Flag)-360;

    
    % plot default and reconstructed coordinates of all cameras and the
    % mount
    fig = figure('Position',[100 100 900 600]);
    hold on

    % default mount coordinates
    mount = scatter(M_RA, M_DEC, 40, 'xb');

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
    
    % mean reconstructed coordinates
    reco = scatter(mean(CenterRA,2), mean(CenterDec,2),40, '+r');
    
    hold off
            
    legend([mount, reco], {'Mount', 'Reco'}, 'Location', 'SouthEast');
    xlabel('HA [deg]');
    ylabel('Dec [deg]');
    exportgraphics(fig,'~/log/PM.png','Resolution',300)
        

    % plot mean HA offset
    plotOffsets(AllResult(1).Result.M_HA, ...
        AllResult(1).Result.M_DEC, Report.MeanDiffHA, ...
        'HA offset (mean for 4 cameras)', ...
        '~/log/PM_offsets_HA.png')

    % plot mean Dec offset
    plotOffsets(AllResult(1).Result.M_HA, ...
        AllResult(1).Result.M_DEC, ...
        Report.MeanDiffDec, ...
        'Dec offset (mean for 4 cameras)', ...
        '~/log/PM_offsets_Dec.png')
       

    for Icam = 1:1:4
        
        % plot HA offset
        plotOffsets(AllResult(1).Result.M_HA, ...
            AllResult(1).Result.M_DEC, DiffHA(:,Icam), ...
            'HA offset Camera '+string(Icam), ...
            '~/log/PM_offsets_HA_cam'+string(Icam)+'.png')

        % plot Dec offset
        plotOffsets(AllResult(1).Result.M_HA, ...
            AllResult(1).Result.M_DEC, DiffDec(:,Icam), ...
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


function Row = getAstrometricSolution(ImageFileName)
        
    Head   = {'RA','Dec','HA','M_JRA','M_JDEC','M_JHA', ...
            'M_RA','M_DEC','M_HA', 'JD','LST', ...
            'CenterRA','CenterDec', 'CenterHA','Scale','Rotation','Ngood',...
            'AssymRMS'};

	AI = AstroImage(ImageFileName);
            
    Keys = AI.getStructKey({'RA','DEC','HA','M_JRA','M_JDEC', ...
        'M_JHA','M_RA','M_DEC','M_HA','JD','LST'});
    try
        [~, ~, S] = imProc.astrometry.astrometryCropped(...
            AI, 'RA',Keys.RA, 'Dec',Keys.DEC, ...
            'CropSize',[]); %, Args.astrometryCroppedArgs{:});
                        
        CenterHA = Keys.LST - S.CenterRA;
        if CenterHA<-180
            CenterHA = CenterHA+360;
        elseif CenterHA>180
            CenterHA = CenterHA-360;
        end

        Row = [Keys.RA, Keys.DEC, Keys.HA, ...
        	Keys.M_JRA, Keys.M_JDEC, Keys.M_JHA, ...
            Keys.M_RA, Keys.M_DEC, Keys.M_HA, Keys.JD, Keys.LST, ...
            S.CenterRA, S.CenterDec, CenterHA, S.Scale, S.Rotation, ...
            S.Ngood, S.AssymRMS];
                
    catch
        fprintf('Astrometry failed for image %s\n',ImageFileName);
        Row = ones(1, length(Head))*NaN;
    end

    Row = array2table(Row);
    Row.Properties.VariableNames = Head;


end


function writePMFile(ConfigFile, date, PM)

    % write config file
    FID = fopen(ConfigFile,'w');
    fprintf(FID,'# pointing model interpolation data\n');
    fprintf(FID,'# Generated on: %s\n',date);
    fprintf(FID,'# format:       [M_HA,  M_Dec,  offsetHA,  offsetDec]\n');
            
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