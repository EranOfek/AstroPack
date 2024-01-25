function [AllResult] = pointingModel_Solve(Files, Args)
    % Calculate pointing model from a list of images and write it to a configuration file.
    % Input  : - File name template to analyze.
    %            Default is 'LAST*_PointingModel*sci*.fits'.
    %          * ...,key,val,...
    %            see code.
    % Example: [R,PM,Report] = pipeline.last.pointingModel('LAST*_PointingModel*sci*.fits','StartDate',[08 06 2022 17 54 00],'EndDate',[08 06 2022 18 06 00]);
    
    arguments
        Files                             = 'LAST*_PointingModel*sci*.fits';
        Args.Dirs                         = 'ALL';
        Args.StartDate                    = -Inf;
        Args.EndDate                      = Inf;
        Args.Nfiles                       = Inf;  % use only last N files
        
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

        FN = FileNames.generateFromFileName(Files);
        FN = selectByDate(FN, Args.StartDate, Args.EndDate);        
        List = genFile(FN); 

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
                Result = TableRow;
            else
                Result = [Result; TableRow];
            end
        end

        AllResult(Idirs).Result = Result;
        
    end
    
end

%%% Internal Functions



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
        
    HeadHead   = {'RA','DEC','HA',...
              'M_JRA','M_JDEC','M_JHA',...
              'M_RA','M_DEC','M_HA',...
              'M_ARA','M_ADEC','M_AHA',...
              'M_ADRA','M_ADDEC','M_ADHA',...
              'JD','LST'};
    Nhh = numel(HeadHead);
    HeadSol = {'CenterRA','CenterDec', 'CenterHA',...
              'Scale','Rotation',...
              'Ngood',...
              'AssymRMS'};


    Head = [HeadHead, HeadSol];
    Nh   = numel(Head);

	AI = AstroImage(ImageFileName);
            
    Keys = AI.getStructKey(HeadHead);
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

        Row = nan(1, Nhh);
        for Ihh=1:1:Nhh
            Row(1,Ihh) = Keys.(HeadHead{Ihh});
        end
        Row = [Row, [S.CenterRA, S.CenterDec, CenterHA, S.Scale, S.Rotation, S.Ngood, S.AssymRMS]];
                
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