function [AllResult] = pointingModel_Solve(Files, Args)
    % Perform astrometric solution to a list of images and save the data needed for pointing model.
    %   For calculating and writing the pointing model use:
    %   pipeline.last.pointingModel_Write
    % Input  : - File name template to analyze.
    %            Default is 'LAST*_PointingModel*sci*.fits'.
    %          * ...,key,val,...
    %            see code.
    % Author : Eran Ofek (Jan 2024)
    % Example: [R] = pipeline.last.pointingModel_Solve('LAST*_PointingModel*sci*.fits','StartDate',[08 06 2022 17 54 00],'EndDate',[08 06 2022 18 06 00]);
    
    arguments
        Files             = 'LAST*_PointingModel*sci*.fits';
        Args.Dirs         = 'ALL';  % or vector of numbedrs [1 2 3 4]
        Args.StartDate    = -Inf;
        Args.EndDate      = Inf;
        Args.Nfiles       = Inf;  % use only last N files
        Args.SaveName     = '/home/ocs/pointingModelAstrometry.mat';
    end
    
    
    Dirs = getImageDirs(Args.Dirs);    
    Ndirs = numel(Dirs);
    
    % For each camera
    for Idirs=1:1:Ndirs
               
        cd(Dirs{Idirs});

        % get list of files
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
            
            ImgName = split(List{Ifile}, '/');
            fprintf('%i %s\n', Ifile, ImgName{end});
            TableRow = getAstrometricSolution(List{Ifile});
            
            if Ifile==1
                Result = TableRow;
            else
                Result = [Result; TableRow];
            end
        end

        ResultAllCams(Idirs).Result = Result;
        
    end
    
    save('-v7.3', Args.SaveName, 'ResultAllCams');
    
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
    elseif isnumeric(Dirs)
        N = numel(Dirs);
        for Icam=1:1:N
            CamN = Dirs(Icam);
            ImgDirs{Icam} = pipeline.last.constructCamDir(CamN);
        end
    else
        error('Dirs must be a char array or cell array');
    end

end


function Row = getAstrometricSolution(ImageFileName)
        
    HeadHead   = {'RA','DEC',...
              'M_JRA','M_JDEC',...
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

