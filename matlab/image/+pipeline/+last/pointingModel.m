function Result = pointingModel(Files, Args)
    % Calculate pointing model from a lsit of images.
    %   
    % Example: R = pipeline.last.pointingModel([],'StartDate',[08 06 2022 17 54 00],'EndDate',[08 06 2022 18 06 00]);
    
    arguments
        Files                             = 'LAST*PointingModel*sci*.fits';
        Args.Dirs                         = 'ALL'; %{};
        Args.StartDate                    = [];
        Args.EndDate                      = [];
        Args.Nfiles                       = 108;  % use only last N files
        %Args.Dir                          = pwd;
        Args.astrometryCroppedArgs cell   = {};
        %Args.backgroundArgs cell          = {};
        %Args.findMeasureSourcesArgs cell  = {};
    end
    
    if isempty(Files)
        Files = 'LAST*sci*.fits';
    end
    
    PWD = pwd;
    
    if ischar(Args.Dirs)
        if strcmp(Args.Dirs, 'ALL')
            % find all 4 dirs of data
            PN = pipeline.last.constructProjName([],[],1,1,1);
            
            
    
    cd(Args.Dir);
    
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
        catch
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
    
    % generate scattered interpolanets
    % Fha = scatteredInterpolant(R4.HA, R4.Dec, (R4.CenterRA-R4.RA).*cosd(R4.CenterDec),'linear','nearest');
    % Fdec = scatteredInterpolant(R4.HA, R4.Dec, (R4.CenterDec-R4.Dec),'linear','nearest');
    
end