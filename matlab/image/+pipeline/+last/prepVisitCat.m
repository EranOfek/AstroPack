function [OutTable] = prepVisitCat(Args)
    % Prepare a local catalog of visits, or update the visits catalog.
    %     This will be obsolete when the DB will be ready
    % Input  : * ...,key,val,... 
    %            See code.
    % Output : - Table of all coadded sub images in all visits.
    % Author : Eran Ofek (2024 Jul) 
    % Example: % first time:
    %          pipeline.last.prepVisitCat('TouchFile','/home/eran/.prepVisitCat', 'UseTouchFile',false, 'UseStatus',true);
    %          % second time
    %          pipeline.last.prepVisitCat('UseTouchFile',false,'UseStatus',true);


    arguments
        Args.BasePath          = '/marvin';
        Args.Template          = '.status'; %'LAST.*_coadd_*.fits'
        Args.TemplateFITS      = 'LAST*_coadd_Image_1.fits';
        Args.DirTemplate       = 'LAST.01.*';

        Args.Nsub              = 24;

        Args.UseStatus logical = false; %true; %false;
        Args.Msg               = 'prepVisitCatV5';

        Args.TouchFile            = '/home/eran/.prepVisitCat';
        Args.UseTouchFile logical = true;
        Args.CleanStatus logical  = false;
    end

    if ~isempty(Args.TouchFile)
        % write touch file
        system(sprintf('touch %s',Args.TouchFile));
    end
    if Args.UseTouchFile
        Newer = Args.TouchFile;
    else
        Newer = [];
    end

    PWD = pwd;
    cd(Args.BasePath);


    Dir = dir(Args.DirTemplate);
    Ndir = numel(Dir);
    for Idir=1:1:Ndir
        [Idir, Ndir]

        FullPath = fullfile(Dir(Idir).folder, Dir(Idir).name);
        cd(FullPath);

        F = io.files.findFiles(Args.Template, 'IgnoreHidden',false, 'MinSize',[], 'MaxSize',[], 'Newer',Newer);

        Ind = 0;
        Nmax = numel(F).*24;
        Table = nan(Nmax,35);
        AllVisit = strings(Nmax,1);
        AllField = strings(Nmax,1);
    
        fprintf('Dir %d out of %d - %d new visits found\n', Idir, Ndir, numel(F));
        
    
    
        N = numel(F);
        for I=1:1:N
            [Idir, Ndir, I, N]
            cd(F(I).folder);

            % clean .status file
            if Args.CleanStatus
                system('sudo chmod a+w .status');
                system("grep -v prepVisitCat .status > .status")
            end

            % Update status file
            if Args.UseStatus
                StatFile = tools.timeStamp.readMsgFile('.status',F(I).folder);
            end
            if Args.UseStatus && any(contains({StatFile.Msg}, Args.Msg))
                % already done - skip
                %'skip'
            else
    
                try
                % get info from folder name
                TmpSp    = split(F(I).folder,filesep);
                Year     = str2double(TmpSp{4});
                Month    = str2double(TmpSp{5});
                Day      = str2double(TmpSp{6});
                Visit    = string(TmpSp{end});
                ProjName = TmpSp{3};
                TmpPr    = split(ProjName, '.'); 
                Node     = str2double(TmpPr{2});
                Mount    = str2double(TmpPr{3});
                Camera   = str2double(TmpPr{4});
                
                Files    = dir(Args.TemplateFITS);
                Nfiles   = numel(Files);
                catch
                    'a'
                end
        
                for Ifiles=1:1:Nfiles
                    
    
                    Ind = Ind + 1;
                    try
                        H = AstroHeader(Files(Ifiles).name);
        
        
                        StH=H.getStructKey({'CROPID',...
                            'MIDJD', 'AVNCOADD', 'EXPTIME',...
                            'RAU1', 'RAU2', 'RAU3', 'RAU4',...
                            'DECU1', 'DECU2', 'DECU3', 'DECU4', ...
                            'AIRMASS', 'FWHM', 'MED_A', 'MED_B', 'MED_TH', 'GM_RATEX', 'GM_RATEY', 'FOCUS', 'MNTTEMP', 'MEDBCK',...
                            'PH_COL1', 'PH_MEDC', 'PH_NSRC', 'PH_RMS', 'PH_ZP',...
                            'AST_ARMS', 'AST_NSRC','FIELDID'});
                        
                        StH = tools.struct.structEmpty2NaN(StH);
    
                        CropID = StH.CROPID;
                        try
                        Table(Ind,:) = [Node, Mount, Camera, Year, Month, Day, CropID,...
                            StH.MIDJD, StH.AVNCOADD, StH.EXPTIME,...
                            StH.RAU1, StH.RAU2, StH.RAU3, StH.RAU4,...
                            StH.DECU1, StH.DECU2, StH.DECU3, StH.DECU4, ...
                            StH.AIRMASS, StH.FWHM, StH.MED_A, StH.MED_B, StH.MED_TH, StH.GM_RATEX, StH.GM_RATEY, StH.FOCUS, StH.MNTTEMP, StH.MEDBCK,...
                            StH.PH_COL1, StH.PH_MEDC, StH.PH_NSRC, StH.PH_RMS, StH.PH_ZP,...
                            StH.AST_ARMS, StH.AST_NSRC];
                        catch
                            'b'
                        end
                        AllVisit(Ind) = Visit;
                        AllField(Ind) = string(StH.FIELDID);
                    catch ME
                        % can't read file
                    end

        
                end %for Ifiles=1:1:Nfiles
    
                % Update status file
                if Args.UseStatus
                    system('sudo chmod a+w .status');
                    Result=tools.timeStamp.writeMsg(Args.Msg, 'FileName','.status','Path',F(I).folder);
                end

            end % if Args.UseStatus && any(contains({StatFile.Msg}, Args.Msg))
    
        end %for I=1:1:N
    
        cd(PWD);
        IndNN    = find(~isnan(Table(:,1)));
        Table    = Table(IndNN,:);
        AllVisit = AllVisit(IndNN);
        AllField = AllField(IndNN);
    
        T(Idir).T = [array2table(Table), table(AllVisit), table(AllField)];
        T(Idir).T.Properties.VariableNames ={'Node', 'Mount', 'Camera', 'Year', 'Month', 'Day', 'CropID',...
                    'MIDJD', 'AVNCOADD', 'EXPTIME',...
                    'RAU1', 'RAU2', 'RAU3', 'RAU4',...
                    'DECU1', 'DECU2', 'DECU3', 'DECU4', ...
                    'AIRMASS', 'FWHM', 'MED_A', 'MED_B', 'MED_TH', 'GM_RATEX', 'GM_RATEY', 'FOCUS', 'MNTTEMP', 'MEDBCK',...
                    'PH_COL1', 'PH_MEDC', 'PH_NSRC', 'PH_RMS', 'PH_ZP',...
                    'AST_ARMS', 'AST_NSRC', 'Visit', 'FieldID'};
    
    end
    

    OutTable = T(1).T;
    for Idir=2:1:Ndir
        OutTable = [OutTable; T(Idir).T];
    end

end
