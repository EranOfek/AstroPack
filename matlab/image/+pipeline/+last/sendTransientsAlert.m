function [Status] = sendTransientsAlert(ADc, Args)
    %{
    Send an alert for each LAST transient candidate.
    Input   : - AstroDiff cutouts on transients.
              * ...,key,val,...
                'SaveProducts' - Bool on whether to save stamp products. If
                       true, products will be sent with the alert. If
                       false, alert will be text only. Default is false.
                'SavePath' - Path to directory in which to save products in
                       case SaveProducts is true. If empty, prodcuts won't 
                       be saved. Default is the ''.       
                'BasePath' - Path under which telescope data can be found.
                       If empty, BasePath will be constructed assuming LAST
                       site infrastructure. Default is ''.
    Output  : - Result message.
    Author  : Ruslan Konno (Aug 2024)
    Example : VisitPath = '/path/to/visit/dir'
              [AD, ADc] = runTransientsPipe(VisitPath)
              sendTransientsAlert(ADc)
    %}

    arguments
        ADc

        Args.SaveProducts logical = false;
        Args.UseLASTtools logical = false;
        Args.SavePath = '';
        Args.BasePath = '';

    end

    Status = 'Uncontrolled exit.';

    % Return if no transients candidates empty.
    if isempty(ADc(1).Table)
        Status = 'No transients found.';
        return
    end    

    % Get number of transient cutouts.
    Nadc = numel(ADc);

    % Run loop on each transient cutout
    for Iadc = 1:Nadc
        Transient = ADc(Iadc);

        % Get meta data
        RA = Transient.CatData.getCol('RA');
        Dec = Transient.CatData.getCol('Dec');
        JD = Transient.New.julday;
        DT = celestial.time.jd2date(JD,'H','YMD');
        DateString = strcat(num2str(DT(1)),'-',sprintf('%02.0f',DT(2)), ...
            '-',sprintf('%02.0f',DT(3)),{' '},sprintf('%02.0f',DT(4)), ...
            ':',sprintf('%02.0f',DT(5)),':',sprintf('%02.0f',DT(6)),' UTC');
        Score = Transient.CatData.getCol('SCORE');
        Mag = Transient.CatData.getCol('MAG_PSF');
        Mount = Transient.CatData.getCol('MOUNT');
        Cam = Transient.CatData.getCol('CAM');
        CropID = Transient.CatData.getCol('CROPID');

        % Construct detection message
        Msg = strcat('New transient at', {' '},...
            DateString{1}, {' '},...
            'and RA, Dec =',{' '},sprintf('%.7f',RA),',',sprintf('%.7f',Dec),{' '}, ...
            'with a score of',{' '},sprintf('%.2f',Score),{' '},...
            'and magnitude of',{' '},sprintf('%.2f',Mag),'.');

        % Find transient catalogs on the same field observed within one
        % month interval of transient detection.
        FN = FileNames.generateFromFileName(Transient.New.ImageData.FileName);

        FNzogy = FN.copy();
        FNzogy.Time = {'*.*.*'};
        FNzogy.Level = {'coadd.zogyD'};
        FNzogy.Product = {'Cat'};
        FNzogy.CropID = 0;
        MonthTransient = DT(2);
        YearTransient = DT(1);

        if MonthTransient == 1
            MonthBefore = 12;
            YearBefore = YearTransient - 1;
        else
            MonthBefore = MonthTransient-1;
            YearBefore = YearTransient;
        end

        if MonthTransient == 12
            MonthAfter = 1;
            YearAfter = YearTransient + 1;
        else
            MonthAfter = MonthTransient + 1;
            YearAfter = YearTransient;
        end

        if isempty(Args.BasePath)
            DataDir = strcat('data',num2str(2-mod(Cam,2)));
            Args.BasePath = strcat('/',tools.os.get_computer, ...
                '/',DataDir,'/archive');
        end

        SearchStringBefore = strcat(Args.BasePath,'/',FNzogy.ProjName, ...
            '/',num2str(YearBefore),'/',sprintf('%02.0f',MonthBefore), ...
            '/*/proc/*/',FNzogy.genFile);
        SearchStringTransient = strcat(Args.BasePath,'/',FNzogy.ProjName, ...
            '/',num2str(YearTransient),'/',sprintf('%02.0f',MonthTransient), ...
            '/*/proc/*/',FNzogy.genFile);
        SearchStringAfter = strcat(Args.BasePath,'/',FNzogy.ProjName, ...
            '/',num2str(YearAfter),'/',sprintf('%02.0f',MonthAfter), ...
            '/*/proc/*/',FNzogy.genFile);

        TranCats = AstroCatalog(SearchStringTransient{1});
        TranCatsBefore = AstroCatalog(SearchStringBefore{1});
        TranCatsAfter = AstroCatalog(SearchStringAfter{1});

        if numel(TranCatsBefore) > 1
            TranCats = [TranCatsBefore, TranCats];
        elseif numel(TranCatsBefore) == 1 && ~isempty(TranCatsBefore(1).Table)
            TranCats = [TranCatsBefore, TranCats];
        end

        if numel(TranCatsAfter) > 1
            TranCats = [TranCats, TranCatsAfter];
        elseif numel(TranCatsAfter) == 1 && ~isempty(TranCatsAfter(1).Table)
            TranCats = [TranCats, TranCatsAfter];
        end

        % Get number of catalogs
        Ncats = numel(TranCats);

        % Construct a LC with points and upper limits
        LC_Point = 0;
        LC_UL = 0;

        LastUL_JD = 0;
        LastUL_Mag = 0;

        for Icat = Ncats:-1:1
            TC = merge(TranCats(Icat));
            Match = TC.coneSearch(RA, Dec, 3);

            % LC points
            if Match.Nsrc > 0
                LC_Point = LC_Point + 1;
                Idx = Match.Ind(find(Match.Dist == min(Match.Dist)));
                TC_Row = TC.selectRows(Idx);
                LC_Mag(LC_Point) = TC_Row.getCol('MAG_PSF');
                LC_JD(LC_Point) = TC_Row.getCol('JD') - JD;
                LC_MagErr(LC_Point) = TC_Row.getCol('MAGERR_PSF');
            % LC upper limits
            else
                if ~TC.isColumn('FLAGS_TRANSIENT') || ~TC.isColumn('MOUNT')
                    continue
                end
                
                LC_UL = LC_UL + 1;
                Query_Mount = TC.getCol('MOUNT');
                Query_Cam = TC.getCol('CAM');
                Query_CropID = TC.getCol('CROPID');

                SubSelect = (Query_Mount == Mount) ...
                            & (Query_Cam == Cam) ...
                            & (Query_CropID == CropID);

                TC_SubSelect = TC.selectRows(SubSelect);

                QJD = median(TC_SubSelect.getCol('JD'), 'all');
                QMag = median(TC_SubSelect.getCol('N_LIMMAG'), 'all');

                % Remember the last non-detection before transient
                % detection
                if QJD > LastUL_JD && QJD < JD
                    LastUL_JD = QJD;
                    LastUL_Mag = QMag;
                end

                LC_UL_Mag(LC_UL) = QMag;

                LC_UL_JD(LC_UL) = QJD - JD;

            end

        end

        if LC_Point < 1
            continue
        end

        % If a non-detection is found, construct non-detection message.
        if LastUL_JD > 0
            LastUL_DT = celestial.time.jd2date(LastUL_JD,'H','YMD');
            LastUL_DateString = strcat(num2str(LastUL_DT(1)),'-',sprintf('%02.0f',LastUL_DT(2)), ...
                '-',sprintf('%02.0f',LastUL_DT(3)),{' '},sprintf('%02.0f',LastUL_DT(4)), ...
                ':',sprintf('%02.0f',LastUL_DT(5)),':',sprintf('%02.0f',LastUL_DT(6)),' UTC');
            T0mT = JD - LastUL_JD;
            LastUL_Msg = strcat('Last non-detection was on',{' '}, ...
                LastUL_DateString{1},{' '},'(T0-T=',num2str(T0mT),{' '},'d) with limiting mag of', ...
                {' '},sprintf('%.2f',LastUL_Mag),'.');
            Msg{1} = strcat(Msg{1},'\n',LastUL_Msg{1});

        end

        % If there is a galaxy match, construct potential host match message.
        GalN = Transient.CatData.getCol('GAL_N');

        if GalN > 0
            GalDist = Transient.CatData.getCol('GAL_DIST');
    
            [GLADEpCat,~,~] = catsHTM.cone_search('GLADEp', RA*pi/180, Dec*pi/180, ...
                GalDist*1.5, 'OutType','AstroCatalog');
    
            if GLADEpCat.sizeCatalog > 0
                Bmag = GLADEpCat.getCol('B');
                Redshift = GLADEpCat.getCol('z_cmb');

                Gal_Msg = strcat('Potential host;', {' '}, ...
                    sprintf('%.2f',GalDist), {' '},'arcsec away,', {' '}, ...
                    sprintf('%.2f',Bmag),{' '},'quiescient Bmag,',{' '}, ...
                    sprintf('%.3f',Redshift),{' '},'redshift.');
                Msg{1} = strcat(Msg{1},'\n',Gal_Msg{1});
            end
        end

        % Add a SDDS SkyServer link.

        SDSSLink = imProc.vo.getLinkForSource(Transient,[], @VO.SDSS.navigator_link);
        SDSS_Msg = strcat('<',SDSSLink.Link,'|','Check ', {' '},'SkyServer>');
        Msg{1} = strcat(Msg{1},'\n',SDSS_Msg{1});

        % Add a TNS link.
        TNSLink = strcat('https://www.wis-tns.org/search?ra=', ...
            num2str(RA),'&decl=',num2str(Dec),'&radius=10&coords_unit=arcsec');
        TNS_Msg = strcat('<',TNSLink,'|','Check ', {' '},'TNS>');
        Msg{1} = strcat(Msg{1},'\n',TNS_Msg{1});

        % If SavePath is given, make a stamp image.
        if ~isempty(Args.SavePath)

            % Construct image name
            ImageFN = FN.copy();
            ImageFN.Level = {'coadd.zogyD'};
            ImageFN.Product = {'Image'};
            ImageFN.FileType = {'png'};
            ImageFN.Version = Icat;
            Image_FilenameCell = ImageFN.genFile;
            Image_Filename = Image_FilenameCell{1};
            Image_DirFilenameCell = strcat(Args.SavePath,'/',ImageFN.genFile);
            Image_DirFilename = Image_DirFilenameCell{1};

            % Draw image
            Fig = tiledlayout('flow', 'TileSpacing', 'none');%, 'Padding', 'none');
            % Reference image stamp
            nexttile;
            RefMed = median(Transient.Ref.Image, 'all');
            RefStd = std(Transient.Ref.Image, 0, 'all');
            RefMin = RefMed-RefStd*3;
            RefMax = RefMed+RefStd*3;
            imshow(Transient.Ref.Image, [RefMin RefMax]);
            text(2,47,'Ref','Color','white','FontSize',14)
            % New image stamp
            nexttile;
            NewMed = median(Transient.New.Image, 'all');
            NewStd = std(Transient.New.Image, 0, 'all');
            NewMin = NewMed-NewStd*3;
            NewMax = NewMed+NewStd*3;        
            imshow(Transient.New.Image, [NewMin NewMax]);
            text(2,47,'New','Color','white','FontSize',14)
            % Difference image stamp
            nexttile;
            DiffMed = median(Transient.Image, 'all');
            DiffStd = std(Transient.Image, 0, 'all');
            DiffMin = DiffMed-DiffStd*3;
            DiffMax = DiffMed+DiffStd*3;        
            imshow(Transient.Image, [DiffMin DiffMax]);
            text(2,47,'Diff','Color','white','FontSize',14)
            % Lightcurve
            nexttile([1 3]);
            errorbar(LC_JD, LC_Mag, LC_MagErr,'o');
            XlimMin = -5;
            if LC_UL > 0
                hold on;
                scatter(LC_UL_JD, LC_UL_Mag, 'v');
                hold off;
                XlimMin = max(-30,min(LC_UL_JD-5));
            end
            set(gca, 'YDir','reverse');
            xlim([XlimMin 5]);
            set(gca,'fontsize',14)
    
            % If Args.SaveProducts true, save image
            if Args.SaveProducts
                saveas(Fig, Image_DirFilename);
            end
        end
        
        % Use last-tools to send alerts
        if Args.UseLASTtools
            if ~isfile(Image_DirFilename)
                Status='Alerting via last-tools requires a saved image, which does not exist.';
                return
            end
            
            Text_DirFilename = replace(Image_DirFilename,'.png','.txt');
            fid = fopen(Text_DirFilename,'wt');
            fprintf(fid, Msg{1});
            fclose(fid);
            CMD0 = strcat('last-transient-slack-alert --message-file',{' '},Text_DirFilename,' --image-file',{' '},Image_DirFilename);
            [CMD0Status, CMD0Out] = system(CMD0{1});
            if CMD0Status > 0
                Status = sprint('Alerting via last-tools failed: %s', CMD0Out);
                return
            end
            Status =  'Succesful exit, alert(s) sent.';
            return
        end
        
        % Get SlackBot token and transients channel id.

        ChannelID = getenv('SLACK_TRANSIENTS_CHANNEL');
        SlackBotToken = getenv('SLACK_BOT_TOKEN');     

        if isempty(ChannelID)
            Status = 'ChannelID environment variable not set.';
            return
        end

        if isempty(SlackBotToken)
            Status = 'SlackBot token environment variable not set.';
            return
        end

        % Check if cURL is installed.
        [~,CheckCurl] = system('command -v curl');
        if isempty(CheckCurl)
            Status = 'cURL not installed.';
            return
        end

        % Test connection
        [ConnectionTest1,~] = system('curl -D - "https://slack.com/api/api.test"');
    
        if (ConnectionTest1 > 0)
            Status = sprintf('Slack API error at first connection test: %i', ConnectionTest1);
            return
        end

        [~,ConnectTest2Out] = system('curl -X POST https://slack.com/api/api.test');

        ConnectionTest2 = jsondecode(strcat("{",extractAfter(ConnectTest2Out,"{")));

        if ~ConnectionTest2.ok
            Status = sprintf('Slack API error at second connection test: %s', ConnectionTest2.error);
            return
        end

        % Send slack alert.
        % Args.SaveProducts is true and image file exists, send image with
        % slack alert. Otherwise send text alert only.
        if Args.SaveProducts && isfile(Image_DirFilename)

            % Get file size.
            FileForSize = dir(Image_DirFilename);
            Filesize = num2str(FileForSize.bytes);

            % Request image host URL.
            CMD1 = strcat("curl -F files=@",Image_DirFilename," -F filename=",Image_Filename," -F token=",SlackBotToken," -F length=",Filesize," https://slack.com/api/files.getUploadURLExternal");
        
            [~,CMD1out] = system(CMD1);
        
            % Retrieve image host URL.
            Response1 = jsondecode(strcat("{",extractAfter(CMD1out,"{")));

            if ~Response1.ok
                Status = sprintf('Slack API error at host request: %s', Response1.error);
                return
            end
        
            UploadUrl = Response1.upload_url;
            FileID = Response1.file_id;

            % Upload image to host URL.
        
            CMD2 = strcat("curl -F  filename=@",Image_DirFilename," -H 'Authorization: Bearer ",SlackBotToken,"' -v POST ",UploadUrl);
        
            [~,~] = system(CMD2);

            % Authorize post to slack channel.
        
            CMD3 = strcat("curl -X POST -H 'Authorization: Bearer ",SlackBotToken,"' -H 'Content-Type: application/json' -d '",'{"files": [{"id":"',FileID,'", "title":"NewTransient"}], "channel_id": "',ChannelID,'", "initial_comment": "',Msg,'" }',"' https://slack.com/api/files.completeUploadExternal");
        
            [~,CMD3out] = system(CMD3);

            Response3 = jsondecode(strcat("{",extractAfter(CMD3out,"{")));

            if ~Response3.ok
                Status = sprintf('Slack API error at authorization: %s', Response3.error);
                return
            end

        else

            % Post text only message to slack.
            CMD = strcat("curl -d 'text=",Msg{1},"' -d 'channel=",ChannelID,"' -H 'Authorization: Bearer ",SlackBotToken,"' -X POST https://slack.com/api/chat.postMessage");
            [~,~] = system(CMD);
        end

        Status = 'Succesful exit, alert(s) sent.';

    end
end