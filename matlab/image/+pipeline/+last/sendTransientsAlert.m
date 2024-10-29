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
                'SingleEpochThresh' - Score threshold for a passing
                       candidate to be reported if it passes only once.
                       Default is 8.0.
    Output  : - Result message.
    Author  : Ruslan Konno (Aug 2024)
    Example : VisitPath = '/path/to/visit/dir'
              [AD, ADc, TCL1, Status] = runTransientsPipe(VisitPath)
              ADc = matchTransientsToMultiEpochs(ADc, TCL1)
              sendTransientsAlert(ADc)
    %}

    arguments
        ADc

        Args.SaveProducts logical = false;
        Args.UseLASTtools logical = false;
        Args.SavePath = '';

        Args.SingleEpochThresh = 8.0;

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

        Flags = Transient.CatData.getCol('FLAGS_TRANSIENT');
        PassingTran = (Flags == 0);
        NumPassingTran = sum(PassingTran);

        Score = Transient.CatData.getCol('SCORE');

        % Report only if transient candidate has been detected at least
        % twice of with a > Args.SingeEpochThresh sigma significance 
        % within a single epoch

        if NumPassingTran == 1 
            SingleEpochScore = Score(PassingTran);
            if SingleEpochScore < Args.SingleEpochThresh
                continue
            end
        end

        % Get meta data
        RA = Transient.CatData.getCol('RA');
        Dec = Transient.CatData.getCol('Dec');
        JD = Transient.CatData.getCol('JD');
        JD0 = Transient.New.julday;

        DT = celestial.time.jd2date(JD0,'H','YMD');
        DateString = strcat(num2str(DT(1)),'-',sprintf('%02.0f',DT(2)), ...
            '-',sprintf('%02.0f',DT(3)),{' '},sprintf('%02.0f',DT(4)), ...
            ':',sprintf('%02.0f',DT(5)),':',sprintf('%02.0f',DT(6)),' UTC');
        Mag = Transient.CatData.getCol('MAG_PSF');

        Ind0 = find(JD == JD0);

        if numel(Ind0) > 1
            Ind0 = Ind0(1);
        end

        RA0 = RA(Ind0);
        Dec0 = Dec(Ind0);
        Score0 = Score(Ind0);
        Mag0 = Mag(Ind0);

        % Construct detection message
        Msg = strcat('New transient at', {' '},...
            DateString{1}, {' '},...
            'and RA, Dec =',{' '},sprintf('%.7f',RA0),',',sprintf('%.7f',Dec0),{' '}, ...
            'with a score of',{' '},sprintf('%.2f',Score0),{' '},...
            'and magnitude of',{' '},sprintf('%.2f',Mag0),'.');

        % Construct a LC with points and upper limits
        LC_UL = 0;
    
        % LC points
        LC_Mag = Transient.CatData.getCol('MAG_PSF');
        LC_JD = Transient.CatData.getCol('JD') - JD0;
        LC_MagErr = Transient.CatData.getCol('MAGERR_PSF');
        % LC upper limits
        if isprop(Transient,'ULCatData') && ~isempty(Transient.ULCatData)
            LC_UL = Transient.ULCatData.sizeCatalog;
            if LC_UL > 0
                LC_UL_JD = Transient.ULCatData.getCol('JD');
                LC_UL_Mag = Transient.ULCatData.getCol('MagUL');
            end
        end

        % Construct last non-detection message.
        % If available, use a recent observations, 
        % otherwise use reference image.
        if LC_UL > 0
            RelJD = JD0 - LC_UL_JD;
            T0mT = min(RelJD);
            LastUL_JD = LC_UL_JD(find(RelJD == T0mT,1));
            LastUL_Mag = LC_UL_Mag(find(RelJD == T0mT,1));
            LC_UL_JD = -RelJD;

            LastUL_DT = celestial.time.jd2date(LastUL_JD,'H','YMD');
            LastUL_DateString = strcat(num2str(LastUL_DT(1)),'-',sprintf('%02.0f',LastUL_DT(2)), ...
                '-',sprintf('%02.0f',LastUL_DT(3)),{' '},sprintf('%02.0f',LastUL_DT(4)), ...
                ':',sprintf('%02.0f',LastUL_DT(5)),':',sprintf('%02.0f',LastUL_DT(6)),' UTC');
            LastUL_Msg = strcat('Last non-detection (observation) was on',{' '}, ...
                LastUL_DateString{1},{' '},'(T0-T=',num2str(T0mT),{' '},'d) with limiting mag of', ...
                {' '},sprintf('%.2f',LastUL_Mag),'.');
            Msg{1} = strcat(Msg{1},'\n',LastUL_Msg{1});
        else
            Ref_JD = Transient.Ref.HeaderData.getVal('JD');
            T0mT = JD0 - Ref_JD;
            Ref_LimMag = Transient.Ref.HeaderData.getVal('LIMMAG');

            Ref_DT = celestial.time.jd2date(Ref_JD,'H','YMD');
            Ref_DateString = strcat(num2str(Ref_DT(1)),'-',sprintf('%02.0f',Ref_DT(2)), ...
                '-',sprintf('%02.0f',Ref_DT(3)),{' '},sprintf('%02.0f',Ref_DT(4)), ...
                ':',sprintf('%02.0f',Ref_DT(5)),':',sprintf('%02.0f',Ref_DT(6)),' UTC');
            RefUL_Msg = strcat('Last non-detection (reference) was on',{' '}, ...
                Ref_DateString{1},{' '},'(T0-T=',num2str(T0mT),{' '},'d) with limiting mag of', ...
                {' '},sprintf('%.2f',Ref_LimMag),'.');
            Msg{1} = strcat(Msg{1},'\n',RefUL_Msg{1});

        end

        % If there is a galaxy match, construct potential host match message.
        GalN = Transient.CatData.getCol('GAL_N');

        if any(GalN > 0)
            GalDists = Transient.CatData.getCol('GAL_DIST');
            GalDists = GalDists(GalDists>0);
            GalDist = mean(GalDists);
    
            [GLADEpCat,~,~] = catsHTM.cone_search('GLADEp', RA0*pi/180, Dec0*pi/180, ...
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

        if RA0 < 0
            RA0 = 360 + RA0;
        end

        % Add a SDDS SkyServer link.
        TranCat0 = Transient.CatData.selectRows(Ind0);
        SDSSLink = imProc.vo.getLinkForSource(TranCat0,[], @VO.SDSS.navigator_link);
        SDSS_Msg = strcat('<',SDSSLink.Link,'|','Check ', {' '},'SkyServer>');
        Msg{1} = strcat(Msg{1},'\n',SDSS_Msg{1});

        % Add a PS1 link.
        PlusSign = '';
        if Dec0 > 0
            PlusSign = '+';
        end
        PS1Link =  strcat('https://ps1images.stsci.edu/cgi-bin/ps1cutouts?pos=', ...
            num2str(RA0),PlusSign,num2str(Dec0),'&filter=color&size=720');
        PS1_Msg = strcat('<',PS1Link,'|','Check ', {' '},'PS1>');
        Msg{1} = strcat(Msg{1},'\n',PS1_Msg{1});

        % Add a Simbad link.
        SimbadLink =  strcat('http://simbad.u-strasbg.fr/simbad/',...
            'sim-coo?protocol=html&NbIdent=1&Radius=1&Radius.unit=arcmin',...
            '&CooFrame=FK5&CooEpoch=2000&CooEqui=2000&Coord=', ...
            num2str(RA0),PlusSign,num2str(Dec0));
        Simbad_Msg = strcat('<',SimbadLink,'|','Check ', {' '},'Simbad>');
        Msg{1} = strcat(Msg{1},'\n',Simbad_Msg{1});

        % Add a TNS link.
        TNSLink = strcat('https://www.wis-tns.org/search?ra=', ...
            num2str(RA0),'&decl=',num2str(Dec0),'&radius=10&coords_unit=arcsec');
        TNS_Msg = strcat('<',TNSLink,'|','Check ', {' '},'TNS>');
        Msg{1} = strcat(Msg{1},'\n',TNS_Msg{1});

        % If SavePath is given, make a stamp image.
        if ~isempty(Args.SavePath)

            % Construct image name
            FN = FileNames.generateFromFileName(Transient.New.ImageData.FileName);
            ImageFN = FN.copy();
            ImageFN.Level = {'coadd.zogyD'};
            ImageFN.Product = {'Image'};
            ImageFN.FileType = {'png'};
            ImageFN.Version = Iadc;
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