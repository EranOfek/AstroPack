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
              [AD, ADc, TCL1, Status] = pipeline.last.transients.runTransientsPipe(VisitPath)
              [ADc, TCL2, Status] = pipeline.last.transients.matchTransientsToMultiEpochs(ADc, TCL1)
              [Status] = pipeline.last.transients.sendTransientsAlert(ADc)
    %}

    arguments
        ADc

        Args.SaveProducts logical = false;
        Args.UseLASTtools logical = false;
        Args.TransferTranProducts = true;
        Args.SavePath = '';

        Args.SingleEpochThresh = 7.7;
        Args.thisIsATest = false;

    end

    Status = 'Uncontrolled exit.';

    % Return if no transients candidates empty.
    if isempty(ADc(1).Table)
        Status = 'No transients found, nothing to report.';
        return
    end

    % Get number of transient cutouts.
    Nadc = numel(ADc);
    NadcNotReported = 0;

    % Run loop on each transient cutout
    for Iadc = 1:Nadc
        Transient = ADc(Iadc);

        PhotFlags = Transient.PhotCatData.getCol('FLAGS_TRANSIENT');
        PassingTran = (PhotFlags == 0);
        NumPassingTran = sum(PassingTran);

        PhotScore = Transient.PhotCatData.getCol('SCORE');

        % Report only if transient candidate has been detected at least
        % twice of with a > Args.SingeEpochThresh sigma significance 
        % within a single epoch

        if NumPassingTran == 1 
            SingleEpochScore = PhotScore(PassingTran);
            if SingleEpochScore < Args.SingleEpochThresh
                NadcNotReported = NadcNotReported + 1;
                continue
            end
        end

        TC = Transient.CatData;

        TNS_Report = [];
        AT_Report = [];
        LAST_report = [];

        % Get date
        JD0 = Transient.New.julday;

        DT = celestial.time.jd2date(JD0,'H','YMD');
        DateString = strcat(num2str(DT(1)),'-',sprintf('%02.0f',DT(2)), ...
            '-',sprintf('%02.0f',DT(3)),{' '},sprintf('%02.0f',DT(4)), ...
            ':',sprintf('%02.0f',DT(5)),':',sprintf('%02.0f',DT(6)),' UTC');

        RA0 = TC.Table.RA;
        Dec0 = TC.Table.Dec;
        Score0 = TC.Table.SCORE;
        Mag0 = TC.Table.MAG_PSF;

        RAfield = [];
        Decfield = [];

        RAfield.value = RA0;
        Decfield.value = Dec0;
        AT_Report.RA = RAfield;
        AT_Report.Dec = Decfield;

        AT_Report.reporting_group_id = 139;
        AT_Report.discovery_data_source_id = 139;
        AT_Report.reporter = "R. Konno (WIS), E. Zimmerman (WIS), A. Horowicz (WIS), S. Garrappa (WIS), E. O. Ofek (WIS), S. Ben-Ami (WIS), D. Polishook (WIS), P. Chen (WIS), A. Krassilchtchikov (WIS), Y. M. Shani (WIS), E. Segre (WIS), A. Gal-Yam (WIS), S. Spitzer (WIS), and K. Rybicki (WIS) on behalf of the LAST Collaboration";
        AT_Report.discovery_datetime = DateString;
        AT_Report.at_type = 1;
        
        Mount0 = Transient.HeaderData.getVal('MOUNTNUM');
        Camera0 = Transient.HeaderData.getVal('CAMNUM');      
        CropID0 = Transient.HeaderData.getVal('CROPID');
        Object0 = Transient.HeaderData.getVal('OBJECT');

        LAST_report.mount = Mount0;

        if isnumeric(Object0)
            Object0 = sprintf('%i',Object0);
        end

        ObjectParts = split(Object0, '.');
        if numel(ObjectParts) > 1
            Field0 = ObjectParts{1};
        else
            Field0 = Object0;
        end

        LAST_report.object = Object0;
        LAST_report.cropid = CropID0;
        LAST_report.field = Field0;
        LAST_report.camera = Camera0;
        LAST_report.score = Score0;

        % Construct detection message
        Msg = strcat('New transient at', {' '},...
            DateString{1}, {' '},...
            'and RA, Dec =',{' '},sprintf('%.7f',RA0),',',sprintf('%.7f',Dec0),{' '}, ...
            'with a score of',{' '},sprintf('%.2f',Score0),{' '},...
            'and magnitude of',{' '},sprintf('%.2f',Mag0),'.');

        if Transient.AlreadyReported
            Msg{1} = strcat(':mailbox_closed: This transient was already reported before:mailbox_closed:\n',Msg{1});
        end

        if Args.thisIsATest
            Msg{1} = strcat(':wrench: This is a test :wrench:\n',Msg{1});
        end

        TelTarget_Msg = strcat('Discovered in sub-image',{' '},...
            sprintf('%.0i',CropID0),{' '},'of field',{' '},...
            Object0,{' '},'by M',sprintf('%.0i',Mount0),...
            'C',sprintf('%.0i',Camera0),'.');

        Msg{1} = strcat(Msg{1},'\n',TelTarget_Msg{1});

        % Construct a LC with points and upper limits
        LC_UL = 0;
    
        % LC points
        LC_Mag = Transient.PhotCatData.getCol('MAG_PSF');
        LC_JD = Transient.PhotCatData.getCol('JD');
        FirstDetection = min(LC_JD);
        LC_JD = LC_JD - JD0;
        LC_MagErr = Transient.PhotCatData.getCol('MAGERR_PSF');
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
        Ref_JD = Transient.Ref.HeaderData.getVal('JD');
        Ref_LimMag = Transient.Ref.HeaderData.getVal('LIMMAG');
        LastUL_JD = Ref_JD;
        LastUL_Mag = Ref_LimMag;
        T0mTRef = JD0 - Ref_JD;
        T0mT = T0mTRef;
        RefExpTime = Transient.Ref.HeaderData.getVal('EXPTIME');
        LastUL_ExpTime = RefExpTime;

        if LC_UL > 0
            LC_UL_JD_BeforeFirstDet = LC_UL_JD(LC_UL_JD < FirstDetection);
            LC_UL_Mag_BeforeFirstDet = LC_UL_Mag(LC_UL_JD < FirstDetection);
            LC_UL_BeforeFirstDet = numel(LC_UL_JD_BeforeFirstDet);
    
            if LC_UL_BeforeFirstDet > 0
                RelJD = JD0 - LC_UL_JD_BeforeFirstDet;
                T0mT = min(RelJD);
                LastUL_JD = LC_UL_JD_BeforeFirstDet(find(RelJD == T0mT,1));
                LastUL_Mag = LC_UL_Mag_BeforeFirstDet(find(RelJD == T0mT,1));
            end
            LC_UL_JD = LC_UL_JD - JD0;
            LastUL_ExpTime = 400;
        end

        LastUL_DT = celestial.time.jd2date(LastUL_JD,'H','YMD');
        LastUL_DateString = strcat(num2str(LastUL_DT(1)),'-',sprintf('%02.0f',LastUL_DT(2)), ...
            '-',sprintf('%02.0f',LastUL_DT(3)),{' '},sprintf('%02.0f',LastUL_DT(4)), ...
            ':',sprintf('%02.0f',LastUL_DT(5)),':',sprintf('%02.0f',LastUL_DT(6)),' UTC');
        LastUL_Msg = strcat('Last non-detection from observations was on',{' '}, ...
            LastUL_DateString{1},{' '},'(T0-T=',num2str(T0mT),{' '},'d) with limiting mag of', ...
            {' '},sprintf('%.2f',LastUL_Mag),'.');
        Msg{1} = strcat(Msg{1},'\n',LastUL_Msg{1});

        Ref_DT = celestial.time.jd2date(Ref_JD,'H','YMD');
        Ref_DateString = strcat(num2str(Ref_DT(1)),'-',sprintf('%02.0f',Ref_DT(2)), ...
            '-',sprintf('%02.0f',Ref_DT(3)),{' '},sprintf('%02.0f',Ref_DT(4)), ...
            ':',sprintf('%02.0f',Ref_DT(5)),':',sprintf('%02.0f',Ref_DT(6)),' UTC');
        RefUL_Msg = strcat('Reference was on',{' '}, ...
            Ref_DateString{1},{' '},'(T0-T=',num2str(T0mTRef),{' '},'d) with limiting mag of', ...
            {' '},sprintf('%.2f',Ref_LimMag),'.');
        Msg{1} = strcat(Msg{1},'\n',RefUL_Msg{1});

        LAST_report.ref_jd = Ref_JD;

        Ref_FilenameWhole = Transient.Ref.ImageData.FileName;
        Ref_FilenameParts = split(Ref_FilenameWhole,'/');
        Ref_Filename = Ref_FilenameParts{end};
        LAST_report.ref_filename = Ref_Filename;

        NonDetection = [];
        NonDetection.obsdate = LastUL_DateString;
        NonDetection.flux = round(LastUL_Mag,2);
        NonDetection.flux_units = 1;
        NonDetection.filter_value = 1;
        NonDetection.instrument_value = 269;
        NonDetection.exptime = LastUL_ExpTime;
        AT_Report.non_detection = NonDetection;

        NExpTime = Transient.New.HeaderData.getVal('EXPTIME');

        DetectionPhotometry = [];
        DetectionPhotometry.obsdate = DateString;
        DetectionPhotometry.flux = round(Mag0,2);
        DetectionPhotometry.flux_units = 1;
        DetectionPhotometry.filter_value = 1;
        DetectionPhotometry.instrument_value = 269;
        DetectionPhotometry.exptime = NExpTime;

        Photometry = [];
        Photometry.photometry_group = DetectionPhotometry;
        AT_Report.photometry = Photometry;

        TNS_Report.at_report = AT_Report;

        % If there is a galaxy match, construct potential host match message.
        GalN = Transient.CatData.getCol('GAL_N');

        LAST_report.gal_dist = NaN;

        if GalN > 0
            GalDist = Transient.CatData.getCol('GAL_DIST');

            [GLADEpCat,~,~] = catsHTM.cone_search('GLADEp', RA0*pi/180, Dec0*pi/180, ...
                GalDist*1.5, 'OutType','AstroCatalog');

            if GLADEpCat.sizeCatalog > 0

                Rad2Arcsec = 206265;
                Arcsec2Rad = 4.84814e-6;

                GLADEpCat.sortrows('Dec');
    
                [GladeLon, GladeLat] = GLADEpCat.getLonLat('rad');
        
                MatchResGlade = VO.search.search_sortedlat_multi( ...
                    [GladeLon, GladeLat], RA0*pi/180, Dec0*pi/180, ...
                    -GalDist*1.5*Arcsec2Rad);
        
                MatchesGlade = vertcat(MatchResGlade.Nmatch);
    
                DistsGlade = arrayfun(@(a)min(a.Dist),MatchResGlade(MatchesGlade > 0));

                GalDists = Rad2Arcsec * DistsGlade;
                GalDist = min(GalDists);

                Bmag = GLADEpCat.getCol('B');
                Redshift = GLADEpCat.getCol('z_cmb');

                Gal_Msg = strcat(':milky_way: There is a potential host', {' '}, ...
                    sprintf('%.2f',GalDist), {' '},'arcsec away.', {' '},...
                    'It has a redshift of', {' '}, sprintf('%.3f',Redshift(GalDists==GalDist)),{' '},...
                    'and a quiescient Bmag of', {' '}, sprintf('%.2f',Bmag(GalDists==GalDist)),...
                    '.');

                LAST_report.gal_dist = GalDist;
                Msg{1} = strcat(Msg{1},'\n',Gal_Msg{1});
            end
        end

        if RA0 < 0
            RA0 = 360 + RA0;
        end

        % Add a PS1 link.
        PlusSign = '';
        if Dec0 > 0
            PlusSign = '+';
        end

        PS1Link =  strcat('https://ps1images.stsci.edu/cgi-bin/ps1cutouts?pos=', ...
            num2str(RA0),PlusSign,num2str(Dec0),'&filter=color&size=720');
        PS1_Msg = strcat('<',PS1Link,'|','PS1>');
        Msg{1} = strcat(Msg{1},'-',PS1_Msg);

        % Add a Simbad link.
        SimbadLink =  strcat('http://simbad.u-strasbg.fr/simbad/',...
            'sim-coo?protocol=html&NbIdent=1&Radius=1&Radius.unit=arcmin',...
            '&CooFrame=FK5&CooEpoch=2000&CooEqui=2000&Coord=', ...
            num2str(RA0),PlusSign,num2str(Dec0));
        Simbad_Msg = strcat('<',SimbadLink,'|','Simbad>');
        Msg{1} = strcat(Msg{1},'-',Simbad_Msg);

        % Add a JPL horizons link.
        [~,HorizonsLink] = celestial.SolarSys.getJPL_smallBodyByCoo(...
            RA0,Dec0,[DT(3) DT(2) DT(1) DT(4) DT(5) floor(DT(6))],...
            'Execute',false);
        Horizons_Msg = strcat('<',HorizonsLink,'|','Horizons>');
        Msg{1} = strcat(Msg{1},'-',Horizons_Msg);

        % Add a TNS link.
        TNSLink = strcat('https://www.wis-tns.org/search?ra=', ...
            num2str(RA0),'&decl=',num2str(Dec0),'&radius=10&coords_unit=arcsec');
        TNS_Msg = strcat('<',TNSLink,'|','TNS>');
        Msg{1} = strcat(Msg{1},'-',TNS_Msg);

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

            % Prepare ref image cutout
            RefImage = Transient.Rbs;
            RefImageLowLim = prctile(RefImage(:),100-99.5);
            RefImageHighLim = prctile(RefImage(:),99.5);
            RefImagePlot = (RefImage-RefImageLowLim)./...
                           (RefImageHighLim-RefImageLowLim);
            RefImagePlot = min(max(RefImagePlot,0),1);
            RefImagePlot = asinh(10*RefImagePlot)/3;
            RefImagePlot = rot90(RefImagePlot,2);            

            % Prepare new image cutout
            NewImage = Transient.Nbs;
            NewImageLowLim = prctile(NewImage(:),100-99.5);
            NewImageHighLim = prctile(NewImage(:),99.5);
            NewImagePlot = (NewImage - NewImageLowLim)./...
                           (NewImageHighLim - NewImageLowLim);
            NewImagePlot = min(max(NewImagePlot,0),1);
            NewImagePlot = asinh(10*NewImagePlot)/3;
            NewImagePlot = rot90(NewImagePlot,2);

            % Prepare diff image cutout
            DiffImage = Transient.Image;
            DiffImageMinVal = min(DiffImage(:));
            DiffImageMaxVal = max(DiffImage(:));
            DiffImage = (DiffImage - DiffImageMinVal)/(DiffImageMaxVal - DiffImageMinVal);
            
            [DiffImageSizeX, DiffImageSizeY] = size(Transient.Image);

            DiffImageHalfSizeX = floor(DiffImageSizeX / 2);
            DiffImageHalfSizeY = floor(DiffImageSizeY / 2);
            
            DiffImageXStart = DiffImageHalfSizeX-5;
            DiffImageXEnd = DiffImageHalfSizeX+5;
            DiffImageYStart = DiffImageHalfSizeY-5;
            DiffImageYEnd = DiffImageHalfSizeY+5;

            DiffImageRoi = DiffImage(DiffImageXStart:DiffImageXEnd, ...
                DiffImageYStart:DiffImageYEnd);
            DiffImageRoiMin = min(DiffImageRoi(:));
            DiffImageRoiMax = max(DiffImageRoi(:));

            DiffImagePlot = imadjust(DiffImage, ...
                [DiffImageRoiMin DiffImageRoiMax], []);
            DiffImagePlot = rot90(DiffImagePlot,2);

            % Create individual cutouts
            FigRef = figure('Position',[1,1,51,51],'Visible','on');
            FigNew = figure('Position',[1,1,51,51],'Visible','on');
            FigDiff = figure('Position',[1,1,51,51],'Visible','on');

            axRef = axes(FigRef);
            Image_DirFilenameRef = replace(Image_DirFilename,'.png','_Ref.png');
            Image_FilenamePartsRef = split(Image_DirFilenameRef,'/');
            Image_FilenameRef = Image_FilenamePartsRef{end};
            imshow(RefImagePlot, 'Parent', axRef);

            % If Args.SaveProducts true, save images
            if Args.SaveProducts
                exportgraphics(axRef, Image_DirFilenameRef, 'Resolution', 300);
                LAST_report.ref_cutout = Image_FilenameRef;
            end

            axNew = axes(FigNew);
            Image_DirFilenameNew = replace(Image_DirFilename,'.png','_New.png');
            Image_FilenamePartsNew = split(Image_DirFilenameNew,'/');
            Image_FilenameNew = Image_FilenamePartsNew{end};
            imshow(NewImagePlot, 'Parent', axNew);

            % If Args.SaveProducts true, save images
            if Args.SaveProducts
                exportgraphics(axNew, Image_DirFilenameNew, 'Resolution', 300);
                LAST_report.new_cutout = Image_FilenameNew;
            end
    
            axDiff = axes(FigDiff); %#ok<*LAXES>
            Image_DirFilenameDiff = replace(Image_DirFilename,'.png','_Diff.png');
            Image_FilenamePartsDiff = split(Image_DirFilenameDiff,'/');
            Image_FilenameDiff = Image_FilenamePartsDiff{end};
            imshow(DiffImagePlot, 'Parent', axDiff);
            
            % If Args.SaveProducts true, save images
            if Args.SaveProducts
                exportgraphics(axDiff, Image_DirFilenameDiff, 'Resolution', 300);
                LAST_report.diff_cutout = Image_FilenameDiff;
            end

            % Draw mosaic image
            FigAll = figure;
            figure(FigAll);
            tiledlayout('flow', 'TileSpacing', 'none');%, 'Padding', 'none');
            % Reference image stamp
            nexttile;
            imshow(RefImagePlot);
            text(2,47,'Ref','Color','white','FontSize',14);
            % New image stamp
            nexttile;
            imshow(NewImagePlot);
            text(2,47,'New','Color','white','FontSize',14);
            % Difference image stamp
            nexttile;
            imshow(DiffImagePlot);
            text(2,47,'Diff','Color','white','FontSize',14);
            % Lightcurve
            nexttile([1 3]);
            errorbar(LC_JD, LC_Mag, LC_MagErr,'o');
            XlimMin = -5;
            LAST_report.detections_jd = {};
            LAST_report.detections_mag = {};
            LAST_report.detections_magerr = {};
            if numel(LC_JD) > 1
                LAST_report.detections_jd = LC_JD+JD0;
                LAST_report.detections_mag = LC_Mag;
                LAST_report.detections_magerr = LC_MagErr;
            else
                LAST_report.detections_jd{end+1} = LC_JD+JD0;
                LAST_report.detections_mag{end+1} = LC_Mag;
                LAST_report.detections_magerr{end+1} = LC_MagErr;
            end
            LAST_report.nondetections_jd = {};
            LAST_report.nondetections_mag = {};

            if LC_UL > 0
                hold on;
                scatter(LC_UL_JD, LC_UL_Mag, 'v');
                hold off;
                XlimMin = max(-30,min(LC_UL_JD-5));
                if LC_UL > 1
                    LAST_report.nondetections_jd = LC_UL_JD+JD0;
                    LAST_report.nondetections_mag = LC_UL_Mag;
                else
                    LAST_report.nondetections_jd{end+1} = LC_UL_JD+JD0;
                    LAST_report.nondetections_mag{end+1} = LC_UL_Mag;
                end
            end
            set(gca, 'YDir','reverse');
            xlim([XlimMin 5]);
            set(gca,'fontsize',14)

            % If Args.SaveProducts true, save image
            TNS_Report.last_report = LAST_report;
            if Args.SaveProducts
                %saveas(FigAll, Image_DirFilename);
                exportgraphics(FigAll, Image_DirFilename, 'Resolution', 300);
                Json_DirFilename = replace(Image_DirFilename,'.png','.json');
                Json_Filename = replace(Image_Filename,'.png','.json');
                Json = jsonencode(TNS_Report, 'ConvertInfAndNaN',false);
                fid = fopen(Json_DirFilename,'w');
                fprintf(fid, Json);
                fclose(fid);
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
            CMD0 = strcat('last-transient-slack-alert --message-file',{' '},Text_DirFilename,' --image-file',{' '},Image_DirFilename, ' --json-file',{' '},Json_DirFilename);
            [CMD0Status, CMD0Out] = system(CMD0{1});
            if CMD0Status > 0
                Status = sprint('Alerting via last-tools failed: %s', CMD0Out);
                return
            end

            if Args.TransferTranProducts
                % TODO: replace this with a last-tool script call later
    
                CutoutsRemote = 'last@marvin:/BIGDATA/last/data/temp/transients/cutouts';
                JsonRemote = 'last@marvin:/BIGDATA/last/data/temp/transients/json';
    
                MoveRefCutoutCMD = strcat('rsync -a ',{' '},Image_DirFilenameRef,{' '},CutoutsRemote);
                MoveNewCutoutCMD = strcat('rsync -a ',{' '},Image_DirFilenameNew,{' '},CutoutsRemote);
                MoveDiffCutoutCMD = strcat('rsync -a ',{' '},Image_DirFilenameDiff,{' '},CutoutsRemote);
                MoveJsonCMD = strcat('rsync -a ',{' '},Json_DirFilename,{' '},JsonRemote);

                [~, ~] = system(MoveRefCutoutCMD{1});
                [~, ~] = system(MoveNewCutoutCMD{1});
                [~, ~] = system(MoveDiffCutoutCMD{1});
                % json should be moved last
                pause(1);
                [~, ~] = system(MoveJsonCMD{1});
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

            JsonUploaded = false;
            if isfile(Json_DirFilename)

                try
                    % Get file size.
                    FileForSizeJson = dir(Json_DirFilename);
                    FilesizeJson = num2str(FileForSizeJson.bytes);
        
                    % Request image host URL.
                    CMD1Json = strcat("curl -F files=@",Json_DirFilename," -F filename=",Json_Filename," -F token=",SlackBotToken," -F length=",FilesizeJson," https://slack.com/api/files.getUploadURLExternal");
                
                    [~,CMD1outJson] = system(CMD1Json);
                    Response1Json = jsondecode(strcat("{",extractAfter(CMD1outJson,"{")));

                    UploadUrlJson = Response1Json.upload_url;
                    FileIDJson = Response1Json.file_id;
                
                    % Upload image to host URL.
                
                    CMD2Json = strcat("curl -F  filename=@",Json_DirFilename," -H 'Authorization: Bearer ",SlackBotToken,"' -v POST ",UploadUrlJson);
                
                    [~,~] = system(CMD2Json);

                    JsonUploaded = true;
                catch
                    warning('Json upload failed.');
                end
            end

            % Authorize post to slack channel.

            if JsonUploaded
                CMD3 = strcat("curl -X POST -H 'Authorization: Bearer ",SlackBotToken,"' -H 'Content-Type: application/json' -d '",'{"files": [{"id":"',FileID,'", "title":"NewTransient"},{"id":"',FileIDJson,'", "title":"NewTransientJson"}], "channel_id": "',ChannelID,'", "initial_comment": "',Msg{1},'" }',"' https://slack.com/api/files.completeUploadExternal");
            else
                CMD3 = strcat("curl -X POST -H 'Authorization: Bearer ",SlackBotToken,"' -H 'Content-Type: application/json' -d '",'{"files": [{"id":"',FileID,'", "title":"NewTransient"}], "channel_id": "',ChannelID,'", "initial_comment": "',Msg{1},'" }',"' https://slack.com/api/files.completeUploadExternal");
            end
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

    if NadcNotReported == Nadc
        Status = 'No transient reported, none significant enough.';
    end

end