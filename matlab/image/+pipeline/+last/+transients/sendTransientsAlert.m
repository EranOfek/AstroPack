function [Status] = sendTransientsAlert(ADc, Args)
    %{
    Save the products of each reported LAST transient candidate and send
    them to the remote transients archive.
    For each candidate that was reported by matchTransientsToMultiEpochs,
    the Ref/New/Diff stamps are saved as PNG, the TNS-style report as JSON,
    and all four are rsynced to the remote archive (the JSON last).
    Input   : - AstroDiff cutouts on transients.
              * ...,key,val,...
                'SavePath' - Path to directory in which to save products.
                       Required, must not be empty.
                'TransferTranProducts' - Bool on whether to rsync the products
                       to the remote archive. If false, the products are
                       only saved. Default is true.
                'CutoutsRemote' - rsync destination of the stamps.
                       Default is 'euclid@euclid:/home/euclid/lastdata/transients/cutouts'.
                'JsonRemote' - rsync destination of the JSON reports.
                       Default is 'euclid@euclid:/home/euclid/lastdata/transients/json'.
                'MaskBrightRelCount' - Default is 10.
                'MaskBrightAbsCount' - Default is 1000.
    Output  : - Result message.
    Author  : Ruslan Konno (Aug 2024)
    Example : VisitPath = '/path/to/visit/dir'
              [AD, ADc, TCL1, Status] = pipeline.last.transients.runTransientsPipe(VisitPath)
              [ADc, TCL2, Status] = pipeline.last.transients.matchTransientsToMultiEpochs(ADc, TCL1)
              [Status] = pipeline.last.transients.sendTransientsAlert(ADc, 'SavePath',VisitPath)
    %}

    arguments
        ADc

        Args.SavePath = '';
        Args.TransferTranProducts logical = true;
        Args.CutoutsRemote = 'euclid@euclid:/home/euclid/lastdata/transients/cutouts';
        Args.JsonRemote    = 'euclid@euclid:/home/euclid/lastdata/transients/json';

        Args.MaskBrightRelCount = 10;
        Args.MaskBrightAbsCount = 1000;

    end

    Status = 'Uncontrolled exit.';

    if isempty(Args.SavePath)
        Status = 'SavePath is empty, nothing saved or sent.';
        return
    end

    % Get number of transient cutouts.
    Nadc = numel(ADc);
    NadcNotReported = 0;
    Nsent = 0;
    FailMsg = {};

    % Return if no transients candidates empty.
    if (Nadc == 0) || isempty(ADc(1).Table)
        Status = 'No transients found, nothing to report.';
        return
    end

    % Run loop on each transient cutout
    for Iadc = 1:Nadc
        Transient = ADc(Iadc);

        % Report only candidates flagged by matchTransientsToMultiEpochs

        if ~Transient.CatData.isColumn('Reported')
            NadcNotReported = NadcNotReported + 1;
            continue
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
            ':',sprintf('%02.0f',DT(5)),':',sprintf('%02.0f',fix(DT(6))),' UTC');

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
        AT_Report.reporter = "R. Konno (WIS), E. Zimmerman (WIS), A. Horowicz (WIS), S. Garrappa (WIS), E. O. Ofek (WIS), S. Ben-Ami (WIS), D. Polishook (WIS), P. Chen (WIS), A. Krassilchtchikov (WIS), Y. M. Shani (WIS), E. Segre (WIS), A. Gal-Yam (WIS), and S. Spitzer (WIS) on behalf of the LAST Collaboration";
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

        % Last non-detection.
        % If available, use a recent observations,
        % otherwise use reference image.
        Ref_JD = Transient.Ref.HeaderData.getVal('JD');
        Ref_LimMag = Transient.Ref.HeaderData.getVal('LIMMAG');
        LastUL_JD = Ref_JD;
        LastUL_Mag = Ref_LimMag;
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
            ':',sprintf('%02.0f',LastUL_DT(5)),':',sprintf('%02.0f',fix(LastUL_DT(6))),' UTC');

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

        % If there is a galaxy match, get the distance to the potential host.
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

                LAST_report.gal_dist = GalDist;
            end
        end

        % Construct image name
        % named after the coadd (issue #1315: AstroFileName, not FileNames)
        ImageFN = AstroFileName.parseString2AstroFileName(Transient.New.ImageData.FileName);
        ImageFN.Level = "coadd.zogyD";
        ImageFN.Product = "Image";
        ImageFN.FileType = "png";
        ImageFN.Version = Iadc;
        Image_Filename = char(ImageFN.genFile);
        % SavePath may be a cell/string array with one path per file
        % (FileNames/AstroFileName genPath): use the first, as before
        Image_DirFilename = strcat(Args.SavePath,'/',Image_Filename);
        if ~ischar(Image_DirFilename)
            Image_DirFilename = Image_DirFilename(1);
        end
        Image_DirFilename = char(Image_DirFilename);

        MaskKernel = ones(3,3);

        % Prepare new image cutout
        NewImage = Transient.Nbs;

        % Get peak count within transient RoI

        [NewImageSizeX, NewImageSizeY] = size(NewImage);

        NewImageHalfSizeX = floor(NewImageSizeX / 2);
        NewImageHalfSizeY = floor(NewImageSizeY / 2);

        NewImageXStart =NewImageHalfSizeX-5;
        NewImageXEnd = NewImageHalfSizeX+5;
        NewImageYStart = NewImageHalfSizeY-5;
        NewImageYEnd = NewImageHalfSizeY+5;

        NewImageRoi = NewImage(NewImageXStart:NewImageXEnd, ...
            NewImageYStart:NewImageYEnd);
        NewImageRoiPeak = max(NewImageRoi,[],'all');

        % Mask bright NewImage pixels outside of the RoI
        NewImageRel2Roi = NewImage/NewImageRoiPeak;
        NewImageBrightMask = (NewImage > Args.MaskBrightAbsCount) & ...
            (NewImageRel2Roi > Args.MaskBrightRelCount);
        NewImageBrightMask = (conv2(NewImageBrightMask, MaskKernel, "same") > 0);
        NewImageBrightMask(NewImageXStart:NewImageXEnd, ...
            NewImageYStart:NewImageYEnd) = 0;
        NewImageMasked = NewImage;
        NewImageMasked(NewImageBrightMask) = 1;

        % Renormalize New Image for plot
        NewImageLowLim = prctile(NewImageMasked(:),100-99.5);
        NewImageHighLim = prctile(NewImageMasked(:),99.5);
        NewImagePlot = (NewImageMasked - NewImageLowLim)./...
                       (NewImageHighLim - NewImageLowLim);
        NewImagePlot = min(max(NewImagePlot,0),1);
        NewImagePlot = asinh(10*NewImagePlot)/3;
        NewImagePlot = rot90(NewImagePlot,2);


        % Prepare ref image cutout
        RefImage = Transient.Rbs;

        [RefImageSizeX, RefImageSizeY] = size(RefImage);

        RefImageHalfSizeX = floor(RefImageSizeX / 2);
        RefImageHalfSizeY = floor(RefImageSizeY / 2);

        RefImageXStart = RefImageHalfSizeX-5;
        RefImageXEnd = RefImageHalfSizeX+5;
        RefImageYStart = RefImageHalfSizeY-5;
        RefImageYEnd = RefImageHalfSizeY+5;

        % Mask bright NewImage pixels outside of the RoI
        RefImageRel2Roi = RefImage/NewImageRoiPeak;
        RefImageBrightMask = (RefImage > Args.MaskBrightAbsCount) & ...
            (RefImageRel2Roi > Args.MaskBrightRelCount);
        RefImageBrightMask = (conv2(RefImageBrightMask, MaskKernel, "same") > 0);
        RefImageBrightMask(RefImageXStart:RefImageXEnd, ...
            RefImageYStart:RefImageYEnd) = 0;
        RefImageMasked = RefImage;
        RefImageMasked(RefImageBrightMask) = 1;

        RefImageLowLim = prctile(RefImageMasked(:),100-99.5);
        RefImageHighLim = prctile(RefImageMasked(:),99.5);
        RefImagePlot = (RefImageMasked-RefImageLowLim)./...
                       (RefImageHighLim-RefImageLowLim);
        RefImagePlot = min(max(RefImagePlot,0),1);
        RefImagePlot = asinh(10*RefImagePlot)/3;
        RefImagePlot = rot90(RefImagePlot,2);

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

        % Save the individual cutouts. The figures are closed afterwards:
        % the demon runs for days and would otherwise accumulate them.
        Image_DirFilenameRef = replace(Image_DirFilename,'.png','_Ref.png');
        Image_DirFilenameNew = replace(Image_DirFilename,'.png','_New.png');
        Image_DirFilenameDiff = replace(Image_DirFilename,'.png','_Diff.png');

        CutoutFiles = {Image_DirFilenameRef, Image_DirFilenameNew, Image_DirFilenameDiff};
        CutoutPlots = {RefImagePlot, NewImagePlot, DiffImagePlot};
        for Icut = 1:numel(CutoutFiles)
            FigCut = figure('Position',[1,1,51,51],'Visible','off');
            axCut = axes(FigCut); %#ok<*LAXES>
            imshow(CutoutPlots{Icut}, 'Parent', axCut);
            exportgraphics(axCut, CutoutFiles{Icut}, 'Resolution', 300);
            close(FigCut);
        end

        [~, Name, Ext] = fileparts(Image_DirFilenameRef);
        LAST_report.ref_cutout = [Name, Ext];
        [~, Name, Ext] = fileparts(Image_DirFilenameNew);
        LAST_report.new_cutout = [Name, Ext];
        [~, Name, Ext] = fileparts(Image_DirFilenameDiff);
        LAST_report.diff_cutout = [Name, Ext];

        % Light curve: detections and upper limits
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
            if LC_UL > 1
                LAST_report.nondetections_jd = LC_UL_JD+JD0;
                LAST_report.nondetections_mag = LC_UL_Mag;
            else
                LAST_report.nondetections_jd{end+1} = LC_UL_JD+JD0;
                LAST_report.nondetections_mag{end+1} = LC_UL_Mag;
            end
        end

        % Save the JSON report
        TNS_Report.last_report = LAST_report;
        Json_DirFilename = replace(Image_DirFilename,'.png','.json');
        Json = jsonencode(TNS_Report, 'ConvertInfAndNaN',false);
        fid = fopen(Json_DirFilename,'w');
        if fid < 0
            FailMsg{end+1} = sprintf('cannot write %s', Json_DirFilename); %#ok<AGROW>
            continue
        end
        fprintf(fid, '%s', Json);   % not as a format: '%' or '\' in the report would be mangled
        fclose(fid);

        % Send the products to the remote archive
        if Args.TransferTranProducts
            % TODO: replace this with a last-tool script call later
            Ok = true;
            for Icut = 1:numel(CutoutFiles)
                [RsyncStatus, RsyncOut] = system(sprintf('rsync -a %s %s', CutoutFiles{Icut}, Args.CutoutsRemote));
                if RsyncStatus > 0
                    Ok = false;
                    FailMsg{end+1} = sprintf('rsync of %s failed: %s', CutoutFiles{Icut}, strtrim(RsyncOut)); %#ok<AGROW>
                end
            end
            % json should be moved last, and only if the stamps arrived
            if Ok
                pause(1);
                [RsyncStatus, RsyncOut] = system(sprintf('rsync -a %s %s', Json_DirFilename, Args.JsonRemote));
                if RsyncStatus > 0
                    FailMsg{end+1} = sprintf('rsync of %s failed: %s', Json_DirFilename, strtrim(RsyncOut)); %#ok<AGROW>
                else
                    Nsent = Nsent + 1;
                end
            end
        end

    end

    if NadcNotReported == Nadc
        Status = 'No transient reported, none significant enough.';
    elseif ~Args.TransferTranProducts
        Status = sprintf('Products of %d transient(s) saved, not transferred.', Nadc - NadcNotReported);
    else
        Status = sprintf('%d of %d reported transient(s) sent.', Nsent, Nadc - NadcNotReported);
    end
    if ~isempty(FailMsg)
        Status = sprintf('%s Failures: %s', Status, strjoin(FailMsg, '; '));
    end

end
