% A static class to control and manipulate ds9
% Description: A static class for intearction with the ds9 display.
%              This include functions to load images, change their
%              properties, create and plot region files, printing, image
%              examination, interaction with SIM content and more.
%              Type "ds9." followed by <tab> to see the full list of
%              functions.
%              Full manual is available in manual_ds9.pdf
% Input  : null
% Output : null
% Tested : Matlab R2014a
%     By : Eran O. Ofek                    Jul 2016
%    URL : http://weizmann.ac.il/home/eofek/matlab/
% Reliable: 2
%--------------------------------------------------------------------------


classdef DS9analysis < handle

    properties
        Images               % AstroImage | FileNames | cell
        Names cell   = {};

        % Frame2Ind    = [1];   % [ImageIndInFrame1, ImageIndInFram2,...]
        % 
        % MapInd       = [];

    end

    
    %properties (Hidden)
    %end
    
    methods % Constructor method
        function Obj = DS9analysis(Image, DispInd)
            %

            arguments
                Image   = [];
                DispInd = 1;
            end

            if ~isempty(Image)
                Obj.Images = Image;


            end

        end

    end

    methods % setters/getters
        % function set.Frame2Ind(Obj, Input)
        %     % Setter for Frame2Ind (which image index to displa in each frame).
        %     % Input  : - self.
        %     %          - A vector of image indices per each frame.
        %     %            if image index is NaN then skip frame.
        % 
        % 
        %     Nframe = ds9.frame;
        %     Ninput = numel(Input);
        %     Nim    = numel(Obj.Images);
        %     if max(Input)<Nim
        %         error('Max. image index (%d) must be smaller or equal to the numbre of Images (%d)', max(Input), Nim);
        %     end
        % 
        %     for Iinput=1:1:Ninput
        %         Iim = Input(Iinput);
        %         if isnan(Iim)
        %             % do not display an image in frame
        %         else
        %             ds9.disp(Obj.Images(Iim), Iinput);
        %         end
        %     end
        %     % delete extra frames
        %     for I=Nframe:-1:Ninput+1
        %         ds9.delete_frame;
        %     end
        % end
            
        
    end


    methods  % display and load
        function ImageInd=frame2ImageIndex(Obj, Frame)
            % Retune ImageIndex from frame index
            % Input  : - Self.
            %          - Frame index. If empty, get currentframe from ds9
            %            window. Default is [].
            % Output : - Image index
            % Author : Eran Ofek (Jun 2023)

            arguments
                Obj
                Frame = [];
            end

            if isempty(Frame)
                % get current frame
                Frame = str2double(ds9.frame);
            end

            ImageInd = Obj.Frame2Ind(Frame);
        end

        function FrameInd=ImageIndex2Frame(Obj, ImageIndex)
            % Return frame index from image index
            % Input  : - Self.
            % %        - Image index.
            % Output : - Frame index.
            % Author : Eran Ofek (Jun 2023)

            FrameInd = find(Obj.Frame2Ind==ImageIndex);
            if numel(FrameInd)~=1
                error('Found %d corresponding frames to image index %d',numel(FrameInd),ImageIndex);
            end

        end

        function Obj=load(Obj, Image, Frames, Args)
            % Load images to a DS9analysis object
            % Input  : - Self.
            %          - An AstroImage object, or a cell array of images,
            %            or a file name.
            %            The images will be loaded to frame 1..N according
            %            to their order.
            %          - A vector of frames into to load the
            %            images. If empty, use 1..N.
            %            Default is [].
            %          * ...,key,val,...
            %            'LikeLAST' - A logical indicating if to read LAST like names.
            %                   If true then will use AstroImage.readFileNamesObj
            %                   to read images and thir corresponding Mask, PSF,
            %                   Cat meta data.
            %                   Default is true.
            %            'Names' - A cell array of optional names to give
            %                   the images. If given then the images can be
            %                   accessed by names. Default is {}.
            %            'Disp' - A logical indicating if to display the
            %                   images. Default is true.
            %            'Zoom' - zoom parameter for display.
            %                   If empty, do nothing. Default is [].
            % Output : - An updated object.
            % Author : Eran Ofek (Jun 2023)
            % Example: D9.load([AIreg(2), AIreg(1), Scorr],'Names',{'N','R','Scorr'})

            arguments
                Obj
                Image

                Frames                   = [];
                Args.LikeLAST logical    = true;
                Args.Names               = {};
                Args.Disp logical        = true;
                Args.Zoom                = [];
            end

            if isa(Image, 'AstroImage')
                AI = Image;
            else
                if Args.LikeLAST
                    AI = AstroImage.readFileNamesObj(Image);
                else
                    if ischar(Image) || iscell(Image)
                        AI = AstroImage(Image);
                    end
                end
            end

            Nim = numel(AI);
            if isempty(Frames)
                Frames = (1:1:Nim);
            else
                Frames = Args.Frames;
            end
           
            for Iim=1:1:Nim
                FrameInd = Frames(Iim);
                if isempty(Obj.Images)
                    Obj.Images = AstroImage;
                end
                Obj.Images(FrameInd) = AI(Iim);
                if ~isempty(Args.Names)
                    Obj.Names{Frames(Iim)} = Args.Names{Iim};
                end
            end

            if Args.Disp
                for Iim=1:1:Nim
                    ds9.disp(Obj.Images(Iim), Frames(Iim));
                    if ~isempty(Args.Zoom)
                        ds9.zoom(Args.Zoom);
                    end
                end
            end


        end

        function [Frame, FrameName]=frame(Obj, Frame)
            % set/get frame by index or name
            % Input  : - Self.
            %          - Frame index or frame name (one of the char arrays
            %            in the Names property.
            %            If empty, then will only return the current frame
            %            index and name.
            %            Default is [].
            % Output : - Current frame index.
            %          - Current frame name as indicated by the corresponding
            %            element in the Names property.
            %            If not available then return NaN.
            % Author : Eran Ofek (Jun 2023)
            
            arguments
                Obj
                Frame  = [];
            end
           
            
            if isempty(Frame)
                % get frame
                Frame = str2double(ds9.frame);
            else
                % set frame
                if ischar(Frame)
                    Frame = find(strcmp(Obj.Names, Frame));
                end
                ds9.frame(Frame);
            end
            if numel(Obj.Names)>=Frame
                FrameName = Obj.Names{Frame};
            else
                FrameName = NaN;
            end
            
        end
        
    end

    methods  % switch images in frame
        function AI = getImage(Obj, Ind)
            % Get image by index from Images property
            % Input  : - self.
            %          - Index of image in the AstroImage|FileNames|cell
            % Output : - A single element AstroImage object.
            
            if isempty(Obj.Images)
                error('Images is not populated');
            end

            switch class(Obj.Images)
                case 'AstroImage'
                    AI = Obj.Images(Ind);
                case 'FileNames'
                    FN = reorderEntries(Obj.Images, Ind, 'CreateNewObj',true);
                    AI = AstroImages.readFromFileNamesObj(FN);
                case 'cell'
                    AI = AstroImages(Obj.Images{Ind});
                otherwise
                    error('Unknown Images class');
            end
            
        end
        
        function Obj=sortByJD(Obj)
            % Sor the Images property by images JD
            % Input  : - self.
            % Output : - self in which the Images are sorted by JD.
            % Author : Eran Ofek (May 2023)

            switch class(Obj.Images)
                case 'AstroImage'
                    JD = Obj.Images.julday;
                    [~,SI] = sort(JD(:));
                    Obj.Images = Obj.Images(SI);
                case 'FileNames'
                    Obj.Images.sortByJD;
                case 'cell'
                    error('Can not sort by time an Images cell property of class cell');
            end
        end
        % 
        % goto: next | prev | first | last | ind
        
    end

    methods % asteroids/moving sources
        function [AstData,AstTable,BadCand, ReportMPC]=blinkAstCrop(Obj, AstData, Args)
            % Display AstCrop
            % Example: D9=DS9analysis;
            %          [AstData,AstTable,~,ReportMPC] = D9.blinkAstCrop;
            %          [AstData,AstTable,~,ReportMPC] = D9.blinkAstCrop(AstData,'Id',2);
            %          [AstData,AstTable,BadCand,ReportMPC] = D9.blinkAstCrop('all','Id',[]);

            arguments
                Obj
                AstData          = [];  % use 'all' for rdir
                Args.Id          = 1;  % if [] loop over all
                Args.StampsStep  = [];
                Args.AstFileTemp = '*merged_Asteroids*.mat';
                Args.Zoom        = 8;
                Args.DispInfo logical = true;
                Args.ReportType       = 'FittedDetection3'; %'AllDetections'; %'FittedDetection';
                Args.generateReportMPCArgs cell  = {};
                Args.PlotKnown logical           = true;
                Args.INPOP                       = [];
                Args.OrbEl                       = [];
                Args.SearchGAIA logical          = true;
                Args.SearchRadGAIA               = 3;
                Args.MagGAIA                     = 20.5;
            end
            RAD = 180./pi;

            if Args.PlotKnown
                Args.INPOP = celestial.INPOP;
                Args.INPOP.populateAll;
                Args.OrbEl = celestial.OrbitalEl.loadSolarSystem('merge');
            end


            if isempty(AstData)
                % attempt to load Asteroids MAT file
                Files = dir(Args.AstFileTemp);
                if numel(Files)==0
                    error('No asteroid file found');
                end
                Nf = 1;

            else
                if (ischar(AstData) || isstring(AstData)) && strcmp(AstData, 'all')
                    Files = io.files.rdir(Args.AstFileTemp);
                    Nf    = numel(Files);
                else
                    % assume input is struct
                    Nf = 1;
                    Files = [];

                end

            end

            BD = BitDictionary;

            ReportMPC = '';
            Ireport = 0;
            Id = Args.Id;
            Ibad = 0;
            for If=1:1:Nf
                if isempty(Files)
                    % already loaded
                else
                    PWD = pwd;
                    cd(Files(If).folder);
                    AstData = io.files.load2(Files(If).name);
                    
                    cd(PWD);
                    Id = [];
                end
                if isempty(Id)
                    Id = (1:1:numel(AstData.AstCrop));
                end
                Nast = numel(AstData.AstCrop);
                if max(Id)>Nast
                    error('Requested Id=%d is > Number of asteroids in file is %d',max(Id), Nast);
                end
    
                for Iid=1:1:numel(Id)
                    I = Id(Iid);

                    % check for GAIA star
                    if Args.SearchGAIA
                        
                        [GaiaCat,Col] = catsHTM.cone_search('GAIADR3',AstData.AstCrop(I).SelectedCatPM.Table.RA./RAD, AstData.AstCrop(I).SelectedCatPM.Table.Dec./RAD, Args.SearchRadGAIA, 'OutType','astrocatalog');
                        if min(GaiaCat.Table.phot_bp_mean_mag)<Args.MagGAIA     
                            % skip - GAIA star found at position
                            Skip = true;
                        else
                            Skip = false;
                        end
                    end

                    % Skip if Overlap or NearEdge
                    if BD.findBit(AstData.AstCrop(I).SelectedCatPM.Table.FLAGS,{'Overlap','NearEdge'}, 'Method','any');
                        Skip = true;
                    end


                    if ~Skip
                        Nstamp = numel(AstData.AstCrop(I).Stamps);
                        if isempty(Args.StampsStep)
                            % show only first and last images
                            StampInd = [1 Nstamp];
                        else
                            StampInd = (1:Args.StampStep:Nstamp);
                        end

                        try
                            Obj.load(AstData.AstCrop(I).Stamps(StampInd), 'Zoom',Args.Zoom);
                            ds9.match_wcs;
                        catch ME
                            'a'
                        end
                        % Display information
                        NsrcCat = sizeCatalog(AstData.AstCrop(I).SelectedCatPM);
                        switch NsrcCat
                            case 0
                                error('Possible bug: %d sources found in catalog', NsrcCat);
                            case 1
                                AstSrcId = 1;
                            otherwise
                                [Dist, PA] = sphere_dist(AstData.AstCrop(I).SelectedCatPM, AstData.AstCrop(I).RA, AstData.AstCrop(Args.Id).Dec, 'rad', 'deg');
                                [MinDist, AstSrcId] = min(Dist);
                                if MinDist>(5./3600)
                                    error('Possible problem" distance of nearest source to cutout center is too large %f arcsec', MinDist.*3600);
                                end
                        end
                        
                        AstTable = AstData.AstCrop(I).SelectedCatPM.toTable;
                        AstTable = AstTable(AstSrcId,:);
            
                        if Args.DispInfo
                            fprintf('Folder %s \n',Files(If).folder);
                            fprintf('File %s loaded\n',Files(If).name);
                            fprintf('SubImage      : %d\n',AstData.AstCrop(I).FieldIndex);
                            fprintf('RA            : %s\n', celestial.coo.convertdms(AstTable.RA, 'd', 'SH'));
                            fprintf('Dec           : %s\n',celestial.coo.convertdms(AstTable.Dec, 'd', 'SD'));
                            fprintf('Nobs          : %d\n',AstTable.Nobs);
                            fprintf('Noutlier      : %d\n',AstTable.Noutlier);
                            fprintf('PM_RA         : %f [time-deg/day]\n',AstTable.PM_RA);
                            fprintf('PM_Dec        : %f [deg/day]\n',AstTable.PM_Dec);
                            fprintf('JD_PM         : %15.6f\n',AstTable.JD_PM);
                            fprintf('MAG mean      : %f\n', AstTable.Mean_MAG_PSF);
                            fprintf('MAG range     : %f\n', AstTable.Range_MAG_PSF);
                            fprintf('S/N mean      : %f\n', AstTable.Mean_SN_3);
                            
                            FlagsName = BD.bitdec2name(AstTable.FLAGS);
                            FlagsName = FlagsName{1};
                            fprintf('FLAGS         :');
                            for Ifn=1:1:numel(FlagsName)
                                fprintf('  %s', FlagsName{Ifn});
                            end
                            fprintf('\n');
                            BDmc = BitDictionary('BitMask.MergedCat.Default');
                            FlagsName = BDmc.bitdec2name(AstTable.MergedCatMask);
                            FlagsName = FlagsName{1};
                            fprintf('MergedCat     :');
                            for Ifn=1:1:numel(FlagsName)
                                fprintf('  %s', FlagsName{Ifn});
                            end
                            fprintf('\n');
            
                            fprintf('PolyDeltaChi2 : %f\n', AstTable.PolyDeltaChi2);
                        end
            
                        % plot known asteroids
                        if Args.PlotKnown
                            KA = Obj.plotKnownAst('OrbEl',Args.OrbEl, 'INPOP',Args.INPOP);
                        end
                        ds9.plotc(AstData.AstCrop(I).SelectedCatPM,'sg')
            
                        %if If<Nf || I<numel(Id)
                        
                        %end
                        Ans = input('a - add to report; otherwise continue : ','s');
                        switch lower(Ans)
                            case 'a'
                                % add to report
                                Ireport = Ireport + 1;
                                if Ireport==1
                                    AddHeader = true;
                                else
                                    AddHeader = false;
                                end
    
                                % prep MPC report for asteroid
                                Args.generateReportMPCArgs{1} = 'ObsName';
                                Args.generateReportMPCArgs{2} = sprintf('Large Array Survey Telescope (LAST) Node %02d Mount %02d Tel %02d',...
                                                                        AstData.AstCrop(I).Stamps(1).HeaderData.Key.NODENUMB,...
                                                                        AstData.AstCrop(I).Stamps(1).HeaderData.Key.MOUNTNUM,...
                                                                        AstData.AstCrop(I).Stamps(1).HeaderData.Key.CAMNUM);
                                %
        
        
                                switch Args.ReportType
                                    case 'AllDetections'
                                        ReportMPC = imProc.asteroids.generateReportMPC(AstData.AstCrop(I).Stamps,...
                                                                            'RA', AstData.AstCrop(I).RA, 'Dec', AstData.AstCrop(I).Dec,...
                                                                            'generateReportMPCArgs',Args.generateReportMPCArgs, 'AstIndex',Ireport);
                                    case 'FittedDetection'
                                        ReportMPC = imProc.asteroids.generateReportMPC(AstData.AstCrop(I).SelectedCatPM,...
                                                                            'RA', AstData.AstCrop(I).RA, 'Dec', AstData.AstCrop(I).Dec,...
                                                                            'ColMag','Mean_MAG_PSF',...
                                                                            'generateReportMPCArgs',Args.generateReportMPCArgs, 'AstIndex',Ireport);
                                    case 'FittedDetection3'
                                        % Evaluate fitted motion at two points
                                        Nim = numel(AstData.AstCrop(I).Stamps);
                                        JD1 = AstData.AstCrop(I).JD(1);
                                        JDe = AstData.AstCrop(I).JD(Nim);
                                        JDm = AstData.AstCrop(I).SelectedCatPM.JD;
                                        
                                        % Note PM_RA is in time units rather than angular units
                                        % so no cos(Dec) correction is needed
                                        RA1  = AstData.AstCrop(I).SelectedCatPM.Table.RA + AstData.AstCrop(I).SelectedCatPM.Table.PM_RA.*(JD1-JDm); %./cosd(AstData.AstCrop(I).SelectedCatPM.Table.Dec);
                                        RA2  = AstData.AstCrop(I).SelectedCatPM.Table.RA + AstData.AstCrop(I).SelectedCatPM.Table.PM_RA.*(JDm-JDm); %./cosd(AstData.AstCrop(I).SelectedCatPM.Table.Dec);
                                        RA3  = AstData.AstCrop(I).SelectedCatPM.Table.RA + AstData.AstCrop(I).SelectedCatPM.Table.PM_RA.*(JDe-JDm); %./cosd(AstData.AstCrop(I).SelectedCatPM.Table.Dec);
                                        Dec1 = AstData.AstCrop(I).SelectedCatPM.Table.Dec + AstData.AstCrop(I).SelectedCatPM.Table.PM_Dec.*(JD1-JDm);
                                        Dec2 = AstData.AstCrop(I).SelectedCatPM.Table.Dec + AstData.AstCrop(I).SelectedCatPM.Table.PM_Dec.*(JDm-JDm);
                                        Dec3 = AstData.AstCrop(I).SelectedCatPM.Table.Dec + AstData.AstCrop(I).SelectedCatPM.Table.PM_Dec.*(JDe-JDm);
                    
                                        Mag = AstData.AstCrop(I).SelectedCatPM.Table.Mean_MAG_PSF;
                                        Filter = AstData.AstCrop(I).Stamps(1).HeaderData.Key.FILTER;
                    
                                        % [JD, RA, Dec, Mag, Filter, AstIndex]
                                        Table = [[JD1;JDm;JDe], [RA1;RA2;RA3], [Dec1;Dec2;Dec3], [Mag;Mag;Mag], [NaN; NaN; NaN], [1;1;1].*Ireport]; %, 'VariableNames',{'JD','RA','Dec','Mag','Filter','AstIndex'});
                                        ReportMPC = [ReportMPC, imUtil.asteroids.generateReportMPC(Table, 'Filter','C', 'AddHeader',AddHeader, Args.generateReportMPCArgs{:})];
                                        
                                        ReportMPC
    
                                        % ReportMPC = imProc.asteroids.generateReportMPC(AstData.AstCrop(Args.Id).Stamps([1 Nim]),...
                                        %                                     'RA', [RA1; RA2], 'Dec', [Dec1; Dec2],...
                                        %                                     'ColMag','Mean_MAG_PSF',...
                                        %                                     'generateReportMPCArgs',Args.generateReportMPCArgs);
                                    otherwise
                                        error('Unknown ReportType option');
                                end
                            case 'q'
                                % quit
                                break;
                            otherwise
                                % skip
                                % but keep info about target
                                Ibad = Ibad + 1;
                                BadCand(Ibad).Folder = Files(If).folder;
                                BadCand(Ibad).File   = Files(If).name;
                                BadCand(Ibad).Id     = I;
                                BadCand(Ibad).SubImage = AstData.AstCrop(I).FieldIndex;
                                BadCand(Ibad).SelectedCatPM = AstData.AstCrop(I).SelectedCatPM;
                            
                        end
                    end % if ~Skip
                end %for Iid=1:1:numel(Id)
            end % for If=1:1:Nf

        end

        function [Result, OrbEl] = plotKnownAst(Obj, Args)
            % Search for known minor bodies in image coordinates and plot their positions.
            % Input  : - A DS9analayis object.
            %          * ...,key,val,...
            %            'MagLimit' - Search asteroid mag. limit.
            %                   Default is Inf.
            %            'PlotDesig' - Add designation label near each
            %                   marked asteroid.
            %                   Default is true.
            %            'AddNumber' - Add Asteroid number to label.
            %                   Default is true.
            %            'AddMag' - Add asteroid mag to label.
            %                   Default is true.
            %            'OrbEl' - A celestial.OrbitalEl object containing
            %                   the asteroids orbital elements.
            %                   If empty, then will load the 'merge' file.
            %                   Default is [].
            %            'INPOP' - A populated celestial.INPOP object.
            %                   If empty, then will create one.
            %                   Default is [].
            % Output : - An AstroCatalog containing the asteroids found
            %            within the search radius + buffer.
            %          - A populated celestial.OrbitalEl object.
            % Author : Eran Ofek (Dec 2023)
            % Example: D9 = DS9analayis;
            %          [KA,OrbEl] = D9.plotKnownAst
            %          [KA] = D9.plotKnownAst('OrbEl',OrbEl);


            arguments
                Obj
                Args.MagLimit  = Inf;
                Args.PlotDesig logical = true;
                Args.AddNumber logical = true;
                Args.AddMag logical    = true;
                Args.OrbEl     = [];
                Args.INPOP     = [];
                
            end

            if isempty(Args.INPOP)
                Args.INPOP = celestial.INPOP;
                Args.INPOP.populateAll;
            end

            if isempty(Args.OrbEl)
                OrbEl = celestial.OrbitalEl.loadSolarSystem('merge');
            else
                OrbEl = Args.OrbEl;
            end

            FrameInd = ds9.frame;
            
            CooCenter = imProc.astrometry.getCooCenter(Obj.Images(FrameInd), 'OutCooUnits','deg');
            JD        = Obj.Images(FrameInd).julday;

            [Result] = searchMinorPlanetsNearPosition(OrbEl, JD, CooCenter(1), CooCenter(2), CooCenter(3), 'INPOP',Args.INPOP, 'CooUnits','deg', 'SearchRadiusUnits','deg', 'MagLimit',Args.MagLimit);

            if ~isemptyCatalog(Result)
                DesigCell = Result.Catalog.Desig;
                Nast      = numel(DesigCell);
    
                if Args.AddNumber
                    AstNumber = OrbEl.desig2number(Result.Catalog.Desig);
                    for Iast=1:1:Nast
                        DesigCell{Iast} = sprintf('%s / %d', DesigCell{Iast}, AstNumber(Iast));
                    end
                end
                if Args.AddMag
                    for Iast=1:1:Nast
                        DesigCell{Iast} = sprintf('%s / %4.1f', DesigCell{Iast}, Result.Catalog.Mag(Iast));
                    end
                end
    
                ds9.plotc(Result.Catalog.RA, Result.Catalog.Dec, 'Text',DesigCell)
            end
        end


    end
    
    methods  % basic utilities
        function [X, Y, Val, AI, Key, Coo] = getXY(Obj, Coo, Mode, Args)
            % Get X/Y position for user clicked/specified position
            % Input  : - self.
            %          - If empty, then prompt the user to click the ds9
            %            window in a give position.
            %            Alterantively, a vector of [RA, Dec] in decimal or
            %            radians.
            %            Or, a cell of sexagesimal coordinates {RA, Dec}.
            %          - Mode: Number of cliked mouse points to select, or
            %            'q' for multiple points selection
            %            terminated by clicking 'q'.
            %            Default is 1.
            %          * ...,key,val,...
            %            'CooSys' - Coordinate system of user specified
            %                   coordinates: 'sphere'|'pix'. Default is 'sphere.
            %            'CooUnits' - Coordinates units. Default is 'deg'.
            %            'OutUnits' - Units of output RA/Dec coordinates.
            %                   Default is 'deg'.
            %            'Msg' - Printed message for mouse click:
            %                   Default is 'Select point in ds9 using mouse'
            % Output : - X position.
            %          - Y position.
            %          - Image value at position.
            %          - AstroImage at current frame for which
            %            positions/values where obtained.
            %          - Clicked key.
            %          - [RA, Dec] at position.
            % Author : Eran Ofek (May 2023)
            % Example: [X,Y, Val,~,Key,Coo] = D9.getXY()

            arguments
                Obj
                Coo    = [];
                Mode   = 1;
                Args.CooSys    = 'sphere';
                Args.CooUnits  = 'deg';
                Args.OutUnits  = 'deg';
                Args.Msg       = 'Select point in ds9 using mouse';
            end

            Frame = ds9.frame;
            Ind   = Frame; %Obj.MapInd(Frame);
            AI    = Obj.getImage(Ind);

            
            Key = [];
            if isempty(Coo)
                fprintf('%s\n',Args.Msg);
                [X, Y, PixVal, Key] = ds9.getpos(1);
            else
                if iscell(Coo)
                    % assume Coo in sexagesimal coordinates
                    [X, Y] = AI.WCS.sky2xy(Coo{1}, Coo{2});
                else

                    switch lower(CooSys)
                        case 'sphere'
                            [X, Y] = AI.WCS.sky2xy(Coo(1), Coo(2), 'InUnits',Args.CooUnits);
                        case 'pix'
                            X = Coo(1);
                            Y = Coo(2);
                        otherwise
                            error('Unknown CooSys option');
                    end
                end
            end

            if nargout>2
                Xpix      = round(X);
                Ypix      = round(Y);
                Val       = AI.Image(Ypix, Xpix);
            end
            if nargout>5
                [RA, Dec] = AI.WCS.xy2sky(X, Y, 'OutUnits',Args.OutUnits);
                Coo = [RA, Dec];
            end
        end

    end

    methods  % tools
        function Result = dist(Obj, Coo, Args)
            % Calculate distance between two specified points in ds9
            % Input  : - self.
            %          - If empty, then prompt the user to click the ds9
            %            window in a give position.
            %            Alterantively, a vector of [RA, Dec] in decimal or
            %            radians.
            %            Or, a cell of sexagesimal coordinates {RA, Dec}.
            %          * ...,key,val,...
            %            'CooSys' - Coordinate system of user specified
            %                   coordinates: 'sphere'|'pix'. Default is 'sphere.
            %            'CooUnits' - Coordinates units. Default is 'deg'.
            %            'OutUnits' - Units of output angular properties.
            %                   Default is 'deg'.
            % Output : - A structure with the following fields:
            %            .DistPix - Distance [pix]
            %            .PApix - P.A. 
            %            .X - X of two points
            %            .Y - Y of two points
            %            If WCS is available, the following are calculated:
            %            .RA - RA of two points.
            %            .Dec - Dec of two points.
            %            .DistAng - Angular distance.
            %            .PAang - P.A. relative to the North
            % Author : Eran Ofek (May 2023)
            % Example: R=D9.dist
            
            
            arguments
                Obj
                Coo              = [];  % [X1 Y1; X2 Y2]
                Args.CooSys      = 'sphere';
                Args.CooUnits    = 'deg';
                Args.OutUnits    = 'deg';
            end
            
            Mode = 1;
            if isempty(Coo)
                [X1, Y1, Val1, AI] = getXY(Obj, Coo, Mode, 'CooSys',Args.CooSys, 'CooUnits',Args.CooUnits, 'Msg','Select point 1 in ds9 using mouse');
                [X2, Y2, Val2, ~]  = getXY(Obj, Coo, Mode, 'CooSys',Args.CooSys, 'CooUnits',Args.CooUnits, 'Msg','Select point 2 in ds9 using mouse');
            else
                X1 = Coo(1,1);
                Y1 = Coo(1,2);
                X2 = Coo(2,1);
                Y2 = Coo(2,2);
            end
            Result.DistPix = sqrt((X1-X2).^2 + (Y1-Y2).^2);
            Result.PApix   = atan2(Y2-Y1, X2-X1);
            Result.X   = [X1, X2];
            Result.Y   = [Y1, Y2];
            
            % check if WCS is available
            if AI.WCS.Success
                [Result.RA, Result.Dec] = AI.WCS.xy2sky(Result.X, Result.Y, 'OutUnits',Args.OutUnits);
                
                Factor = convert.angular(Args.OutUnits, 'rad', 1);
                Result.RA     = Result.RA.*Factor;
                Result.Dec    = Result.Dec.*Factor;
                [Result.DistAng, Result.PA] = celestial.coo.sphere_dist(Result.RA(1), Result.Dec(1), Result.RA(2), Result.Dec(2));
                Result.DistAng = convert.angular('rad', Args.OutUnits, Result.DistAng);
                Result.PAang   = convert.angular('rad', Args.OutUnits, Result.PA);
                
            end
            
        end
        
        function Result = radial(Obj, Coo, Mode, Args)
            % Calculate radial plots around selected positions.
            % Input  : - self.
            %          - If empty, then prompt the user to click the ds9
            %            window in a give position.
            %            Alterantively, a vector of [RA, Dec] in decimal or
            %            radians.
            %            Or, a cell of sexagesimal coordinates {RA, Dec}.
            %          - Mode: Number of cliked mouse points to select, or
            %            'q' for multiple points selection
            %            terminated by clicking 'q'.
            %            Default is 1.
            %          * ...,key,val,...
            %            'Center' - A logical indicating if to center the
            %                   radial plot on the nearest 1st moment position.
            %                   Default is true.
            %            'Radius' - radius of radiual plot.
            %                   Default is 15 pix.
            %            'Step' - Bin size step of the radial plot.
            %                   Default is 1 pix.
            %            'CooSys' - Coordinate system of user specified
            %                   coordinates: 'sphere'|'pix'. Default is 'sphere.
            %            'CooUnits' - Coordinates units. Default is 'deg'.
            %            'Plot' - A logical indicating if to plot the last
            %                   radial plot (pixel mean value vs. radius).
            %                   Default is true.
            % Output : - A structure array with element per selected
            %            position.
            %            The following fields are available:
            %            .R - radius
            %            .N - number of points in each radius bin.
            %            .MeanR - Mean radius of points in bin.
            %            .MeanV - Mean image val of points in bin.
            %            .MedV - Median image val of points in bin.
            %            .StdV - Std image val of points in bin.
            % Author : Eran Ofek (Oct 2023)
            % Example: R=D9.radial

            arguments
                Obj
                Coo              = [];  % [X1 Y1; X2 Y2]
                Mode             = 1;
                Args.Center logical    = true;
                Args.Radius            = 15;
                Args.Step              = 1;
                Args.CooSys            = 'sphere';
                Args.CooUnits          = 'deg';
                Args.moments2args cell = {};
                
                Args.Plot logical      = true;
            end
            
            [X, Y, Val, AI] = getXY(Obj, Coo, Mode, 'CooSys',Args.CooSys, 'CooUnits',Args.CooUnits);
            
            Cube = imUtil.cut.image2cutouts(AI.Image, X, Y, Args.Radius);
            
            Xcut = Args.Radius + 1;
            Ycut = Args.Radius + 1;
            
            [M1, M2, Aper] = imUtil.image.moment2(Cube, Xcut, Ycut, Args.moments2args{:});
            
            % calc radial profiles
            Result = imUtil.psf.radialProfile(Cube, [M1.X, M1.Y], 'Radius',Args.Radius, 'Step',Args.Step);
            
            if Args.Plot
                plot(Result(end).R, Result(end).MeanV, 'k-');
            end
            
        end
        
        function [M1, M2, Aper, RADec, AI] = moments(Obj, Coo, Mode, Args)
            % Measure 1st, 2nd moments, and aper phot at user click or specified position.
            % Input  : - self.
            %          - If empty, then prompt the user to click the ds9
            %            window in a give position.
            %            Alterantively, a vector of [RA, Dec] in decimal or
            %            radians.
            %            Or, a cell of sexagesimal coordinates {RA, Dec}.
            %          - Mode: Number of cliked mouse points to select, or
            %            'q' for multiple points selection
            %            terminated by clicking 'q'.
            %            Default is 1.
            %          * ...,key,val,...
            %            'CooSys' - Coordinate system of user specified
            %                   coordinates: 'sphere'|'pix'. Default is 'sphere.
            %            'CooUnits' - Coordinates units. Default is 'deg'.
            %
            %            'HalfSize' - Moments stamp half size.
            %                   Default is 7.
            %            'MaxIter' - Maximum number of moment estimation
            %                   iterations. Default is 10.
            %            'MaxStep' -  Maximum step size (pixels) in X and Y shifts
            %                   allowd in each iteration. Default is 0.1.
            %            'AperRadius' - Vector of aperture radii, in which to calculate
            %                   aperture photometry.
            %                   Default is [2 4 6].
            %            'Annulus' - Vector of inner and outer radius of background
            %                   annulus. Default is [10 14].
            %            'OutUnits' - Units of output RA, Dec position
            %                   based on 1st moments. Default is 'deg'.
            % Output : - A structure with 1st moment information:
            %            .RoundX - Vector of roundex X position
            %            .RoundY - Vector of roundex Y position
            %            .DeltaLastX - Vector of the X shifts in the last position
            %                   iteration.
            %            .DeltaLastY - Vector of the Y shifts in the last position
            %                   iteration.
            %            .Iter - Number of position iterations.
            %            .X    - 1st moment X position
            %            .Y    - 1st moment Y position.
            %            .Xstart - Starting X position,
            %            .Ystart - Starting Y position.
            %           - A second momement information.
            %             A structure with the following fields.
            %             .X2 - X^2 2nd moment.
            %             .Y2 - Y.^2 2nd moment.
            %             .XY - X*Y 2nd moment.
            %           - Photometry information. A structure with the following fields.
            %             .AperRadius - Vector of apertures radius.
            %             .AperPhot - Matrix of aperture photometry. Column per
            %                         aperture.
            %             .AperArea - Matrix of apertures area. Column per aperture.
            %             .BoxPhot  - Vector of the full box photometry (if requested)
            %             .AnnulusBack - Annulus background.
            %             .AnnulusStd - Annulus StD.
            %             .WeightedAper - Weighted photometry. Weighted by the user
            %                           specified weight function.
            %           - [RA, Dec] of of 1st moemnt position.
            %           - The AstroImage object from which the information
            %             was extracted.
            % Author : Eran Ofek (May 2023)
            % Example: [M1, M2, Aper, RADec, AI] = D9.moments;
            
            arguments
                Obj
                Coo              = [];
                Mode             = 1;
                Args.CooSys      = 'sphere';
                Args.CooUnits    = 'deg';
                
                Args.HalfSize    = 7;
                Args.MaxIter     = 10;
                Args.MaxStep     = 0.1;
                Args.AperRadius  = [2 4 6];
                Args.Annulus     = [10 14];
                Args.OutUnits    = 'deg';
            end

            [X, Y, Val, AI] = getXY(Obj, Coo, Mode, 'CooSys',Args.CooSys, 'CooUnits',Args.CooUnits);
            
            
            %[Cube, RoundX, RoundY, X, Y] = imUtil.cut.image2cutouts(AI.Image, X, Y, Args.HalfSize);
            [M1, M2, Aper]               = imUtil.image.moment2(AI.Image, X, Y, 'SubBack',true,...
                                                                            'MaxIter',Args.MaxIter,...
                                                                            'MaxStep',Args.MaxStep,...
                                                                            'AperRadius',Args.AperRadius,...
                                                                            'Annulus',Args.Annulus);
            
            if nargout>3
                [RA, Dec] = AI.WCS.xy2sky(M1.X, M1.Y, 'OutUnits',Args.OutUnits);
                RADec = [RA, Dec];
            end
                
        end
        
        function Result = forcedPhot(Obj, Coo, Mode, Args)
            % Forced photometry on specified position in ds9.
            % Input  : - self.
            %          - If empty, then prompt the user to click the ds9
            %            window in a give position.
            %            Alterantively, a vector of [RA, Dec] in decimal or
            %            radians.
            %            Or, a cell of sexagesimal coordinates {RA, Dec}.
            %          - Mode: Number of cliked mouse points to select, or
            %            'q' for multiple points selection
            %            terminated by clicking 'q'.
            %            Default is 1.
            %          * ...,key,val,...
            %            'CooSys' - Coordinate system of user specified
            %                   coordinates: 'sphere'|'pix'. Default is 'sphere.
            %            'CooUnits' - Coordinates units. Default is 'deg'.
            %
            %            'forcedPhotArgs' - A cell array of additional
            %                   arguments to pass to imProc.sources.forcedPhot
            %            'OutType' - Output type:
            %                   'ms' - A MatchedSources object.
            %                   'ac' - An AstroCatalog object.
            %                   't'  - A table object.
            %                   Default is 't'.
            % Output : - A MatchedSources object with the output measured
            %            forced photometry parameters.
            % Author : Eran Ofek (May 2023)
            % Example: R=D9.forcedPhot

            arguments
                Obj
                Coo              = [];  % [X1 Y1; X2 Y2]
                Mode             = 1;
                Args.CooSys      = 'sphere';
                Args.CooUnits    = 'deg';
                
                Args.forcedPhotArgs cell = {};
                Args.OutType             = 't';
            end
            
            [X, Y, Val, AI] = getXY(Obj, Coo, Mode, 'CooSys',Args.CooSys, 'CooUnits',Args.CooUnits);
            
            MS = imProc.sources.forcedPhot(AI,'Coo',[X(:) Y(:)], 'AddRefStarsDist',false, 'Moving',false, 'CooUnits','pix', Args.forcedPhotArgs{:});
            
            switch lower(Args.OutType)
                case {'ms','matchedsources'}
                    % do nothing
                    Result = MS;
                case {'ac','astrocatalog'}
                    Result = MS.convert2AstroCatalog;
                case {'t','table'}
                    Result = MS.convert2AstroCatalog;
                    Result = Result.toTable;
                otherwise
                    error('Unknown OutType option');
            end
            
        end
            
        function [MaskName, MaskVal]=getMask(Obj, Coo, Args)
            % Get Mask bit values/names at user clicked/specified position
            % Input  : - self.
            %          - If empty, then prompt the user to click the ds9
            %            window in a give position.
            %            Alterantively, a vector of [RA, Dec] in decimal or
            %            radians.
            %            Or, a cell of sexagesimal coordinates {RA, Dec}.
            %          * ...,key,val,...
            %            'CooSys' - Coordinate system of user specified
            %                   coordinates: 'sphere'|'pix'. Default is 'sphere.
            %            'CooUnits' - Coordinates units. Default is 'deg'.
            % Output : - A cell array of bit mask names at the specified
            %            position. If empty, then bit mask is 0.
            %          - Bit mask decimal value.
            % Author : Eran Ofek (May 2023)
            % Example: D9.getMask

            arguments
                Obj
                Coo    = [];
                Args.CooSys   = 'sphere';
                Args.CooUnits = 'deg';
            end

            [X, Y, Val, AI] = getXY(Obj, Coo, 'CooSys',Args.CooSys, 'CooUnits',Args.CooUnits);
            
            Xpix      = round(X);
            Ypix      = round(Y);
            if isempty(AI.Mask)
                error('No Mask image');
            end
            MaskVal   = AI.Mask(Ypix,Xpix);
            MaskName  = AI.MaskData.Dict.bitdec2name(MaskVal);
            MaskName  = MaskName{1};

        end

        function [Back, Var, X, Y, AI] = getBack(Obj, Coo, Mode, Args)
            % Get Back/Var values at user clicked/specified position
            % Input  : - self.
            %          - If empty, then prompt the user to click the ds9
            %            window in a give position.
            %            Alterantively, a vector of [RA, Dec] in decimal or
            %            radians.
            %            Or, a cell of sexagesimal coordinates {RA, Dec}.
            %          - Mode: Number of cliked mouse points to select, or
            %            'q' for multiple points selection
            %            terminated by clicking 'q'.
            %            Default is 1.
            %          * ...,key,val,...
            %            'CooSys' - Coordinate system of user specified
            %                   coordinates: 'sphere'|'pix'. Default is 'sphere.
            %            'CooUnits' - Coordinates units. Default is 'deg'.
            % Output : - Background value.
            %          - Variance value.
            %          - Clicked X position.
            %          - Clicked Y position.
            %          - AstroImage on which the operation was performed.
            % Author : Eran Ofek (May 2023)
            
            arguments
                Obj
                Coo    = [];
                Mode   = 1;
                Args.CooSys   = 'sphere';
                Args.CooUnits = 'deg';
            end

            [X, Y, Val, AI] = getXY(Obj, Coo, Mode, 'CooSys',Args.CooSys, 'CooUnits',Args.CooUnits);
            
            Xpix      = round(X);
            Ypix      = round(Y);
            if isempty(AI.Back) || isempty(AI.Var)
                error('No Back or Var image');
            end
            Back = AI.Back(Ypix, Xpix);
            Var  = AI.Var(Ypix, Xpix);            
            
        end
            
        % plot
        % plotAll  % in all frames
        
        function [Result,Dist,CatInd]=near(Obj, Coo, Radius, Args)
            % Get sources in AstroImage catalog within search radius from clicked/specified position.
            % Input  : - self.
            %          - If empty, then prompt the user to click the ds9
            %            window in a give position.
            %            Alterantively, a vector of [RA, Dec] in decimal or
            %            radians.
            %            Or, a cell of sexagesimal coordinates {RA, Dec}.
            %          * ...,key,val,...
            %            'CooSys' - Coordinate system of user specified
            %                   coordinates: 'sphere'|'pix'. Default is 'sphere.
            %            'CooUnits' - Coordinates units. Default is 'deg'.
            %            'OutType' - Type of output catalog:
            %                   'table' | 'astrocatalog'.
            %                   Default is 'table'.
            % Output : - Catalog of of sources within search radius,
            %            obtained from the CatData property in the
            %            AstroImage object associated with the current
            %            image.
            %          - Vector of distances [pix] from the clicked search
            %            position, for each source in the output catalog.
            %          - Vector of indices of the returned sources in the
            %            original AstroCatalog in the AstroImage.
            % Author : Eran Ofek (May 2023)
            % Example: [R,Dist]=D9.near
            
            arguments
                Obj
                Coo    = [];
                Radius = 10;  % pix
                Args.CooSys   = 'sphere';
                Args.CooUnits = 'deg';
                Args.OutType  = 'table';  % 'table'|'astrocatalog'
            end
            
            Mode  = 1;
            [X, Y, Val, AI] = getXY(Obj, Coo, Mode, 'CooSys',Args.CooSys, 'CooUnits',Args.CooUnits);

            if AI.CatData.sizeCatalog==0
                error('Source catalog in AstroImage is empty');
            end

            CatXY = AI.CatData.getXY;

            Dist  = sqrt( (CatXY(:,1)-X).^2 + (CatXY(:,2)-Y).^2);

            Flag = Dist<Radius;
            CatInd = find(Flag);
            Dist   = Dist(CatInd);
            switch lower(Args.OutType)
                case 'table'
                    Result = AI.CatData.toTable;
                    Result = Result(CatInd,:);
                case 'astrocatalog'
                    Result = AI.CatData.copy;
                    Result.Catalog = Result.Catalog(CatInd,:);
                otherwise
                    error('Unknown OutType option');
            end


        end

        % nearestAll
        % RADec=getCoo
    end

    methods % imexam
        function Result=imexam(Obj, Coo, Args)
            % Interactive image examination tool
            
            arguments
                Obj
                Coo
                Args
            end
            
            Cont = true;
            while Cont
                [X, Y, Val, AI, Key] = getXY(Obj, Coo, Mode, Args);
                
                
                
            end
            
            
        end
    end


    methods (Static) % Unit-Test
        Result = unitTest()
            % unitTest for ds9

    end
    
    
end % end class
            
