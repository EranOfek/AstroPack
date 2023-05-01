% Class for time-tagged events table/images
%
% Examples:
%      % get data:
%      VO.Chandra.wget_obsid(366)
%      P=PhotonsList('acisf00366N006_evt2.fits');
%      or:
%      P=PhotonsList.readPhotonsList1('acisf00366N006_evt2.fits');
%      P.populateBadTimes
%      [a,b] = P.nphotons
%      P.selectEnergy([200 8000]);
%      [a,b] = P.nphotons
%      % populate the image field, X/Y correspinds to image [default is sky coo]
%      P.constructImage; 
%      % Add RA/Dec to catalog
%      P.addSkyCoo


%
% #functions (autogen)
% TimeTagImage - what to read?
% coo2pix -
% events2image -
% findGoodTimes -
% pix2coo -
% readPhotonsList1 - Obj = PhotonsList.readPhotonsList1('/data/euler/eran/work/Chandra/ao21/cat2/22335/acisf22335_repro_evt2.fits');
% selectEnergy - select photons within some energy ranges
% #/functions (autogen)
%

classdef PhotonsList < Component
    properties (Dependent)
        Header
        Time
        Energy
        
        SkyXY
        TDetXY
        DetXY
        ChipXY
        
    end
    
    properties
        
        Events(1,1) AstroCatalog
        BadTimes(:,2)                     = zeros(0,2);
        
        HeaderData(1,1) AstroHeader                      % maybe redundent if part of AstroImage
        Back BackImage
        Mask MaskImage
        WCS(1,1) AstroWCS
        
        Image                             = [];
        X
        Y
        
        %FlagGood(:,1) logical             = true(0,1);
        %FlagEnergy(:,1) logical           = true(0,1);
        
        ColTime            = 'time';
        ColEnergy          = 'energy';
        ColTDet            = {'tdetx','tdety'};
        ColDet             = {'detx','dety'};
        ColChip            = {'chipx','chipy'};
        ColSky             = {'x','y'};
                                            % CHIP	pixel numbers on ACIS chip or HRC segment
                                            % TDET	tiled detector, an artificial system to show the whole instrument plane
                                            % DET	detector or mirror coordinates
                                            % SKY	a pixel plane aligned with ICRS RA and Dec

        
    end
    
    methods % constructor
        function Obj = PhotonsList(List)
            % PhotonsList constructor
            % Input  : - If empty, return a single enpty PhotonsList
            %            object.
            %            If a char array or a cell of char arrays, then
            %            will read each photon events file into a
            %            PhotonsList object.
            % Output : - A PhotonsList object.
            % Author : Eran Ofek (Apr 2023)
            
            arguments
                List   = [];
            end
            
            if isempty(List)
                Iobj = 1;
                Obj(Iobj).Image  = [];
            else
                if ~iscell(List)
                    List = {List};
                end
                
                Nlist = numel(List);
                for Iobj=1:1:Nlist
                    Obj(Iobj) = PhotonsList.readPhotonsList1(List{Iobj});
                end
            end
            
        end
        
    end
    
    methods % setters/getters
        function Result = get.Image(Obj)
            % getter for Image
            % If empty, then will calculate the image using the sky x/y
            % coordinates.
            
            if isempty(Obj.Image)
                [Obj] = constructImage(Obj);
            end
            Result = Obj.Image;
        end
        
        function Result = get.Header(Obj)
            % Getter for Header
            
            Nobj = numel(Obj);
            for Iobj=1:1:Nobj
                Result = Obj(Iobj).HeaderData.Data;
            end
        end
        
        function Result = get.Time(Obj)
            % Getter for Time column
            
            Result = getCol(Obj, Obj.ColTime);
        end
        
        function Result = get.Energy(Obj)
            % Getter for Energy column
            
            Result = getCol(Obj, Obj.ColEnergy);
        end
        
        function XY = get.SkyXY(Obj)
            % Getter for sky [X,Y] columns
            
            XY = getCol(Obj, Obj.ColSky);
           
        end
        
        function XY = get.TDetXY(Obj)
            % Getter for TDet [X,Y] columns
            
            XY = getCol(Obj, Obj.ColTDet);
           
        end
        
        function XY = get.DetXY(Obj)
            % Getter for Det [X,Y] columns
            
            XY = getCol(Obj, Obj.ColDet);
           
        end
        
        function XY = get.ChipXY(Obj)
            % Getter for Chip [X,Y] columns
            
            XY = getCol(Obj, Obj.ColChip);
        end
        
    end
    
    methods % basic functions getCol
        function Result = getCol(Obj, ColNames)
            % get content of column
            % Input  : - A single elements PhotonsList object.
            %          - A Column name, or a cell array of column names.
            % Output : - A matrix of the requested columns content.
            % Author : Eran Ofek (Feb 2022)
            % Example: P=PhotonsList.readPhotonsList1('acisf21421N002_evt2.fits');
            %          
           
            arguments
                Obj(1,1)
                ColNames
            end
            
            Result = getCol(Obj.Events, ColNames);
        end
    end
    
    methods (Static)  % static methods / reading photon-tagged lists
        function [Obj] = readPhotonsList1(File, Args)
            % Read time-taged photons list from a FITS file into a PhotonsList object
            %   Read also the header and WCS from the header.
            % Input  : - A FITS file name to read.
            %          * ...,key,val,...
            %            'HDU' - HDU number in the FITS image.
            %            'HeaderHDU' - Header HDU. Default is 1.
            %            'ReadBadTimes' - Read bad times from file.
            %                   Default is true.
            % Output : - A PhotonsList object.
            % Author : Eran Ofek (Feb 2022)
            % Obj = PhotonsList.readPhotonsList1('/data/euler/eran/work/Chandra/ao21/cat2/22335/acisf22335_repro_evt2.fits');
            
            arguments
                File
                Args.HDU                  = 2; % HDU of table
                Args.HeaderHDU            = 1;
                Args.ReadBadTimes logical = true;
            end
            
            Obj = PhotonsList;
            
            %ImIO = ImageIO(File, 'HDU',Args.HDU, 'IsTable',true , 'readTableArgs',{'OutTable','astrocatalog'});
            
            %[Out, HeaderT] = FITS.readTable1(File,'HDUnum',Args.HDU, 'OutTable','AstroCatalog');
            [Out, HeaderT] = FITS.readTable1(File,'HDUnum',Args.HDU, 'OutTable','AstroCatalog', 'BreakRepCol',true, 'TableType','bintable');
            
            if Args.ReadBadTimes
                % Chandra HDU 3 [Start, End] times
                % Chandra HDU 4 [Start, End] good times
                Obj.BadTimes = zeros(0,2);
            end
            
            % read header
            [HeadCell] = FITS.readHeader1(File, Args.HeaderHDU);
            Obj.HeaderData.Data = HeadCell;
            Obj.Events = Out;
            
            % get WCS from HeaderT
            
            % BUG HERE!!!
            %Obj.WCS = AstroWCS.xrayHeader2wcs(HeaderT);
            
            
            Telescope = Obj.HeaderData.getVal('TELESCOP');
            switch lower(Telescope)
                case 'chandra'
                    % Chandra
                    Obj.WCS = AstroWCS.xrayHeader2wcs(HeaderT, 'Num1',11,'Num2',12);
                case 'XRT'
                    % Swift-XRT
                    Obj.WCS = AstroWCS.xrayHeader2wcs(HeaderT, 'Num1',2,'Num2',3);
                otherwise
                    error('Unknown X-ray telescope');
            end
                        
        end
        
    end
    
    
    methods (Static)    % static functions
        function [Image,X,Y] = events2image(XY, Args)
            % Generate an image from a list of [X,Y] positions.
            % Input  : - A two column matrix of [X,Y] positions.
            %          * ...,key,val,...
            %            'BinSize' - Bin size in X and Y. Default is [1 1]. 
            %            'CCDSEC' - A vector of [Xmin, Xmax, Ymin, Ymax],
            %                   in which to construct the image.
            %                   If empty, will use the min.max values in
            %                   the XY values. Default is [].
            % Output : - The image constructed from the list of XY
            %            positions.
            %          - A vector of X position corresponding to the center
            %            of the X pixels.
            %          - A vector of Y position corresponding to the center
            %            of the Y pixels.
            % Author : Eran Ofek (Feb 2022)
            % Example: P=PhotonsList.readPhotonsList1('acisf21421N002_evt2.fits');
            %          XY = getCol(P,{'x','y'});
            %          [Image,X,Y] = PhotonsList.events2image(XY);
            
            arguments
                XY
                Args.BinSize = [1 1];
                Args.CCDSEC  = [];
            end
            
            if isempty(Args.CCDSEC)
                CCDSEC = [min(XY(:,1)), max(XY(:,1)), min(XY(:,2)), max(XY(:,2))];
            else
                CCDSEC = Args.CCDSEC;
            end
            
            Xedges = (CCDSEC(1):Args.BinSize(1):CCDSEC(2));
            Yedges = (CCDSEC(3):Args.BinSize(2):CCDSEC(4));
            
            Image  = histcounts2(XY(:,1), XY(:,2), Xedges, Yedges);
            X      = (Xedges(1:end-1) + Xedges(2:end)).*0.5;
            Y      = (Yedges(1:end-1) + Yedges(2:end)).*0.5;
        end
        
    end
    
    
    methods % good times and selections
        function [Result,ResultGood] = nphotons(Obj)
            % count photons in image, including photons in good images
            % Input  : - A PhotonsList object
            % Output : - Total number of photons in each PhotonsList
            %            element.
            %          - Number of photons in good times in each PhotonsList
            %            element.
            % Author : Eran Ofek (Apr 2023)
            
           
            Result = zeros(size(Obj));
            Nobj   = numel(Obj);
            for Iobj=1:1:Nobj
                Result(Iobj) = sizeCatalog(Obj(Iobj).Events);
            end
            
            if nargout>1
                ResultGood = zeros(size(Obj));
                for Iobj=1:1:Nobj
                    [~,Flag]=Obj(Iobj).removeBadTimes('RemoveBadTimes',false);
                    ResultGood(Iobj) = sum(~Flag.Flag);
                end
            end
            
        end
        
        function [Obj] = populateBadTimes(Obj, Args)
            % Identify bad times and populate the bad times property.
            % Input  : - A PhotonsList object.
            %          * ...,key,val,...
            %            'ColTime' - Column name containing the time tags.
            %                   If empty, then use the PhotonsList object
            %                   ColTime property. Default is [].
            %            'NperBin' - Mean number of points per bin that
            %                   will be used to estimate the bin size.
            %                   Default is 100.
            %            'TimeBin' - Time bin. If not empty this will override
            %                   the 'NperBin' argument. Default is [].
            %            'MeanFun' - A function handle that will be used to
            %                   calculate the mean of histogram.
            %                   Default is @tools.math.stat.nanmedian
            %            'ThresholdSN' - Threshold S/N for bins above the
            %                   mean that will be flagges as bad times.
            %                   Default is 4.
            % Output : - A PhotonsList object with the BadTimes property
            %            populated. The bad times contains a two matrix
            %            ciolumn with [Start End] of each bad time window.
            % Author : Eran Ofek (Fen 2022)
            % Example: P=PhotonsList.readPhotonsList1('acisf21421N002_evt2.fits');
            %          P = populateBadTimes(P)
            
            
            arguments
                Obj
                Args.ColTime                   = [];
                Args.NperBin                   = 100;
                Args.TimeBin                   = [];
                Args.MeanFun function_handle   = @tools.math.stat.nanmedian;
                Args.ThresholdSN               = 4;
                
            end
            
            Nobj = numel(Obj);
            if ~isempty(Args.ColTime)
                [Obj(1:1:Nobj).ColTime] = deal(Args.ColTime);
            end
            
            for Iobj=1:1:Nobj
                Times     = getCol(Obj(Iobj), Obj(Iobj).ColTime);
                Nt        = numel(Times);
                MinTime   = min(Times);
                MaxTime   = max(Times);
                TimeRange = MaxTime - MinTime;
                
                if isempty(Args.TimeBin)
                    % use NperBin
                    TimeBin = Args.NperBin .* TimeRange ./Nt;
                else
                    TimeBin = Args.TimeBin;
                end
                
                Edges = (MinTime: TimeBin: MaxTime);
                Nhist = histcounts(Times, Edges);
                
                Mean = Args.MeanFun(Nhist,[1 2]);
                
                BadBins = Nhist > (Mean + sqrt(Mean).*Args.ThresholdSN);
                
                BadBinsInd = find(BadBins);
                
                Obj(Iobj).BadTimes = [Edges(BadBinsInd).', Edges(BadBinsInd+1).'];
              
            end
            
        end
        
        function [Obj, FlagBad] = removeBadTimes(Obj, Args)
            % Remove bad times from PhotonsList
            % Input  : - A PhotonsList object.
            %          * ...,key,val,...
            %            'RePop' - Repopulate the BadTimes property in the
            %                   PhotonsList object using populateBadTimes.
            %                   Default is true.
            %            'RemoveBadTimes' - Remove bad times from
            %                   PhotonsList object. Default is true.
            %            'CreateNewObj' - Create a new copy of the object.
            %                   Default is false.
            %            'ColTime' - Column name containing the time tags.
            %                   If empty, then use the PhotonsList object
            %                   ColTime property. Default is [].
            %            'NperBin' - Mean number of points per bin that
            %                   will be used to estimate the bin size.
            %                   Default is 100.
            %            'TimeBin' - Time bin. If not empty this will override
            %                   the 'NperBin' argument. Default is [].
            %            'MeanFun' - A function handle that will be used to
            %                   calculate the mean of histogram.
            %                   Default is @tools.math.stat.nanmedian
            %            'ThresholdSN' - Threshold S/N for bins above the
            %                   mean that will be flagges as bad times.
            %                   Default is 4.
            % Output : - A PhotonsList object with the optionaly removed
            %            photons in bad times.
            %          - A structure array with a .Flag field containing a
            %            vector of logical of all the bad photons.
            % Author : Eran Ofek (Feb 2022)
            % Example: P=PhotonsList.readPhotonsList1('acisf21421N002_evt2.fits');
            %          P = removeBadTimes(P)
            
           
            arguments
                Obj
                Args.RePop logical             = true;
                Args.RemoveBadTimes logical    = true;
                Args.CreateNewObj logical      = false;
                Args.ColTime                   = [];
                Args.NperBin                   = 100;
                Args.TimeBin                   = [];
                Args.MeanFun function_handle   = @tools.math.stat.nanmedian;
                Args.ThresholdSN               = 4;
            end
            
            if Args.CreateNewObj
                Result = Obj.copy;
            else
               Result = Obj;
            end
            
            
            Nobj = numel(Obj);
            if ~isempty(Args.ColTime)
                [Obj(1:1:Nobj).ColTime] = deal(Args.ColTime);
            end
            
            if Args.RePop
                [Obj] = populateBadTimes(Obj, 'ColTime',Args.ColTime,...
                                          'NperBin',Args.NperBin,...
                                          'TimeBin',Args.TimeBin,...
                                          'MeanFun',Args.MeanFun,...
                                          'ThresholdSN',Args.ThresholdSN);
            end
            
            for Iobj=1:1:Nobj
                % remove bad times from PhotonList
                Times     = getCol(Obj(Iobj), Obj(Iobj).ColTime);
              
                [FlagBad(Iobj).Flag] = tools.array.find_ranges_flag(Times, Obj(Iobj).BadTimes);
                if Args.RemoveBadTimes
                    Obj(Iobj).Events.Catalog = Obj(Iobj).Events.Catalog(~FlagBad(Iobj).Flag,:);
                end
            end
        end
        
        function [Obj, FlagEnergy] = selectEnergy(Obj, EnergyRange, Args)
            % Select photons within some energy ranges
            % Input  : - An PhotonsList object (multi elements supported).
            %          - A two column matrix of energy ranges [min max].
            %          * ...,key,val,...
            %            'CreateNewObj' - A logical indicating if to create
            %                   a new copy of the object. Default is false.
            % Output : - The PhotonsList object with only photons in the
            %            selected energy ranges.
            %          - A vector of flagged photons (in energy range), but
            %            only for the last element in the PhotonsList object.
            % Author : Eran Ofek (Feb 2022)
            % Example: P=PhotonsList.readPhotonsList1('acisf21421N002_evt2.fits');
            %          P.selectEnergy([200 8000]);
            
            arguments
                Obj
                EnergyRange
                Args.CreateNewObj logical    = false;
            end
            
            if Args.CreateNewObj
                Result = Obj.copy;
            else
                Result = Obj;
            end
            
            Nen = size(EnergyRange,1);
            
            Nobj = numel(Obj);
            for Iobj=1:1:Nobj
                ColInd = Obj.Events.colname2ind(Obj(Iobj).ColEnergy);
            
                EnergyVec  = Obj.Events.Catalog(:,ColInd);
                FlagEnergy = true(size(EnergyVec));
                for Ien=1:1:Nen
                    FlagEnergy = FlagEnergy & (EnergyVec>EnergyRange(Ien,1) & EnergyVec<EnergyRange(Ien,2));
                end
                
                Result(Iobj).Events.Catalog = Obj(Iobj).Events.Catalog(FlagEnergy,:);
                
            end
                    
        end
    end

    methods % sources
        function [Src] = getSrcPhotons(Obj, RA, Dec, Args)
            % Get photons around sources RA/Dec position.
            %   Given a list of RA/Dec or X/Y positions, return all photons
            %   around these positions.
            % Input  : - A PhotonsList object.
            %          - J2000.0 RA, or sky x coordinates.
            %          - J2000.0 Dec, or sky y coordinates.
            %          * ...,key,val,...
            %            'InUnits' - Input units 'rad','deg' for spherical.
            %                   or 'sky' if input coordinates are sky x/y
            %                   coordinates. Default is 'deg'.
            %            'SearchRadius' - Returm all photons within this
            %                   diatance for each source position.
            %                   Default is 10.
            %            'Annulus' - Annuulus radii at which to get
            %                   background photons. Default is [20 30].
            %            'SearchRadiusUnits' - Search radius units
            %                   'pix' | 'arcsec'. Default is 'pix.
            %            'ReturnCol' - Columns to return for seleced
            %                   photons.
            %                   If 'RA','Dec' are added then RA/Dec will be
            %                   added to the PhotonsList object (and it
            %                   will be updated).
            %                   Default is {'time','energy','ccd_id','chipx','chipy','x','y','grade','RA','Dec'}
            %            'ColSky' - Column names that contains the sky x/y
            %                   coordinates. If empty, then use the ColSky
            %                   property in the PhotonsList object.
            %                   Default is [].
            % Output : - A structure array with element for each source
            %            position requested. The following fields are
            %            available:
            %            .Flag - A vector of logical indicating the
            %                   selected photons for the source.
            %            .FlagBack - The same as Flag but for the annulus
            %                   background.
            %            .Data - A matrix with only the selected photons
            %                   (i.e., all the photons within the search radius
            %                   from the source position). The matrix
            %                   columns are those in the 'ReturnCol' input
            %                   argument.
            %            .DataBack - The same as Data, but for the annulus
            %                   background photons.
            % Author : Eran Ofek (Feb 2022)
            % Example: P=PhotonsList.readPhotonsList1('acisf21421N002_evt2.fits');
            %          Src = P.getSrcPhotons(91.423,-86.632)
            
            arguments
                Obj(1,1)
                RA
                Dec
                Args.InUnits           = 'deg';   % 'sky' - for X/Y position
                Args.SearchRadius      = 10;
                Args.Annulus           = [20 30];
                Args.SearchRadiusUnits = 'pix';
                Args.ReturnCol         = {'time','energy','ccd_id','chipx','chipy','x','y','grade','RA','Dec'};
                Args.ColSky            = [];
            end
            
            ARCSEC_DEG = 3600;
            
            if isempty(Args.ColSky)
                Args.ColSky = Obj.ColSky;
            end
            
            if any(strcmp(Args.ReturnCol, 'RA')) || any(strcmp(Args.ReturnCol, 'Dec'))
                % RA/Dec requested - add to catalog
                Obj.addSkyCoo;  % add RA/Dec to catalog
            end
            
            
            switch lower(Args.SearchRadiusUnits)
                case 'pix'
                    % do nothing
                    SearchRadius = Args.SearchRadius;  % [pix]
                    Annulus      = Args.Annulus;
                case 'arcsec'
                    PixScale     = abs(Obj.WCS.CD(1,1)).*ARCSEC_DEG;  % arcsec/pix
                    SearchRadius = Args.SearchRadius./PixScale;   % [pix]
                    Annulus      = Args.Annulus./PixScale;
                otherwise
                    error('Unknown SearchRadiusUnits option');
            end
            SearchRadius2 = SearchRadius.^2;  % [pix^2]
            Annulus2      = Annulus.^2;
            
            switch lower(Args.InUnits)
                case 'sky'
                    % coordinates are already X/Y sky
                    Xsrc = RA;
                    Ysrc = Dec;
                otherwise
                    % coordinates are RA/Dec
                    % Convert to sky X/Y
                    [Xsrc, Ysrc] = Obj.sky2xy(RA, Dec, 'InUnits',Args.InUnits);
            end
            XY     = getCol(Obj, Args.ColSky);   % XY of all photons
            
            ReturnData = getCol(Obj, Args.ReturnCol);
            
            Nsrc = numel(Xsrc);
            for Isrc=1:1:Nsrc
                % search for photons of each source
                
                Dist2 = (XY(:,1) - Xsrc).^2 + (XY(:,2) - Ysrc).^2;
                Src(Isrc).Flag     = Dist2<SearchRadius2;
                Src(Iscc).FlagBack = Dist2<Annulus2; 
                Src(Isrc).Data     = ReturnData(Src(Isrc).Flag,:);
                Src(Isrc).DataBack = ReturnData(Src(Isrc).FlagBack,:);
                
            end
            
        end
        
        function [Obj, Image] = constructImage(Obj, Args)
            % construct image in any coordinate system, and optionaly
            % select energy and ccd_id.
            % Input  : - A PhotonsList object.
            %          * ...,key,val,...
            %            'CooSys' - Coordinate system in which to construct
            %                   the image. This is either a {x,y} column
            %                   names, or 'det'|'tdet'|'chip'|'sky'.
            %                   Default is 'sky'.
            %            'BinSize' - Bin size in X and Y. Default is [1 1]. 
            %            'CCDSEC' - A vector of [Xmin, Xmax, Ymin, Ymax],
            %                   in which to construct the image.
            %                   If empty, will use the min.max values in
            %                   the XY values. Default is [].
            %            'CCDID' - Select a specific CCD. If empty, use
            %                   all. Default is [].
            %            'ColCCDID' - Column name containing the CCDID.
            %                   default is 'ccd_id'.
            %            'EnergyRange' - [Min Max] energy to select.
            %                   If empty, select all.
            %                   Default is [].
            %            'ColEnergy' - Column name containing the photons
            %                   energy. Default is 'energy'.
            % Output : - An PhotonsList object in which the Image, X, Y are
            %            populated.
            %          - An image matrix of the last element in the object.
            % Author : Eran Ofek (Feb 2022)
            % Example: P=PhotonsList.readPhotonsList1('acisf21421N002_evt2.fits');
            %          P.constructImage;
            %          P.constructImage('CooSys','chip','EnergyRange',[1000 3000])
            
            arguments
                Obj
                Args.CooSys      = 'sky';   %  {'det','tdet','chip','sky'} or cell array of x/y col names
                Args.BinSize     = [1 1];
                Args.CCDSEC      = [];
                Args.CCDID       = [];
                Args.ColCCDID    = 'ccd_id';
                Args.EnergyRange = [];
                Args.ColEnergy   = 'energy';
            end
            
            Nobj = numel(Obj);
            
            
            if iscell(Args.CooSys)
                Col = Args.CooSys;
            else
                switch lower(Args.CooSys)
                    case 'sky'
                        Col = Obj(1).ColSky;
                    case 'chip'
                        Col = Obj(1).ColChip;
                    case 'det'
                        Col = Obj(1).Det;
                    case 'tdet'
                        Col = Obj(1).TDet;
                    otherwise
                        error('Unknown CooSys option');
                end
            end

            for Iobj=1:1:Nobj
                if isempty(Args.CCDID)
                    % use all CCDID
                    XY = getCol(Obj(Iobj), Col);
                    Flag = true(size(XY,1),1);
                else
                    % use specific CCDID
                    XY   = getCol(Obj(Iobj), [Col, Args.ColCCDID]);
                    Flag = XY(:,3)==Args.CCDID;
                    XY   = XY(Flag,1:2);
                end
                if ~isempty(Args.EnergyRange)
                    % select events by Energy
                    Energy = getCol(Obj(Iobj), Args.ColEnergy);
                    Energy = Energy(Flag);
                    FlagE  = Energy>Args.EnergyRange(1) & Energy<Args.EnergyRange(2);
                    XY     = XY(FlagE,:);
                end
                if ~isempty(XY)
                    [Obj(Iobj).Image, Obj(Iobj).X, Obj(Iobj).Y] = PhotonsList.events2image(XY, 'BinSize',Args.BinSize, 'CCDSEC',Args.CCDSEC);
                else
                    Obj(Iobj).Image = [];
                    Obj(Iobj).X     = [];
                    Obj(Iobj).Y     = [];
                end
            end
            if nargout>1
                Image = Obj(end).Image;
            end
            
        end
        
        function [Obj, Back] = background(Obj, Args)
            % Estimate Poisson background in PhotonsList image
            %   The background expectency is estimated by poissfit the
            %   counts in the pixels. This is done in 2 iterations. After
            %   the first iterations high values are removed.
            % Input  : - A PhotonsList object.
            %          * ...,key,val,...
            %            'MaxProb' - The cumulative probability of poisson
            %                   distribution that is used to calc the max
            %                   value above the cut the distribution.
            %                   Default is 1-1e-4.
            %            'MaxMinHist' - The min number of counts to use.
            %                   Default is 3.
            % Output : - A PhotonsList object in which the Back property is
            %            populated.
            %          - The background level in the last PhotonsList
            %            image.
            % Author : Eran Ofek (Feb 2022)
            % Example: P=PhotonsList.readPhotonsList1('acisf21421N002_evt2.fits');
            %          P.constructImage;
            %          P.background
            
            arguments
                Obj
                Args.MaxProb     = 1-1e-4;
                Args.MaxMinHist  = 3;
            end
           
            Nobj = numel(Obj);
            for Iobj=1:1:Nobj
                % calculate background level
                
                Lambda   = poissfit(Obj(Iobj).Image);
                Max      = max(Args.MaxMinHist, poissinv(Args.MaxProb, Lambda));
                SelIm    = Obj(Iobj).Image(Obj(Iobj).Image<=Max);
                Back     = poissfit(SelIm);
                
                if isempty(Obj(Iobj).Back)
                    Obj(Iobj).Back = BackImage;
                end
                Obj(Iobj).Back.Image = Back;
            end
            
        end
        
    end
    
    methods % astrometry
        function [RA, Dec] = xy2sky(Obj, X, Y, Args)
            % Convert X,Y coordinates to RA/Dec
            %   Note that in ds9 these are called detector coordinates,
            %   while in the Chandra event file they are called sky
            %   coordinates.
            % Input  : - A single-element PhotonsList object.
            %          - X (sky) coordinate.
            %          - Y (sky) coordinate.
            %          * ...,key,val,...
            %            'OutUnits' - Default is 'deg'.
            % Output : - J2000.0 RA
            %          - J2000.0 Dec
            % Author : Eran Ofek (Feb 2022)
            % Example: P=PhotonsList.readPhotonsList1('acisf21421N002_evt2.fits');
            %          [r,d]=P.WCS.xy2sky(4075,4094);
            
            arguments
                Obj(1,1)
                X
                Y
                Args.OutUnits    = 'deg';
            end
            RAD = 180./pi;

            DX = (X - Obj.WCS.CRPIX(1)).*Obj.WCS.CD(1,1)./RAD;
            DY = (Y - Obj.WCS.CRPIX(2)).*Obj.WCS.CD(2,2)./RAD;
            [RA,Dec] = celestial.proj.pr_ignomonic(DX, DY, Obj.WCS.CRVAL./RAD);

            RA  = convert.angular('rad', Args.OutUnits, RA);
            Dec = convert.angular('rad', Args.OutUnits, Dec);
            
        end
        
        function [X, Y] = sky2xy(Obj, RA, Dec, Args)
            % Convert RA/Dec coordinates to X/Y.
            %   Note that in ds9 these are called detector coordinates,
            %   while in the Chandra event file they are called sky
            %   coordinates.
            % Input  : - A single-element PhotonsList object.
            %          - J2000.0 RA
            %          - J2000.0 Dec
            %          * ...,key,val,...
            %            'InUnits' - Default is 'deg'.
            % Output : - X (sky) coordinates.
            %          - Y (sky) coordinates.
            % Author : Eran Ofek (Feb 2022)
            % Example: P=PhotonsList.readPhotonsList1('acisf21421N002_evt2.fits');
            %          [r,d]=P.xy2sky(4075,4094);
            %          [x,y]=P.sky2xy(r,d);
            
            arguments
                Obj(1,1)
                RA
                Dec
                Args.InUnits    = 'deg';
            end
            RAD = 180./pi;

            RA  = convert.angular(Args.InUnits,'rad',RA);
            Dec = convert.angular(Args.InUnits,'rad',Dec);
            
            [X,Y] = celestial.proj.pr_gnomonic(RA,Dec, 1, Obj.WCS.CRVAL./RAD);
            X = X.*RAD./Obj.WCS.CD(1,1) + Obj.WCS.CRPIX(1);
            Y = Y.*RAD./Obj.WCS.CD(2,2) + Obj.WCS.CRPIX(2);
            
        end
        
        function Obj = addSkyCoo(Obj, Args)
            % Add RA/Dec to events list
            % Input  : - A PhotonsList object.
            %          * ...,key,val,...
            %            'CooUnits' - Units of added coordinates.
            %                   Default is 'deg'.
            %            'ColRA' - RA column name in the events table.
            %                   Default is 'RA'.
            %            'ColDec' - Dec column name in the events table.
            %                   Default is 'Dec'.
            % Output : - A PhotonsList object with the added RA/Dec.
            % Author : Eran Ofek (Feb 2022)
            % Example: P=PhotonsList.readPhotonsList1('acisf21421N002_evt2.fits');
            %          P.addSkyCoo;
           
            arguments
                Obj
                Args.CooUnits      = 'deg';
                Args.ColRA         = 'RA';
                Args.ColDec        = 'Dec';
            end
            
            Nobj = numel(Obj);
            for Iobj=1:1:Nobj
                XY = getCol(Obj(Iobj), Obj(Iobj).ColSky);
                [RA, Dec] = xy2sky(Obj(Iobj), XY(:,1), XY(:,2), 'OutUnits',Args.CooUnits);
               
                % insert/replace columns
                Obj(Iobj).Events = replaceCol(Obj(Iobj).Events, [RA, Dec], {Args.ColRA, Args.ColDec}, Inf, {Args.CooUnits, Args.CooUnits});
        
            end
        end
    end
       
    
    methods (Static) % Unit-Test
        Result = unitTest()
    end
    
end
