% Class for time-tagged events table/images
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
    
    properties
        
        Events(1,1) AstroCatalog
        Image
        X
        Y
        %Header(1,1) AstroHeader                      % maybe redundent if part of AstroImage
        BadTimes(:,2)                     = zeros(0,2);
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
        function Obj = TimeTagImage(varargin)
            % what to read?
            
            Obj.ImageData   = ImageComponent;
            Obj.EventsTable = AstroCatalog;
            
        end
        
    end
    
    methods % setters/getters
        function Result = get.Image(Obj)
            % getter for Image
            % If empty, then will calculate the image using the sky x/y
            % coordinates.
            
            if isempty(Obj.Image)
                XY = getCol(Obj, Obj.ColSky);
                [Obj.Image, Obj.X, Obj.Y] = PhotonsList.events2image(XY);
            end
            Result = Obj.Image;
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
        function Obj = readPhotonsList1(File, Args)
            % Read time-taged photons list into a PhotonsList object
            % Input  : - A FITS file name to read.
            %          * ...,key,val,...
            %            'HDU' - HDU number in the FITS image.
            % Output : - A PhotonsList object.
            % Author : Eran Ofek (Feb 2022)
            % Obj = PhotonsList.readPhotonsList1('/data/euler/eran/work/Chandra/ao21/cat2/22335/acisf22335_repro_evt2.fits');
            
            arguments
                File
                Args.HDU                  = 1; % HDU or dataset
                Args.ReadBadTimes logical = true;
            end
            
            ImIO = ImageIO(File, 'HDU',Args.HDU, 'IsTable',true , 'readTableArgs',{'OutTable','astrocatalog'});
            
            Obj = PhotonsList;
            Obj.Events = ImIO.Data;
            
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

    
    methods % astrometry
        function pix2coo
            
        end
        
        function coo2pix
            
        end
        
    end
       
    
    methods (Static) % Unit-Test
        Result = unitTest()
    end
    
end
