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
    properties (Dependent)
        Image
    end
    properties
        Events(1,1) AstroCatalog
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
        function Result = events2image(Table, Args)
            %
            
            
        end
        
    end
    
    
    methods % good times and selections
        function [Obj, GoodTimes, FlagGood] = findGoodTimes(Obj, Args)
            %
            
            arguments
                Obj
                Args.NperBin           = 100;
                Args.BinTime           = [];
                Args.ThresholdSN       = 10;
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
