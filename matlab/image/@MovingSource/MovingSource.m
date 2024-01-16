% MovingSource class
%   MovingSources class can be used to store detection of minor planet
%   (or any moving) objects.
% Description: 
% Author : Eran Ofek (Jan 2024)
%
% Properties:
%   Note that the RA, Dec, PM_RA, PM_Dec, Mag properties are always
%   scalars. They are obtained from the first line of MergedCat.
%
% Methods:
%
%
% Example:
%   % read all AstCrop files into MovingSources object.
%   MP=MovingSource.readFromAstCrop();
%   
%   % remove sources that are likely statis sources and NearEdge || Overlap
%   FlagStatic = MP.nearStatisSrc
%   FlagGood    = MP.selectByBitMask;
%   MP = MP(~[FlagStatic.Flag].' & FlagGood(:));
%   % Populate known asteroids:
%   MP.popKnownAst;
%   % Some inspection operations:
%   [Dist, Mag]=MP.nearestKnownAst;
%   % possible unknwon asteroids
%   UNK = isnan(Dist);
%   % generate report blindly
%   Report = MP.reportMPC;
%   % blink and report
%   [Report,Res] = MS.blink;
%

classdef MovingSource < Component
    
  
 
    properties (Dependent) % Access image data directly
    end
    
    properties (SetAccess = public)
        JD
        RA
        Dec
        PM_RA
        PM_Dec
        CooUnits
        PMUnits
        Mag
        Info              % additional optional information
        ImageID     = []; % struct with fields defined by KeyID
        Stamps      = AstroImage;
        MergedCat
        KnownAst    = [];
        FileName    = '';
    end

    properties (Hidden, SetAccess = public)
        KeyID     = {'NODENUMB','MOUNTNUM','CAMNUM','CROPID','FIELDID','COUNTER'};
        KeyJD     = 'JD';
        ColRA     = 'RA';
        ColDec    = 'Dec';
        ColPM_RA  = 'PM_RA';
        ColPM_Dec = 'PM_Dec';
        ColMag    = {'Mean_MAG_PSF','Mean_MAG_APER_2'};

        %PopulatedKnownAst logical = false;
    end

    properties (Hidden, SetAccess = public)
        PopKA logical         
        OrbEl                 = []; %celestial.OrbitalEl.loadSolarSystem('merge');
        INPOP                 = []; %celestial.INPOP.init;
        AstSearchRadius       = 10;
        AstSearchRadiusUnits  = 'arcsec';
        AstMagLimit           = Inf;
    end
    
    
    methods % Constructor
       
        function Obj = MovingSources(Nobj, Args)
            % Constructor of MinorPlanet class 
            % Input  : - Number of elements in object.
            %            If empty, set to 1.
            %            Default is [].
            %          * ...,key,val,...
            %            'PopKA' - A logical indicating of to populate the
            %                   KnonwAst property. If true, then will
            %                   search for known asteroids within Obj.AstSearchRadius
            %                   and Obj.AstMagLimit.
            %                   Default is false.
            %            'OrbEl' - A celestial.OrbitalEl object.
            %                   If empty, then will be populated with
            %                   celestial.OrbitalEl.loadSolarSystem('merge')
            %                   only if 'PopKA' is true.
            %            'INPOP' - A celestial.INPOP object.
            %                   If empty, then will be populated with
            %                   celestial.INPOP.init
            %                   only if 'PopKA' is true.
            %            'AstSearchRadius' - Known asteroid search radius
            %                   to put in the Obj.AstSearchRadius hidden property.
            %                   Default is 10.
            %            'AstSearchRadiusUnits' - Known asteroid search
            %                   radius units
            %                   to put in the Obj.AstSearchRadiusUnits hidden property.
            %                   Default is 'arcsec'.
            %            'AstMagLimit' - Known asteroid magnitude limit
            %                   to put in the Obj.AstMagLimit hidden property.
            %                   Default is Inf.
            %
            % Output : - A MovingSource object.
            % Author : Eran Ofek (Jan 2024)
            % Example: MP = MovingSource;
            %          MP = MovingSource(3, 'PopKA',true);
            
            arguments
                Nobj = [];
                Args.PopKA logical         = false;
                Args.OrbEl                 = []; %celestial.OrbitalEl.loadSolarSystem('merge');
                Args.INPOP                 = []; %celestial.INPOP.init;
                Args.AstSearchRadius       = 10;
                Args.AstSearchRadiusUnits  = 'arcsec';
                Args.AstMagLimit           = Inf;

            end

            %Obj@ImageComponent(varargin{:});
            %for Iobj=1:1:numel(Obj)
            %    Obj(Iobj).DataType = AstroDataType.ExpTime;
            %end

            if isempty(Nobj)
                Nobj = 1;
            end
            if Args.PopKA
                if isempty(Args.OrbEl)
                    Args.OrbEl = celestial.OrbitalEl.loadSolarSystem('merge');
                end
                if isempty(Args.INPOP)
                    Args.INPOP = celestial.INPOP.init;
                end
            end

            for Iobj=1:1:Nobj
                Obj(Iobj).PopKA    = Args.PopKA;
                if Args.PopKA
                    Obj(Iobj).OrbEl = Args.OrbEl; 
                    Obj(Iobj).INPOP = Args.INPOP;
                end
                Obj(Iobj).AstSearchRadius      = Args.AstSearchRadius;
                Obj(Iobj).AstSearchRadiusUnits = Args.AstSearchRadiusUnits;
                Obj(Iobj).AstMagLimit          = Args.AstMagLimit;
            end
            
        end

    end
 
    methods % getters/setters
        function Val=get.ImageID(Obj)
            % getter for ImageID
            % Read info from stamps header

            if isempty(Obj.ImageID)
                IsEmptyImage = isemptyImage(Obj.Stamps);
                if all(IsEmptyImage)
                    % do nothing
                    Val = [];
                else
                    % read ImageID from header
                    Nstamps = numel(Obj.Stamps);
                    Val = getStructKey(Obj.Stamps, Obj.KeyID);
                end
            else
                % do nothing
                Val = Obj.ImageID;
            end
            Obj.ImageID = Val;
        end

        function Val=get.JD(Obj)
            % getter for JD

            Key = 'JD';
            KeyName = 'JD';

            if isempty(Obj.(Key))
                if isempty(Obj.MergedCat)
                    Val = [];
                else
                    Val = Obj.MergedCat.(KeyName);
                end
            else
                Val = Obj.(Key);
            end
            
        end

        function Val=get.RA(Obj)
            % getter for RA

            Key = 'RA';
            ColName = 'ColRA';

            if isempty(Obj.(Key))
                if isempty(Obj.MergedCat)
                    Val = [];
                else
                    Val = getCol(Obj.MergedCat, Obj.(ColName), false, false, 'SelectRows',1);
                end
            else
                Val = Obj.(Key);
            end
            
        end

        function Val=get.Dec(Obj)
            % getter for Dec

            Key = 'Dec';
            ColName = 'ColDec';

            if isempty(Obj.(Key))
                if isempty(Obj.MergedCat)
                    Val = [];
                else
                    Val = getCol(Obj.MergedCat, Obj.(ColName), false, false, 'SelectRows',1);
                end
            else
                Val = Obj.(Key);
            end
            
        end

        function Val=get.CooUnits(Obj)
            % getter for CooUnits

            Key = 'CooUnits';
            ColName = 'ColRA';

            if isempty(Obj.(Key))
                if isempty(Obj.MergedCat)
                    Val = [];
                else
                    [~,Tmp] = getCol(Obj.MergedCat, Obj.(ColName), false, false, 'SelectRows',1);
                    Val = Tmp{1};
                end
            else
                Val = Obj.(Key);
            end
        end

        function Val=get.PM_RA(Obj)
            % getter for PM_RA

            Key = 'PM_RA';
            ColName = 'ColPM_RA';

            if isempty(Obj.(Key))
                if isempty(Obj.MergedCat)
                    Val = [];
                else
                    Val = getCol(Obj.MergedCat, Obj.(ColName), false, false, 'SelectRows',1);
                end
            else
                Val = Obj.(Key);
            end
            
        end

        function Val=get.PM_Dec(Obj)
            % getter for PM_Dec

            Key = 'PM_Dec';
            ColName = 'ColPM_Dec';

            if isempty(Obj.(Key))
                if isempty(Obj.MergedCat)
                    Val = [];
                else
                    Val = getCol(Obj.MergedCat, Obj.(ColName), false, false, 'SelectRows',1);
                end
            else
                Val = Obj.(Key);
            end
            
        end

        function Val=get.PMUnits(Obj)
            % getter for PMUnits

            Key = 'PMUnits';
            ColName = 'ColPM_RA';

            if isempty(Obj.(Key))
                if isempty(Obj.MergedCat)
                    Val = [];
                else
                    [~,Tmp] = getCol(Obj.MergedCat, Obj.(ColName), false, false, 'SelectRows',1);
                    Val = Tmp{1};
                end
            else
                Val = Obj.(Key);
            end
        end

        function Val=get.Mag(Obj)
            % getter for Mag

            Key = 'Mag';
            ColName = 'ColMag';

            if isempty(Obj.(Key))
                if isempty(Obj.MergedCat)
                    Val = [];
                else
                    Val = getColDic(Obj.MergedCat, Obj.(ColName)); %, false, false, 'SelectRows',1);
                    if numel(Val)>1
                        Val = Val(1);
                    end
                end
            else
                Val = Obj.(Key);
            end
            
        end

        function set.PopKA(Obj, Val)
            % setter for PopKA (populated KnownAst)

            if Val && ~isempty(Obj.JD) && ~isempty(Obj.RA) && ~isempty(Obj.Dec)
                if isempty(Obj.OrbEl)
                    Obj.OrbEl = celestial.OrbitalEl.loadSolarSystem('merge');
                end
                if isempty(Obj.INPOP)
                    Obj.INPOP = celestial.INPOP.init;
                end
                [Result] = searchMinorPlanetsNearPosition(Obj.OrbEl, Obj.JD, Obj.RA, Obj.Dec, Obj.AstSearchRadius,...
                                    'INPOP',Obj.INPOP,...
                                    'CooUnits',Obj.CooUnits,...
                                    'SearchRadiusUnits',Obj.AstSearchRadiusUnits,...
                                    'MagLimit',Obj.AstMagLimit,...
                                    'AddDist',true,...
                                    'ConeSearch',true);
                Obj.KnownAst = Result;

            end
            Obj.PopKA = Val;
            
        end


    end

    methods (Static)  % static methods: read/write/convert
        % conversions
        function Obj=read(File, Args)
            % Read a MovingSource object from file
            % Input  : - Either a a mat file name containing a MovingSource
            %            object, or a structure with a single field containing a
            %            MovingSource object, or an old format AstCrop
            %            structure.
            %            If empty, then will recursivly look for all mat file with
            %            specific template and upload them.
            %          * ...,key,val,...
            %            '
            % Output : - A MovingSource object
            % Author : Eran Ofek (Jan 2024)
            % Example: MP = MovingSource.read()

            
        end

        function Obj=readFromAstCrop(AstCrop, Args)
            % Read AstCrop structure (old format) into MovingSource object
            % Input  : - AstCrop strtucture, or mat file name containing
            %            AstCrop structure.
            %            If empty, then recursivly search for all files
            %            indicated by template 'AstFileTemp'.
            %            Default is [].
            %          * ...,key,val,...
            %            'Path' - Move to this path. Default is pwd.
            %            'Id' - Id of AstCrop element. If empty, read all.
            %                   Default is [].
            %            'AstFileTemp' - File template to search.
            %                   Default is '*merged_Asteroids*.mat'.
            %            'PopKA' - Populate known asteroids.
            %                   Default is false.
            %            'KeepOnlyFirstAndLast' - A logical indicating if
            %                   to delete all the Stamps images excep the first
            %                   and last.
            %                   Default is true.
            % Output : - A MovingSource object.
            % Example: MP=MovingSource.readFromAstCrop(AC)
            %          MP=MovingSource.readFromAstCrop('LAST.01.10.01_20231107.010845.394_clear_091+10_001_001_001_sci_merged_Asteroids_1.mat')
            %          MP=MovingSource.readFromAstCrop()

            arguments
                AstCrop          = [];
                Args.Path        = pwd;
                Args.Id          = [];
               
                Args.AstFileTemp   = '*merged_Asteroids*.mat';
                Args.PopKA logical = false;
                Args.KeepOnlyFirstAndLast logical  = true;
            end

            PWD = pwd;
            cd(Args.Path);

            if isempty(AstCrop)
                Files = io.files.rdir(Args.AstFileTemp);
            elseif ischar(AstCrop) || isstring(AstCrop)
                Files(1).folder = pwd;
                Files(1).name   = AstCrop;
            else
                % do nothing
            end

            Obj = MovingSource;
            
            

            if isstruct(AstCrop)
                Ncrop = numel(AstCrop.AstCrop);
                Iall = 0;
                for Icrop=1:1:Ncrop
                    if isempty(Args.Id) || Icrop==Args.Id
                        Iall = Iall + 1;
                        Obj(Iall).Stamps    = AstCrop.AstCrop(Icrop).Stamps;
                        Obj(Iall).MergedCat = AstCrop.AstCrop(Icrop).SelectedCatPM;
                    end
                end

            else
                Nf = numel(Files);
                Iall = 0;
                for If=1:1:Nf
                    AstCrop = io.files.load2(fullfile(Files(If).folder, Files(If).name));
                    Ncrop   = numel(AstCrop.AstCrop);
                    for Icrop=1:1:Ncrop
                        if isempty(Args.Id) || Icrop==Args.Id
                            Iall = Iall + 1;
                            Obj(Iall).Stamps    = AstCrop.AstCrop(Icrop).Stamps;
                            Obj(Iall).MergedCat = AstCrop.AstCrop(Icrop).SelectedCatPM;
                            Obj(Iall).FileName  = fullfile(Files(If).folder,  Files(If).name);

                            % Remove some image to save memory
                            if Args.KeepOnlyFirstAndLast
                                Nstamp = numel(Obj(Iall).Stamps);
                                Obj(Iall).Stamps(2:Nstamp-1).deleteProp({'Image','Back','Var','Mask','PSFData'});
                            end

                        end
                    end
                end
            end

            cd(PWD);
            Nobj = numel(Obj);
            for Iobj=1:1:Nobj
                Obj(Iobj).PopKA = Args.PopKA;
            end
        end

    end

    methods  % utilities
        function Obj=popKnownAst(Obj)
            % Populate KnownAst in all elements of MovingSource object
            % Input  : - A MovingSource object.
            % Output : - A populated MovingSource object.
            % Author : Eran Ofek (Jan 2024)
            % Example: MP.popKnownAst;
            
            Nobj = numel(Obj);
            for Iobj=1:1:Nobj
                Obj(Iobj).PopKA = true;
            end
        end
        
        function [FlagComb, Flag]=nearStatisSrc(Obj, Args)
            % Check for static sources near the moving source
            %   Search a PGC galaxy or GAIA star near the moving source.
            %   The PGC search is based on the galaxy radius.
            %   The GAIA search distance is magnitude dependent (e.g.,
            %   brighter GAIA stars have larger radius of influence).
            % Input  : - A MovingSource object.
            %          * ...,key,val,...
            %            See code for details.
            % Output : - A Flag indicating if the source is good (i.e., no
            %            associated static source).
            %          - A structure array (element per MovingSource
            %            element) with the following fields:
            %            .GAIA - A logical indicating if there is a GAIA
            %                   star that may contaminate the detection.
            %            .MagGAIA - The mag. of the GAIA contaminants.
            %            .PGC - A logical indicating if there is a PGC
            %                   galaxy that may contaminate the detection.
            %            .GLADE - A logical indicating if there is a GLADEp
            %                   galaxy that may contaminate the detection.
            %            .Flag - A logical flag indicating if there is a
            %                   contaminant of any kind.
            % Author : Eran Ofek (Jan 2024)
            % Example: Res=MP(3).nearStatisSrc
            arguments
                Obj
                Args.CatNameGAIA = 'GAIADR3';
                
                Args.ColMagGAIA  = 'phot_bp_mean_mag';
                Args.MaxMagGAIA  = 20.5;
                
                Args.CatNamePGC  = 'PGC';
                Args.CatNameGlade = 'GLADEp';
                Args.MaxDistGlade = 10;
                
                Args.SearchRadius      = 60;
                Args.SearchRadiusUnits = 'arcsec';

                

            end

            Nobj = numel(Obj);
            for Iobj=1:1:Nobj
                %
                
                CooFactor = convert.angular(Obj(Iobj).CooUnits, 'rad');
                RA_rad  = Obj(Iobj).RA.*CooFactor;   % [rad]
                Dec_rad = Obj(Iobj).Dec.*CooFactor;  % [rad]

                % GAIA
                Flag(Iobj).GAIA = false;
                Flag(Iobj).MagGAIA = [];
                CatGAIA = catsHTM.cone_search(Args.CatNameGAIA, RA_rad, Dec_rad, Args.SearchRadius, 'RadiusUnits',Args.SearchRadiusUnits, 'OutType','AstroCatalog');
                Dist    = CatGAIA.sphere_dist(RA_rad, Dec_rad, 'rad', 'arcsec');
                DistThresholdPerStar = max(20-CatGAIA.Table.(Args.ColMagGAIA), 3);
                FlagM   = CatGAIA.Table.(Args.ColMagGAIA)<Args.MaxMagGAIA & Dist<DistThresholdPerStar;

                if any(FlagM)
                    Flag(Iobj).GAIA = true;
                    Flag(Iobj).MagGAIA = CatGAIA.Table.(Args.ColMagGAIA)(FlagM);
                end

                % PGC
                Flag(Iobj).PGC = false;
                CatPGC = catsHTM.cone_search(Args.CatNamePGC, RA_rad, Dec_rad, Args.SearchRadius, 'RadiusUnits',Args.SearchRadiusUnits, 'OutType','AstroCatalog');
                if ~CatPGC.isemptyCatalog
                    Dist    = CatPGC.sphere_dist(RA_rad, Dec_rad, 'rad', 'arcsec');
                    
                    GalRadius = 3.*10.^(CatPGC.Table.LogD25);
                    if ~isempty(Dist) && any(Dist<GalRadius)
                        Flag(Iobj).PGC = true;
                    end
                end
                
                % GLADE
                Flag(Iobj).GLADE = false;
                CatGLADE = catsHTM.cone_search(Args.CatNameGlade, RA_rad, Dec_rad, Args.SearchRadius, 'RadiusUnits',Args.SearchRadiusUnits, 'OutType','AstroCatalog');
                if ~CatGLADE.isemptyCatalog
                    Dist    = CatGLADE.sphere_dist(RA_rad, Dec_rad, 'rad', 'arcsec');

                    if ~isempty(Dist) && any(Dist<Args.MaxDistGlade)
                        Flag(Iobj).GLADE = true;
                    end
                end
                
                Flag(Iobj).Flag = Flag(Iobj).GAIA || Flag(Iobj).PGC & Flag(Iobj).GLADE;
            end

            FlagComb = ~[Flag.Flag];
            FlagComb = FlagComb(:);

        end

        function [Dist, Mag]=nearestKnownAst(Obj)
            % Return the angular distance for the nearest known asteroid.
            %   Will also pipulate the KnownAst property in fot already
            %   populated.
            % Input  : - A MovingSource object.
            % Output : - A vector of angular distance [arcsec].
            %            Each element corresponds to one element in the
            %            MovingSource. NaN if no source was found.
            %          - Like angular distance, but for the magnitude of
            %            the nearest asteroid.
            % Author : Eran Ofek (Jan 2024)
            % Example: [Dist,Mag]=MP.nearestKnownAst

            arguments
                Obj
            end

            Nobj = numel(Obj);
            Dist = nan(Nobj,1);
            Mag  = nan(Nobj,1);
            for Iobj=1:1:Nobj
                Obj(Iobj).PopKA = true;
                if ~isemptyCatalog(Obj(Iobj).KnownAst)
                    Dist(Iobj) = Obj(Iobj).KnownAst.Table.Dist;
                    Mag(Iobj)  = Obj(Iobj).KnownAst.Table.Mag;
                end
            end

        end

        function [Flag] = selectByBitMask(Obj, Args)
            % Select elements of MovingSource object that have specific BitMask.
            %   This function check the value of the FLAGS column in the
            %   MergedCat property of the MovingSource object. It returns
            %   list of elements that satisfy some criteria (i.e., some of
            %   the bits are on or off).
            % Input  : - A MovingSource object.
            %          * ...,key,val,...
            %            'Flags' - A cell array of FLAGS to select (or not)
            %                   Default is {''NearEdge','Overlap'}.
            %            'ColFlags' - Column name containing the flags
            %                   information. Default is 'FLAGS'.
            %            'Method' - Select 'any' | 'all' flags.
            %                   Default is 'any'.
            %            'NotFlags' - Apply not the search bits.
            %                   If true then select elements that do not
            %                   contain the flags in 'Flags'.
            %                   Default is true.
            %            'BitDict' - BitDictionary object.
            %                   Default is BitDictionary.
            % Output : - Logical flags indicating, for each element, if the
            %            flags were satisfied.
            %          - The selected elements of the MovingSource object.
            % Author : Eran Ofek (Jan 2024)
            % Example: [~,MP]=MP.selectByBitMask;
            
            
            arguments
                Obj
                Args.Flags                   = {'NearEdge','Overlap'};
                Args.ColFlags                = 'FLAGS';
                Args.Method                  = 'any';
                Args.NotFlags logical        = true;
                Args.BitDict                 = BitDictionary;
                
            end
            
            Nobj = numel(Obj);
            Flag = false(Nobj,1);
            for Iobj=1:1:Nobj
                DecFlag = Obj(Iobj).MergedCat.getCol(Args.ColFlags);
                % If there are multiple entries - use only the first:
                DecFlag = DecFlag(1);
                if Args.NotFlags
                    Flag(Iobj) = ~Args.BitDict.findBit(DecFlag, Args.Flags, 'Method',Args.Method);
                else
                    Flag(Iobj) = Args.BitDict.findBit(DecFlag, Args.Flags, 'Method',Args.Method);
                end
            end
                            
        end
    
        function Result=selectMovingFromStamps(Obj, Args)
            % Create a catalog of the moving source position as a function of time
            %   as appear in the MovingSource Stamps.
            %   The positions are selected as the nearest source within the
            %   search radius, to the RA and Dec, in each Stamp.
            % Input  : - A MovingSource object.
            %          * ...,key,val,...
            %            'SearchRadius' - Search radius. Default is 4.
            %            'SearchRadiusUnits' - Search radius units.
            %                   Default is 'arcsec'.
            %            'TableCol' - Additional columns to add after 'JD'.
            %                   Default is {'RA','Dec','MAG_PSF','FLAGS'}.
            % Output : - An AstroCatalog object with a table of positions
            %            of the moving source in each stamp.
            %            If no source found within the search radius, then
            %            the time entry will contain NaN.
            % Author : Eran Ofek (Jan 2024)
            % Example: Result = MP.selectMovingFromStamps;
            
            arguments
                Obj
                Args.SearchRadius      = 4;
                Args.SearchRadiusUnits = 'arcsec';
                Args.TableCol          = {'RA','Dec','MAG_PSF','FLAGS'};
            end
           
            SearchRadiusAS = convert.angular(Args.SearchRadiusUnits, 'arcsec', Args.SearchRadius);
            NextraCol      = numel(Args.TableCol);
            
            Nobj = numel(Obj);
            Result = AstroCatalog(size(Obj));
            
            ColNames = ['JD', Args.TableCol];
            
            for Iobj=1:1:Nobj
                JD = Obj(Iobj).Stamps.julday; % all JD

                Nstamp = numel(Obj(Iobj).Stamps);
                Cat    = nan(Nstamp, 1+ NextraCol);
                for Istamp=1:1:Nstamp
                    Dist = Obj(Iobj).Stamps(Istamp).CatData.sphere_dist(Obj(Iobj).RA, Obj(Iobj).Dec, Obj(Iobj).CooUnits, 'arcsec');
                    [MinDist, MinInd] = min(Dist);
                    if MinDist>SearchRadiusAS
                       % no source found
                       Cat(Istamp,1) = JD(Istamp);
                    else
                        % source found
                        Cat(Istamp,:) = [JD(Istamp), Obj(Iobj).Stamps(Istamp).CatData.getCol(Args.TableCol, false, false, 'SelectRows',MinInd)];
                    end

                end
                Result(Iobj) = AstroCatalog({Cat}, 'ColNames',ColNames);
                
            end
            
        end
        
        function ReportMPC=reportMPC(Obj, Args)
            % Generate MPC report for MovingSource object
            %   Generate an MPC report for all elements of a MovingSource object
            % Input  : - A MovingSource object.
            %          * ...,key,val,...
            %            'ReportType' - Options are:
            %                   'AllDetections'
            %                   'FittedDetection'
            %                   'FittedDetection3'
            %            'AddHeader' - Default is true.
            %            'Filter' - Default is 'C'.
            %            'ColMag' - Default is 'MAG_PSF'.
            %            'PM_IsArcTime' -  If false then units of PM_RA is
            %                   arc/time. If true its time-arc/time
            %                   FFU: *** IN FUTURE DEFAULT WILL BE FALSE ***
            %                   Default is true.
            %            'DefaultObsName' - LAST default. Default is true.
            %            'StartAstIndex' - AstIndex start with this number + 1.
            %                   Default is 0.
            %            'ReportMPC' - Char array of MPC report to which to
            %                   concat this report.
            %                   Default is ''.
            % Output : - Char array of MPC report.
            % Author : Eran Ofek (Jan 2024)
            % Example: MP.reportMPC
            
            arguments
                Obj
                Args.ReportType     = 'FittedDetection3';
                
                Args.AddHeader      = true;
                Args.Filter         = 'C';
                Args.ColMag         = 'MAG_PSF';
                Args.PM_IsArcTime logical   = true;  % if false then uinst of PM_RA is arc/time
                
                Args.generateReportMPCArgs cell = {};
                Args.DefaultObsName logical     = true;

                Args.StartAstIndex = 0;
                Args.ReportMPC     = '';
            end
            
            
            % prep MPC report for asteroid
            % KeyID     = {'NODENUMB','MOUNTNUM','CAMNUM','CROPID','FIELDID','COUNTER'};
            Iobj = 1;
            ObsName = sprintf('Large Array Survey Telescope (LAST) Node %02d Mount %02d Tel %02d',...
                                                    Obj(Iobj).ImageID(1).NODENUMB,...
                                                    Obj(Iobj).ImageID(1).MOUNTNUM,...
                                                    Obj(Iobj).ImageID(1).CAMNUM);
                                                
        
            AddHeader = Args.AddHeader;
            
            AstIndex = Args.StartAstIndex;
            Nobj = numel(Obj);
            ReportMPC = Args.ReportMPC;
            for Iobj=1:1:Nobj
                Result           = Obj(Iobj).selectMovingFromStamps;
                VecJD            = Result.getCol('JD');
                [VecRA, VecDec]  = Result.getLonLat('deg');
                Mag              = Result.getCol(Args.ColMag);
                MedMag           = median(Mag, 1, 'omitnan');
                FlagNN = ~isnan(VecRA);
                NN     = sum(FlagNN);
                
                AstIndex = AstIndex + 1;
                switch Args.ReportType
                    case 'AllDetections'
                        % [JD, RA, Dec, Mag, Filter, AstIndex]
                        ReportTable  = [JD(FlagNN), VecRA(FlagNN), VecDec(FlagNN), nan(NN,1),  AstIndex.*ones(NN,1)];
                        CooUnits     = 'deg';
                        
                    case 'FittedDetection'
                        % [JD, RA, Dec, Mag, Filter, AstIndex]
                        ReportTable = [Obj(Iobj).JD, Obj(Iobj).RA, Obj(Iobj).Dec, Obj(Iobj).Mag, NaN, AstIndex];
                        CooUnits = Obj(Iobj).CooUnits;
                        
                    case 'FittedDetection3'
                        % Evaluate fitted motion at three points
                        JDm = Obj(Iobj).JD;
                        JD1 = min(VecJD);
                        JD3 = max(VecJD);
                        JD2 = 0.5.*(JD1 + JD3);

                        if Args.PM_IsArcTime || strcmp(Obj(Iobj).PMUnits, 'tdeg/deg')
                            CC = 1;
                        else
                            CC = 1./cos(convert.angular(Obj(Iobj).CooUnits, 'rad', Obj(Iobj).Dec));
                        end
                        
                        % Note PM_RA is in time units rather than angular units
                        % so no cos(Dec) correction is needed
                        RA1  = Obj(Iobj).RA + Obj(Iobj).PM_RA.*(JD1-JDm).*CC;
                        RA2  = Obj(Iobj).RA + Obj(Iobj).PM_RA.*(JD2-JDm).*CC;
                        RA3  = Obj(Iobj).RA + Obj(Iobj).PM_RA.*(JD3-JDm).*CC;
                        Dec1 = Obj(Iobj).Dec + Obj(Iobj).PM_Dec.*(JD1-JDm);
                        Dec2 = Obj(Iobj).Dec + Obj(Iobj).PM_Dec.*(JD2-JDm);
                        Dec3 = Obj(Iobj).Dec + Obj(Iobj).PM_Dec.*(JD3-JDm);
                        
                        
                        % [JD, RA, Dec, Mag, Filter, AstIndex]
                        ReportTable = [ [JD1; JD2; JD3], [RA1; RA2; RA3], [Dec1; Dec2; Dec3], MedMag.*ones(3,1), nan(3,1), AstIndex.*ones(3,1)];
                        CooUnits = Obj(Iobj).CooUnits;
                        
                    otherwise
                        error('Unknown RepotyType option');
                end
                
                if Args.DefaultObsName
                    ReportMPC = [ReportMPC, imUtil.asteroids.generateReportMPC(ReportTable, 'Filter',Args.Filter,...
                                                                                  'AddHeader',AddHeader,...
                                                                                  'CooUnits',CooUnits,...
                                                                                  'ObsName',ObsName,...
                                                                                  Args.generateReportMPCArgs{:})];
                else
                    ReportMPC = [ReportMPC, imUtil.asteroids.generateReportMPC(ReportTable, 'Filter',Args.Filter,...
                                                                                  'AddHeader',AddHeader,...
                                                                                  'CooUnits',CooUnits,...
                                                                                  Args.generateReportMPCArgs{:})];
                end                                                          
                AddHeader = false;
            end

            
        end
        
        function [Table,Variability]=lightCurveVariability(Obj, Args)
            % Create simple light curve for moving source
            %   Based on the Stamps catalog
            % Input  : - A MovingSourceObject.
            %          * ...,key,val,...
            %            'SearchRadius' - Search radius. Default is 4.
            %            'SearchRadiusUnits' - Search radius units.
            %                   Default is 'arcsec'.
            %            'TableCol' - Additional columns to add after 'JD'.
            %                   Default is {'RA','Dec','MAG_PSF','FLAGS'}.
            %            'PolyDeg' - A cell array in wich each element contains all
            %                   the degrees of the polynomial to fit.
            %                   E.g., [0:1:2], is a full 2nd deg polynomial.
            %                   The first cell corresponds to the null hypothesis.
            %                   The Delta\chi2^2 is calculated relative to the null
            %                   hypothesis. In addition, the error normalization is
            %                   calculated such that the chi^2/dof of the null
            %                   hypothesis will be 1 (with uniform errors).
            %                   Default is {[0], [0:1:1], [0:1:2]}.
            %
            % Output : - An AstroTable with the light/position curves.
            %          - A structure array (element per MovingSource
            %            element) with the following fields:
            %            .PS - power spectrum.
            %            .VarPolyHT - See details in: timeSeries.fit.fitPolyHyp
            %            .MedMag - Median mag.
            %            .StdMag - Std mag.
            %            .RStdMag - RStd mag.
            % Author : Eran Ofek (Jan 2024)
            % Example: [Table,Variability]=MP.lightCurveVariability

            arguments
                Obj
                Args.SearchRadius      = 4;
                Args.SearchRadiusUnits = 'arcsec';
                Args.TableCol          = {'RA','Dec','MAG_PSF','FLAGS'};
                Args.PolyDeg cell      = {[0], [0:1:1], [0:1:2]};
            end

            Table=selectMovingFromStamps(Obj, 'SearchRadius',Args.SearchRadius,...
                                               'SearchRadiusUnits',Args.SearchRadiusUnits,...
                                               'TableCol',Args.TableCol);
            
            Nobj = numel(Obj);
            Variability = struct('PS',cell(Nobj,1), 'VarPolyHT',cell(Nobj,1), 'MedMag',cell(Nobj,1), 'StdMag',cell(Nobj,1), 'RStdMag',cell(Nobj,1));
            for Iobj=1:1:Nobj
                if ~isemptyCatalog(Table(Iobj))
                    % LC analysis / variability
                    Time = Table(Iobj).Table.JD;
                    Mag  = Table(Iobj).Table.MAG_PSF;
                    % power spectrum
                    PS = timeSeries.period.period([Time, Mag]);
                    % polynomial variability hypothesis testing
                    VarPolyHT = timeSeries.fit.fitPolyHyp(Time, Mag, 'PolyDeg',Args.PolyDeg);
                    Variability(Iobj).PS        = PS;
                    Variability(Iobj).VarPolyHT = VarPolyHT;
                    Variability(Iobj).MedMag    = median(Mag, 'all','omitnan');
                    Variability(Iobj).StdMag    = std(Mag, [], 'all','omitnan');
                    Variability(Iobj).RStdMag   = tools.math.stat.rstd(Mag);
                end
            end

        end

    end

    methods % display and plot
        function Info=dispInfo(Obj, Args)
            % Display moving source information on screen
            % Input  : - A MovingSource object.
            %          * ...,key,val,...
            %            See code for options.
            % Output : - Info structure (not yet available).
            % Author : Eran Ofek (Jan 2024)
            % Example: MP(1).dispInfo;

            arguments
                Obj

                Args.Plot logical  = true;
                Args.BitDic        = BitDictionary;
                Args.CatBitDic     = BitDictionary('BitMask.MergedCat.Default');
            end

            Info = [];
            Nobj = numel(Obj);
            for Iobj=1:1:Nobj

                if Args.Plot
                    fprintf('File %s loaded\n',Obj(Iobj).FileName);
                    fprintf('ID for 1st stamp:\n')
                    FN = fieldnames(Obj(Iobj).ImageID(1));
                    Nfn = numel(FN);
                    for Ifn=1:1:Nfn
                        Val = Obj(Iobj).ImageID(1).(FN{Ifn});
                        if isnumeric(Val)
                            if Val==floor(Val)
                                % integer
                                fprintf('  %-11s : %d\n', FN{Ifn}, Val);
                            else
                                % float:
                                fprintf('  %-11s : %f\n', FN{Ifn}, Val);
                            end
                        else
                            % char
                            fprintf('  %-11s : %s\n', FN{Ifn}, Val);
                        end
                    end
                    fprintf('RA            : %s\n', celestial.coo.convertdms(Obj(Iobj).RA, 'd', 'SH'));
                    fprintf('Dec           : %s\n',celestial.coo.convertdms(Obj(Iobj).Dec, 'd', 'SD'));
                    
                    fprintf('PM_RA         : %f [time-deg/day]\n',Obj(Iobj).PM_RA);
                    fprintf('PM_Dec        : %f [deg/day]\n',Obj(Iobj).PM_Dec);
                    fprintf('JD_PM         : %15.6f\n',Obj(Iobj).JD);
                    
                    % LAST specific
                    fprintf('PM_TdistProb  : %8.6f\n',Obj(Iobj).MergedCat.Table.PM_TdistProb);
                    fprintf('Nobs          : %d\n',Obj(Iobj).MergedCat.Table.Nobs);
                    fprintf('Noutlier      : %d\n',Obj(Iobj).MergedCat.Table.Noutlier);

                    fprintf('MAG mean      : %f\n', Obj(Iobj).Mag);

                    % LAST specific
                    fprintf('MAG range     : %f\n', Obj(Iobj).MergedCat.Table.Range_MAG_PSF);
                    fprintf('S/N mean      : %f\n', Obj(Iobj).MergedCat.Table.Mean_SN_3);
                
                    
                    FlagsName = Args.BitDic.bitdec2name(Obj(Iobj).MergedCat.Table.FLAGS);
                    FlagsName = FlagsName{1};
                    fprintf('FLAGS         :');
                    for Ifn=1:1:numel(FlagsName)
                        fprintf('  %s', FlagsName{Ifn});
                    end
                    fprintf('\n');
                    FlagsName = Args.CatBitDic.bitdec2name(Obj(Iobj).MergedCat.Table.MergedCatMask);
                    FlagsName = FlagsName{1};
                    fprintf('MergedCat     :');
                    for Ifn=1:1:numel(FlagsName)
                        fprintf('  %s', FlagsName{Ifn});
                    end
                    fprintf('\n');
                    fprintf('PolyDeltaChi2 : %f\n', Obj(Iobj).MergedCat.Table.PolyDeltaChi2);
                    
                    % KnownAst
                    if ~isempty(Obj(Iobj).KnownAst)
                        if isemptyCatalog(Obj(Iobj).KnownAst)
                            fprintf('Known Asteroid : None\n')
                        else
                            [~,Imin] = min(Obj(Iobj).KnownAst.Table.Dist);
                            fprintf('Known Asteroid : %s / Mag=%5.2f\n',Obj(Iobj).KnownAst.Table.Desig{Imin}, Obj(Iobj).KnownAst.Table.Mag(Imin));
                        end
                        
                    end
                end
            end

        end
    
        function blink1(Obj, Args)
            % Display stamps along with sources, asteroid, and known minor planets positions and blink
            %   the images.
            %   
            % Input  : - A single element MovingSource object.
            %          * ...,key,val,...
            %            'Ndisp' - Number if stamps to dispaly.
            %                   For example, if 2, then will display the
            %                   first and last stamp in the vector of
            %                   Stamps.
            %                   Default is 2.
            %            'Zoom' - Zoom argument to pass to ds9.
            %                   Default is 'to fit'.
            %            'PlotSrc' - Plot arguments for the statis sources
            %                   detected in the Stamps.
            %                   If [], then do not plot static sources.
            %                   If cell, then will pass multiple arguments
            %                   contained in the cell.
            %                   Default is 'sg'.
            %            'PlotAst' - The same as 'PlotSrc', but for the
            %                   moving source (asteroid) detected in the
            %                   images.
            %                   Default is 'sr'.
            %            'PlotKnown' - The same as 'PlotSrc', but for the
            %                   Known Asteroid detected in the
            %                   images.
            %                   Default is 'or'.
            %            'PlotIndiv' - The same as 'PlotSrc', but for the
            %                   individual position detections of the
            %                   asteroid in each one of the stamps.
            %                   Default is {'sc','Size',8}.
            %            'MaxDistIndiv' - Plot individual image detection
            %                   within this search radius [arcsec] from the
            %                   moving source position.
            %                   Default is 3.
            %            'AutoBlink' - Start auto blink. Default is true.
            %            'DispInfo' - A logical indicating if to display
            %                   text information regarding the moving source
            %                   on the screen.
            %                   Default is true.
            %
            % Output : null
            % Author : Eran Ofek (Jan 2024)
            % Example: MP(1).blink1

            arguments
                Obj(1,1)
                Args.Ndisp             = 2; % number of stamps to display
                Args.Zoom              = 'to fit';
                Args.PlotSrc           = 'sg';
                Args.PlotAst           = 'sr';
                Args.PlotKnown         = 'or';
                Args.PlotIndiv         = {'sc','Size',8};
                Args.MaxDistIndiv      = 3; % arcsec
                Args.AutoBlink         = true;

                Args.DispInfo logical  = true;
            end
            ARCSEC_DEG = 3600;

            if ~iscell(Args.PlotSrc)
                Args.PlotSrc = {Args.PlotSrc};
            end
            if ~iscell(Args.PlotAst)
                Args.PlotAst = {Args.PlotAst};
            end
            if ~iscell(Args.PlotKnown)
                Args.PlotKnown = {Args.PlotKnown};
            end
            if ~iscell(Args.PlotIndiv)
                Args.PlotIndiv = {Args.PlotIndiv};
            end
            
            Nstamp = numel(Obj.Stamps);
            Nstep  = Nstamp./(Args.Ndisp-1) - 1;
            StampVec = (1:Nstep:Nstamp);
            NstV     = numel(StampVec);
            for IstV=1:1:NstV
                IndS = StampVec(IstV);

                if Args.DispInfo && IstV==1
                    % display information:
                    Obj.dispInfo;
                end

                %ds9.single;
                ds9(Obj.Stamps(IndS), IstV);
                ds9.zoom(Args.Zoom)
                if ~isempty(Args.PlotSrc)
                    ds9.plot(Obj.Stamps(IndS),Args.PlotSrc{:});
                end
                if ~isempty(Args.PlotAst)
                    ds9.plotc(Obj.MergedCat, Args.PlotAst{:});
                end
                if ~isempty(Args.PlotSrc)
                    if ~isempty(Obj.KnownAst)
                        if Obj.KnownAst.sizeCatalog>0
                            ds9.plotc(Obj.KnownAst, Args.PlotKnown{:});
                        end
                    end
                end
                if ~isempty(Args.PlotIndiv)
                    VecRA  = nan(Nstamp,1);
                    VecDec = nan(Nstamp,1);

                    for Istamp=1:1:Nstamp
                        DistDeg = Obj.Stamps(Istamp).CatData.sphere_dist(Obj.RA, Obj.Dec, Obj.CooUnits, 'deg');
                        [MinDist,Imin] = min(DistDeg);
                        if ~isempty(Imin) && (MinDist.*ARCSEC_DEG)<Args.MaxDistIndiv
                            VecRA(Istamp) = Obj.Stamps(Istamp).CatData.Table.RA(Imin);
                            VecDec(Istamp) = Obj.Stamps(Istamp).CatData.Table.Dec(Imin);
                        end
                    end
                    ds9.plotc(VecRA, VecDec, Args.PlotIndiv{:});
                     
                end

            end
            ds9.match_wcs;
            if Args.AutoBlink
                ds9.blink;
            end
            
        end

        function [ReportMPC,Result]=blink(Obj, Args)
            % Interactivly loop over all moving sources with blink1.
            %   Each element in the MovingSources object will be displayed
            %   in ds9 using the blink1 method.
            %   The user will be prompt to add comments/select/reject and
            %   prepare an MPC report.
            % Input  : - A MovingSources object.
            %          * ...,key,val,...
            %            'Ndisp' - Number if stamps to dispaly.
            %                   For example, if 2, then will display the
            %                   first and last stamp in the vector of
            %                   Stamps.
            %                   Default is 2.
            %            'Zoom' - Zoom argument to pass to ds9.
            %                   Default is 'to fit'.
            %            'PlotSrc' - Plot arguments for the statis sources
            %                   detected in the Stamps.
            %                   If [], then do not plot static sources.
            %                   If cell, then will pass multiple arguments
            %                   contained in the cell.
            %                   Default is 'sg'.
            %            'PlotAst' - The same as 'PlotSrc', but for the
            %                   moving source (asteroid) detected in the
            %                   images.
            %                   Default is 'sr'.
            %            'PlotKnown' - The same as 'PlotSrc', but for the
            %                   Known Asteroid detected in the
            %                   images.
            %                   Default is 'or'.
            %            'PlotIndiv' - The same as 'PlotSrc', but for the
            %                   individual position detections of the
            %                   asteroid in each one of the stamps.
            %                   Default is {'sc','Size',8}.
            %            'MaxDistIndiv' - Plot individual image detection
            %                   within this search radius [arcsec] from the
            %                   moving source position.
            %                   Default is 3.
            %            'AutoBlink' - Start auto blink. Default is true.
            %            'DispInfo' - A logical indicating if to display
            %                   text information regarding the moving source
            %                   on the screen.
            %                   Default is true.
            %
            %            'ReportMPC' - Char array of MPC report to which to
            %                   concat this report.
            %                   Default is ''.
            %            'AddHeader' - Default is true.
            %            'StartAstIndex' - AstIndex start with this number + 1.
            %                   Default is 0.
            %            'reportMPCArgs' - Cell array of additional
            %                   arguments to pass to MovingSources/reportMPC.
            %                   Default is {}.
            %
            % Output : - A char array of MPC report.
            %          - A structure array of results for each
            %            MovingSources element.
            %
            % Author : Eran Ofek (Jan 2024)
            % Example: MP.blink

            arguments
                Obj
                Args.Ndisp             = 2; % number of stamps to display
                Args.Zoom              = 'to fit';
                Args.PlotSrc           = 'sg';
                Args.PlotAst           = 'sr';
                Args.PlotKnown         = 'or';
                Args.PlotIndiv         = {'sc','Size',8};
                Args.MaxDistIndiv      = 3; % arcsec
                Args.AutoBlink         = true;
                Args.DispInfo logical  = true;

                Args.ReportMPC          = '';
                Args.AddHeader logical  = true;
                Args.StartAstIndex      = 0;
                Args.reportMPCArgs cell = {};
                

            end

            ReportMPC = Args.ReportMPC;
            AddHeader = Args.AddHeader;

            AstIndex  = Args.StartAstIndex;

            PromptChar = '......Options : \n   q - quit\n   r - add to report\n   Any other text to add to bad comment\n Your selection : ';

            Nobj = numel(Obj);
            for Iobj=1:1:Nobj
                fprintf('... MovingSource object element %d out of %d\n',Iobj, Nobj);
                % blink current element
                Obj(Iobj).blink1('Ndisp',Args.Ndisp,...
                           'Zoom',Args.Zoom,...
                           'PlotSrc',Args.PlotSrc,...
                           'PlotAst',Args.PlotAst,...
                           'PlotKnown',Args.PlotKnown,...
                           'PlotIndiv',Args.PlotIndiv,...
                           'MaxDistIndiv',Args.MaxDistIndiv,...
                           'AutoBlink',Args.AutoBlink,...
                           'DispInfo',Args.DispInfo);

                % Prompt to user
                fprintf('.....\n');
                Ans = input(PromptChar,'s');
                Result(Iobj).Comment = Ans;
                %if numel(Ans)==1
                    if numel(Ans)==1
                        switch lower(Ans)
                            case 'q'
                                Result(Iobj).Flag = NaN;
                                break;
                            case 'r'
                                % add to report
                                ReportMPC = Obj(Iobj).reportMPC('AddHeader',AddHeader, 'ReportMPC',ReportMPC,...
                                                                'StartAstIndex',AstIndex,...
                                                                Args.reportMPCArgs{:});
                                                                
                                AddHeader = false;
                                Result(Iobj).Flag = true;
                            otherwise
                                % skip
                                Result(Iobj).Flag = false;
                        end
                    else
                        % Skip
                        Result(Iobj).Flag = false;
                    end
                     
                %end
                ds9.single;
            end
            ds9.single;

        end
    end

    
    methods (Static) % Unit-Test
        Result = unitTest()
            % unitTest for BackImage class

    end
    
end

           
