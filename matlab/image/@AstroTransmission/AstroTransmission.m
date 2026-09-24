% AstroTransmission
%
% #functions (autogen)
% AstroTransmission - constructor for AstroTransmission Populate an AstroTransmission object.
% atmosphericExtinction - Return Atmospheric extinction The extinction im mag/transmission is provided in the Flux field.
% filterFun - Apply a 1-D function (filter) on the Flux field in AstroSpec object. The function do not check if the wavelength is equally spaced. The function operates on the Flux, FluxErr, and Back fields.
% genNames - Generate names for filters
% genTopHat - Generate a series of top-hat filters (Type='filter').
% get -
% getFilt -
% interp1 - Interpolate the elements of AstroSpec into a new wavelength grid. The Mask will be interpolated using nearest interpolation. This function works only if the Data is convertable to a numeric matrix.
% interpAndKeepOverlap - Given two AstroSpec objects, interpolate the first into the wavelength grid defined by the second and keep only the overlaping points.
% interpLogSpace - Interpolate an AstroSpec object into a logarithmic wavelength grid.
% interpOverNan - Interpolate AstroSpec object over NaNs.
% interpOverRanges - Interpolate AstroSpec object over some ranges Useful for interpolation over ranges containing spectral lines.
% length - Return length of each spectrum in AstroSpec object
% plot - Plot all the filters in an AstroTransmission object
% readAstFilter - Read an (old) AstFilter into AstroTransmission
% scaleSynphot - Scale spectrum such that its synthetic magnitude will be forced to some value.
% selectWave - Select lines from AstroSpec (don't generate a new copy)
% set.WaveUnits - setter for WaveUnits - convert Wave property
% sort - Sort elements of AstroTransmission object by wavelength
% synphot - Synthetic photometry on an AstroSpec object spectra
% #/functions (autogen)
%
% use case thoughts:
% get(Telescope, Camera, Family, Filter, <type>, <AM>)
% getTel(Telescope) - telescope only
% getFilt(Family, Filter) - in this case 'type'=filter
% getCam(Camera) - type='qe
% getCoating(Coating)
% getAM(AM, File) - Type=am

% Celestron BrightStar-XLT schmidt coatings:
% 400 0.7
% 450 0.85
% 500 0.88
% 520 0.89
% 550 0.88
% 600 0.87
% 650 0.85
% 700 0.80
% 750 0.75
% 800 0.7  (extrapolated)
% 850 0.65
% 900 0.6
% 1000 0.5
%

classdef AstroTransmission < Component
    properties
        Family      = '';
        Band        = '';
        
        Type            % 'filter' | 'telescope' | 'cam' | 'coating' | 'am' | 'ism' | 'sys'=filter+telescope+qe | 'int'=filter+telescope+qe+am
        Telescope   = '';    % '' - only filter
        Camera      = '';    % '' - only filter
        Extinction  = 'VLT';
        AM          = 1;
        Ebv         = 0;
        Rv          = 3.08;
        
        Wave
        Tran
        WaveUnits   = 'A';
        TranUnits   = 'none';   % 'none' | 'trans' | 'intNorm' | 'peakNorm' | 'cm^2'
        
        ZP          = NaN;  % for 'sys'|'int
        ZPsys       = '';   % 'Vega'|'AB'
        
    end
    
    
    methods % constructor
        function Obj = AstroTransmission(Matrix, Args)
            % constructor for AstroTransmission
            %   Populate an AstroTransmission object.
            % Input  : - A scalar indicating the number of empty
            %            AstroTransmission objects, or a two column matrix
            %            of [Wave, Transmission].
            %            Default is 1.
            %          * ...,key,val,...
            %            'Family' - Filter family
            %            'Band' - Filter name.
            %            'Type' - 'filter' | 'telescope' | 'qe' | 'am' | 'ism' | 'sys'=filter+telescope+qe | 'int'=filter+telescope+qe+am
            %                   Default is 'filter'.
            %            'Telescope' - Default is ''.
            %            'Camera' -Default is ''.
            %            'Extinction' - Default is ''.
            %            'AM' - Default is 1.
            %            'Ebv' - Default i 0.
            %            'Rv' - Default is 3.08.
            %            'WaveUnits' - Default is 'A'.
            %            'TranUnits' - options: 'none' | 'trans' | 'intNorm' | 'peakNorm' | 'cm^2'
            %                   Default is 'none'.
            %            'ZP' - ZP for system (for 'sys'|'int)
            %            'ZPsys' - Default is 'AB'.
            % Output : - An AstroTransmission object.
            % Author : Eran Ofek (Sep 2021)
            % Exanple: AT = AstroTransmission(rand(100,2),'Band','a','Family','a')
            
            arguments
                Matrix             = 1; % num. of elements or   [Wave(Ang), Trans]
                Args.Family
                Args.Band
                Args.Type          = 'filter';
                Args.Telescope     = '';
                Args.Camera        = '';
                Args.Extinction    = '';
                Args.AM            = 1;
                Args.Ebv           = 0;
                Args.Rv            = 3.08;
                Args.WaveUnits     = 'A';
                Args.TranUnits     = 'none';
                Args.ZP            = 'NaN';
                Args.ZPsys         = 'AB';
            end
            
            if numel(Matrix)==1 && ~iscell(Matrix)
                % construct empty AstroTransmission
                for I=1:1:Matrix
                    Obj(I).Family = '';
                end
            else
                if iscell(Matrix)
                    Matrix = Matrix{1};
                end
                Obj.Wave = Matrix(:,1);
                Obj.Tran = Matrix(:,2);
                Obj = tools.struct.copyProp(Args, Obj);
            end
            
        end
        
    end
   
    methods % setters and getters
        function Obj = set.WaveUnits(Obj, Units)
            % setter for WaveUnits - convert Wave property
            
            Obj.Wave      = convert.length(Obj.WaveUnits, Units, Obj.Wave);
            Obj.WaveUnits = Units;
        end
        
    end
   
    methods (Static) % get/create transmissions
        function Result = getFilt(Family, Band)
            %
            
        end
        
        
        function Result = genTopHat(Ranges, Family, BandPrefix, Args)
            % Generate a series of top-hat filters (Type='filter').
            % Input  : - A two column matrix of [Min, Max] wavelength
            %            range. Each line per each fiter.
            %          - Filter family name. Default is 'TopHat'.
            %          - Filter band name prefix. Default is 'Band'.
            %          * ...,key,val,...
            %            'WaveUnits' - Default is 'A'.
            % Output : - An AstroTransmission object of the top-hat
            %            transmission curves
            % Author : Eran Ofek (Sep 2021)
            % Example: Result = AstroTransmission.genTopHat([4000 4100; 5000 6000]);
            
            arguments
                Ranges
                Family          = 'TopHat'
                BandPrefix      = 'Band';
                Args.WaveUnits  = 'A';
            end
            
            Nr = size(Ranges,1);
            for Ir=1:1:Nr
                Min = min(Ranges(Ir,:));
                Max = max(Ranges(Ir,:));
                
                Wave = [Min*0.999, Min, Max, Max*1.001];
                Tran = [0,         1,   1,   0];
                
                Result(Ir) = AstroTransmission([Wave(:), Tran(:)],...
                                            'WaveUnits',Args.WaveUnits,...
                                            'Family',Family,...
                                            'Band',sprintf('%s%d',BandPrefix,Ir),...
                                            'Type','filter');
                
%                 Result(Ir).Wave = Wave(:);
%                 Result(Ir).Tran = Tran(:);
%                 Result(Ir).WaveUnits = Args.WaveUnits;
%                 Result(Ir).Family    = Family;
%                 Result(Ir).Band      = sprintf('%s%d',BandPrefix,Ir);
%                 Result(Ir).Type      = 'filter';
            end
            
        end
        
        
        function Result = genHadamard(Wave, Order, Family, BandPrefix, Args)
            % Generate a series of Hadamard filters (Type='filter').
            % Input  : - A vector of wavelength grid.
            %            Default is (4000:1:9000);
            %          - Hadamard matrix order. Default is 8.
            %            Must be powers of 2.
            %          - Filter family name. Default is 'Hadamard'.
            %          - Filter band name prefix. Default is 'Band'.
            %          * ...,key,val,...
            %            'WaveUnits' - Default is 'A'.
            % Output : - An AstroTransmission object of the top-hat
            %            transmission curves
            % Author : Eran Ofek (Sep 2021)
            % Example: Result = AstroTransmission.genHadamard;
            
            arguments
                Wave            = (4000:1:9000);
                Order           = 8;
                Family          = 'Hadamard'
                BandPrefix      = 'Band';
                Args.WaveUnits  = 'A';
            end
            
            Wmin  = min(Wave);
            Wmax  = max(Wave);
            Range = Wmax - Wmin;
            Nwave = numel(Wave);
            
            HadMat = hadamard(Order);
            HadRes = imresize(HadMat, [Order Nwave], 'nearest');
            
            Flag   = HadRes<0;
            HadRes(Flag) = 0;
            HadRes = [zeros(Order,1), HadRes, zeros(Order,1)];
            
            Wave   = [Wmin-1, Wave(:).', Wmax+1];
            
            for I=1:1:Order
                Tran = HadRes(I,:);
                Result(I) = AstroTransmission([Wave(:), Tran(:)],...
                                            'WaveUnits',Args.WaveUnits,...
                                            'Family',Family,...
                                            'Band',sprintf('%s%d',BandPrefix,I),...
                                            'Type','filter');
            end
        end
        
        
        function Result = get(Family, Band, Type, Args)
            %
            
           arguments
                Family
                Band
                Type              = 'filter';
                Args.Telescope    = [];  % [] - get all
                Args.Camera       = [];  % [] - get all
                Args.Extinction   = [];  % [] - get all
                Args.AM           = [];  % [] - get original
            end
    
            
            
        end
        
        function Result = readAstFilter(varargin)
            % Read an (old) AstFilter into AstroTransmission
            % Input  : * Any arguments og AstFilter.get
            % Output : - An AstroTransmission
            % Author : Eran Ofek (Sep 2021)
            % Example: AT=AstroTransmission.readAstFilter;
            %          AT=AstroTransmission.readAstFilter('SDSS')
            %          AT=AstroTransmission.readAstFilter('SDSS','g')
            
            AF = AstFilter.get(varargin{:});
            N  = numel(AF);
            % Result = AstroTransmission(N); % BUG
            for I=1:1:N
                Result(I) = AstroTransmission(1);
                Result(I).Family     = AF(I).family;
                Result(I).Band       = AF(I).band;
                Result(I).Wave       = AF(I).nT(:,1);
                Result(I).Tran       = AF(I).nT(:,2);
                Result(I).WaveUnits  = 'A';
                Result(I).TranUnits  = 'none';
                
                Result(I).Type       = 'filter';
                Result(I).Telescope  = '';
                Result(I).Camera     = '';
                Result(I).Extinction = '';
                Result(I).AM         = 0;
            end
        end
    end
    
    methods (Static)  % aux functions
        function [Name] = genNames(Prefix, Num)
            % Generate names for filters
            % Input  : - Filter name prefix. Default is 'Filt'/
            %          - Number of names. Default is 2.
            % Output : - A cell array of filter names.
            % Author : Eran Ofek (Sep 2021)
            % Example: [Name] = AstroTransmission.genNames
           
            arguments
                Prefix = 'Filt';
                Num    = 2;
            end
            
            Name = cell(1, Num);
            for I=1:1:Num
                Name{I} = sprintf('%s%d',Prefix,I);
            end
            
        end
    end
    
 
    
    methods (Static)  % read spectra from spectral libraries

        function Trans = atmosphericExtinction(File, Args)
            % Return Atmospheric extinction
            %   The extinction im mag/transmission is provided in the Flux
            %   field.
            % Input  : - A file name from which to read transmission.
            %            If empty, will return list of available files (in
            %            dir-like output).
            %            Options include: 'VLT' | 'KPNO' | 'SNfactory'
            %            Default is 'VLT'.
            %          * ...,key,val,...
            %            'AM' - AirMass in which to return extinction.
            %                   If a vector then AstroSpec output is an
            %                   array.
            %                   Default is 1.
            %            'Wave' - Wavelength grid [Ang] in which to return the
            %                   output. If empty, will use default.
            %                   Default is [].
            %            'InterpMethod' - Interpolation method.
            %                   Default is 'linear'.
            %            'OutType' - ['trans'] | 'mat'.
            %            'OutUnits' -  'mag' | ['trans']
            %                   'trans' is transmission.
            % Output : - Extinction as a function of wavelength.
            %            [Wave(Ang), Extinction(mag/trans)].
            %            If AM is a vector and OutType='mat', will return
            %            only the last requested AM.
            % Author : Eran Ofek (Sep 2021)
            % Example: Trans = AstroTransmission.atmosphericExtinction([])
            %          Trans = AstroTransmission.atmosphericExtinction
            %          Trans = AstroTransmission.atmosphericExtinction('VLT','AM',2,'OutUnits','trans')
            %          Trans = AstroTransmission.atmosphericExtinction('VLT','AM',2,'OutUnits','trans','OutType','mat')
            %          Trans = AstroTransmission.atmosphericExtinction('VLT','AM',[2 3]);
           
            arguments
                File                  = 'VLT';    % [] - show all; 'KPNO' | 'SNfactory' | 'VLT'
                Args.AM               = 1;
                Args.Wave             = [];       % [] - use original wave grid
                Args.InterpMethod     = 'linear';
                Args.OutType          = 'tran';  % 'tran' | 'mat'
                Args.OutUnits         = 'trans';    % 'mag' | 'trans'
            end
            DataName = 'Atmosphere';
            
            Ins = Installer;
            if isempty(File)
                Trans = Ins.getFilesInDataDir(DataName);
            else
                [Files, Dir] = Ins.getFilesInDataDir(DataName);
                PWD = pwd;
                cd(Dir);
                
                Ind   = contains({Files.name}, File);
                Data = io.files.load2(Files(Ind).name);  % [Wave(Ang), Extinc(Mag)]
                
                if ~isempty(Args.Wave)
                    % interpolate transission into a new grid
                    Data = [Args.Wave(:), interp1(Data(:,1), Data(:,2), Args.Wave(:), Args.InterpMethod)];
                end
                
                % apply AirMass
                DataN = Data;
                
                Nam = numel(Args.AM);
                for Iam=1:1:Nam
                
                    if Args.AM(Iam)~=1
                        DataN(:,2) = DataN(:,2).*Args.AM(Iam);
                    end

                    switch lower(Args.OutUnits)
                        case 'mag'
                            % do nothing
                        case 'trans'
                            DataN(:,2) = 10.^(-0.4.*DataN(:,2));
                        otherwise
                            error('Unknown OutUnits option');
                    end

                    switch lower(Args.OutType)
                        case 'mat'
                            Trans = DataN;
                        case 'tran'
                            Trans(Iam) = AstroTransmission(DataN, 'Family','atmosphere',...
                                                                  'Band',File,...
                                                                  'Type','am',...
                                                                  'Camera','',...
                                                                  'Extinction',File,...
                                                                  'AM',Args.AM(Iam),...
                                                                  'WaveUnits','A',...
                                                                  'TranUnits',Args.OutUnits);
                                                                  
                        otherwise
                            error('Unknown OutType option');
                    end
                end
            end
        end
    end
    
    methods  % resampling, sort, interpolation
        
        % REQUIRE EDITING
        
        function Result = sort(Obj, Args)
            % Sort elements of AstroTransmission object by wavelength
            % Input  : - An AstroSpec object.
            %          * ...,key,val,...
            %            'CreateNewObj' - [], true, false.
            %                   If true, create new deep copy
            %                   If false, return pointer to object
            %                   If [] and Nargout==0 then do not create new copy.
            %                   Otherwise, create new copy. Default is [].
            % Output : - An AstroSpec object with the elements sorted by
            %            wavelength.
            % Author : Eran Ofek (Aug 2021)
            % Example: S = AstroSpec({rand(100,3)});
            %          S.sort;
            
            arguments
                Obj
                Args.CreateNewObj           = [];
            end
                
            [Result] = createNewObj(Obj, Args.CreateNewObj, nargout, 0);
            
            Nobj = numel(Obj);
            for Iobj=1:1:Nobj
                [Result(Iobj).Data, Isort] = sortrows(Obj(Iobj).Data, 'Wave');
                
                if ~isempty(Obj(Iobj).MaskData.Data)
                    Result(Iobj).MaskData.Data = Obj(Iobj).MaskData.Data(Isort);
                end
            end
        end
                
        function Obj = selectWave(Obj, Flag)
            % Select lines from AstroSpec (don't generate a new copy)
            % Input  : - An AstroSpec object
            %          - A vector of logical flags or indices of lines to
            %            select.
            % Output : - The original AstroSpec with the removed lines.
            % Author : Eran Ofek (Aug 2021)
            % Example: AS = AstroSpec({rand(100,3)});
            %          AS = selectWave(AS, [1 2 3]);
            
            Nobj = numel(Obj);
            for Iobj=1:1:Nobj
                Obj(Iobj).Data = Obj(Iobj).Data(Flag,:);
            end
            
        end
        
        function Result = interp1(Obj, NewWave, Args)
            % Interpolate the elements of AstroTransmission into a new wavelength grid.
            %       The Mask will be interpolated using nearest
            %       interpolation. This function works only if the Data is
            %       convertable to a numeric matrix.
            % Input  : - An AstroTransmission object.
            %          - A vector of new wavelngth grid on which to
            %            interpolate the AstroSpec object.
            %            Alternatively, a AstroSpec object with wavelength.
            %            If a scalar, this is the wavelength step, while
            %            the min and max wavelength will be taken from the
            %            first element in the AstroTransmission object.
            %          * ...,key,val,...
            %            'Method' - Interpolation method. See interp1 for
            %                   options. Default is 'linear'.
            %            'ExtraArgs' - A cell array of additional arguments
            %                   to pass to interp1. Default is {}.
            %            'RemoveNan' - A logical indicating of to remove
            %                   NaNs. Default is false.
            %            'CreateNewObj' - [], true, false.
            %                   If true, create new deep copy
            %                   If false, return pointer to object
            %                   If [] and Nargout==0 then do not create new copy.
            %                   Otherwise, create new copy. Default is [].
            % Output : - An AstroSpec object with the interpolated spectra.
            % Author : Eran Ofek (Aug 2021)
            % Example: S = AstroTransmission({rand(100,3)});
            %          S.sort;
            %          S.interp1([0:0.1:1]);
            
            arguments
                Obj
                NewWave
                Args.InterpMethod           = 'linear';
                Args.ExtraArgs cell         = {};
                Args.RemoveNan(1,1) logical = false;
                Args.CreateNewObj           = [];
            end
          
            if isa(NewWave, 'AstroTransmission')
                NewWave = NewWave.Wave;
            end
            NewWave = NewWave(:);
            
            if numel(NewWave)==1
                % generate an evenly spaced grid based on first element in
                % Obj
                Step = NewWave;
                
                Min = min(Obj(1).Wave);
                Max = max(Obj(1).Wave);
                
                NewWave = ((Min-Step):Step:(Max+Step)).';
            end
            
            [Result] = createNewObj(Obj, Args.CreateNewObj, nargout, 0);
            
            Nobj = numel(Obj);
            
            for Iobj=1:1:Nobj
                Result(Iobj).Tran = interp1(Obj(Iobj).Wave, Obj(Iobj).Tran, NewWave, Args.InterpMethod, 'extrap');
                % make sure no value below zero
                Flag0 = Result(Iobj).Tran<0;
                Result(Iobj).Tran(Flag0) = 0;
                Result(Iobj).Wave = NewWave;
            end
                
        end
        
        function [New1, New2] = interpAndKeepOverlap(Obj1, Obj2, Args)
            % Given two AstroSpec objects, interpolate the first into the
            % wavelength grid defined by the second and keep only the
            % overlaping points.
            % Input  : - The first AstroSpec object (multi elements
            %            supported).
            %          - The second AstroSpec object (multi elements
            %            supported).
            %            'Method' - Interpolation method. See interp1 for
            %                   options. Default is 'linear'.
            %            'ExtraArgs' - A cell array of additional arguments
            %                   to pass to interp1. Default is {}.
            %            'CreateNewObj' - [], true, false.
            %                   If true, create new deep copy
            %                   If false, return pointer to object
            %                   If [] and Nargout==0 then do not create new copy.
            %                   Otherwise, create new copy. Default is [].
            % Output : - An AstroSpec object with the interpolated spectra.
            % Author : Eran Ofek (Aug 2021)
            % Example: S1 = AstroSpec({rand(100,3)});
            %          S1.sort
            %          S2 = AstroSpec({rand(100,3).*0.5});
            %          S2.sort
            %          [New1, New2] = interpAndKeepOverlap(S1, S2);
           
            arguments
                Obj1
                Obj2
                Args.Method           = 'linear';
                Args.ExtraArgs cell   = {};
                Args.CreateNewObj     = [];
            end
            
            [New1] = createNewObj(Obj1, Args.CreateNewObj, nargout, 0);
            [New2] = createNewObj(Obj2, Args.CreateNewObj, nargout, 1);

            N1 = numel(New1);
            N2 = numel(New2);
            N  = max(N1, N2);
            for I=1:1:N
                I1 = min(I, N1);
                I2 = min(I, N2);
                
                New1(I1) = interp1(New1(I1), New2(I2).Wave, 'Method',Args.Method, 'ExtraArgs',Args.ExtraArgs, 'CreateNewObj',false);
                % remove NaNs
                FlagNN   = ~isnan(New1(I1).Flux) | isnan(New2(I2).Flux);
                New1(I1) = selectWave(New1(I1), FlagNN);
                New2(I2) = selectWave(New2(I2), FlagNN);
            end
            
        end
            
        function Result = interpOverRanges(Obj, Ranges, Args)
            % Interpolate AstroSpec object over some ranges
            %       Useful for interpolation over ranges containing
            %       spectral lines.
            % Input  : - An AstroSpec object.
            %          - A two column matrix of [Min Max] ranges.
            %            Each line is a wavelength range, and data points
            %            within (<>) this range will be interpolated.
            %          * ...,key,val,...
            %            'Method' - Interpolation method. See interp1 for
            %                   options. Default is 'linear'.
            %            'ExtraArgs' - A cell array of additional arguments
            %                   to pass to interp1. Default is {}.
            %            'CreateNewObj' - [], true, false.
            %                   If true, create new deep copy
            %                   If false, return pointer to object
            %                   If [] and Nargout==0 then do not create new copy.
            %                   Otherwise, create new copy. Default is [].
            % Output : - An AstroSpec object with the interpolated spectra.
            % Author : Eran Ofek (Aug 2021)
            % Example: S = AstroSpec({rand(100,3)});
            %          S.sort;
            %          Ranges = [0.2 0.5; 0.6 0.9];
            %          R = S.interpOverRanges(Ranges);
           
            arguments
                Obj
                Ranges(:,2)
                Args.Method char            = 'linear';
                Args.ExtraArgs cell         = {};
                Args.CreateNewObj           = [];
            end
            
            [Result] = createNewObj(Obj, Args.CreateNewObj, nargout, 0);
            
            Nranges = size(Ranges, 1);
                
            Nobj = numel(Obj);
            for Iobj=1:1:Nobj
                Wave = Obj(Iobj).Wave;
                for Iranges=1:1:Nranges
                    Flag = Wave > Ranges(Iranges,1) & Wave < Ranges(Iranges,2);
                    Result(Iobj).Data = Result(Iobj).Data(~Flag,:);
                    if ~isempty(Result(Iobj).MaskData.Data)
                        Result(Iobj).MaskData.Data = Result(Iobj).MaskData.Data(~Flag);
                    end
                    Result(Iobj).interp1(Wave, 'CreateNewObj',false, 'Method',Args.Method, 'ExtraArgs',Args.ExtraArgs);
                end
            end
        end
        
        function Result = interpOverNan(Obj, Args)
            % Interpolate AstroSpec object over NaNs.
            % Input  : - An AstroSpec object.
            %          * ...,key,val,...
            %            'Method' - Interpolation method. See interp1 for
            %                   options. Default is 'linear'.
            %            'ExtraArgs' - A cell array of additional arguments
            %                   to pass to interp1. Default is {}.
            %            'DataProp' - Data property in which to look for
            %                   NaNs. Default is 'Flux'.
            %            'CreateNewObj' - [], true, false.
            %                   If true, create new deep copy
            %                   If false, return pointer to object
            %                   If [] and Nargout==0 then do not create new copy.
            %                   Otherwise, create new copy. Default is [].
            % Output : - An AstroSpec object with the interpolated spectra.
            %            (NaNs are interpolated over).
            % Author : Eran Ofek (Aug 2021)
            % Example: S = AstroSpec({rand(100,3)});
            %          S.sort;
            %          S.Flux(2:5) = NaN;
            %          R = S.interpOverNan;
           
            arguments
                Obj
                Args.Method char            = 'linear';
                Args.ExtraArgs cell         = {};
                Args.DataProp               = 'Flux';
                Args.CreateNewObj           = [];
            end
            
            [Result] = createNewObj(Obj, Args.CreateNewObj, nargout, 0);
                            
            Nobj = numel(Obj);
            for Iobj=1:1:Nobj
                Wave = Obj(Iobj).Wave;
                
                Flag = isnan(Obj(Iobj).(Args.DataProp));
                Result(Iobj).Data = Result(Iobj).Data(~Flag,:);
                if ~isempty(Result(Iobj).MaskData.Data)
                    Result(Iobj).MaskData.Data = Result(Iobj).MaskData.Data(~Flag);
                end
                Result(Iobj).interp1(Wave, 'CreateNewObj',false, 'Method',Args.Method, 'ExtraArgs',Args.ExtraArgs);
            end
        end
        
        function Result = interpLogSpace(Obj, Args)
            % Interpolate an AstroSpec object into a logarithmic wavelength grid.
            % Input  : - An AstroSpec object.
            %          * ...,key,val,...
            %            'Res' - Resolution (Dlambda/lambda) to use fot the
            %                   log-spacing. If empty, estimate using
            %                   wave-diff. Default is [].
            %            'Method' - Interpolation method. See interp1 for
            %                   options. Default is 'linear'.
            %            'ExtraArgs' - A cell array of additional arguments
            %                   to pass to interp1. Default is {}.
            %            'CreateNewObj' - [], true, false.
            %                   If true, create new deep copy
            %                   If false, return pointer to object
            %                   If [] and Nargout==0 then do not create new copy.
            %                   Otherwise, create new copy. Default is [].
            % Output : - An AstroSpec object with logarithmic spaced
            %            wavelength grid.
            % Output : - An AstroSpec object with the interpolated spectra.
            % Author : Eran Ofek (Aug 2021)
            % Example: Spec = AstroSpec.synspecGAIA('Temp',[5750],'Grav',[4.5]);
            %          Result = interpLogSpace(Spec);
            
            arguments
                Obj
                Args.Res               = [];
                Args.Method            = 'linear';
                Args.ExtraArgs cell    = {};
                Args.CreateNewObj      = [];
            end
            
            [Result] = createNewObj(Obj, Args.CreateNewObj, nargout, 0);
            
            Nobj = numel(Obj);
            for Iobj=1:1:Nobj
                if isempty(Args.Res)
                    % estimate resolution
                    Res = min(Obj(Iobj).Wave(1:end-1)./diff(Obj(Iobj).Wave));
                    Res = Res .* log10(max(Obj(Iobj).Wave));
                else
                    Res = Args.Res .* log10(max(Obj(Iobj).Wave));
                end
                
                NewWave = logspace(log10(min(Obj(Iobj).Wave)), log10(max(Obj(Iobj).Wave)), ceil(Res));
                
                Result(Iobj) = interp1(Result(Iobj), NewWave, 'Method',Args.Method, 'ExtraArgs',Args.ExtraArgs{:}, 'RemoveNan',true, 'CreateNewObj',false);
            end
            
        end
        
        function Result = length(Obj)
            % Return length of each spectrum in AstroSpec object
            % Input  : - An AstroSpec object.
            % Output : - A vector of lengths of each spectrum.
            % Author : Eran Ofek (Aug 2021)
            % Example: AS = AstroSpec({rand(100,3)});
            %          AS.length
           
            Nobj   = numel(Obj);
            Result = nan(size(Obj));
            for Iobj=1:1:Nobj
                Result(Iobj) = numel(Obj(Iobj).Wave);
            end
        end
        
        function Result = trapz(Obj)
            % Trapezoidal integration of the filters
            % Input  : - AstroTransmission object.
            % Output : - Array of integrals.
            % Author : Eran Ofek (Mar 2024)
           
            Nobj = numel(Obj);
            Result = zeros(size(Obj));
            for Iobj=1:1:Nobj
                Result(Iobj) = trapz(Obj(Iobj).Wave, Obj(Iobj).Tran);
            end
            
        end
        
        function [Result, Wave] = convert2matrix(Obj, NewWave)
            % Generate a matrix of response functions from a set of AstroTransmission filters
            %   Given an array of AstroTransmission, each containing a
            %   filter transmission, enerate a matrix of size (Ntran,
            %   Nwave), of all the transmissions.
            %   Here Ntran is the number of elements in the input object,
            %   and Nwave is the number of wavelength.
            % Input  : - An array of AstroTransmission objects.
            %          - A vector the new wavelngth grid on which to
            %            generate the matrix. If empty, then use default
            %            values (but in this case they are assumed to have
            %            identrical wavelenth grid).
            %            Default is [].
            % Output : - A matrix of transmissions. Line per transmission.
            %          - A vector of common wavelength for all
            %            transmissions.
            % Author : Eran Ofek (Sep 2022)
            % Example: F2=AstroTransmission.genHadamard([4000:1:9000],48);
            %          F2=F2.interp1([3999:1:9001]);
            %          [Result, Wave] = convert2matrix(F2);
            
            arguments
                Obj
                NewWave   = [];
            end
            
            if ~isempty(NewWave)
                % interp1
                
                ObjC = interp1(Obj, NewWave, 'CreateNewObj',true);
            else
                ObjC = Obj;
            end
            
            Nwave = length(ObjC);
            if ~all(Nwave==Nwave(1))
                error('Number of wavelength in each spectra must be identical, provide the NewWave argument');
            end
            
            Nobj   = numel(ObjC);
            Result = zeros(Nobj, Nwave(1));
            for Iobj=1:1:Nobj
                Result(Iobj,:) = ObjC(Iobj).Tran;
            end
            Wave = ObjC(Iobj).Wave;
            
        end
    end
    
    methods % Flux operators
        
        

    end
    
    
    methods % synthetic photometry
        
        % REQUIRE EDITING
        
        function [Result, Flag, FilterWave] = synphot(Obj, FilterFamily, FilterName, Args)
            % Synthetic photometry on an AstroSpec object spectra
            % Input  : - An AstroSpec object (multi elements supported)
            %          - A cell of filter family names, an AstFilter
            %            object, or a matrix of transmissions.
            %          - A cell array of filter names. The output structure
            %            will have a field name for each one of these names.
            %          * ...,key,val,...
            %            'MagSys' - Mag system: ['AB'] | 'Vega'
            %            'Device' - Device ['photon'] | 'bol'
            %            'SpecFluxUnits' - Default is 'cgs/A'
            %            'SpecWaveUnits' - Default is 'A'
            %            'InterpMethod' - Default is 'linear'.
            %            'IsOutMat' - A logical indicating if the output is
            %                   structure array (true) or matrix (false) of
            %                   [Spec, Band].
            %                   Default is false.
            %               Not supported yet
            %            'Ebv' - E_{B-V} [mag] extinction to apply to
            %                   spectra. Default is 0.
            %            'R' - R_V to use for extinction. Default is 3.08.
            % Output : - A structure array of syntheic magnitudes.
            %            Element per object element, and ach filter is
            %            stored in a field with its name.
            %          - Like Mag results, but for the fraction of
            %            extrapolated part of the filter.
            %            0 means no extrapolation.
            %          - A vector of the filter central wavelengths.
            % Author : Eran Ofek (Aug 2021)
            % Example: AS = AstroSpec.blackBody((4000:10:9000)', [5000; 6000]);
            %          [Result, Flag, FilterWave] = synphot(AS, {'SDSS','SDSS'}, {'g','r'})
            %          Spec = AstroSpec.synspecGAIA('Temp',[5750 5500 5550],'Grav',[4.5]);
            %          Spec = AstroSpec.synspecGAIA('Temp',[5750],'Grav',[4.0, 4.5]);
            %          T=[5000 0; 5001 1; 5999 1; 6000 0];  % filter transmission
            %          Spec.interpOverNan;
            %          [Result, Flag, FilterWave] = synphot(Spec, T, 'F55');

            arguments
                Obj
                FilterFamily       % AstroFilter object, Name, Matrix
                FilterName         % numer in Result
                
                Args.MagSys   = 'AB';
                Args.Device   = 'photon';   % 'bol' | 'photon'
                Args.SpecFluxUnits        = 'cgs/A';
                Args.SpecWaveUnits        = 'A';
                Args.InterpMethod         = 'linear';
                %Args.Algo     = 'cos';
                %Args.Ebv      = 0;
                %Args.R        = 3.08;
                Args.IsOutMat logical  = false;
            end
           
            [FilterCell, Name] = AstroSpec.read2FilterMatrix(FilterFamily, FilterName);
            
            Nname = numel(Name);
            Nobj  = numel(Obj);
            FilterWave = zeros(1, Nname);
            for Iobj=1:1:Nobj
                Spec = [Obj(Iobj).Wave(:), Obj(Iobj).Flux(:)];
                
                for Iname=1:1:Nname
                    [Mag, FiltFlag, FilterWave(Iname)] = astro.spec.synthetic_phot(Spec, FilterCell{Iname}, [], Args.MagSys,...
                                                                                'Device',Args.Device,...
                                                                                'SpecFluxUnits',Args.SpecFluxUnits,...
                                                                                'SpecWaveUnits',Args.SpecWaveUnits,...
                                                                                'InterpMethod',Args.InterpMethod);

                    
                    Result(Iobj).(Name{Iname})     = Mag;
                    Flag(Iobj).(Name{Iname})       = FiltFlag;
                end
            end
            if Args.IsOutMat
                % convert to matrix
                Out = nan(Nobj, Nname);
                for Iname=1:1:Nname
                    Out(:,Iname) = [Result.(Name{Iname})].';
                end
                Result = Out;
            end
        end

        function Result = scaleSynphot(Obj, Mag, FilterFamily, FilterName, Args)
            % Scale spectrum such that its synthetic magnitude will be forced to some value.
            % Input  : - An AstroSpec object (multi elements supported)
            %          - Magnitude - Each spectrum will be scaled such that
            %            its synthetic mag will be equal to this mag.
            %            This can be a scalar (for all spectra), or vector
            %            of elements per spectra.
            %          - A cell of filter family names, an AstFilter
            %            object, or a matrix of transmissions.
            %          - A cell array of filter names. The output structure
            %            will have a field name for each one of these names.
            %          * ...,key,val,...
            %            'MagSys' - Mag system: ['AB'] | 'Vega'
            %            'Device' - Device ['photon'] | 'bol'
            %            'Algo' - Algorithm - see astro.spec.synphot
            %                   Default is 'cos'
            % Author : Eran Ofek (Sep 2021)
            % Example: AS = AstroSpec.blackBody((4000:10:9000)', [5000; 6000]);
            %          Mag  = synphot(AS,'SDSS','r')
            %          R  = scaleSynphot(AS, 20, 'SDSS','r')
            %          Mag  = synphot(R,'SDSS','r')
            arguments
                Obj
                Mag
                FilterFamily
                FilterName
                Args.MagSys   = 'AB';
                Args.Device   = 'photon';
                Args.Algo     = 'cos';
                Args.CreateNewObj = [];
            end

            MagSyn = synphot(Obj, FilterFamily, FilterName, 'MagSys',Args.MagSys, 'Device',Args.Device, 'Algo',Args.Algo);
            MagSynVec = [MagSyn.(FilterName)];
            Factor = 10.^(-0.4.*(Mag - MagSynVec));
            Result = scaleFlux(Obj, Factor, 'CreateNewObj',Args.CreateNewObj);
        end
    end
    
    methods % filtering
        
        % REQUIRE EDITING
        
        function Result = filterFun(Obj, Function, varargin)
            % Apply a 1-D function (filter) on the Flux field in AstroSpec
            % object. The function do not check if the wavelength is
            % equally spaced.
            % The function operates on the Flux, FluxErr, and Back fields.
            % Input  : - An AstroSpec object.
            %          - A function handle. E.g., @medfilt1, @hampel,
            %            @sgolayfilt,...
            %          * Additional arguments to pass to the specific
            %            function.
            % Output : - If no output argument is requested, then will modify the input
            %            AstroSpec object. Otherwise, will create a new
            %            copy of the object.
            %            The oupt contains the filtered spectra.
            % Author : Eran Ofek (Aug 2021)
            % Example: AS=AstroSpec.blackBody((4000:10:9000)', [5000; 6000]);
            %          AS.filterFun(@medfilt1,10)
            
            [Result] = createNewObj(Obj, [], nargout, 0);
            
            Nobj = numel(Obj);
            for Iobj=1:1:Nobj
                Result(Iobj).Flux = Function(Obj(Iobj).Flux, varargin{:});
            end
        end
        
    end
    
    methods  % fitting

    end
    
    methods  % plots
        function H = plot(Obj, varargin)
            % Plot all the filters in an AstroTransmission object
            % Input  : - An AstroTransmission object (multi elements supported)
            %          * Additional input arguments to pass to the plot
            %            command.
            % Output : - An handle for the last plot.
            % Author : Eran Ofek (Sep 2021)
            % Example: AT=AstroTransmission.readAstFilter('SDSS')
            %          AT.plot
           
            IsHold = ishold;
            
            Nobj = numel(Obj);
            for Iobj=1:1:Nobj
                H = plot(Obj(Iobj).Wave, Obj(Iobj).Tran, varargin{:});
                hold on;
            end
            
            H = xlabel(sprintf('Wavelength [%s]',Obj(1).WaveUnits));
            H.FontSize    = 18;
            H.Interpreter = 'latex';
            H = ylabel(sprintf('Tran [%s]',Obj(1).TranUnits));
            H.FontSize    = 18;
            H.Interpreter = 'latex';
            
            if ~IsHold
                hold off;
            end
            
            
        end
        
    end
    
    
    methods (Static)  % unitTest
        Result = unitTest
            % unitTest for AstroSpec

    end
    
end
