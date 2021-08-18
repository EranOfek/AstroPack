

classdef AstroSpec < Component
    properties (Dependent)
        Wave
        Flux
        FluxErr
        Back
        Mask
        WaveUnits
        FluxUnits
    end
    
    properties
        Data table
        MaskData MaskImage
        Z                    = [];   % spectrum redshift-frame [0 - restframe]
        Vel                  = [];   % override Z
        DistZ                = [];   % redshift for ditsnace
        LumDist              = [];   % [pc]
        Ebv                  = [];   % Vector - [mag]
        Zext                 = 0;    % Vector - redshift of extinction
        R                    = 3.08; % Vector - 
    end
        
    
    properties (Hidden, SetAccess=private)
        %CooUnitsKnown(1,1) logical                                      = false;
    end
    
    properties (Hidden, Constant)
        DefColNameWave                   = 'Wave';
        DefColNameFlux                   = 'Flux';
        DefColNameFluxErr                = 'FluxErr';
        DefColNameBack                   = 'Back';
        DefColNameMask                   = 'Mask';
    end
  
    
    
    methods % constructor
        function Obj = AstroSpec(Matrix, Columns, Units)
            % constructor for AstroSpec
            % Input  : - An AstroSpec, Matrix, table, or cell array of
            %            matrix/tables.
            %          - A cell array of column names.
            %          - A cell array of unit names.
            % Output : - An AstroSpec object
            % Exanple: AS = AstroSpec(rand(100,2))
            
            arguments
                Matrix
                Columns cell         = {};
                Units cell           = {};
            end
            DefColNames = {'Wave', 'Flux', 'FluxErr', 'Back', 'Mask'};
            DefColUnits = {'Ang' , 'cgs/A', 'cgs/A','cgs/A',''};
            
           
            if isa(Matrix, 'AstroSpec')
                Obj = Matrix;
            else
                if ischar(Matrix)
                    % read from file
                    error('Read from file not yet available');
                    
                elseif istable(Matrix)
                    Obj.Data = Matrix;
                    if ~isempty(Columns)
                        Obj.Data.Properties.VariableNames = Columns;
                    end
                    if ~isempty(Units)
                        Obj.Data.Properties.VariableUnits = Units;
                    end
                elseif isnumeric(Matrix)
                    if numel(Matrix)==1
                        for I=1:1:Matrix
                            Obj(I) = AstroSpec(zeros(0,2), Columns, Units);
                        end
                    else
                        Ncol = size(Matrix,2);
                        if isempty(Columns)
                            % use default column names
                            Columns = DefColNames(1:Ncol);
                        end
                        if isempty(Units)
                            % use default column names
                            Units = DefColUnits(1:Ncol);
                        end

                        Obj.Data = array2table(Matrix, 'VariableNames', Columns);
                        Obj.Data.Properties.VariableUnits = Units;
                    end
                elseif  iscell(Matrix)
                    Ncell = numel(Matrix);
                    for Icell=1:1:Ncell
                        Obj(Icell) = AstroSpec(Matrix{Icell}, Columns, Units);
                    end
                else
                    error('Unknown input (Matrix) option');
                end
            end    
        end
        
    end
   
    methods % setters and getters
        function Result = get.Wave(Obj)
            % getter for Wave
            
            if any(strcmp(Obj.Data.Properties.VariableNames, Obj.DefColNameWave))
                Result = Obj.Data.(Obj.DefColNameWave);
            else
                % field not available
                Result = [];
            end
        end
        
        function set.Wave(Obj, Input)
            % setter for Wave
            
            Obj.Data.(Obj.DefColNameWave) = Input;
        end
        
        function Result = get.WaveUnits(Obj)
            % getter for WaveUnits
            
            Flag = ismember(Obj.Data.Properties.VariableNames, Obj.DefColNameWave); % index of wave columns
            if ~any(Flag)
                error('Wavelength column is not populated');
            end
            
            Result = Obj.Data.Properties.VariableUnits{Flag};
        end
        
        function set.WaveUnits(Obj, OutUnits)
            % setter for WaveUnits
            
            Flag = ismember(Obj.Data.Properties.VariableNames, Obj.DefColNameWave); % index of wave columns
            if ~any(Flag)
                error('Wavelength column is not populated');
            end
            CurUnits  = Obj.Data.Properties.VariableUnits{Flag};
            WaveData  = Obj.Data.(Obj.DefColNameWave);
            
            Obj.Data.(Obj.DefColNameWave) = convert.length(CurUnits, OutUnits, WaveData);
            Obj.Data.Properties.VariableUnits{Flag} = OutUnits;
            
        end
        
        function Result = get.Flux(Obj)
            % getter for Flux
            
            if any(strcmp(Obj.Data.Properties.VariableNames, Obj.DefColNameFlux))
                Result = Obj.Data.(Obj.DefColNameFlux);
            else
                % field not available
                Result = [];
            end
        end
        
        function set.Flux(Obj, Input)
            % setter for Flux
            
            Obj.Data.(Obj.DefColNameFlux) = Input;
        end
        
        function Result = get.FluxErr(Obj)
            % getter for FluxErr
            
            if any(strcmp(Obj.Data.Properties.VariableNames, Obj.DefColNameFluxErr))
                Result = Obj.Data.(Obj.DefColNameFluxErr);
            else
                % field not available
                Result = [];
            end
        end
        
        function set.FluxErr(Obj, Input)
            % setter for FluxErr
            
            Obj.Data.(Obj.DefColNameFluxErr) = Input;
        end
        
        function Result = get.Back(Obj)
            % getter for Back
            
            if any(strcmp(Obj.Data.Properties.VariableNames, Obj.DefColNameBack))
                Result = Obj.Data.(Obj.DefColNameBack);
            else
                % field not available
                Result = [];
            end
        end
        
        function set.Back(Obj, Input)
            % setter for Back
            
            Obj.Data.(Obj.DefColNameBack) = Input;
        end
        
        function Result = get.FluxUnits(Obj)
            % getter for FluxUnits
            
            Flag = ismember(Obj.Data.Properties.VariableNames, Obj.DefColNameFlux); % index of flux columns
            if ~any(Flag)
                error('Flux column is not populated');
            end
            
            Result = Obj.Data.Properties.VariableUnits{Flag};
        end
        
        function set.FluxUnits(Obj, OutUnits)
            % setter for FluxUnits
            
            Flag = ismember(Obj.Data.Properties.VariableNames, Obj.DefColNameFlux); % index of wave columns
            if ~any(Flag)
                error('Wavelength column is not populated');
            end
            CurUnits     = Obj.Data.Properties.VariableUnits{Flag};
            WaveData     = Obj.Data.(Obj.DefColNameWave);
            FluxData     = Obj.Data.(Obj.DefColNameFlux);
            FluxErrData  = Obj.Data.(Obj.DefColNameFlux);
            BackData     = Obj.Data.(Obj.DefColNameFlux);
            
            Obj.Data.(Obj.DefColNameFlux)    = convert.flux(FluxData,    CurUnits, OutUnits, WaveData, Obj.WaveUnits);
            Obj.Data.(Obj.DefColNameFluxErr) = convert.flux(FluxErrData, CurUnits, OutUnits, WaveData, Obj.WaveUnits);
            Obj.Data.(Obj.DefColNameBack)    = convert.flux(BackData,    CurUnits, OutUnits, WaveData, Obj.WaveUnits);
            
            Obj.Data.Properties.VariableUnits{Flag} = OutUnits;
            
        end
        
        function Result = get.Mask(Obj)
            % getter for Mask data
            
            Result = Obj.MaskData.Data;
        end
        
        function set.Mask(Obj, Input)
            % setter for Mask data
            
            Obj.MaskData.Data = Input;
        end
        
    end
    
    methods (Static)  % aux functions
        function [Factor] = applyExtinctionZ(ObsWave, Zext, EbvZ, RZ)
            % Calculate extinction to observed wavelngth vector in various redshifts.
            % Input  : - A vector of wavelength [Ang] in the observed
            %            frame.
            %          - A vector redshifts in which to apply extinction.
            %          - A vector of E_{B-V} to apply in each redshift.
            %            Use negative E_{B-V} to apply the inverse
            %            operator.
            %          - A vector of R_V to use in each redshift.
            %            Default is 3.08.
            % Output : - A vector of extinction factors, corresponding to
            %            the observed wavelength.
            % Author : Eran Ofek (Aug 2021)
            % Example: Factor = AstroSpec.applyExtinctionZ([5000 5010]', [0.5, 0], [0.2, 0.05], [2, 3.08])
            
            arguments
                ObsWave
                Zext
                EbvZ
                RZ                          = 3.08;
            end
            
            Zext = Zext(:);
            EbvZ = EbvZ(:);
            RZ   = RZ(:);
            Nz   = numel(Zext);
            EbvZ = EbvZ.*ones(Nz,1);
            RZ   = RZ.*ones(Nz,1);
            
            ObsWaveMicrons = ObsWave./1e4;  % Ang -> micrometer
            A_LambdaMag = zeros(numel(ObsWaveMicrons), Nz);
            for Iz=1:1:Nz
                ObsWaveMicronsZ = ObsWaveMicrons ./ (1 + Zext(Iz));
                A_LambdaMag(:,Iz)  = astro.spec.extinction(EbvZ(Iz), ObsWaveMicronsZ, [], RZ(Iz));
            end
            Factor = 10.^(-0.4.*sum(A_LambdaMag, 2));
        end
        
        function [FilterCell, Name] = read2FilterMatrix(Family, Name)
            % Convet a filter name/AstFilter to cell of transmissions
            % Input  : - A cell of family names, a family name, a matrix or
            %            an AstFilter object.
            %          - A filter name or a cell of names.
            % Output : - A cell array of filter transmissions.
            %          - A cell array of filter names.
            % Author : Eran Ofek (Aug 2021)
            % Example: [FilterCell, Name] = AstroSpec.read2FilterMatrix(Family, Name)
            
            if ischar(Name)
                Name = {Name};
            end
            
            if ischar(Family)
                Family = {Family};
            end
                
            if iscellstr(Family)
                Family = AstFilter.get(Family, Name);
            end
            
            if isa(Family, 'AstFilter')
                Nf = numel(Family);
                FilterCell = cell(1, Nf);
                for If=1:1:Nf
                    FilterCell{If} = Family(If).nT;
                end
            elseif isnumeric(Family)
                FilterCell{1} = Family;
            else
                error('Unknown Family option');
            end
                
        end
    end
    
    methods (Static) % special spectra
        function Result = blackBody(WaveData, Temp, Args)
            % Create an AstroSpec object with black body spectra
            %       Return specific (per Ang) luminosity or emittence.
            % Input  : - A vector of wavelengths, or an AstroSPec object
            %            from which the wavelength vector will be taken.
            %          - A vector of temperature.
            %          * ...,key,val,...
            %            'Radius' - Scalar or vector of BB radii for conversion
            %                   to luminosity. If empty,
            %                   then return the spectra in units of
            %                   emittemce from a cm^2/s/A on the BB.
            %            'RadiusUnits' - Radius units. Default is 'cm'.
            %            'Dist' - Scalar of vector of distances. If empty,
            %                   then return the spectra in units of
            %                   emittemce from a cm^2/s/A on the BB.
            %            'DistUnits' - Distance units. Default is 'pc'.
            %            'SpecType' - Spectrum type:
            %                    'P'  - planck formula, default.
            %                    'RJ' - Rayleigh-Jeans approximation
            %                    'W'  - Wein spectrum.
            % Output : - An AstroSPec array, with element per temperature.
            % Author : Eran Ofek (Aug 2021)
            % Example: Result = AstroSpec.blackBody((4000:10:9000)', [5000; 6000])
            
            arguments
                WaveData
                Temp                    = 5700;  % K
                Args.Radius             = [];
                Args.RadiusUnits        = 'cm';
                Args.Dist               = [];
                Args.DistUnits          = 'pc';
                Args.SpecType char      = 'P';   % 'P' | 'RJ' | 'W'
            end
            
            Nt     = numel(Temp);
            Nrad   = numel(Args.Radius);
            Ndist  = numel(Args.Dist);
            Result = AstroSpec(Nt);
            for It=1:1:Nt
                if isa(WaveData, 'AstroSpec')
                    Ias       = min(numel(WaveData), It);
                    Wave      = WaveData(Ias).Wave;
                    WaveUnits = Wavedata(Ias).WaveUnits;
                    WaveAng   = convert.length(WaveUnits, 'Ang', Wave);
                else
                    WaveAng   = WaveData;
                    WaveUnits = 'Ang';
                end
                [~,~,Flux] = astro.spec.black_body(Temp(It), WaveAng);
                if ~isempty(Args.Radius) && ~isempty(Args.Dist)
                    Ir     = min(Nt, Nrad);
                    Id     = min(Nt, Ndist);
                    Radius = convert.length(Args.RadiusUnits, 'cm', Args.Radius(Ir));
                    Dist   = convert.length(Args.DistUnits,   'cm', Args.Dist(Id));
                    
                    Flux   = Flux .* (Radius./Dist).^2;
                end
                
                Result(It) = AstroSpec({[WaveAng, Flux]}, {'Wave','Flux'}, {WaveUnits, 'cgs/A'});
            end
            
        end
            
    end
    
    methods (Static)  % read spectra from spectral libraries
        
    end
    
    methods % synthetic photometry
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
            %            'Algo' - Algorithm - see astro.spec.synphot
            %                   Default is 'cos'
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
            
            arguments
                Obj
                FilterFamily       % AstroFilter object, Name, Matrix
                FilterName         % numer in Result
                
                Args.MagSys   = 'AB';
                Args.Device   = 'photon';
                Args.Algo     = 'cos';
                Args.Ebv      = 0;
                Args.R        = 3.08;
            end
           
            [FilterCell, Name] = AstroSpec.read2FilterMatrix(FilterFamily, FilterName);
            
            Nname = numel(Name);
            Nobj  = numel(Obj);
            FilterWave = zeros(1, Nname);
            for Iobj=1:1:Nobj
                Spec = [Obj(Iobj).Wave(:), Obj(Iobj).Flux(:)];
                
                for Iname=1:1:Nname
                    [Mag, FiltFlag, FilterWave(Iname)] = astro.spec.synphot(Spec, FilterCell{Iname}, [], Args.MagSys, Args.Algo, Args.Ebv, Args.R, Args.Device);
                    Result(Iobj).(Name{Iname})     = Mag;
                    Flag(Iobj).(Name{Iname})       = FiltFlag;
                end
            end
        end
    end
    
    methods % filtering
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
    
    methods  % plots
        function H = plot(Obj, varargin)
            % Plot all the spectra in an AstroSpec object
            % Input  : - An AstroSpec object (multi elements supported)
            %          * Additional input arguments to pass to the plot
            %            command.
            % Output : - AN handle for the last plot.
            % Author : Eran Ofek (Aug 2021)
            % Example: Result = AstroSpec.blackBody((4000:10:9000)', [5000; 6000]);
            %          Result.plot  
           
            IsHold = ishold;
            
            Nobj = numel(Obj);
            for Iobj=1:1:Nobj
                H = plot(Obj(Iobj).Wave, Obj(Iobj).Flux, varargin{:});
                hold on;
            end
            
            H = xlabel(sprintf('Wavelength [%s]',Obj(1).WaveUnits));
            H.FontSize    = 18;
            H.Interpreter = 'latex';
            H = ylabel(sprintf('Flux [%s]',Obj(1).FluxUnits));
            H.FontSize    = 18;
            H.Interpreter = 'latex';
            
            if ~IsHold
                hold off;
            end
            
            
        end
        
    end
    
    methods  % redshift/extinction/luminosity
        function Result = redshift(Obj, Zout, Zin, Args)
            % Apply redshift to a spectrum
            %       Divide wave by (1+z) and multiply flux by (1+z)
            %       Correct also FluxErr and Back
            % Input  : - An AstroSpec object.
            %          - Output redshift. Use 0, in order to convert the
            %            spectrum to rest frame. Default is 0.
            %          - Input redshift. If empty, then will attempt to
            %            read it from the Z property. Default is [].
            %          * ...,key,val,...
            %            'CreateNewObj' - [], true, false.
            %                   If true, create new deep copy
            %                   If false, return pointer to object
            %                   If [] and Nargout==0 then do not create new copy.
            %                   Otherwise, create new copy. Default is [].
            %            'CorrectFluxErr' - Default is true.
            %            'CorrectBack'    - Default is true.
            % Output : - An updated AstroSpec object.
            %            If output is not requested, then by default will
            %            change the input object (see CreateNewObj arg).
            % Author : Eran Ofek (Aug 2021)
            % Example: S = AstroSpec({rand(100,3)});
            %          NS = AS.copyObject;
            %          NS.redshift=0.5;
            %          NS.Z=0.5;       
            %          NS.redshift(0)  

            arguments
                Obj
                Zout                             = 0;
                Zin                              = [];  % Z of spectra
                Args.CreateNewObj                = [];
                Args.CorrectFluxErr(1,1) logical = true;
                Args.CorrectBack(1,1) logical    = true;
            end
            
            [Result] = createNewObj(Obj, Args.CreateNewObj, nargout, 0);
        
            Nobj = numel(Obj);
            for Iobj=1:1:Nobj
                if ~isempty(Zin)
                    % reset Z property in object
                    Obj(Iobj).Z   = Zin;
                end
                
                if isempty(Obj(Iobj).Z)
                    error('Z must be specified either in object or as Zin input argument');
                end
                
                % apply redshift to wavelength
                % convert to rest frame
                Result(Iobj).Wave    = Result(Iobj).Wave .* (1 + Zout) ./ (1 + Result(Iobj).Z);
                Result(Iobj).Flux    = Result(Iobj).Flux .* (1 + Result(Iobj).Z) ./ (1 + Zout);
                if (Args.CorrectFluxErr)
                    Result(Iobj).FluxErr = Result(Iobj).FluxErr .* (1 + Result(Iobj).Z) ./ (1 + Zout);
                end
                if (Args.CorrectBack)
                    Result(Iobj).Back    = Result(Iobj).Back .* (1 + Result(Iobj).Z) ./ (1 + Zout);
                end
                
                Result(Iobj).Z       = [];     % needed [so Z setter will not apply the redshift]
                Result(Iobj).Z       = Zout;
            end
            
            
        end
        
        
    end
    
    
    
    
    methods (Static)  % unitTest
        function Result = unitTest
            % unitTest for AstroSpec
            
            % constructor
            AS = AstroSpec({rand(100,4)});
            
            % setters/getters
            NS = AS.copyObject;
            NS.WaveUnits = 'cm';
            if ~(strcmp(NS.WaveUnits,'cm') && all(abs(AS.Wave./NS.Wave./1e8 - 1)<10.*eps))
                error('Problem with WaveUnits conversion');
            end
            
            % convert flux units
            NS.FluxUnits = 'AB';
            
        end
    end
    
end