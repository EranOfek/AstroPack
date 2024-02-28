% AstroSpec
%
% #functions (autogen)
% AstroSpec - constructor for AstroSpec
% applyAtmosphericExt - Apply atmospheric extinction (airmass) to AstroSpec object. One to one, or one to many.
% applyExtinctionZ - Calculate extinction to observed wavelngth vector in various redshifts.
% atmosphericExtinction - Return Atmospheric extinction The extinction im mag/transmission is provided in the Flux field.
% blackBody - Create an AstroSpec object with black body spectra Return specific (per Ang) luminosity or emittence.
% filterFun - Apply a 1-D function (filter) on the Flux field in AstroSpec object. The function do not check if the wavelength is equally spaced. The function operates on the Flux, FluxErr, and Back fields.
% fitMag - Fit AstroSpec spectra to observed magnitudes in some bands.
% fitSpec - UNDER CONSTRUCTION
% funBinary - Perform a binary operation on two AstroSpec objects The operation is applied one to one or one to many.
% funFlux - Apply a function to the Flux, FluxErr, Back columns.
% get.Back - getter for Back
% get.Cont - getter for Cont
% get.Flux - getter for Flux
% get.FluxErr - getter for FluxErr
% get.FluxUnits - getter for FluxUnits
% get.Mask - getter for Mask data
% get.Wave - getter for Wave
% get.WaveUnits - getter for WaveUnits
% interp1 - Interpolate the elements of AstroSpec into a new wavelength grid. The Mask will be interpolated using nearest interpolation. This function works only if the Data is convertable to a numeric matrix.
% interpAndKeepOverlap - Given two AstroSpec objects, interpolate the first into the wavelength grid defined by the second and keep only the overlaping points.
% interpLogSpace - Interpolate an AstroSpec object into a logarithmic wavelength grid.
% interpOverNan - Interpolate AstroSpec object over NaNs.
% interpOverRanges - Interpolate AstroSpec object over some ranges Useful for interpolation over ranges containing spectral lines.
% length - Return length of each spectrum in AstroSpec object
% plot - Plot all the spectra in an AstroSpec object
% read2FilterMatrix - Convet a filter name/AstFilter to cell of transmissions
% redshift - Apply redshift to a spectrum Divide wave by (1+z) and multiply flux by (1+z) Correct also FluxErr and Back
% scaleFlux - Scale (multiply) flux properties by some factor.
% scaleSynphot - Scale spectrum such that its synthetic magnitude will be forced to some value.
% selectWave - Select lines from AstroSpec (don't generate a new copy)
% set.Back - setter for Back
% set.Cont - setter for Cont
% set.Flux - setter for Flux
% set.FluxErr - setter for FluxErr
% set.FluxUnits - setter for FluxUnits
% set.Mask - setter for Mask data
% set.Wave - setter for Wave
% set.WaveUnits - setter for WaveUnits
% sort - Sort elements of AstroSpec object by wavelength
% specCALSPEC - Get STSCI CALSPEC template spectrum from ../spec/CALSPEC/ data directory.
% specGalQSO - Get galaxy/qso template spectrum from ../spec/SpecGalQSO/ data directory. Description: Get Galaxy or QSO spectral template from local database.
% specStarsPickles - Load Pickles stellar spectra into an AstroSpec object
% synphot - Synthetic photometry on an AstroSpec object spectra
% synspecGAIA - Load GAIA synthetic spectra and if necessey interpolate over Temperature (other parameters are not interpolated).
% #/functions (autogen)
%

classdef AstroSpec < Component
    properties (Dependent)
        Wave
        Flux
        FluxErr
        Back
        Cont
        Mask
        WaveUnits
        FluxUnits
    end
    
    properties
        Data table                % table 
        MaskData MaskImage   = MaskImage;
        Z                    = [];   % spectrum redshift-frame [0 - restframe]
        Vel                  = [];   % override Z
        DistZ                = [];   % redshift for ditsnace
        LumDist              = [];   % [pc]
        Ebv                  = [];   % Vector - [mag]
        Zext                 = 0;    % Vector - redshift of extinction
        R                    = 3.08; % Vector -
        Lines                % optional lines list
        Ref
    end
        
    
    properties (Hidden, SetAccess=private)
        %CooUnitsKnown(1,1) logical                                      = false;
    end
    
    properties (Hidden, Constant)
        DefColNameWave                   = 'Wave';
        DefColNameFlux                   = 'Flux';
        DefColNameFluxErr                = 'FluxErr';
        DefColNameBack                   = 'Back';
        DefColNameCont                   = 'Cont';
        DefColNameMask                   = 'Mask';
    end
  
    
    
    methods % constructor
        function Obj = AstroSpec(Matrix, Columns, Units)
            % constructor for AstroSpec
            % Input  : - An AstroSpec, Matrix, table, or cell array of
            %            matrix/tables. Default is 1.
            %          - A cell array of column names.
            %          - A cell array of unit names.
            % Output : - An AstroSpec object
            % Exanple: AS = AstroSpec(rand(100,2))
            
            arguments
                Matrix               = 1;
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
            
            if isempty(Obj.Data)
                Obj.Data = array2table(Input, 'VariableNames',{Obj.DefColNameWave});
            end
            Obj.Data.(Obj.DefColNameWave) = Input;
        end
        
        function Result = get.WaveUnits(Obj)
            % getter for WaveUnits
            
            Flag = ismember(Obj.Data.Properties.VariableNames, Obj.DefColNameWave); % index of wave columns
            if ~any(Flag)
                fprintf('Wavelength column is not populated because column names in table are not valid')
            end
            
            Result = Obj.Data.Properties.VariableUnits{Flag};
        end
        
        function set.WaveUnits(Obj, OutUnits)
            % setter for WaveUnits
            
            Flag = ismember(Obj.Data.Properties.VariableNames, Obj.DefColNameWave); % index of wave columns
            if ~any(Flag)
                error('Wavelength column is not populated');
            end
            if isempty(Obj.Data.Properties.VariableUnits)
                Obj.Data.Properties.VariableUnits{(Flag)} = OutUnits;
            else
                CurUnits  = Obj.Data.Properties.VariableUnits{Flag};
                WaveData  = Obj.Data.(Obj.DefColNameWave);

                Obj.Data.(Obj.DefColNameWave) = convert.length(CurUnits, OutUnits, WaveData);
                Obj.Data.Properties.VariableUnits{Flag} = OutUnits;
            end
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
            
            if isempty(Obj.Data)
                Obj.Data = array2table(Input, 'VariableNames',{Obj.DefColNameFlux});
            end
            Obj.Data.(Obj.DefColNameFlux) = Input;
        end
        
        function Result = get.Cont(Obj)
            % getter for Cont
            
            if any(strcmp(Obj.Data.Properties.VariableNames, Obj.DefColNameCont))
                Result = Obj.Data.(Obj.DefColNameCont);
            else
                % field not available
                Result = [];
            end
        end
        
        function set.Cont(Obj, Input)
            % setter for Cont
            
            Obj.Data.(Obj.DefColNameCont) = Input;
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
            
            if ~any(Flag) || isempty(Obj.Data.Properties.VariableUnits{(Flag)})
                Obj.Data.Properties.VariableUnits{(Flag)} = OutUnits;
            else
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
                A_LambdaMag(:,Iz)  = astro.extinction.extinction(EbvZ(Iz), ObsWaveMicronsZ, [], RZ(Iz));
            end
            Factor = 10.^(-0.4.*sum(A_LambdaMag, 2));
        end
        
        function [FilterCell, Name] = read2FilterMatrix(Family, Name, ClassDefault)
            % Convet a filter name/AstFilter to cell of transmissions
            % Input  : - A cell of family names, a family name, a matrix or
            %            an AstFilter object or AstroTransmission object.
            %          - A filter name or a cell of names.
            %          - Class from which to read filter:
            %            ['AstFilter'] | 'AstroTransmission'
            % Output : - A cell array of filter transmissions.
            %          - A cell array of filter names.
            % Author : Eran Ofek (Aug 2021)
            % Example: [FilterCell, Name] = AstroSpec.read2FilterMatrix('SDSS', 'g')
            
            arguments
                Family
                Name
                ClassDefault     = 'AstFilter';
            end
            
            if ischar(Name)
                Name = {Name};
            end
            
            if ischar(Family)
                Family = {Family};
            end
                
            if iscellstr(Family)
                switch lower(ClassDefault)
                    case 'astfilter'
                        Family = AstFilter.get(Family, Name);
                    case 'astrotransmission'
                        Family = AstFilter.getFilt(Family, Name);
                        if isempty(Name)
                            % populate Name from Family
                            Name = {Family.Band};
                        end
                    otherwise
                        error('Unknown class option');
                end
            end
            
            if isa(Family, 'AstFilter')
                Nf = numel(Family);
                FilterCell = cell(1, Nf);
                for If=1:1:Nf
                    FilterCell{If} = Family(If).nT;
                end
            elseif isa(Family, 'AstroTransmission')
                Nf = numel(Family);
                FilterCell = cell(1, Nf);
                PopName = false;
                if isempty(Name)
                    Name = cell(1, Nf);
                    PopName = true;
                end
                for If=1:1:Nf
                    FilterCell{If} = [convert.length(Family(If).WaveUnits, 'A', Family(If).Wave), Family(If).Tran];
                    if PopName
                        Name{If}       = Family(If).Band;
                    end
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
                    WaveAng   = reshape(WaveData,[],1);
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
        function Spec = synspecGAIA(Args)
            % Load GAIA synthetic spectra and if necessey interpolate over
            % Temperature (other parameters are not interpolated).
            % Input  : * ...,key,val,...
            %            'Temp' - Vector of fffective temperature of spectra [K], in
            %                   range of 3,500 to 400,000 K.
            %                   Default is 5750.
            %            'GraV' - Vector of surface gravity (log10(cgs)]
            %                   Default is 4.5
            %            'Metal' - Vector of metalicity.
            %                   Must be in (-2.5:0.5:0.5).
            %                   Default is 0.
            %            'Rot' - Rotation [km/s]. Default is 0.
            %            'OutType' - ['astrospec'] | 'astspec' | 'mat'.
            % Outout : - An AstroSpec object with specta.
            % Author : Eran Ofek (Aug 2021)
            % Example: Spec = AstroSpec.synspecGAIA
            %          Spec = AstroSpec.synspecGAIA('Temp',[5750 5500 5550],'Grav',[4.5]);
            %          Spec = AstroSpec.synspecGAIA('Temp',[3500:1000:1e4],'Grav',[4.5]);
            
            arguments
                Args.Temp           = 5750;
                Args.Grav           = 4.5
                Args.Metal          = 0;
                Args.Rot            = 0;
                Args.OutType        = 'astrospec';   % 'astrospec' | 'astspec' | 'mat'
                Args.VecTemp        = [(3500:250:10000), (10500:500:12000), (13000:1000:35000), 40000, 45000, (1e5:1e5:4e5)];
                Args.VecMetal       = (-2.5:0.5:0.5);
            end
            
            
            Nt   = numel(Args.Temp);
            Ng   = numel(Args.Grav);
            Nm   = numel(Args.Metal);
            Nr   = numel(Args.Rot);
            Nmax = max([Nt,Ng,Nm,Nr]);
            Temp = Args.Temp(:).*ones(Nmax,1);
            Grav = Args.Grav(:).*ones(Nmax,1);
            Metal= Args.Metal(:).*ones(Nmax,1);
            Rot  = Args.Rot(:).*ones(Nmax,1);
            
            WaveFileName = 'GAIA_Wave1A.mat';
            
            PWD = pwd;
            % FFU: This dir is in config/Install.Data.Dirs.yml
            cd('~/matlab/data/spec/GAIA_SpecTemplate');
            
            FileSuffix = 'K2SNWNVD01F.mat';
            W          = io.files.load2(sprintf('%s',WaveFileName));
            
            
            for Is=1:1:Nmax
                if (Metal(Is)<0)
                    MetalSign = 'M';
                else
                    MetalSign = 'P';
                end

                if any(Temp(Is)==Args.VecTemp)
                    % read a single file
                    SpecName = sprintf('T%05dG%02d%s%02dV%03d%s',...
                        round(Temp(Is)),round(Grav(Is).*10),MetalSign,round(abs(Metal(Is)).*10),round(Rot(Is)),FileSuffix);

                    SpecMat  = [W, io.files.load2(SpecName)];
                else
                    % requires interpolation in T and M
                    DeltaT = Temp(Is) - Args.VecTemp;
                    DeltaTpos = DeltaT(DeltaT>0);
                    DeltaTneg = DeltaT(DeltaT<0);
                    
                    T1 = Temp(Is) - min(DeltaTpos);
                    T2 = Temp(Is) - max(DeltaTneg);
                    
                    SpecName1 = sprintf('T%05dG%02d%s%02dV%03d%s',...
                        T1,round(Grav(Is).*10),MetalSign,round(abs(Metal(Is)).*10),round(Rot(Is)),FileSuffix);
                    SpecName2 = sprintf('T%05dG%02d%s%02dV%03d%s',...
                        T2,round(Grav(Is).*10),MetalSign,round(abs(Metal(Is)).*10),round(Rot(Is)),FileSuffix);

                    SpecMat1  = [io.files.load2(SpecName1)];
                    SpecMat2  = [io.files.load2(SpecName2)];
                    
                    % weights
                    T2w = abs(T1 - Temp(Is))./abs(T2-T1);
                    T1w = abs(T2 - Temp(Is))./abs(T2-T1);
                    
                    SpecMat   = [W, T1w.*SpecMat1 + T2w.*SpecMat2];
                    
                end

                switch lower(Args.OutType)
                    case 'astrospec'
                        Spec(Is) = AstroSpec(SpecMat);
                        Spec(Is).Z   = 0;
                        Spec(Is).Vel = 0;
                    case 'astspec'
                        Spec(Is)=AstSpec.mat2spec(SpecMat,{'Wave','Int'},{'Ang','erg*cm^-2 *s^-1*Ang^-1'});
                        Spec(Is).z = 0;
                        Spec(Is).source = 'GAIA local DB';
                        Spec(Is).ObjName = sprintf('GAIA synspec T=%f, g=%f, M=%f, R=%f',Temp(Is),Grav(Is),Metal(Is),Rot(Is));
                    case 'mat'
                        % do nothing
                        Spec = SpecMat;
                    otherwise
                        error('Unknown OutType option');
                end
            end
            cd(PWD);
            
        end

        function Result = specGalQSO(Name, OutType)
            % Get galaxy/qso template spectrum from ../spec/SpecGalQSO/ data directory.
            % Description: Get Galaxy or QSO spectral template from local
            %              database.
            % Input  : - If empty, will return all available file names.
            %            If a single file name, then load it.
            %            If 'all', then return all spectra.
            %          - Output type: 'astrospec'|'mat'. Default is
            %            'AstroSpec'.
            % Output : - An AstroSoec object or a matrix with the requested
            %            spectrum. If the first argument is empty then this
            %            is a cell array of available file names.
            % Author : Eran Ofek (Sep 2021)
            % Example: AstroSpec.specGalQSO
            %          A=AstroSpec.specGalQSO('QSO_NIR');
            %          A=AstroSpec.specGalQSO('all');
            % Reliable: 2
            
            arguments
                Name      = [];
                OutType   = 'AstroSpec';
            end
            DataName = 'SpecGalQSO';
            Suffix   = '.txt';
            I = Installer;
            
            
            if isempty(Name)
                % return all available file names
                Files = I.getFilesInDataDir(DataName);
                Result = {Files.name};
            else
                switch lower(Name)
                    case 'all'
                        % load all spectra
                        Files = I.getFilesInDataDir(DataName);
                        Name = {Files.name};
                end
                if ischar(Name)
                    Name = {Name};
                end

                Nd = numel(Name);
                
                Dir = I.getDataDir(DataName);
                Iast = 0;
                for Id=1:1:Nd
                    if strcmp(Name{Id}, 'ReadMe')
                        % ignore
                    else
                        Iast = Iast + 1;
                        FullName = sprintf('%s%s%s', Dir, filesep, Name{Id});
                        if strcmp(FullName(end-3:end),Suffix)
                            % file name already contains suffix
                        else
                            % add suffix
                            FullName = sprintf('%s%s',FullName,Suffix);
                        end
                        Spec = io.files.load2(FullName);

                        switch lower(OutType)
                            case 'mat'
                                Result = Spec;
                            case 'astrospec'
                                Result(Iast)   = AstroSpec({Spec});
                                Result(Iast).Z = 0;
                            otherwise
                                error('Unknown OutType option');
                        end
                    end
                end
            end

        end
        
        function [Result, FilesList] = specStarsPickles(SpType, LumClass, OutType)
            % Load Pickles stellar spectra into an AstroSpec object
            % Input  : - Spectral type - e.g., 'G', 'G2',...
            %            or file name.
            %            If empty, then return a cell array of all
            %            available spectra names. Default is [].
            %          - Luminosity class. e.g., 'V'. If empty, return all.
            %            Default is ''.
            %          - Output type: 'mat' | ['AstroSpec'].
            % Output : - An AstroSpec object containing the requested
            %            spectra.
            %          - A cell array of all file names in the Pickles data
            %            directory.
            % Author : Eran Ofek (Sep 2021)
            % Example: [~,List] = AstroSpec.specStarsPickles
            %          Result = AstroSpec.specStarsPickles('wg8iii.mat')
            %          Result = AstroSpec.specStarsPickles('G')
            %          Result = AstroSpec.specStarsPickles('G','V')
            %          Result = AstroSpec.specStarsPickles('G2')
            %          Result = AstroSpec.specStarsPickles('G2','V')

            arguments
                SpType    = [];            % [] - return list
                LumClass  = '';            % luminosity class.
                OutType   = 'AstroSpec';   % 'AstroSpec' | 'mat'
            end

            DataName = 'PicklesStellarSpec';

            I = Installer;
            [Files, Dir] = I.getFilesInDataDir(DataName);
            FilesList = {Files.name};
            if isempty(SpType)
                % get list of all spectra
                Result = [];
            else
                if ~contains(SpType,'.mat')
                    % not a single file
                    % Spectral type
                    if isempty(LumClass)
                        LumClass = '[iv]+';
                    else
                        LumClass = lower(LumClass);
                    end

                    if numel(SpType)==1
                        Template = sprintf('uk%s\\d+%s.mat',lower(SpType), LumClass);
                    else
                        Template = sprintf('uk%s%s.mat',lower(SpType), LumClass);
                    end
                    RE = regexp(FilesList, Template, 'match');
                    Files = FilesList(~cellfun(@isempty, RE));

                else
                    % load a single file
                    Files = {SpType};
                end
                % load all files

                Nf = numel(Files);
                if Nf == 0
                    error('Requested stellar class not found in the library');
                end
                for If=1:1:Nf
                    Mat        = io.files.load2(Files{If});
                    switch lower(OutType)
                        case 'astrospec'
                            Result(If) = AstroSpec({Mat});
                        case 'mat'
                            Result = Mat;
                        otherwise
                            error('Unknwon OutType option');
                    end
                end
            end


        end
        
        function Result = specPhoenix(Args)
            % Get a Phoenix model stellar spectrum from a prepared grid
            % Reference: 
            % Input: -
            %       * ...,key,val,... 
            % Output: - An AstroSpec object
            % Author : A.M. Krassilchtchikov (Oct 2023)
            % Example: Sp1=AstroSpec.specPhoenix('T',5770,'logg',4.44,'Res','high');
            %          Sp2=AstroSpec.specPhoenix('T',2240,'logg',3.5);
            arguments
                Args.Res = 'low';
                Args.T   = 5770;
                Args.logg = 4.44;
            end
            % load the data cube
            DataName = 'PhoenixStellarSpec';
            I = Installer;
            DataDir = I.getDataDir(DataName);
            if strcmpi(Args.Res,'low') 
                File = strcat(DataDir,'/phoenix_mtl0_rescale10.mat');
            else
                File = strcat(DataDir,'/phoenix_mtl0_rescale2.mat');
            end
            io.files.load1(File); % 'PhoenixWaveGrid','PhoenixSpec','PhoenixTGrid','PhoenixLoggGrid'
            Flux = interpn(PhoenixWaveGrid, PhoenixTGrid, PhoenixLoggGrid, PhoenixSpec, PhoenixWaveGrid, Args.T, Args.logg);
            Result = AstroSpec({[PhoenixWaveGrid, Flux]},{'Wave','Flux'},{'A','cgs/A'});
            
        end
        
        function Result = specHSTStarlib23(Name, OutType)
            % Get Starlib spectra from ../spec/Starlib23/ data directory
            % Reference: http://astro.wsu.edu/hststarlib/, https://arxiv.org/abs/2301.05335
            % Input  : - If empty, will return all available file names
            %            If a single object name, then load it
            % Output : - An AstroSpec object
            % Author : A.M. Krassilchtchikov (Oct 2023)
            % Example: A=AstroSpec.specHSTStarlib23('HD216640');
            %          AstroSpec.specHSTStarlib23; % get a list of all the
            %          available spectra from this collection
            arguments
                Name    = [];
                OutType   = 'AstroSpec';
            end
            Skip = {'names of auxiliary files in the same dir should be put here'};
            DataName = 'Starlib23';
            Suffix   = '.fits';
            I = Installer;
            
            if isempty(Name)
                % return all available file names
                Files = I.getFilesInDataDir(DataName);
                Result = {Files.name};
            else
                if ischar(Name)
                    Name = {Name};
                end
                Nd = numel(Name);
                
                Dir = I.getDataDir(DataName);
                Iast = 0;
                for Id=1:1:Nd
                    if strcmp(Name{Id}, 'ReadMe')
                        % ignore
                    else
                        if ~contains(Name{Id},Skip)
                            Iast = Iast + 1;
                            FullName = sprintf('%s%s%s', Dir, filesep, Name{Id});
                            if strcmp(FullName(end-4:end),Suffix)
                                % file name already contains suffix
                            else
                                % add suffix
                                FullName = sprintf('%s%s',FullName,Suffix);
                            end
%                             FullName
                            TT  = fitsread(FullName,'binarytable');
                            switch lower(OutType)
                                case 'table'
                                    Result = table(TT{1},TT{6},TT{7},'VariableNames', {'Wave, A', 'Flux, cgs/A', 'Flux error, cgs/A'});
%                                 case 'mat'
%                                     Result = [TT{1}, TT{6}];
                                case 'astrospec'
                                    Result(Iast) = AstroSpec({[TT{1}, TT{6}, TT{7}]},{'Wave','Flux','FluxErr'},{'A','cgs/A','cgs/A'});
                                otherwise
                                    error('Unknown OutType option');
                            end
                        end
                    end
                end
            end
        end
        
        function [Result, Files] = specCALSPEC(Name, OutType)
            % Get STSCI CALSPEC template spectrum from ../spec/CALSPEC/ data directory.
            % Input  : - If empty, will return all available file names.
            %            If a single file name, then load it.
            %            If 'all', then return all spectra.
            %          - Output type: 'astrospec'|'mat'|'table'. Default is
            %            'AstroSpec'.
            %            Only the 'table' option returns all the available
            %            columns. Others resturn [Wave, Flux] only.
            % Output : - An AstroSoec object or a matrix with the requested
            %            spectrum. If the first argument is empty then this
            %            is a cell array of available file names.
            %          - List of all available files in the CALSPEC
            %            directory.
            % Author : Eran Ofek (Oct 2021)
            % Example: AstroSpec.specCALSPEC
            %          A=AstroSpec.specCALSPEC('QSO_NIR');
            %          A=AstroSpec.specCALSPEC('all');
            % Reliable: 2
            
            arguments
                Name      = [];
                OutType   = 'AstroSpec';
            end
            Skip = {'WDcovar_002.fits','index.html'};
            DataName = 'CALSPEC';
            Suffix   = ''; %.fits';
            I = Installer;
            
            
            if isempty(Name)
                % return all available file names
                Files = I.getFilesInDataDir(DataName);
                Result = {Files.name};
            else
                switch lower(Name)
                    case 'all'
                        % load all spectra
                        Files = I.getFilesInDataDir(DataName);
                        Name = {Files.name};
                end
                if ischar(Name)
                    Name = {Name};
                end

                Nd = numel(Name);
                
                Dir = I.getDataDir(DataName);
                Iast = 0;
                for Id=1:1:Nd
                    if strcmp(Name{Id}, 'ReadMe')
                        % ignore
                    else
                        if ~contains(Name{Id},Skip)
                            Iast = Iast + 1;
                            FullName = sprintf('%s%s%s', Dir, filesep, Name{Id});
                            if strcmp(FullName(end-3:end),Suffix)
                                % file name already contains suffix
                            else
                                % add suffix
                                FullName = sprintf('%s%s',FullName,Suffix);
                            end
                            FullName
                            TT = FITS.readTable1(FullName);
                            switch lower(OutType)
                                case 'table'
                                    Result = TT;
                                case 'mat'
                                    Result = [TT.WAVELENGTH, TT.FLUX];
                                case 'astrospec'
                                    Result(Iast) = AstroSpec({[TT.WAVELENGTH, TT.FLUX]},{'Wave','Flux'},{'A','cgs/A'});
                                otherwise
                                    error('Unknown OutType option');
                            end
                        end
                    end
                end
            end
            
        end

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
            %            'OutType' - ['AstroSpec'] | 'mat'.
            %            'OutUnits' -  ['mag'] | 'trans'
            %                   'trans' is transmission.
            % Output : - Extinction as a function of wavelength.
            %            [Wave(Ang), Extinction(mag/trans)].
            %            If AM is a vector and OutType='mat', will return
            %            only the last requested AM.
            % Author : Eran Ofek (Sep 2021)
            % Example: Trans = AstroSpec.atmosphericExtinction([])
            %          Trans = AstroSpec.atmosphericExtinction
            %          Trans = AstroSpec.atmosphericExtinction('VLT','AM',2,'OutUnits','trans')
            %          Trans = AstroSpec.atmosphericExtinction('VLT','AM',2,'OutUnits','trans','OutType','mat')
            %          Trans = AstroSpec.atmosphericExtinction('VLT','AM',[2 3]);
           
            arguments
                File                  = 'VLT';    % [] - show all; 'KPNO' | 'SNfactory' | 'VLT'
                Args.AM               = 1;
                Args.Wave             = [];       % [] - use original wave grid
                Args.InterpMethod     = 'linear';
                Args.OutType          = 'AstroSpec';  % 'astrospec' | 'mat'
                Args.OutUnits         = 'mag';    % 'mag' | 'trans'
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
                        case 'astrospec'
                            Trans(Iam) = AstroSpec({DataN});
                        otherwise
                            error('Unknown OutType option');
                    end
                end
            end
        end
        
        function Result = sunSpec
            % Get the Solar spectrum into an AstropSpec object
            % Input  : null
            % Output : - An AstroSpec object with the solar spectrum.
            %            Flux is in erg/cm^2/s/A at 1AU (no atmosphere)
            % Author : Eran Ofek (Nov 2022)
            % Example: S=AstroSpec.sunSpec
            
            SunSpec = io.files.load2('SunSpec.mat');
            Result  = AstroSpec({SunSpec});
            
        end
         
        function [Result_ST,Result_A,Result_S,Result_E,Result_STE,Result_AE] = mieScattering(Radius, RadiusW, Theta, N, Lambda)
            % Mie scattering spectrum for a specific scattering angle and
            %   linear combination of particle sizes.
            % Input  : - Particle radius.
            %          - Particle-radius weight. Default is 1.
            %          - Scattering angle (theta) in [deg].
            %          - Refractive index.
            %          - Wavelength in the same units as the particle
            %            radius.
            % Output : - An AstroSpec object with a Mie scattering efficiency spectrum
            %            'scat_theta/ext'
            %            for some specific scattering angle Theta, and for
            %            a particles with the given size distribution.
            %            This is the scattering spectrum per unit area of
            %            the scatters.
            % Author : Eran Ofek (Nov 2022)
            % Example: Result = AstroSpec.mieScattering(1, 1, 58.1, 1.7+0.3.*1i);
           
            
            arguments
                Radius   = 1e4;  %(0.5:0.1:1.5)';
                RadiusW  = 1;  % radius weight.
                Theta    = 58.1;
                N        = 1.7 + 0.3.*1i;  % can be a vector of the same length as Lambda
                Lambda   = logspace(log10(1000), log10(15000), 100).'; %logspace(-2,1,100).';
                %Out      = {'scat_theta/ext','abs','scat_theta','ext'};  % 'eff' | 'scat/abs'
            end
           
            
            Nl = numel(Lambda);
            Nr = numel(Radius);
            Nn = numel(N);
            Nrw = numel(RadiusW);
            Spec_ST = zeros(Nl,2);
            Spec_ST(:,1) = Lambda(:);
            
            Spec_A = zeros(Nl,2);
            Spec_A(:,1) = Lambda(:);
            
            Spec_S = zeros(Nl,2);
            Spec_S(:,1) = Lambda(:);
            
            Spec_E = zeros(Nl,2);
            Spec_E(:,1) = Lambda(:);
            
            Spec_STE = zeros(Nl,2);
            Spec_STE(:,1) = Lambda(:);
            
            Spec_AE = zeros(Nl,2);
            Spec_AE(:,1) = Lambda(:);
            
            
            for Il=1:1:Nl
                In = min(Nn,Il);
                for Ir=1:1:Nr
                    [S, C, ANG] = calcmie(Radius(Ir), N(In), 1, Lambda(Il), 180, 'ConvergenceFactor',1);
                    % Equation 3 in REF 1:
                    S1=squeeze(abs(S(1,1,:))).^2;
                    S2=squeeze(abs(S(2,2,:))).^2;
                    a      = 2.*pi.*Radius(Ir)./Lambda(Il); % size parameter (alpha)

                    % Note that there is an error in Eq. 8 in REF 1 - should be ^-1 instead of
                    % ^-2
                    %Itheta = (2.*k.^2).^-1 .*(S1 + S2);
                    Itheta = (2.*pi.*a.^2).^-1 .*(S1 + S2);
                    % to calculate the angular Mie cross section
                    IthetaT = interp1(ANG(:),Itheta(:), Theta);
                    %IthetaT = IthetaT; %./(pi.*Radius(Ir).^2);

                    Irw = min(Nrw,Ir);
                  
                    Spec_ST(Il,2)  = Spec_ST(Il,2) + RadiusW(Irw).*IthetaT;   % 1/sr
                    Spec_A(Il,2)   = Spec_A(Il,2) + RadiusW(Irw) .*C.abs./(pi.*Radius(Ir).^2);  % total abs/(pi*r^2)
                    Spec_S(Il,2)   = Spec_S(Il,2) + RadiusW(Irw) .*C.sca./(pi.*Radius(Ir).^2);  % total abs/(pi*r^2)
                    Spec_E(Il,2)   = Spec_E(Il,2) + RadiusW(Irw) .*C.ext./(pi.*Radius(Ir).^2);  % total abs/(pi*r^2)
                    Spec_STE(Il,2) = Spec_STE(Il,2) + RadiusW(Irw) .*IthetaT.*4.*pi./(  C.ext./(pi.*Radius(Ir).^2) );
                    Spec_AE(Il,2)  = Spec_AE(Il,2) + RadiusW(Irw) .*(C.abs./(pi.*Radius(Ir).^2)) ./(  C.ext./(pi.*Radius(Ir).^2) );  % total abs/(pi*r^2)

                end
            end
            
            Spec_ST(:,2)  = Spec_ST(:,2)./sum(RadiusW);
            Spec_A(:,2)   = Spec_A(:,2)./sum(RadiusW);
            Spec_S(:,2)   = Spec_E(:,2)./sum(RadiusW);
            Spec_E(:,2)   = Spec_E(:,2)./sum(RadiusW);
            Spec_STE(:,2) = Spec_STE(:,2)./sum(RadiusW);
            Spec_AE(:,2)  = Spec_AE(:,2)./sum(RadiusW);
            
            
            Result_ST  = AstroSpec({Spec_ST});
            Result_A   = AstroSpec({Spec_A});
            Result_S   = AstroSpec({Spec_S});
            Result_E   = AstroSpec({Spec_E});
            Result_STE = AstroSpec({Spec_STE});
            Result_AE  = AstroSpec({Spec_AE});
            
            
        end
    
        function AS=getSkyArcsSpecLines(Name)
            % Get sky/arcs spectra and lines list DB
            % Input  : - If empty, then return an AstroSpec object with all
            %            spectra/lines list in DB.
            %            If 'show', then return a cell array with spectra
            %            names.
            %            If a char array of spectra name (e.g., 'Cd'), then
            %            return a single element AstroSpec object with the
            %            requested spectra/lines list.
            % Output : - An AstroSpec object or list of lines.
            % Author : Eran Ofek (Dec 2023)
            % Example: AS=AstroSpec.getSkyArcsSpecLines
            %          AS=AstroSpec.getSkyArcsSpecLines('show')
            %          AS=AstroSpec.getSkyArcsSpecLines('SkyLow')
            
            arguments
                Name    = [];
            end
            
            % prep AstroSpec from old format
            % I=1;AS(I)=AstroSpec;AS(I).Wave=SpecArcs(I).Spec(:,1);AS(I).Flux=SpecArcs(I).Spec(:,2); S(I).WaveUnits='A';ASI=1;AS(I)=AstroSpec;AS(I).Wave=SpecArcs(I).Spec(:,1);AS(I).Flux=SpecArcs(I).Spec(:,2); S(I).WaveUnits='A';AS(I)I=1;AS(I)=AstroSpec;AS(I).Wave=SpecArcs(I).Spec(:,1);AS(I).Flux=SpecArcs(I).Spec(:,2); S(I).WaveUnits='A';AS(I).FluxUnits='';

            AS = io.files.load2('WaveCalib_AstroSpec.mat');
            
            if isempty(Name)
                % return all spectra and lines list
            else
                if strcmp(Name, 'show')
                    AS = {AS.Name};
                else
                    % search specific spec/lines list
                    
                    Ind = strcmp(Name, {AS.Name});
                    AS = AS(Ind);
                    
                end
            end
                
                
        end
    
        function AS=getSpecPhotStandard(Name, Dec, Args)
            % Get spectrum of standard star
            %   This function retrieve spectrum of spectrophotometric
            %   standard stars as AstroSpec object.
            %   The function supports one of the following standard stars
            %   lists:
            %       SpecPhotStandardStar.mat - 52 standard stars
            % Input  : - Star name (e.g., 'HIP45880'), or RA [deg|rad]
            %          - Optional Dec [deg|rad]
            %          * ...,key,val,...
            %            'SearchRadius' - Default is 100.
            %            'SearchRadiusUnits' - Default is 'arcsec'.
            %            'CooUnits' - Default is 'deg'.
            %            'List' - List of std stars:
            %                   'SpecPhotStandardStar.mat' - 52 std stars.
            % Output : - An AstroSpec object.
            %            Some info like RA, Dec, Mag is stored in the
            %            Userdata.
            %            The .Lines properties containing the grid points of
            %               wavelengths [Ang] that can be used as ancor points
            %               in the interpolation of the spectra over
            %               emission/absorbtion lines.
            % Author : Eran Ofek (Dec 2023)
            % Example: AstroSpec.getSpecPhotStandard('HIP45880')
            %          AstroSpec.getSpecPhotStandard(140.33, 81.724)

            arguments
                Name    = [];
                Dec     = [];
                Args.SearchRadius      = 100;
                Args.SearchRadiusUnits = 'arcsec';
                Args.CooUnits          = 'deg';
                Args.List              = 'SpecPhotStandardStar.mat'
            end

            % prep AstroSpec from old format
            %             for I=1:1:numel(SpecPhot_Stand)
            %                 AS(I) = AstroSpec;
            %                 AS(I).Name = SpecPhot_Stand(I).Name;
            %                 AS(I).UserData.SpecType = SpecPhot_Stand(I).SpecType;
            %                 AS(I).UserData.RA       = SpecPhot_Stand(I).RA;
            %                 AS(I).UserData.Dec      = SpecPhot_Stand(I).Dec;
            %                 AS(I).UserData.MagV     = SpecPhot_Stand(I).MagV;
            %                 AS(I).Lines             = SpecPhot_Stand(I).GridWave;
            %                 AS(I).Wave              = SpecPhot_Stand(I).Spec(:,1);
            %                 AS(I).Flux              = SpecPhot_Stand(I).Spec(:,2);
            %                 AS(I).WaveUnits         = 'A';
            %                 AS(I).FluxUnits         = 'erg/cm^2/s/A'; 
            %             end

            AS = io.files.load2(Args.List);
            Nas = numel(AS);
            if isempty(Name)
                % return all spectra
            else
                if ischar(Name)
                    if strcmp(Name, 'show')
                        % show spectra name
                        AS = {AS.Name};
                    else
                        % search by star name
                        Ind = [];
                        for Ias=1:1:Nas
                            Flag = strcmp(Name, AS(Ias).Name);
                            if any(Flag)
                                Ind = Ias;
                            end
                        end
                        if isempty(Ind)
                            error('Star name not found');
                        end
                        AS = AS(Ind);

                    end
                else
                    % search by coordinates
                    RA = Name;
                    Factor = convert.angular(Args.CooUnits, 'rad');
                    RA     = Factor.*RA;
                    Dec    = Factor.*Dec;
                    SearchRadius = convert.angular(Args.SearchRadiusUnits, 'rad', Args.SearchRadius); % [rad]
                    
                    StdRA  = zeros(Nas,1);
                    StdDec = zeros(Nas,1);
                    for Ias=1:1:Nas
                        StdRA(Ias)  = AS(Ias).UserData.RA;  % [rad]
                        StdDec(Ias) = AS(Ias).UserData.Dec; % [rad]
                    end
                    Dist = celestial.coo.sphere_dist_fast(RA, Dec, StdRA, StdDec);
                    Flag = Dist<SearchRadius;
                    if any(Flag)
                        [~,Ias] = min(Dist);
                        AS = AS(Ias);
                    else
                        AS = [];
                    end
                        
                end
            end


                
            
        end
        
    end
    
    methods  % resampling, sort, interpolation
        function Result = sort(Obj, Args)
            % Sort elements of AstroSpec object by wavelength
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
            % Interpolate the elements of AstroSpec into a new wavelength grid.
            %       The Mask will be interpolated using nearest
            %       interpolation. This function works only if the Data is
            %       convertable to a numeric matrix.
            % Input  : - An AstroSpec object.
            %          - A vector of new wavelngth grid on which to
            %            interpolate the AstroSpec object.
            %            Alternatively, a AstroSpec object with wavelength.
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
            % Example: S = AstroSpec({rand(100,3)});
            %          S.sort;
            %          S.interp1([0:0.1:1]);
            
            arguments
                Obj
                NewWave
                Args.Method                 = 'linear';
                Args.ExtraArgs cell         = {};
                Args.RemoveNan(1,1) logical = false;
                Args.CreateNewObj           = [];
            end
          
            if isa(NewWave, 'AstroSpec')
                NewWave = NewWave.Wave;
            end
            NewWave = NewWave(:);
            
            [Result] = createNewObj(Obj, Args.CreateNewObj, nargout, 0);
            
            Nobj = numel(Obj);
            
            for Iobj=1:1:Nobj
                % special treatment for Mask
                if ~isempty(Obj(Iobj).MaskData.Data)
                    Result(Iobj).MaskData.Data = interp1(Obj(Iobj).Wave, Obj(Iobj).MaskData.Data, NewWave, 'nearest');
                end
                VarNames = Obj(Iobj).Data.Properties.VariableNames;
                VarUnits = Obj(Iobj).Data.Properties.VariableUnits;
                Result(Iobj).Data = array2table(interp1(Obj(Iobj).Wave, table2array(Obj(Iobj).Data), NewWave, Args.Method, Args.ExtraArgs{:}));
                Result(Iobj).Data.Properties.VariableNames = VarNames;
                Result(Iobj).Data.Properties.VariableUnits = VarUnits;
                
                if Args.RemoveNan
                    Flag = isnan(Result(Iobj).Wave) | isnan(Result(Iobj).Flux);
                    Result(Iobj) = selectWave(Result(Iobj), ~Flag);
                end
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
        
    end
    
    methods % Flux operators
        
        function Result = funFlux(Obj, Fun, Args)
            % Apply a function to the Flux, FluxErr, Back columns.
            % Input  : - An AstroSpec object.
            %          - A function handle to apply.
            %            If empty, then do nothing.
            %          * ...,key,val,...
            %            'FunArgs' - A cell array of additional arguments to pass to
            %                   the function.
            %            'DataProp' - A cell array of data properties on
            %                   which to apply the function.
            %                   Default is {'Flux', 'FluxErr', 'Back'}.
            %            'CreateNewObj' -  [], true, false.
            %                   If true, create new deep copy
            %                   If false, return pointer to object
            %                   If [] and Nargout==0 then do not create new copy.
            %                   Otherwise, create new copy. Default is [].
            % Output : - An AstroSpec objec after aplying the function.
            % Author : Eran Ofek (Aug 2021)
            % Example:  Spec = AstroSpec.synspecGAIA('Temp',[5750 5500 5550],'Grav',[4.5]);
            %           Res  = funFlux(Spec, @log10);
            
            arguments
                Obj
                Fun                       % if empty do nothing
                Args.FunArgs cell         = {};
                Args.DataProp             = {'Flux', 'FluxErr', 'Back'};
                Args.CreateNewObj         = [];
            end
            
            [Result] = createNewObj(Obj, Args.CreateNewObj, nargout, 0);
           
            if ~isempty(Fun)
                Nobj = numel(Obj);
                Nd   = numel(Args.DataProp);
                for Iobj=1:1:Nobj
                    for Id=1:1:Nd
                        if ~isempty(Obj(Iobj).(Args.DataProp{Id}))
                            Result(Iobj).(Args.DataProp{Id}) = Fun(Obj(Iobj).(Args.DataProp{Id}), Args.FunArgs{:});
                        end
                    end
                end
            end
            
        end
        
        function Result = scaleFlux(Obj, Factor, Args)
            % Scale (multiply) flux properties by some factor.
            % Input  : - An AstroSpec object.
            %          - Scaling factor by which to multiply the flux
            %            properties. If scaler, then multiply all spectra.
            %            Alternatively, this can be a vector with the same
            %            number of elements as in the AstroSpec object.
            %            In this case, each spectrum will multiply by the
            %            corresponding scale factor.
            %          * ...,key,val,...
            %            'IsMagFactor' - A logical indicating if the
            %                   scaling factor is in magnitude units.
            %                   If true, then Factor will be set to:
            %                   10.^(-0.4*Factor). Default is false.
            %            'DataProp' - A cell array of data properties on
            %                   which to apply the function.
            %                   Default is {'Flux', 'FluxErr', 'Back'}.
            %            'CreateNewObj' -  [], true, false.
            %                   If true, create new deep copy
            %                   If false, return pointer to object
            %                   If [] and Nargout==0 then do not create new copy.
            %                   Otherwise, create new copy. Default is [].
            % Output : - An AstroSpec objec after aplying the scale.
            % Author : Eran Ofek (Sep 2021)
            % Example: AS = AstroSpec.blackBody((4000:10:9000)', [5000; 6000]);
            %          scaleFlux(AS, 2)
            %          scaleFlux(AS, [2,3])

            arguments
                Obj
                Factor
                Args.IsMagFactor(1,1) logical = false;
                Args.DataProp                 = {'Flux', 'FluxErr', 'Back'};
                Args.CreateNewObj             = [];
            end
            
            [Result] = createNewObj(Obj, Args.CreateNewObj, nargout, 0);

            if Args.IsMagFactor
                Factor = 10.^(-0.4.*Factor);
            end

            Nfactor = numel(Factor);
            Nobj = numel(Obj);
            Nd   = numel(Args.DataProp);
            for Iobj=1:1:Nobj
                Ifactor = min(Iobj, Nfactor);
                for Id=1:1:Nd
                    if ~isempty(Obj(Iobj).(Args.DataProp{Id}))
                        Result(Iobj).(Args.DataProp{Id}) = Obj(Iobj).(Args.DataProp{Id}) .* Factor(Ifactor);
                    end
                end
            end

        end

        function Result = funBinary(Obj1, Obj2, Fun, Args)
            % Perform a binary operation on two AstroSpec objects
            %   The operation is applied one to one or one to many.
            % Input  : - An AstroSpec object (multi elements supported).
            %          - An AstroSpec object (multi elements supported).
            %          - Function handle for operator (e.g., @plus, @times).
            %          * ...,key,val,...
            %            'AddArgs' - A cell array of additional arguments
            %                   to pass to the function. Default is {}.
            %            'Prop' - A cell array of properties on which to
            %                   apply the operator.
            %                   Default is {'Flux','FluxErr','Back','Mask'}.
            %            'KeepOnlyOverlap' - A logical indicating if to
            %                   keep only overlaping region (true).
            %                   If false, will return the wavelength grid
            %                   of the first input AstroSpec argument.
            %                   Default is false.
            %            'InterpMethod' - Default is 'linear'.
            % Output : - An AstroSpec object.
            % Author : Eran Ofek (Sep 2021)
            % Example: AS = AstroSpec.specStarsPickles('M','V')
            %          Res = funBinary(AS,AS(1), @plus)
            
            arguments
                Obj1
                Obj2
                Fun function_handle
                Args.AddArgs cell             = {};
                Args.Prop cell                = {'Flux','FluxErr','Back','Mask'};
                Args.KeepOnlyOverlap logical  = false;
                Args.InterpMethod             = 'linear';
            end
            
            Nprop = numel(Args.Prop);
            
            Nobj1 = numel(Obj1);
            Nobj2 = numel(Obj2);
            
            Nobj = max(Nobj1, Nobj2);
            Result = AstroSpec([Nobj,1]);
            for Iobj=1:1:Nobj
                Iobj1 = min(Iobj, Nobj1);
                Iobj2 = min(Iobj, Nobj2);
                
                if Args.KeepOnlyOverlap
                    [New1, New2] = interpAndKeepOverlap(Obj1(Iobj1), Obj2(Iobj2), 'Method',Args.InterpMethod, 'CreateNewObj',true);
                else
                    New2 = interp1(Obj2, Obj1(Iobj1).Wave, 'Method',Args.InterpMethod, 'CreateNewObj',true);
                    New1 = Obj1(Iobj1).copy();
                end
                
                Result(Iobj) = New1;
                for Iprop=1:1:Nprop
                    if ~isempty(Result(Iobj).(Args.Prop{Iprop}))
                        Result(Iobj).(Args.Prop{Iprop}) = Fun(New1.(Args.Prop{Iprop}), New2.(Args.Prop{Iprop}), Args.AddArgs{:});
                    end
                end
            end
        end
        
        function Result = times(Obj1, Obj2)
            % Multiply two spectra or spectrum and a filter.
            %   The result is interpolated to the wavelength grid of the
            %   second input object.
            %   Multiplication is done element by elemnt, multi to multi,
            %   or multi to single.
            % Input  : - An AstroSpec object.
            %          - An AstroSpec object or an AstFilter object.
            % Output : - An AstroSpec object.
            % Author : Eran Ofek (Nov 2022)
            
           
            arguments
                Obj1 AstroSpec
                Obj2 
                %Args.CreateNewObj logical   = true;
            end
            
            
            
            switch class(Obj2)
                case 'AstroSpec'
                    % Obj2 is an AstroSpec object
                    %[New1, New2] = interpAndKeepOverlap(Obj1, Obj2);
                    % Given two AstroSpec objects, interpolate the first into the
                    % wavelength grid defined by the second and keep only the
                    % overlaping points.
                    
                    N1 = numel(Obj1);
                    N2 = numel(Obj2);
                    N  = max(N1, N2);
            
                    for I=1:1:N
                        I1 = min(I, N1);
                        I2 = min(I, N2);

                        InterpFlux = interp1(Obj1(I1).Wave, Obj1(I1).Flux, Obj2(I2).Wave);
                        Result(I) = AstroSpec({[Obj2(I2).Wave, InterpFlux .* Obj2(I2).Flux]});
                    end
                case 'AstFilter'
                    N1 = numel(Obj1);
                    N2 = numel(Obj2);
                    N  = max(N1, N2);
                    
                    for I=1:1:N
                        I1 = min(I, N1);
                        I2 = min(I, N2);

                        InterpFlux = interp1(Obj1(I1).Wave, Obj1(I1).Flux, Obj2(I2).nT(:,1));
                       
                        Result(I) = AstroSpec({[Obj2(I2).nT(:,1), InterpFlux.*Obj2(I2).nT(:,2)]});
                    end
                    
                otherwise
                    error('Unsupported class for second input object');
            end
            
        end
        
        function Result = rdivide(Obj1, Obj2)
            % Divide an AstroSpec elemnts by spectrum, filter, scalar
            % Input  : - An AstroSPect object.
            %          - A cell array with scalars
            % Output : - An AstroSPec object in which each input spectrum
            %            is divided by the corresponding scalar, or
            %            spectrum.
            % Author : Eran Ofek (Nov 2022)
            
           
            arguments
                Obj1
                Obj2
            end
            
            N1 = numel(Obj1);
            N2 = numel(Obj2);
            N  = max(N1,N2);
            for I=1:1:N
                if iscell(Obj2)
                    I1 = min(I,N1);
                    I2 = min(I,N2);
                    Result(I) = AstroSpec({[Obj1(I1).Wave, Obj1(I2).Flux./Obj2{I2}]});
                end
            end
            
        end
        
        function Result = trapz(Obj)
            % Integrate (using trapz) the entire spectra
            % Input  : - An AstroSpec object.
            % Output : - A vector of integration results, one per element.
            % Author : Eran Ofek (Nov 2022)
            
            N = numel(Obj);
            Result = zeros(N,1);
            for I=1:1:N
                Result(I) = trapz(Obj(I).Wave, Obj(I).Flux);
            end
        end
    end
    
    
    methods % extinction
        function [Result, TranAM] = applyAtmosphericExt(Spec, OutAM, InAM, Args)
            % Apply atmospheric extinction (airmass) to AstroSpec object.
            %   One to one, or one to many.
            % Input  : - An AstroSpec object.
            %          - Output airmass.
            %          - Input airmass (of the AstroSpec object).
            %            Default is 0.
            %          * ...,key,val,...
            %            'File' - A file name from which to read transmission.
            %                   If empty, will return list of available files (in
            %                   dir-like output).
            %                   Options include: 'VLT' | 'KPNO' | 'SNfactory'
            %                   Default is 'VLT'.
            %            'Prop' - A cell array of properties on which to
            %                   apply the operator.
            %                   Default is {'Flux','FluxErr','Back','Mask'}.
            %            'KeepOnlyOverlap' - A logical indicating if to
            %                   keep only overlaping region (true).
            %                   If false, will return the wavelength grid
            %                   of the first input AstroSpec argument.
            %                   Default is false.
            %            'InterpMethod' - Default is 'linear'.
            % Output : - An AstroSpec object.
            % Author : Eran Ofek (Sep 2021)
            % Example: AS = AstroSpec.specStarsPickles('M','V')
            %          Res = applyAtmosphericExt(AS, 1.4)
            
            arguments
                Spec
                OutAM
                InAM                             = 0;
                Args.File                        = 'VLT';
                Args.Prop                        = {'Flux','FluxErr','Back','Mask'};
                Args.KeepOnlyOverlap logical     = false;
                Args.InterpMethod                = 'linear';
            end
            
            DiffAM = OutAM - InAM;
            TranAM = AstroSpec.atmosphericExtinction(Args.File, 'AM',DiffAM, 'OutUnits','trans','OutType','AstroSpec', 'Wave',[]);
            
            Result = funBinary(Spec, TranAM, @times, 'Prop',Args.Prop, 'KeepOnlyOverlap',Args.KeepOnlyOverlap, 'InterpMethod',Args.InterpMethod);
                
        end
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
            %
            % G= AstroSpec.specGalQSO('Gal_Sb.txt'); 
            % A=G.redshift(z);
            % mag=synphot(A,Result,{'a','b','c','d','e','f','g','h'});
            % plot(z, [mag.a]-[mag.e])
            
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

            MagSyn = synphot(Obj, FilterFamily, FilterName, 'MagSys',Args.MagSys, 'Device',Args.Device);
%             MagSyn = synphot(Obj, FilterFamily, FilterName, 'MagSys',Args.MagSys, 'Device',Args.Device, 'Algo',Args.Algo);
            MagSynVec = [MagSyn.(FilterName)];
            Factor = 10.^(-0.4.*(Mag - MagSynVec));
            Result = scaleFlux(Obj, Factor, 'CreateNewObj',Args.CreateNewObj);
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
    
    methods  % fitting
        function Result = fitSpec(Obj, ModelSpec, Args)
            % UNDER CONSTRUCTION
           
            % Example: Spec = AstroSpec.synspecGAIA('Temp',[5750],'Grav',[4.5]);
            %          BB = AstroSpec.blackBody(Spec.Wave, 5750);
            %          Result = fitSpec(Spec, BB)
            
            arguments
                Obj                     % AstroSpec
                ModelSpec               % AstroSpec to fit to Obj
                Args.RelErrModel                     = 0.05;
                Args.InterpModel2spec(1,1) logical   = true;
                Args.FunFlux                         = [];
                Args.FunArgs cell                    = {};
                Args.InterpMethod                    = 'linear';
                Args.FitType                         = 'norm';   % 'add' | 'none' | 'norm' | 'normadd' | 'ext'
                Args.R                               = 3.08;
            end
            
            Nobj = numel(Obj);
            Nms  = numel(ModelSpec);
            Nmax = max(Nobj, Nms);
            for Imax=1:1:Nmax
                Iobj = min(Imax, Nobj);
                Ims  = min(Imax, Nms);
                
                if Args.InterpModel2spec
                    % model over obj
                     [NewModelSpec, NewObj] = interpAndKeepOverlap(ModelSpec(Ims), Obj(Iobj), 'Method',Args.InterpMethod, 'CreateNewObj',true);
                     
                else
                    % Obj over model
                    [NewObj, NewModelSpec] = interpAndKeepOverlap(Obj(Iobj), ModelSpec(Ims), 'Method',Args.InterpMethod, 'CreateNewObj',true);
                end
            
                % apply function to spectra
                NewObj       = funFlux(NewObj,       Args.FunFlux, 'FunArgs',Args.FunArgs, 'CreateNewObj',false);
                NewModelSpec = funFlux(NewModelSpec, Args.FunFlux, 'FunArgs',Args.FunArgs, 'CreateNewObj',false);
                
                % fit
                % scale, additive, extinction
                
                Nw = NewModelSpec.length;
                
                switch lower(Args.FitType)
                    case 'none'
                        ScaledModel = NewModelSpec.Flux;
                        ScaledErr   = NewModelSpec.FluxErr;
                    otherwise
                        % fiiting
                        switch lower(Args.FitType)
                            case 'add'
                                % fit only additive term
                                H    = [ones(Nw,1)];
                            case 'norm'
                                % fit only normalization
                                H    = [NewModelSpec.Flux(:)];
                            case 'normadd'
                                H    = [NewModelSpec.Flux(:), ones(Nw,1)];
                            case 'ext'
                                WaveMicrons       = convert.length(NewModelSpec.WaveUnits, 'micrometer', NewModelSpec.Wave);
                                A_W               = astro.extinction.extinction(1, WaveMicrons, [], Args.R);
                                NewModelSpec.Flux = NewModelSpec.Flux.*10.^(-0.4.*A_W);
                                H                 = [NewModelSpec.Flux(:)];
                            otherwise
                        end
                        Y    = NewObj.Flux;
                        ErrY = NewObj.FluxErr;
                        if isempty(ErrY)
                            ErrY = ones(Nw,1);
                        end
                        [Par, ParErr] = lscov(H, Y, (1./ErrY).^2);
                        ScaledModel   = H*Par;
                        ScaledErr     = Par(1).*NewModelSpec.FluxErr(:);
                end
                        
                Result(Imax).Resid = NewObj.Flux - ScaledModel;
                Result(Imax).Ratio = NewObj.Flux ./ ScaledModel;
                Result(Imax).Std   = std(Result(Imax).Resid);
                if isempty(ScaledErr)
                    ScaledErr = 0;
                end
                ScaledErr = sqrt(ScaledErr.^2 + (ScaledModel.*Args.RelErrModel).^2);
                Result(Imax).Chi2  = sum((Result(Imax).Resid./ScaledErr).^2);
                
            end
        end

        function [Result, ScaledBestModelSpec] = fitMag(ModelSpec, Mag, Family, Bands, Args)
            % Fit AstroSpec spectra to observed magnitudes in some bands.
            % Input  : - An AstroSpec object. Each spectra in this object
            %            is regarded as a model spectra that will be fitted
            %            to some observed magnitudes, and the best spectral
            %            template will be returned.
            %          - A matrix of magnitudes [Source X Band].
            %            Each line corresponds to one source that will be
            %            fitted against all the model spectra. Each column
            %            corresponds to a band in which the source was
            %            observed.
            %          - Band family name (e.g., 'SDSS').
            %          - A cell array of band names (e.g., {'g','r'}).
            %          * ...,key,val,...
            %            'MagErr' - A matrix of magnitude errors
            %                   corresponding to the magnitude matrix.
            %                   Default is 0.01.
            %            'MagSys' - Mag system: ['AB'] | 'Vega'
            %            'Device' - Device ['photon'] | 'bol'
            %            'Algo' - Algorithm - see astro.spec.synphot
            %                   Default is 'cos'
            % Output : - A structure with the following fields:
            %            .IndBestChi2 - A vector (one element per source).
            %                   For each source this gives the index of the
            %                   best fitted (chi2) spectral template.
            %            .BestChi2 - A vector (one per source) of the best
            %                   fitted \chi^2 value.
            %            .BestMeanDiff - A vector (one per source) of the
            %                   best fitted magnitude mean difference
            %                   (normalization).
            %          - An AstroSpec object with the best fit model
            %            spectra scaled to match the input magnitudes.
            % Author : Eran Ofek (Sep 2021)
            % Example: Spec = AstroSpec.synspecGAIA('Temp',[4000:100:8000],'Grav',[4.5]);
            %          M = synphot(Spec(10:11),'SDSS',{'g','r','i'}, 'IsOutMat',true);
            %          Result = fitMag(Spec, M + 50, 'SDSS',{'g','r','i'});

            arguments
                ModelSpec
                Mag          % [Source, Band] matrix
                Family
                Bands cell   % cell array of bands
                Args.MagErr   = 0.01;
                Args.MagSys   = 'AB';
                Args.Device   = 'photon';
                Args.Algo     = 'cos';
            end
        
            Nspec         = numel(ModelSpec);
            [Nsrc, Nband] = size(Mag);
            if Nband~=numel(Bands)
                error('Number of columns in Magnitude matrix must be equal to the number of bands');
            end

            Result.IndBestChi2  = nan(Nsrc,1);
            Result.BestChi2     = nan(Nsrc,1);
            Result.BestMeanDiff = nan(Nsrc,1);
            Result.IndBestRMS   = nan(Nsrc,1);
            Result.BestRMS      = nan(Nsrc,1);

            SynMagMat = synphot(ModelSpec, Family, Bands, 'MagSys',Args.MagSys, 'Device',Args.Device, 'IsOutMat',true);
            for Isrc=1:1:Nsrc
                % Fit mag difference (L2 minimization)
                Diff = Mag(Isrc,:) - SynMagMat;
                MeanDiff  = [ones(Nband,1) \ Diff.'].';

                %MeanDiff = mean(Diff, 2);
                Chi2                 = sum(((Diff - MeanDiff)./Args.MagErr).^2, 2);
                [Result.BestChi2(Isrc), Result.IndBestChi2(Isrc)] = min(Chi2);
                Result.BestMeanDiff(Isrc)           = MeanDiff(Result.IndBestChi2(Isrc));
                AllStd = std(Diff,[],2,'omitnan');
                [Result.BestRMS(Isrc), Result.IndBestRMS(Isrc)] = min(AllStd);
            end

            if nargout>1
                % Return also the scaled (by mean diff) best fitted
                % ModelSpec

                % when switching to non-handle objects - replace this...
                for I=1:1:numel(Result.IndBestChi2)
                    ScaledBestModelSpec(I) = ModelSpec(Result.IndBestChi2(I)).copy();
                end
                % Can't use the next line due to handle-object behavior!!
                %ScaledBestModelSpec = ModelSpec(Result.IndBestChi2);

                ScaledBestModelSpec = scaleFlux(ScaledBestModelSpec, Result.BestMeanDiff, 'IsMagFactor',true);
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
            %            'CreateNewObj' - Create new copy.
            %                   Default is true.
            %            'ApplyFluxZcorr' - Apply the 1+z flux correction.
            %                   Default is true.
            %            'CorrectFluxErr' - Default is true.
            %            'CorrectBack'    - Default is true.
            % Output : - An updated AstroSpec object.
            %            If output is not requested, then by default will
            %            change the input object (see CreateNewObj arg).
            % Author : Eran Ofek (Aug 2021)
            % Example: S = AstroSpec({rand(100,3)});
            %          NS = AS.copy();
            %          NS.redshift=0.5;
            %          NS.Z=0.5;
            %          NS.redshift(0)

            arguments
                Obj
                Zout                             = 0;
                Zin                              = [];  % Z of spectra
                Args.CreateNewObj logical        = true;
                Args.ApplyFluxZcorr logical      = true;
                Args.CorrectFluxErr(1,1) logical = true;
                Args.CorrectBack(1,1) logical    = true;
            end
            
            if Args.CreateNewObj
                Result = Obj.copy;
            else
                Result = Obj;
            end
        
            Nobj = numel(Obj);
            Nz   = numel(Zout);
            Nmax = max(Nobj, Nz);
            
            if Nz>1 && Nobj==1
                % copy spectrum into multi-element object
                Result = AstroSpec(Nmax);
                for Imax=1:1:Nmax
                    Result(Imax) = Obj.copy;
                end
            end
            
            for Imax=1:1:Nmax
                Iobj = min(Imax, Nobj);
                Iz   = min(Imax, Nz);
                if ~isempty(Zin)
                    % reset Z property in object
                    Result(Imax).Z   = Zin;
                end
                
                if isempty(Result(Imax).Z)
                    error('Z must be specified either in object or as Zin input argument');
                end
                
                % apply redshift to wavelength
                % convert to rest frame
                Result(Imax).Wave    = Result(Imax).Wave .* (1 + Zout(Iz)) ./ (1 + Result(Iobj).Z);
                if Args.ApplyFluxZcorr
                    Result(Imax).Flux    = Result(Imax).Flux .* (1 + Result(Iobj).Z) ./ (1 + Zout(Iz));
                end
                if Args.CorrectFluxErr && ~isempty(Result(Imax).FluxErr)
                    Result(Imax).FluxErr = Result(Imax).FluxErr .* (1 + Result(Iobj).Z) ./ (1 + Zout(Iz));
                end
                if Args.CorrectBack && ~isempty(Result(Imax).Back)
                    Result(Imax).Back    = Result(Imax).Back .* (1 + Result(Iobj).Z) ./ (1 + Zout(Iz));
                end
                
                Result(Imax).Z       = [];     % needed [so Z setter will not apply the redshift]
                Result(Imax).Z       = Zout(Iz);
            end
            
            
        end
        
        function Result = scaleWave(Obj, Scale, Args)
            % Scale wavelength axis (multiply by scale).
            % Input  : - An AstroSpec object.
            %          - A scalar or vector of scale.
            %            One scale per AstroSpec element, or scalar for all
            %            of them.
            %          * ...,key,val,...
            %            'CreateNewObj' - A logical indicating if to create
            %                   a new copy of the object.
            %                   Default is false.
            % Output : - An AstroSpec object in which the wavelength axis
            %            was multiplied by scale.
            % Author : Eran Ofek (Dec 2023)
            % Example: AS = AstroSpec({[ones(10,2)]});
            %          AS.scaleWave(2)
            
            arguments
                Obj
                Scale
                Args.CreateNewObj logical   = false;
            end
            
            if Args.CreateNewObj
                Result = Obj.copy;
            else
                Result = Obj;
            end
            
            Ns   = numel(Scale);
            Nobj = numel(Obj);
            for Iobj=1:1:Nobj
                Is = min(Iobj, Ns);
                Result(Iobj).Wave = Result(Iobj).Wave .* Scale(Is);
            end
            
        end
        
        function Result = shiftWave(Obj, Shift, Args)
            % Shift wavelength axis (add a shift).
            % Input  : - An AstroSpec object.
            %          - A scalar or vector of shifts.
            %            One shift per AstroSpec element, or scalar for all
            %            of them.
            %          * ...,key,val,...
            %            'CreateNewObj' - A logical indicating if to create
            %                   a new copy of the object.
            %                   Default is false.
            % Output : - An AstroSpec object in which the wavelength axis
            %            were added to shift.
            % Author : Eran Ofek (Dec 2023)
            % Example: AS = AstroSpec({[ones(10,2)]});
            %          AS.shiftWave(2)
            
            arguments
                Obj
                Shift
                Args.CreateNewObj logical   = false;
            end
            
            if Args.CreateNewObj
                Result = Obj.copy;
            else
                Result = Obj;
            end
            
            Ns   = numel(Shift);
            Nobj = numel(Obj);
            for Iobj=1:1:Nobj
                Is = min(Iobj, Ns);
                Result(Iobj).Wave = Result(Iobj).Wave + Shift(Is);
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
    
    
    methods (Static)  % unitTest
        Result = unitTest
            % unitTest for AstroSpec
    end
    
end
