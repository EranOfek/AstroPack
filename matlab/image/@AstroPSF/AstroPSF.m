% AstroPSF - A container class for PSFs
% Properties :
%       Data - A dependent property that generates the PSF stamp
%       Var -  A dependent property that generates the PSF variance stamp
%       DataPSF - A PSF data. Stamp, or function parameters
%       DataVar - A PSF variance data. Stamp, or function parameters
%       FunPSF - A PSF function handle
%       ArgVals - Arguments values.
%       ArgNames - The argument names.
%       StampSize - PSF stamp size.
% Functionality :
%
% #functions (autogen)
% AstroPSF -
% curve_of_growth - Calculate curve of growth of a PSF includinf radii
% funUnary -
% fwhm - Calculate the FWHM of a PSF using the curve of growth (for alternative method use moment2).
% get.Data - getter for Dependent property Data
% get.Var - getter for Dependent property Var
% getPSF - get PSF from AstroPSF object
% images2cube - Transform an array of AstroPSF into a cube of PSFs
% moment2 - Calculate the moments and perture photometry of PSFs using the imUtil.image.moment2 function.
% multiGaussianPSF - I = G1(x,y, sigmaX(X,Y,Color,Flux), sigmaY(X,Y,Color,Flux), rho(X,Y,Color,Flux) )
% set.Data - setter for Dependent property Data
% set.Var - setter for Dependent property Var
% #/functions (autogen)
%

% TODO:
%   1. resampling and scaling of PSF
%       In some cases we may want to generate the PSF in oversampling
%   2. Info about sub pixel response?
%   3. Info about how the PSF was generated

classdef AstroPSF < Component
    properties (Dependent) % Access image data directly
        Data
        Var
    end
    
    properties (SetAccess = public)
        DataPSF           = [];   % The fun parameters, or an image
        DataVar           = [];
        Scale             = 1;    % used for consistency only
        FunPSF            = [];   % e.g., Map = Fun(Data, X,Y, Color, Flux)
        ArgVals cell      = {};
        ArgNames cell     = {'X','Y','Color','Flux'};
        StampSize         = [];
    end
    
    methods % Constructor
       
        function Obj = AstroPSF(FileName, Args)
            % AstroPSF constructor - read PSF images to AStroPSF object
            % Input  : - File names.
            %            Either an AstroImage, ImageComponent, SIM, imCl
            %            objects from which the image property will be
            %            stored in the DataPSF property of the AstroPSF
            %            object.
            %            Alternatively, a file name or a cell array of file
            %            names to read into the AstroPSF object.
            %          * ...,key,val,...
            %            'HDU' - If file name is a FITS file, then this is
            %                   the HDU. Default is 1.
            %            'FileType' - File type for file names. Default is 'fits'.
            %            'UseRegExp' - Use regular expression for file
            %                   names. Default is false.
            %            'DataVar' - Same as file names, but for the
            %                   variance image of the PSF.
            %            'VarHDU' - Variance image HDU. Default is 1.
            % Output : - An AstroPSF object in which the PSF and variance
            %            are populated.
            % Author : Eran Ofek (May 2022)
            % Example: P=AstroPSF('ztf_20200207460174_000576_zg_c03_o_q3_diffimgpsf.fits')
            
            
            arguments
                FileName                  = [];
                Args.HDU                  = 1;
                Args.FileType             = 'fits';
                Args.UseRegExp logical    = false;
                Args.DataVar              = {};
                Args.VarHDU               = 1;
            end
            
            if ischar(FileName)
                FileName = {FileName};
            end
            Nf = numel(FileName);
            
            for Ifield=1:1:2
                if Ifield==2
                    File      = Args.DataVar;
                    HDU       = Args.VarHDU;
                    FieldName = 'DataVar';
                else
                    File      = FileName;
                    HDU       = Args.HDU;
                    FieldName = 'DataPSF';
                end
                    
                if isempty(File)
                    % define the object
                    Obj.(FieldName) = [];
                else
                    if isa(File,'ImageComponent') || isa(File,'AstroImage')
                        for If=1:1:Nf
                            Obj(If).(FieldName) = File{If}.Image;
                        end
                    elseif isa(File,'SIM') || isa(File,'imCL')
                        for If=1:1:Nf
                            Obj(If).(FieldName) = File{If}.Im;
                        end
                    else
                        ImIO = ImageIO(File, 'HDU',HDU,...
                                                 'FileType',Args.FileType,...
                                                 'IsTable',false,...
                                                 'UseRegExp',Args.UseRegExp);

                        Nobj = numel(ImIO);
                        for Iobj=1:1:Nobj
                            if ~isempty(ImIO(Iobj).Data)
                                Obj(Iobj).(FieldName) = ImIO(Iobj).Data;
                            end
                        end
                        Obj = reshape(Obj, size(ImIO));
                    end

                end % end if isempty...
            end % end for Ifield
            
        end

    end
    
    methods % Setters/Getters
        function Result = get.Data(Obj)
            % getter for Dependent property Data
            Result = getPSF(Obj);
        end
        
        function Obj = set.Data(Obj, DataPSF)
            % setter for Dependent property Data
            Obj.DataPSF = DataPSF;
            % make fun empty
            Obj.FunPSF = [];
        end
        
        function Result = get.Var(Obj)
            % getter for Dependent property Var
            Result = Obj.DataVar;
        end
        
        function Obj = set.Var(Obj, VarPSF)
            % setter for Dependent property Var
            Obj.DataVar = VarPSF;
        end
    end
    
    methods (Static)  % static methods
        function multiGaussianPSF(DataPSF, X, Y, Color, Flux)
            %
            % I = G1(x,y, sigmaX(X,Y,Color,Flux), sigmaY(X,Y,Color,Flux), rho(X,Y,Color,Flux) )
            
            
            
            
        end
            
        
    end
    
    methods % utilities (e.g., isempty)
        function Result = isemptyPSF(Obj)
            % Check if PSFData is empty.
            % Input  : - An AstroPSF object.
            % Output : - An array of logical indicating if each element in
            %            the AstroPSF is empty.
            % Author : Eran Ofek (Jan 2022)
            
            Nobj = numel(Obj);
            Result = false(size(Obj));
            for Iobj=1:1:Nobj
                Result(Iobj) = isempty(Obj(Iobj).Data);
            end
        end
    end
    
    methods % generating PSF
        function Result = getPSF(Obj, DataPSF, FunPSF, StampSize, ArgVals, ArgNames)
            % get PSF from AstroPSF object
            % Input : - A single AstroPSF object.
            %         - DataPSF is empty, will take Obj.DataPSF.
            %           If not empty, will also populate Obj.DataPSF.
            %           Default is empty.
            %         - FunPSF function handle (like DataPSF). Default is [].
            %         - StampSize [I,J]. If empty, use default.
            %           Default is [].
            %         - ArgVals (like DataPSF). Default is [].
            %         - ArgNames (like DataPSF). Default is [].
            % Output : - A PSF stamp.
            % Author : Eran Ofek
            % Example:
            
            arguments
                Obj(1,1)
                DataPSF   = [];
                FunPSF    = [];
                StampSize = [];
                ArgVals   = [];
                ArgNames  = [];
            end
            
            if isempty(DataPSF)
                DataPSF = Obj.DataPSF;
            else
                Obj.DataPSF = DataPSF;
            end
            if isempty(FunPSF)
                FunPSF  = Obj.FunPSF;
             else
                Obj.FunPSF = FunPSF;
            end
            if isempty(StampSize)
                StampSize = Obj.StampSize;
            else
                Obj.StampSize = StampSize;
            end
            if isempty(ArgVals)
                ArgVals  = Obj.ArgVals;
            else
                Obj.ArgVals = ArgVals;
            end
            if isempty(ArgNames)
                ArgNames  = Obj.ArgNames;
            else
                Obj.ArgNames = ArgNames;
            end
        
            if isempty(FunPSF)
                % PSF is an image stamp
                Result = Obj.DataPSF;
            else
                Result = Obj.FunPSF(Obj.DataPSF, Obj.ArgVals{:});
            end
            if ~isempty(StampSize)
                if ~all(size(Result)==StampSize)
                    % pad PSF
                    error('Pad PSF option is not yet available');
                end
            end
            
        end
            
    end
    
    
    methods % PSF properties
        
        function [Result, RadHalfCumSum, RadHalfPeak] = curve_of_growth(Obj, Args)
            % Calculate curve of growth of a PSF includinf radii
            % Input  : - An AstroPSF object
            %          * ...,key,val,...
            %            'ReCenter' - A logical indicating if to find the
            %                   PSF center using the first moment (true),
            %                   or to use the stamp center (false).
            %                   Default is true.
            %            'CenterPSFxy' - PSF center [x,y] coordinates. If [], or not provided, default
            %                   is half the PSF matrix size.
            %            'Step' - Step size of curve of growth. Default is 1.
            %            'Level' - Level at which to calculate radius.
            %                   Default is 0.5.
            %            'InterpMethod' - Interpolation method.
            %                   Default is 'linear';
            %            'EpsStep' - Default is 1e-8.
            % Output : - A structure with the radiao profile, including the following
            %            fields:
            %            .Radius - radius
            %            .Sum    -sum
            %            .Npix   - number of pixels in annulus
            %            .Mean   - mean
            %            .Med    - median
            %            .CumSum - cumulative sum.
            %          - Column vector of RadHalfCumSum, radius of cumsum=level
            %          - Column vector of RadHalfPeak, radius of flux=level.
            % Example: AP = AstroPSF;
            %          AP.DataPSF = imUtil.kernel2.gauss;
            %          [Result, RadHalfCumSum, RadHalfPeak] = curve_of_growth(AP);
            
            arguments
                Obj
                Args.ReCenter(1,1) logical  = true;
                Args.CenterPSFxy            = [];
                Args.Step                   = 1;
                Args.Level                  = 0.5;
                Args.InterpMethod           = 'linear';
                Args.EpsStep                = 1e-8;
            end
            
            % @FIX - @Eran
            if Args.ReCenter
                % use 1st moment to find PSF center
                M1 = moment2(Obj);
                Args.CenterPSFxy = [M1.X, M1.Y];
            end
            Nxy = size(Args.CenterPSFxy,1);
            
            Nobj = numel(Obj);
            RadHalfPeak   = nan(Nobj,1);
            RadHalfCumSum = nan(Nobj,1);
            for Iobj=1:1:Nobj
                Ixy = min(Iobj,Nxy);
                Result(Iobj)        = imUtil.psf.curve_of_growth(Obj(Iobj).Data, Args.CenterPSFxy(Ixy,:), Args.Step);
                N = numel(Result(Iobj).Radius);
                % EpsVec is needed in order to insure monotonicity
                EpsVec = (1:1:N)'.*Args.EpsStep;
                RadHalfCumSum(Iobj) = interp1(Result(Iobj).CumSum + EpsVec, Result(Iobj).Radius, Args.Level, Args.InterpMethod);
                RadHalfPeak(Iobj)   = interp1(Result(Iobj).Med./max(Result(Iobj).Med)-EpsVec, Result(Iobj).Radius, Args.Level, Args.InterpMethod);
            end
            
        end
        
        
        function [varargout] = moment2(Obj, Args)
            % Calculate the moments and perture photometry of PSFs
            %   using the imUtil.image.moment2 function.
            % Input  : - An AstroPSF object in which all the PSFs have the
            %            same size.
            %          * Pairs of ...,key,val,... The following keywords are available:
            %            'moment2Args' - A cell array of arguments to pass
            %                   to imUtiul.image.moment2. Default is {}.
            % Output  : - First moment information.
            %             A structure with the following fields.
            %             .RoundX - Vector of roundex X position
            %             .RoundY - Vector of roundex Y position
            %             .DeltaLastX - Vector of the X shifts in the last position
            %                           iteration.
            %             .DeltaLastY - Vector of the Y shifts in the last position
            %                           iteration.
            %             .Iter - Number of position iterations.
            %             .X    - 1st moment X position
            %             .Y    - 1st moment Y position.
            %             .Xstart - Starting X position,
            %             .Ystart - Starting Y position.
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
            %             .BoxPhot  - Vector of the full box photometry
            %             .AnnulusBack - Annulus background.
            %             .AnnulusStd - Annulus StD.
            %             .WeightedAper - Weighted photometry. Weighted by the user
            %                           specified weight function.
            % Example: AP = AstroPSF;
            %          AP.DataPSF = imUtil.kernel2.gauss;
            %          AP(2).DataPSF = imUtil.kernel2.gauss;
            %          [M1,M2,Aper] = moment2(AP);
            
            arguments
                Obj
                Args.moment2Args cell          = {};
                
            end
            
            Cube     = images2cube(Obj);
            SizeCube = size(Cube);
            X = (SizeCube(2)-1).*0.5;
            Y = (SizeCube(1)-1).*0.5;
            
            [varargout{1:nargout}] = imUtil.image.moment2(Cube, X, Y, Args.moment2Args{:});
            
        end
        
        
        function [FWHM_CumSum, FWHM_Flux] = fwhm(Obj, Args)
            % Calculate the FWHM of a PSF using the curve of growth
            %   (for alternative method use moment2).
            % Input  : - An AstroPSF object.
            %          * ...,key,vall,...
            %            'curvePars' - A cell array of key,val arguments to
            %                   pass to curve_of_growth. Default is {}.
            % Output : - The FWHM calculated from the half cumsum [pix]
            %          - The FWHM calculated from the half peak flux [pix]
            %            radius.
            % Author : Eran Ofek (May 2021)
            % Example: [FWHM_CumSum, FWHM_Flux] = fwhm(AP);
            
           
            arguments
                Obj
                Args.curvePars cell     = {};
            end
            
            [~, FWHM_CumSum, FWHM_Flux] = curve_of_growth(Obj, Args.curvePars{:});
            FWHM_CumSum = 2.*FWHM_CumSum;
            FWHM_Flux   = 2.*FWHM_Flux;
            
        end
        
%         function fitGaussians
%
%         end
    end
    
    methods % functionality
    
        function [CubeData, CubeVar] = images2cube(Obj)
            % Transform an array of AstroPSF into a cube of PSFs
            % Input  : - An AstroPSF object.
            %            The Data size in all the elements must be the
            %            same.
            % Output : - A cube of PSF, where the PSF index is in the 3rd
            %            dimension.
            %          - A cube of PSF variances, where the PSF index is in the 3rd
            %            dimension.
            % Author : Eran Ofek
            % Example: [CubeData, CubeVar] = images2cube(Obj)
           
            Nobj = numel(Obj);
            [Ny, Nx] = size(Obj(1).Data);
            CubeData = zeros(Ny, Nx, Nobj);
            if nargout>1
                CubeVar = zeros(Ny, Nx, Nobj);
            end
            for Iobj=1:1:Nobj
                CubeData(:,:,Iobj) = Obj(Iobj).Data;
                if nargout>1
                    if ~isempty(Obj(Iobj).Var)
                        CubeVar(:,:,Iobj) = Obj(Iobj).Var;
                    end
                end
            end
            
        end
        
        function Result = funUnary(Obj, OperatorOperatorArgs, OutType, DataProp, DataPropOut)
            %
           
            Nobj = numel(Obj)
            
            
        end
        
%         function selectPSF
%
%         end
        
%         function combinePSF
%
%         end

%         function fit
%
%         end
        
%         function pad
%
%         end

    end
    
    methods % plotting
        function surface(Obj)
            % plot PSF using surface
            % Input  : - A single element AstroPSF object
            % Author : Eran Ofek (Jan 2022)
            % Example: AI.PSFData.surface
            
            Data = Obj.getPSF;
            surface(Data);
            colorbar;
        end
        
    end

    methods (Static) % UnitTest
        Result = unitTest()
            % unitTest for AstroPSF
    end
    

end

           
