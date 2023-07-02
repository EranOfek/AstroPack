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
        
        Nstars            = NaN;  % If Nstars=NaN, then PSF wasn't constructed yet
        
        ScaleY            = 1;    % will be employed if pixel Scale X ~= Scale Y
        
        DataPSF2          = [];  % experimental
        DimDef2  cell     = {'WaveMono', 'WaveBand', 'WaveTemp', 'WaveSpType', 'WaveColor',...
                             'PosR', 'PosX', 'PosY', 'PixPhaseX', 'PixPhaseY'};
        DimAxis2 cell     = cell(1,10);       
        
        DataPSF3          = []; % experimental
        DimDef3  cell     = {'Wave', 'PosXR', 'PosY', 'PixPhaseX', 'PixPhaseY'};
        DimAxis3 cell     = cell(1,5);       
                
        FWHM              = [];  % for each of the points in the DimDef space ?
        ContainmentR      = [];  % for each of the points in the DimDef space ?
        
    end
    
    methods % Constructor
       
        function Obj = AstroPSF(FileName, Args)
            % AstroPSF constructor - read PSF images to AstroPSF object
            % Input  : - File names.
            %            Either an AstroImage, ImageComponent, SIM, imCl
            %            objects from which the image property will be
            %            stored in the DataPSF property of the AstroPSF
            %            object.
            %            Alternatively, a file name or a cell array of file
            %            names to read into the AstroPSF object.
            %            Or a matrix containing the PSF.
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
                    elseif ischar(File)
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
                    else
                        % matrix format
                        Obj.(FieldName) = File;
                    end
                    Obj.DataType = AstroDataType.PSF;
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
    
    methods % fitting
        function [Result,FitRes] = fitFunPSF(Obj, Args)
            % Fit a composite function to a PSF stamp and replace it.
            %   The fitted function is any combination of imUtil.kernel2 like
            %   functions. The function center is not fitted, and the free
            %   parameters are the normalization of each function, followed by the
            %   function parameters.
            % Input  : - A PSF stamp.
            %          * ...,key,val,...
            %            'getPSFArgs' - A cell array of arguments to pass
            %                   to the AStroPSF/getPSF method.
            %                   Default is {}.
            %            'Funs' - A cell array of functions to fit.
            %                   Each function in the cell is of the form:
            %                   PSF = Fun(Pars, SizeXY, PosXY), where PosXY=[]
            %                   return the stamp center.
            %                   Default is {@imUtil.kernel2.gauss}
            %            'Par0' - A cell array of initial (guess) parameters for
            %                   each one of the functions in 'Funs'.
            %                   Default is {[2 2 0]}.
            %            'Norm0' - A vector of normalizations, one per each function in
            %                   'Funs'. Default is [1].
            %            'PosXY' - The position of the functions center.
            %                   If empty, use stamp center.
            %                   Default is [].
            %            'LB' - Lower bound for all free parameters in the order:
            %                   [NormFun1, ParsFun1, NormFun2, ParsFun2,...]
            %                   Default is [].
            %            'UB' - Like 'LB', but for the upper bounds.
            %                   Default is [].
            %            'CreateNewObj' - A logical indicating if to create
            %                   a new copy of the input object.
            %                   Default is false.
            % Output : - An AstroPSF object in which the PSFData was
            %            replaced with a fitted version of the PSF stamp.
            %          - A structure with the following fields:
            %            .Par - Best fitted parameters.
            %            .ResNorm - RMS of best fit.
            %            .Resid - Observed - Calculated residuals (note that lsqcurve
            %               returns the calc-obs).
            %            .ExitFlag - Exit flag of lsqcurvefit
            %            .Output - Additional output of lsqcurvefit
            %            .J - Jacobian.
            % Author : Eran Ofek (Jun 2023)
    
            arguments
                Obj
                Args.getPSFArgs = {};
                Args.Funs      = {@imUtil.kernel2.gauss};
                Args.Par0      = {[2 2 0]};
                Args.Norm0     = [1];
                Args.PosXY     = [];
                Args.LB        = [];
                Args.UB        = [];
                Args.CreateNewObj logical = false;
            end
       
            if Args.CreateNewObj
                Result = Obj.copy;
            else
                Result = Obj;
            end
            
            Nobj = numel(Obj);
            for Iobj=1:1:Nobj
                P = Obj(Iobj).getPSF(Args.getPSFArgs{:});
                
                [FitRes(Iobj), Result(Iobj).PSFData] = imUtil.psf.fitFunPSF(P, 'Funs',Args.Funs,...
                                            'Par0',Args.Par0,...
                                            'Norm0',Args.Norm0,...
                                            'PosXY',Args.PosXY,...
                                            'LB',Args.LB,...
                                            'UB',Args.UB);
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
            % Calculate the moments and aperture photometry of PSFs
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
            
            [varargout{1:nargout}] = imUtil.image.moment2(Cube, X, Y, Args.moment2Args{:}, 'SubBack',false);
            
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
        
    end
    
    methods % pad, shift, smooth edges
        function Result = padShift(Obj, NewSizeIJ, Args)
            % Pad a PSF with zeros and shift its center
            %   This function uses: imUtil.psf.padShift
            % Input  : - self.
            %          - The [I, J] size of the the required output zero
            %            padded PSF.
            %          * ...,key,val,...
            %            'fftshift' - One of the following options:
            %                   'fftshift' - apply fftshift to the result
            %                   'ifftshift' - apply ifftshift to the result.
            %                   'none' - Returned centered PSF.
            %                   Default is 'none'.
            %            'OutType' - The output class:
            %                   'AstroPSF' - Return an updated AstroPSF
            %                       object.
            %                   'cube' - Return a cube of PSFs in which the
            %                       3rd index corresponds to the PSF index
            %                       (i.e., element in input AstroPSF).
            %                   Default is 'AstroPSF'.
            %            'CreateNewObj' - A logical indicating if to create
            %                   a new copy of the input object.
            %                   Default is true.
            % Output : - An updated AstroPSF object.
            % Example: R=AI.PSFData.padShift([100,100]);
            %          % full example:
            %          P=AstroPSF(imUtil.kernel2.gauss);
            %          R = P.padShift([30 30]);
            %          imUtil.image.moment2(R.Data,15 ,15)

            arguments
                Obj
                NewSizeIJ
                Args.fftshift    = 'none';
                Args.OutType     = 'AstroPSF';
                Args.CreateNewObj logical = true;
            end

            switch lower(Args.OutType)
                case 'astropsf'
                    if Args.CreateNewObj
                        Result = Obj.copy;
                    else
                        Result = Obj;
                    end
                case 'cube'
                    Result = zeros([NewSizeIJ, numel(Obj)]);
                otherwise
                    error('Unknown OutType option');
            end
            
            Nobj = numel(Obj);
            for Iobj=1:1:Nobj
                switch lower(Args.OutType)
                    case 'astropsf'
                        Result(Iobj).Data = imUtil.psf.padShift(Obj(Iobj).getPSF, NewSizeIJ, 'fftshift',Args.fftshift);
                    case 'cube'
                        Result(:,:,Iobj) = imUtil.psf.padShift(Obj(Iobj).getPSF, NewSizeIJ, 'fftshift',Args.fftshift);
                end
            end
        end

        function Result = full2stamp(Obj, Args)
            % Given a PSF contained in a full-size image, generate a stamp of the PSF.
            % Input  : - An AstroPSF object containing a full-size image matrix or cube of PSFs. If a cube, then
            %            the image index must be in the 3rd dimension.
            %          * ...,key,val,...
            %            'StampHalfSize' - Output stamp half size in [X,Y].
            %                   Default is [7 7] (i.e., stamp will be 15 by 15).
            %            'IsCorner' - A logical indicating if the PSF is in the
            %                   image corner (true) or center (false) in the input
            %                   full-size image.
            %                   Default is true.
            %            'Recenter' - Recenter the PSF using 1st moment estimation.
            %                   Default is false (NOT AVAILABLE).
            %            'zeroConv' - A logical indicating if to call the imUtil.psf.psf_zeroConverge
            %                   in order to smooth the edges of the PSF.
            %                   Default is true.
            %            'zeroConvArgs' - A cell array of arguments to pass to
            %                   imUtil.psf.psf_zeroConverge
            %                   Default is {}.
            %            'Norm' - Normalize the PSF stamp by this value.
            %                   If true, then will normalize the PSF by its sum
            %                   (such that integral will be 1).
            %                   Default is true.
            %            'CreateNewObj' - A logical indicating if to create
            %                   a new copy of the input object.
            %                   Default is false.
            % Output : - An AstroPSF object with the updated PSF.
            %            A PSF (centered) in a stamp.
            % Author : Eran Ofek (Jun 2023)
            % Example: 

            arguments
                Obj
                Args.StampHalfSize        = [7 7];   % [X, Y]
                Args.IsCorner logical     = true;
                Args.Recenter logical     = false;
                Args.zeroConv logical     = true;
                Args.zeroConvArgs cell    = {};
                Args.Norm                 = true;
                Args.CreateNewObj logical = false;
            end
            
            if Args.CreateNewObj
                Result = Obj.copy;
            else
                Result = Obj;
            end
            
            Nobj = numel(Obj);
            for Iobj=1:1:Nobj
                Result(Iobj).Data = imUtil.psf.full2stamp(Obj(Iobj).Data, 'StampHalfSize',Args.StampHalfSize,...
                                                                         'IsCorner',Args.IsCorner,...
                                                                         'Recenter',Args.Recenter,...
                                                                         'zeroConv',Args.zeroConv,...
                                                                         'zeroConvArgs',Args.zeroConvArgs,...
                                                                         'Norm',Args.Norm);
            end
            
        end
        
        function Result=suppressEdges(Obj, Args)
            % Multiply the PSF by edge suppressing function (e.g., cosbell).
            %   Useful in order to vrify that the PSF is zero padded and
            %   approach zero smoothly.
            % Input  : - An AstroPSF object.
            %          * ...,key,val,...
            %            'Fun' - A 2-D function that will multiply the PSF.
            %                   The function is of the form F(Pars, SizeXY)
            %                   Default is @imUtil.kernel2.cosbell
            %            'FunPars' - Vector of parameters that will be
            %                   passed as the first argument to the Fun.
            %                   Default is 5 7
            %            'MultVar' - Multiply also the DataVar property.
            %                   Default is false.
            %            'Norm' - A logical indicating if to normalize the
            %                   sum of the PSF to 1.
            %                   Default is true.
            %            'CreateNewObj' - A logical indicating if to create
            %                   a new copy of the input object.
            %                   Default is false.
            % Output : - A corrected AstroPSF object.
            % Author : Eran Ofek (Jun 2023)
            % Example: P.suppressEdges;
            
            arguments
                Obj
                Args.Fun                     = @imUtil.kernel2.cosbell;
                Args.FunPars                 = [5 7];
                Args.MultVar logical         = false;
                Args.Norm logical            = true;
                Args.CreateNewObj logical    = false;                
            end
            
            if Args.CreateNewObj
                Result = Obj.copy;
            else
                Result = Obj;
            end
            
            Nobj = numel(Obj);
            Size = size(Result(1).DataPSF);
            Fun  = Args.Fun(Args.FunPars, [Size(2) Size(1)]);
            for Iobj=1:1:Nobj
                Result(Iobj).DataPSF  = Result(Iobj).DataPSF .* Fun;
                if Args.Norm
                    Result(Iobj).normPSF;
                end
                if Args.MultVar
                    Result(Iobj).DataVar  = Result(Iobj).DataVar .* Fun;
                end
            end
            
        end

        function Obj=normPSF(Obj)
            % Normalize PSFs such there sum will be 1.
            % Input  : - An AstroPSF object.
            % Output : - An uppdated AstroPSF object (no new copy).
            % Author : Eran Ofek (Jun 2023)

            Nobj = numel(Obj);
            for Iobj=1:1:Nobj
                Obj(Iobj).DataPSF = Obj(Iobj).DataPSF./ sum(Obj(Iobj).DataPSF,[1 2]);
            end
        end
        
        function Obj=even2odd(Obj, Args)
            % Rescale a PSF so that the stamp size becomes odd in both directions
            % Input  : - An AstroPSF object
            % Output : - An updated AstroPSF object with the stamp rescaled
            %            and the scale factors changed appropriately
            % Author : A. Krassilchtchikov (Jun 2023)
            arguments
                Obj
                Args.Method = 'bilinear'; % interpolation method 
            end
            
            Nobj = numel(Obj);
            for Iobj=1:1:Nobj
                SizeX = size(Obj(Iobj).DataPSF,1);
                SizeY = size(Obj(Iobj).DataPSF,2);
                if mod( SizeX, 2 ) == 0 && mod( SizeY, 2 ) == 0
                    Fx = (SizeX + 1) / SizeX; Fy = (SizeY + 1) / SizeY;
                    Obj(Iobj).DataPSF = (1/(Fx*Fy)) .* imresize( Obj(Iobj).DataPSF, [Fx*SizeX Fy*SizeY], 'Method', Args.Method);
                elseif mod( SizeX, 2 ) == 0 && mod( SizeY, 2 ) == 1
                    Fx = (SizeX + 1) / SizeX; Fy = 1;
                    Obj(Iobj).DataPSF = (1/(Fx*Fy)) .* imresize( Obj(Iobj).DataPSF, [Fx*SizeX Fy*SizeY], 'Method', Args.Method);
                elseif mod( SizeX, 2 ) == 1 && mod( SizeY, 2 ) == 1
                    Fx = 1;                   Fy = (SizeY + 1) / SizeY;
                    Obj(Iobj).DataPSF = (1/(Fx*Fy)) .* imresize( Obj(Iobj).DataPSF, [Fx*SizeX Fy*SizeY], 'Method', Args.Method);                    
                else
                    Fx = 1;                   Fy = 1;
                end
                Obj(Iobj).Scale   = Obj(Iobj).Scale * Fx; 
                Obj(Iobj).ScaleY  = Obj(Iobj).Scale * Fy; 
            end
            
        end

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

           
