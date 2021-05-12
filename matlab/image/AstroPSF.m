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
        FunPSF            = [];   % e.g., Map = Fun(Data, X,Y, Color, Flux)
        ArgVals cell      = {};
        ArgNames cell     = {'X','Y','Color','Flux'};
        StampSize         = [];
    end
    
    methods % Constructor
       
        function Obj = AstroPSF
            
            
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
        
        function [M1, M2, Aper] = moment2(Obj, Args)
            % Calculate the moments and perture photometry of PSFs
            %   using the imUtil.image.moment2 function.
            % Input  : - An AstroPSF object in which all the PSFs have the
            %            same size.
            %          * Pairs of ...,key,val,... The following keywords are available:
            %            'AperRadius' - Vector of aperture radii, in which to calculate
            %                       aperture photometry.
            %                       Default is [2 4 6].
            %            'Annulus' - Vector of inner and outer radius of background
            %                       annulus. Default is [8 12].
            %            'BackFun' - Function handle to use for background estimation.
            %                       In order to meaningful this function must ignore
            %                       NaNs.
            %                       Default is @nanmedian.
            %            'MomRadius' - Radius around position in which to calculate the
            %                       moments. Recomended ~1.7 FWHM. Default is 8.
            %            'WeightFun' - The weight function to use for weighting the
            %                       moments and the weighted photometry.
            %                       This can be a scalar indicating the sigma of a
            %                       circularly symmetric Gaussian,
            %                       or a function handle that matrix of radii, and
            %                       return a matrix of weights (e.g., @(r)
            %                       exp(-r.^2./(2.*4))./(2.*pi.*4.^2); ).
            %                       Default is 2.
            %            'Circle' - A flag indicating if to extract the stamps with
            %                       circular shape. Default is false.
            %            'MaxIter' - Maximum number of 1st moment position iterations.
            %                       0 will perform aingle 1st moment calculation.
            %                       Default is 10.
            %            'NoWeightFirstIter' - A flag indicating if not to apply weight
            %                       on the first itearation. Default is true.
            %            'PosConvergence' - Position convergence. Default is 1e-4.
            %            'DynamicWindow' - Apply dynamic windowing. Default is true.
            %            'WindowOnlyOnLastIter' -Default is false.
            %            'FinalIterWithCorrectWin' - Apply an additional final
            %                       iteration with the correct window.
            %            'mexCutout' - use imUtil.image.mexCutout.m (true) or
            %                       imUtil.image.find_within_radius_mat (false).
            %                       Default is true.
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
                Args.AperRadius   {mustBeNumeric(Args.AperRadius)} = [2 4 6];
                Args.Annulus      {mustBeNumeric(Args.Annulus)}    = [8 12];
                Args.BackFun                                       = @nanmedian
                Args.MomRadius    {mustBeNumeric(Args.MomRadius)}  = 8;    % recomended ~1.7 FWHM
                Args.WeightFun                                     = 2;    % sigma or function: @(r) exp(-r.^2./(2.*4))./(2.*pi.*4.^2);
                Args.Circle(1,1) logical                           = false;
                Args.MaxIter      {mustBeNumeric(Args.MaxIter)}    = 10;
                Args.NoWeightFirstIter(1,1) logical                = true; 
                Args.PosConvergence                                = 1e-4;
                Args.DynamicWindow(1,1) logical                    = true;
                Args.WindowOnlyOnLastIter(1,1) logical             = false;
                Args.FinalIterWithCorrectWin(1,1) logical          = true;
                Args.mexCutout(1,1) logical                        = true;
            end
            
            Cube     = images2cube(Obj);
            SizeCube = size(Cube);
            X = (SizeCube(2)-1).*0.5;
            Y = (SizeCube(1)-1).*0.5;
            
            KeyVal = namedargs2cell(Args);
            [M1, M2, Aper] = imUtil.image.moment2(Cube, X, Y, KeyVal{:});
            
        end
        
        function [FWHM_CumSum, FWHM_Flux] = fwhm(Obj, Args)
            % Calculate the FWHM of a PSF using the curve of growth
            %   (for alternative method use moment2).
            % Input  : - An AstroPSF object.
            %          * ...,key,vall,...
            %            'curvePars' - A cell array of key,val arguments to
            %                   pass to curve_of_growth. Default is {}.
            % Output : - The FWHM calculated from the half cumsum.
            %          - The FWHM calculated from the half peak flux
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
        
        function Result = fun_unary(Obj, OperatorOperatorArgs, OutType, DataProp, DataPropOut)
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
    

    methods (Static) % UnitTest
        function Result = unitTest()
            % unitTest for AstroPSF
            % Example: Result = AstroPSF.unitTest
            
            % getPSF
            AP = AstroPSF;
            P = imUtil.kernel2.gauss;
            AP.DataPSF = P;
            if ~all(AP.getPSF==P)
                error('Problem with set/get PSF');
            end
            
            % curve of growth
            AP = AstroPSF;
            AP.DataPSF = imUtil.kernel2.gauss;
            [Result, RadHalfCumSum, RadHalfPeak] = curve_of_growth(AP);

            % images2cube
            AP = AstroPSF;
            AP.DataPSF = imUtil.kernel2.gauss;
            AP(2).DataPSF = imUtil.kernel2.gauss;
            [Cube, CubeVar] = images2cube(AP)
            
            % moments
            AP = AstroPSF;
            AP.DataPSF = imUtil.kernel2.gauss;
            AP(2).DataPSF = imUtil.kernel2.gauss;
            [M1,M2,Aper] = moment2(AP);

            % fwhm
            [FWHM_CumSum, FWHM_Flux] = fwhm(AP);
            
            Result = true;
        end
    end
    

end

            
