% AstroPSF - A container class for PSFs
% Properties :
%       Data - A dependent property that generates the PSF stamp
%       Var -  A dependent property that generates the PSF variance stamp
%       DataPSF - A PSF data. Stamp, or function parameters
%       DataVar - A PSF variance data. Stamp, or function parameters
%       FunPSF - A PSF function handle
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
%
% P = AstroPSF
% [PSF, Var] = P.getPSF()  % return some defaut
% [PSF, Var] = P.getPSF('Flux',Val, 'Wave',5000)
% [PSF, Var] = P.weightPSF('Flux',Val, 'Wave',[5000 5500 6000],'Spec',[0.5 1 0.5])

% [PSF, Var] = P.weightPSF([], 'InterpMethod','nearest'); % use vals from properties

% AstroPSF = P.repopPSF('Wave',[5000 5500 6000],'WaveWeight',[0.5 1 0.5])
% ValPerPSF = fwhm(P)
% [ValX, ValY] = P.fwhm(Method=[], 'Flux',Val, 'Wave',[5000])

classdef AstroPSF < Component
    properties (Dependent) % Access image data directly
        Data
        Var
    end
    
    properties (SetAccess = public)
        DataPSF           = [];    % parameters of a PSF-generating function or a data cube, where the first 2 dimensions are the PSF image stamp (X, Y)
        DataVar           = [];    % variance 
        Oversampling      = [1 1]; % pixel oversampling in X and Y (may be different) 
        FunPSF            = [];    % PSF-generating function, e.g., Map = Fun(Data, X,Y, Color, Flux)
        DimName cell      = {'Wave', 'PosX', 'PosY', 'PixPhaseX', 'PixPhaseY'}; % the standard set of dimensions, but may be changed 
                            % NB: if the names here are changed, the dimension names a user provides to getPSF need to be changed accordingly 
        DimVals cell      = repmat({0}, 1, 5); % ADD x/y values with oversampling...  axes according to DimName
        InterpMethod      = {'nearest'}; % can be n-dimensional with different methods applied at different dimensions        
        StampSize         = [];     % PSF stamp size in X and Y (do we need it as a property?)
        FWHM              = [];     % can be defined for some "average" stamp
        FluxContainmentRadius = []; % can be defined for some "average" stamp  
        Nstars            = NaN;    % if Nstars=NaN, then PSF wasn't constructed yet 
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
    
    methods % generating PSF stamp 
        
        function [Result, Res] = getPSF(Obj, Args)
            % get a PSF stamp (or a matrix of stamps) from an AstroPSF object
            % The position(s) of the desired stamp(s) in the multi-D space of Obj.DataPSF
            % is determined or by the input coordinates from Args.PsfArgs
            % or the mean values of the object's DimVals are taken for each
            % of the dimensions. The Obj.DataPSF cube is interpolated to
            % the requested positions according to the Args.InterpMethod method(s)
            %
            % Input : - An AstroPSF object (or a matrix of objects) 
            %         * ...,key,val,...
            %         'FunPSF' - a PSF-generating function handle
            %         'StampSize' - an option to pad the PSF stamp 
            %         'fftpshift' - if padding is requested, whether to perform fft shift: 
            %                       'none' (default),'fftshift','ifftshift'
            %         'PsfArgs'   - desired position of the stamp in the multi-D space of PSF.DataPSF:
            %                       a cell array of values (or value vectors) corresponding 
            %                       to each of the dimensions of PSF.DataPSF 
            %                       NB: the dynamic dimension names are stored in the DimName cell array
            %         'FunArgs'   - optinal arguments to pass to FunPSF
            %         'InterpMethod' - interpolation method (may be a cell array with different methods for each dimension)
            %         'Oversampling' - resample the output stamp to this value (if not empty) 
            %         'ReNorm'       - whether to renormalize the output PSF stamp
            %         'ReNormMethod' - 'int' or 'rms' 
            % Output : - a 2D PSF stamp (X, Y) or a stack of stamps if a vector of objects or parameters is put in 
            % Author : Eran Ofek, A.M. Krassilchtchikov (Oct 2023)
            % Example: AP = AstroPSF; AP.DataPSF = imUtil.kernel2.gauss; P1 = AP.getPSF;
            %          for more complex examples see AstroPSF.unitTest
            arguments
                %                 Obj(1,1)
                Obj
                Args.FunPSF         = [];
                Args.StampSize      = [];     % if Args.StampSize > size(Result), pad the stamp with 0s
                Args.fftshift       = 'none'; % perform fftshift when padding ('none','fftshift','ifftshift')
                Args.PsfArgs        = {};    % Example: {'Wave', 2800, 'PosX', [2 3]'}
                Args.FunArgs        = {};
                Args.InterpMethod   = [];
                Args.Oversampling   = [];
                Args.ReNorm logical = true;
                Args.ReNormMethod   = 'int';  % 'int' | 'rms'
            end
            
            for IObj = 1:numel(Obj)
                
                if isempty(Args.FunPSF)
                    Args.FunPSF  = Obj(IObj).FunPSF;
                else
                    Obj(IObj).FunPSF = Args.FunPSF;
                end
%                 if isempty(Args.StampSize) % what for?
%                     Args.StampSize = Obj(IObj).StampSize;
%                 else
%                     Obj(IObj).StampSize = Args.StampSize;
%                 end
                if ~isempty(Args.InterpMethod)
                    if ~iscell(Args.InterpMethod)
                        IntMeth = {Args.InterpMethod};
                    else
                        IntMeth = Args.InterpMethod;
                    end
                else
                    IntMeth = Obj(IObj).InterpMethod;
                end
                
                if isempty(Args.FunPSF) % treat PSF as a multidimentional image stamp
                    Ndim = ndims(Obj(IObj).DataPSF)-2; % the number of additional data dimensions in the object
                    if Ndim == 0 % no additional dimensions, just copy the 2D matrix
                        Result = Obj(IObj).DataPSF;
                    else
                        % for each of the existing extra dimensions find if there is an input value for it in Args.PsfArgs
                        % if not, use the _mean value_ of the object's appropriate DimVals vector
                        DimVal = cell(Ndim,1);
                        for Idim = 1:Ndim
                            DName = Obj(IObj).DimName{Idim};
                            Ind = find( strcmpi( DName, Args.PsfArgs ), 1);
                            if isempty(Ind)
                                DimVal{Idim} = ( Obj(IObj).DimVals{Idim}(1) + Obj(IObj).DimVals{Idim}( numel(Obj(IObj).DimVals{Idim}) ) ) / 2.;
                            else
                                DimVal{Idim} = Args.PsfArgs{Ind+1};
                            end
                        end
                        % interpolate
                        X = 1:size(Obj(IObj).DataPSF,1); Y = 1:size(Obj(IObj).DataPSF,2);
                        if numel(IntMeth) == 1 % one method for all the dimensions
                            Result = interpn(X,Y, Obj(IObj).DimVals{1:Ndim}, Obj(IObj).DataPSF, ...
                                X,Y, DimVal{1:Ndim}, IntMeth{1});
                        else  % individual method for each dimension                           
                            Int = cell(Ndim+1,1);
                            Int{1} = Obj(IObj).DataPSF;                             
                            for Idim = 1:Ndim
                                Int1 = squeeze(Int{Idim});
                                Int{Idim+1} = interpn(X,Y, Obj(IObj).DimVals{Idim:Ndim}, Int1, ...
                                    X,Y, DimVal{Idim}, Obj(IObj).DimVals{Idim+1:Ndim}, IntMeth{Idim});
                            end
                            Result = Int{Ndim+1};
                        end
                    end
                else % pass the PSF cube to the FunPSF function
                    Result = Args.FunPSF(Obj(IObj).DataPSF, Args.FunArgs{:});
                end                
                % resample the output PSF stamp 
                if ~isempty(Args.Oversampling)
                    % will renorm at the next step, so do not need to renorm once more here
                    Result = imUtil.psf.oversampling(Result, Obj(IObj).Oversampling, Args.Oversampling, 'ReNorm', false);
                end
                % normalize the stamp 
                if Args.ReNorm
                    Result = imUtil.psf.normPSF(Result,'ReNormMethod',Args.ReNormMethod);
                end
                % pad and shift the stamp (if there are no additional dimensions)
                if ~isempty(Args.StampSize) && Ndim == 0  % 
                    if ~all(size(Result)==Args.StampSize) % pad PSF                        
%                         error('Pad PSF option is not yet available');
                        Result = imUtil.psf.padShift(Result, Args.StampSize, 'fftshift', Args.fftshift);
                    end
                end
            Res{IObj} = Result;    
            end
        end

        function Result = specWeightedPSF(Obj, Args)
            % produce a spectrum-weighted PSF of a single-element AstroPSF object
            % Input  : - a single-element AstroPSF object
            %       * ...,key,val,... 
            %       'Wave' - the wavelength of the input spectral bins (if empty, the grid of the object's PSFdata is assumed)
            %       'Spec' - the spectral weights of per-wavelength PSF stamps (if empty, a flat photon spectrum is assumed)
            %       'Pos'  - additional arguments to pass to getPSF, e.g., position: {'PosX',2,'PosY',3}
            %
            % Output : - a weighted PSF stamp
            % Author : A. Krassilchtchikov (Oct 2023)
            % Example: Pw1 = P.specWeightedPSF('Pos',{'PosX',6},'Wave',[2000 3000 4000 5000],'Spec',[0.5 1 1 0.3]);
            %          Sp  = AstroSpec.blackBody(2000:11000,3500);
            %          Pw2 = P.specWeightedPSF('Pos',{'PosX',6},'Wave',Sp.Wave,'Spec',Sp.Flux');
            arguments
                Obj(1,1)
                Args.Axis  = 'Wave'; % usually, we will weight the PSF with the spectrum, but other axes are also possible 
                Args.Wave  = []; % if empty, the grid of the object's PSFdata is assumed
                Args.Spec  = []; % if empty, a flat photon spectrum is assumed
                Args.Pos   = {}; % additional arguments to pass to getPSF, e.g., position: {'PosX',2,'PosY',3} 
            end
            
            Tiny = 1e-30; 
            
            Ind = find( strcmpi( Args.Axis, Obj.DimName ), 1);      % find the required axis in the object's dimensions
            if isempty(Ind)
                error('No wavelength axis found in the object');
            else
                Wave = Obj.DimVals{Ind};
                if ~isempty(Args.Wave) % regrid the spectrum according to the object's wavelength axis
                    Spec = interp1(Args.Wave, Args.Spec, Wave,'linear',Tiny);                     
                else                   % assume that the spectrum is defined according to the object's wavelength axis                                        
                    if isempty(Args.Spec)
                        Spec = ones(1,numel(Wave)); % at an empty input, assume a flat photon spectrum
                    else
                        Spec = Args.Spec;
                    end
                end
            end
            
            PSFcube = Obj.getPSF('PsfArgs',[{Args.Axis, Wave} Args.Pos]); % get an X x Y x Wave 3D Cube ( PSF x Wave)
            SpShape = reshape(Spec,[1 1 numel(Spec)]);
            SumL    = sum( PSFcube .* SpShape, 3 );           % multiply and sum over the wavelength dimension
            Result  = SumL ./ sum( SumL, [1,2] );             % normalization
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
                Result(Iobj) = isempty(Obj(Iobj).DataPSF);
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
            %            'PsfArgs' - position in a multi-D PSF space to be passed to getPSF
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
                Args.PsfArgs   = {};
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
                P = Obj(Iobj).getPSF('PsfArgs',Args.PsfArgs);
                [FitRes(Iobj), Result(Iobj).DataPSF] = imUtil.psf.fitFunPSF(P, 'Funs',Args.Funs,...
                                            'Par0',Args.Par0,...
                                            'Norm0',Args.Norm0,...
                                            'PosXY',Args.PosXY,...
                                            'LB',Args.LB,...
                                            'UB',Args.UB);
                % as the resulting stamp is 2D, additional dimensions do not exist any more:
                Result(Iobj).DimVals = cellfun(@(x) [0], Result(Iobj).DimVals, 'UniformOutput', false);
            end
        end
        
    end
    
    methods % PSF properties
        
        function [Result, RadHalfCumSum, RadHalfPeak] = curve_of_growth(Obj, Args)
            % Calculate curve of growth of a PSF including radii
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
            %            'PsfArgs' - position in a multi-D PSF space to be passed to getPSF
            % Output : - A structure with the radiao profile, including the following
            %            fields:
            %            .Radius - radius
            %            .Sum    -sum
            %            .Npix   - number of pixels in annulus
            %            .Mean   - mean
            %            .Med    - median
            %            .CumSum - cumulative sum.
            %          - Column vector of RadHalfCumSum, radius of cumsum=level
            %            NaN if PSF is empty.
            %          - Column vector of RadHalfPeak, radius of flux=level.
            %            NaN if PSF is empty.
            % Author: Eran Ofek 
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
                Args.PsfArgs                = {};
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
            Result = struct('Radius',cell(Nobj,1), 'Sum',cell(Nobj,1), 'Npix',cell(Nobj,1), 'Mean',cell(Nobj,1), 'Med',cell(Nobj,1), 'CumSum',cell(Nobj,1));
            for Iobj=1:1:Nobj
                Ixy = min(Iobj,Nxy);
                P = Obj(Iobj).getPSF('PsfArgs',Args.PsfArgs); % get the stamp 
                if isempty(P)
                    RadHalfCumSum(Iobj) = NaN;
                    RadHalfPeak(Iobj)   = NaN;
                else
                    Result(Iobj)        = imUtil.psf.curve_of_growth(P, Args.CenterPSFxy(Ixy,:), Args.Step);
                    N = numel(Result(Iobj).Radius);
                    % EpsVec is needed in order to insure monotonicity
                    EpsVec = (1:1:N)'.*Args.EpsStep;
                    RadHalfCumSum(Iobj) = interp1(Result(Iobj).CumSum + EpsVec, Result(Iobj).Radius, Args.Level, Args.InterpMethod);
                    RadHalfPeak(Iobj)   = interp1(Result(Iobj).Med./max(Result(Iobj).Med)-EpsVec, Result(Iobj).Radius, Args.Level, Args.InterpMethod);
                end
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
            %            'PsfArgs' - position in a multi-D PSF space to be passed to getPSF
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
            % Author: Eran Ofek
            % Example: AP = AstroPSF;
            %          AP.DataPSF = imUtil.kernel2.gauss;
            %          AP(2).DataPSF = imUtil.kernel2.gauss;
            %          [M1,M2,Aper] = moment2(AP);
            
            arguments
                Obj
                Args.moment2Args cell          = {};
                Args.PsfArgs     cell          = {};
                
            end
            
            Cube     = images2cube(Obj,'PsfArgs',Args.PsfArgs);
            SizeCube = size(Cube);
            X = (SizeCube(2)-1).*0.5;
            Y = (SizeCube(1)-1).*0.5;
            
            [varargout{1:nargout}] = imUtil.image.moment2(Cube, X, Y, Args.moment2Args{:}, 'SubBack',false);
            
        end
        
        function [FWHM_CumSum, FWHM_Flux] = fwhm(Obj, Args)
            % Calculate the FWHM of a PSF using the curve of growth
            %   (for alternative method use moment2).
            % Input  : - An AstroPSF object.
            %          * ...,key,val,...
            %            'PsfArgs' - a cell array of key,val arguments to pass to curve_of_growth. Default is {}. 
            %            'curveArgs' - a cell array of additional arguments to be passed to curve_of_growth
            % Output : - The FWHM calculated from the half cumsum [pix]
            %          - The FWHM calculated from the half peak flux [pix]
            %            radius.
            % Author : Eran Ofek (May 2021)
            % Example: [FWHM_CumSum, FWHM_Flux] = fwhm(AP);
            
            arguments
                Obj
                Args.PsfArgs cell     = {};    
                Args.curveArgs cell   = {};
            end
            
            [~, FWHM_CumSum, FWHM_Flux] = curve_of_growth(Obj,'PsfArgs',Args.PsfArgs,Args.curveArgs{:});
            FWHM_CumSum = 2.*FWHM_CumSum;
            FWHM_Flux   = 2.*FWHM_Flux;
            
        end
        
        function [Radius, Val] = radialProfile(Obj,Args)
            % extract radial profiles from an AstroPSF object 
            % Input: - an AstroPSF object
            %        * ...,key,val,...
            %        'PsfArgs' - position in a multi-D PSF space to be passed to getPSF
            %        'Radius' - A radius up to which to calculate the radial
            %                   profile, or a vector of radius edges.
            %                   If empty, set it to the smallest image dim.
            %        'Step'   - Step size for radial edges. Default is 1
            %        'ReCenter' - a [Y, X] position around to calculate the radial profile.
            %                   If empty, use image center. Default is [].
            % Output: - the radial profile: a vector of radii R and a vector of Sum
            % Author: A.M. Krassilchtchikov (Oct 2023)
            % Example: AP = AstroPSF; AP.DataPSF = imUtil.kernel2.gauss;
            %          [R, V] = AP.radialProfile; 
            arguments
                Obj(1,1)
                Args.PsfArgs  = {};
                Args.Radius   = [];
                Args.Step     = 1;
                Args.ReCenter = [];
            end           
            Stamp  = Obj.getPSF('PsfArgs',Args.PsfArgs); % get the stamp
            Prof   = imUtil.psf.radialProfile(Stamp,Args.ReCenter,'Radius',Args.Radius,'Step',Args.Step);
            Radius = Prof.R; Val = Prof.Sum; 
        end
        
%         function fitGaussians
%
%         end
    end
    
    methods % functionality 
        
        function [CubeData, CubeVar] = images2cube(Obj, Args)
            % Transform an array of AstroPSF into a cube of PSFs
            % Input  : - An AstroPSF object.
            %            The Data size in all the elements must be the
            %            same.
            %            * ...,key,val,...
            %            'PsfArgs' - position in a multi-D PSF space to be passed to getPSF
            % Output : - A cube of PSF, where the PSF index is in the 3rd
            %            dimension.
            %          - A cube of PSF variances, where the PSF index is in the 3rd
            %            dimension.
            % Author : Eran Ofek
            % Example: [CubeData, CubeVar] = images2cube(Obj)
            arguments
                Obj
                Args.PsfArgs = {};
            end
           
            Nobj = numel(Obj);
            [Ny, Nx] = size(Obj(1).Data);
            CubeData = zeros(Ny, Nx, Nobj);
            if nargout>1
                CubeVar = zeros(Ny, Nx, Nobj);
            end
            for Iobj=1:1:Nobj
                P = Obj(Iobj).getPSF('PsfArgs',Args.PsfArgs); % get the stamp 
                CubeData(:,:,Iobj) = P;
                if nargout>1 % this is not yet done for multi-D PSF 
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
            % Author: Eran Ofek
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
            %            'PsfArgs' - position in a multi-D PSF space to be passed to getPSF
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
                Args.PsfArgs              = {};
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
                P = Obj.getPSF();
                Result(Iobj).DataPSF = imUtil.psf.full2stamp(P, 'StampHalfSize',Args.StampHalfSize,...
                                                                         'IsCorner',Args.IsCorner,...
                                                                         'Recenter',Args.Recenter,...
                                                                         'zeroConv',Args.zeroConv,...
                                                                         'zeroConvArgs',Args.zeroConvArgs,...
                                                                         'Norm',Args.Norm);
            % as the resulting stamp is 2D, additional dimensions do not exist any more:
            Result(Iobj).DimVals = cellfun(@(x) [0], Result(Iobj).DimVals, 'UniformOutput', false);
            end
        end
        
        function Result = suppressEdges(Obj, Args)
            % Multiply the PSF by edge suppressing function (e.g., cosbell).
            %   Useful in order to verify that the PSF is zero padded and
            %   approach zero smoothly.
            %   See also: imUtil.psf.suppressEdges                
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

        function Obj = normPSF(Obj,Args)
            % Normalize a PSF so that its sum is 1
            % Input  : - An AstroPSF object
            %        * ...,key,val,... 
            %        'ReNormMethod' - 'int' -- normalize to the sum of pixel values; 'rms' -- normalize to rms
            % Output : - An uppdated AstroPSF object (no new copy).
            % Author : Eran Ofek (Jun 2023)
            arguments
                Obj
                Args.ReNormMethod = 'int' % 'int' or 'rms' 
            end
            Nobj = numel(Obj);
            for Iobj=1:1:Nobj
                Obj(Iobj).DataPSF = imUtil.psf.normPSF(Obj(Iobj).DataPSF,'ReNormMethod',Args.ReNormMethod); 
            end
        end
        
        function Obj = even2odd(Obj, Args)
            % Rescale a PSF so that the stamp size becomes odd in both directions
            % Input  : - An AstroPSF object
            % Output : - An updated AstroPSF object with the stamp rescaled
            %            and the scale factors changed appropriately
            % Author : A.M. Krassilchtchikov (Jun 2023)
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
                Obj(Iobj).Oversampling(1)  = Obj(Iobj).Oversampling(1) * Fx; 
                Obj(Iobj).Oversampling(2)  = Obj(Iobj).Oversampling(2) * Fy; 
            end
            
        end
        
    end
    
    methods % plotting
        
        function surface(Obj, Args)
            % plot PSF using surface
            % Input  : - A single element AstroPSF object
            % Author : Eran Ofek (Jan 2022)
            % Input: - AstroPSF Object 
            %        * ...,key,val,...
            %        'PsfArgs' - position in a multi-D PSF space to be passed to getPSF
            % Author: Eran Ofek
            % Example: AI.PSFData.surface
            arguments
                Obj(1,1)
                Args.PsfArgs = {};
            end
            Stamp = Obj.getPSF('PsfArgs',Args.PsfArgs);
            surface(Stamp);
            colorbar;
        end
        
        function plotRadialProfile(Obj, Args)
            % plot radial profiles of the input AstroPSF objects
            % Input : - A stack of AstroPSF objects or a single object
            %       * ...,key,val,...
            %        'Radius' - A radius up to which to calculate the radial
            %                   profile, or a vector of radius edges.
            %                   If empty, set it to the smallest image dim.
            %        'Step'   - Step size for radial edges. Default is 1
            %        'ReCenter' - a [Y, X] position around to calculate the radial profile.
            %                   If empty, use image center. Default is [].
            %        'FigNum'  - number of the plot (def. to Figure 10)
            %        'PsfArgs' - position in a multi-D PSF space to be passed to getPSF
            % Output: - a figure with radial profiles of all the input objects
            % Author: A.M. Krassilchtchikov (Oct 2023)
            % Example: AP2(1) = AstroPSF; AP2(1).DataPSF = rand(15);
            %          AP2(2) = AstroPSF; AP2(2).DataPSF = imUtil.kernel2.gauss;
            %          AP2.plotRadialProfile;
            arguments
                Obj
                Args.Radius   = [];
                Args.Step     = 1;
                Args.ReCenter = [];
                Args.FigNum   = 10;
                Args.PsfArgs  = {};
            end
            figure(Args.FigNum); clf; hold on
            Nobj = numel(Obj);
            for Iobj=1:1:Nobj
                [Rad, Val] = Obj(Iobj).radialProfile('Radius',Args.Radius,'Step',Args.Step,'ReCenter',Args.ReCenter,'PsfArgs',Args.PsfArgs);
                plot(Rad, Val);
            end
            hold off
        end
        
    end
    
    methods (Static) % UnitTest
        Result = unitTest()
            % unitTest for AstroPSF
    end
    
end

           
