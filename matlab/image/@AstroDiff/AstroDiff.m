% Class for astronomical difference/subtraction images and transients data
%
%

classdef AstroDiff < AstroImage
    
    properties (Dependent)
      
    end

    properties
        New AstroImage
        Ref AstroImage
        IsRegistered logical   = false;
        
        S   % with IsFFT - do we need ImageComponent?
        Scorr 
        Z2sigma 

        %
        Fn
        Fr
        Fd
        BackN
        BackR
        VarN
        VarR

       
    end
    
    properties (Hidden)  % auxilary images
        ZeroPadRowsFFT   = [];
        ZeroPadColsFFT   = [];


        %FFT
        R_hat
        Pr_hat
        N_hat
        Pn_hat

        D_hat
        Pd_hat
        S_hat
        
        D_den_hat
        D_num_hat
        D_denSqrt_hat
        P_deltaNhat
        P_deltaRhat
        
        F_S
        
        Z2
        Zvec_hat
    end


    methods % constructor
        function Obj=AstroDiff(New, Ref)
            % Constructor for the AstroDiff class
            % Input  : - A New images. This can be any input that is valid
            %            for the AstroImage class.
            %            E.g., size, cell array of matrices, file names,
            %            AstroImage.
            %            Default is AstroImage(0).
            %          - Like New, but for the Ref image.
            %            Default is AstroImage(0).
            % Output : - An AstroDiff object.
            % Author : Eran Ofek (Jan 2024)
            % Example: AD = AstroDiff;
            %          AD = AstroDiff({randn(100,100)}, {randn(100,100)});
            %          AD = AstroDiff('LAST*.fits','LAST*1*.fits');
            
            arguments
                New    = AstroImage(0);
                Ref    = AstroImage(0);
            end
            
            if ~isa(New, 'AstroImage')
                New = AstroImage(New);
            end
            if ~isa(Ref, 'AstroImage')
                Ref = AstroImage(Ref);
            end
            
            Nn = numel(New);
            Nr = numel(Ref);
            Nmax = max(Nn, Nr);
            if Nn~=Nr && (Nr>1 && Nn>1)
                error('Number of New and Ref images must be comaptible');
            end
            
            for Imax=1:1:Nmax
                In = min(Imax, Nn);
                Ir = min(Imax, Nr);
                
                Obj(Imax).New = New(In);
                Obj(Imax).Ref = Ref(Ir);
            end
            
        end
    end
    
    methods % setters/getters
        function Val=get.R_hat(Obj)
            % getter for R_hat

            if isempty(Obj.R_hat)
                % R_hat is not available - calculate
                if Obj.Ref.isemptyImage
                    error('Ref image is not populated');
                else
                    Obj.R_hat = fft2(Obj.Ref.Image, Obj.ZeroPadRowsFFT, Obj.ZeroPadColsFFT);
                end
            else
                % R_hat is already available - use as is
            end
            Val = Obj.R_hat;
        end

        function Val=get.N_hat(Obj)
            % getter for N_hat

            if isempty(Obj.N_hat)
                % N_hat is not available - calculate
                if Obj.New.isemptyImage
                    error('New image is not populated');
                else
                    Obj.N_hat = fft2(Obj.New.Image, Obj.ZeroPadRowsFFT, Obj.ZeroPadColsFFT);
                end
            else
                % N_hat is already available - use as is
            end
            Val = Obj.N_hat;
        end

        function Val=get.Pr_hat(Obj)
            % getter for Pr_hat

            if isempty(Obj.Pr_hat)
                % Pr_hat is not available - calculate
                if Obj.Ref.isemptyPSF
                    error('Ref PSF is not populated');
                else
                    % pad and shift PSF to full image size
                    ImageSize = size(Obj.Ref.Image);

                    % Padded Pr to the size of the full image and shifted
                    % such that the PSF center is at origin:
                    Pr = Obj.Ref.PSFData.getPSF('StampSize',ImageSize, 'fftshift','fftshift');

                    Obj.Pr_hat = fft2(Pr, Obj.ZeroPadRowsFFT, Obj.ZeroPadColsFFT);
                end
            else
                % Pr_hat is already available - use as is
            end
            Val = Obj.Pr_hat;

        end

        function Val=get.Pn_hat(Obj)
            % getter for Pn_hat

            if isempty(Obj.Pn_hat)
                % Pn_hat is not available - calculate
                if Obj.New.isemptyPSF
                    error('New PSF is not populated');
                else
                    % pad and shift PSF to full image size
                    ImageSize = size(Obj.New.Image);

                    % Padded Pr to the size of the full image and shifted
                    % such that the PSF center is at origin:
                    Pn = Obj.New.PSFData.getPSF('StampSize',ImageSize, 'fftshift','fftshift');

                    Obj.Pn_hat = fft2(Pn, Obj.ZeroPadRowsFFT, Obj.ZeroPadColsFFT);
                end
            else
                % Pn_hat is already available - use as is
            end
            Val = Obj.Pn_hat;

        end

        function Val=get.D_hat(Obj)
            % getter for D_hat

            if isempty(Obj.D_hat)
                % D_hat is not available - calculate
                if Obj.isemptyImage
                    % consider calculating D here
                    fprintf('In the future D will be calculated in the getter')
                    error('D image is not populated');
                else
                    Obj.D_hat = fft2(Obj.Image, Obj.ZeroPadRowsFFT, Obj.ZeroPadColsFFT);
                end
            else
                % D_hat is already available - use as is
            end
            Val = Obj.D_hat;
        end

        function Val=get.Pd_hat(Obj)
            % getter for Pd_hat

            if isempty(Obj.Pd_hat)
                % Pd_hat is not available - calculate
                if Obj.isemptyPSF
                    error('D PSF is not populated');
                else
                    % pad and shift PSF to full image size
                    ImageSize = size(Obj.Image);

                    % Padded Pd to the size of the full image and shifted
                    % such that the PSF center is at origin:
                    Pd = Obj.PSFData.getPSF('StampSize',ImageSize, 'fftshift','fftshift');

                    Obj.Pd_hat = fft2(Pd, Obj.ZeroPadRowsFFT, Obj.ZeroPadColsFFT);
                end
            else
                % Pd_hat is already available - use as is
            end
            Val = Obj.Pd_hat;

        end

        function Val=get.Fn(Obj)
            % getter for Fn

            if isempty(Obj.Fn)
                [Obj, Val, ~] = Obj.estimateFnFr;
            else
                Val = Obj.Fn;
            end
            
        end

        function Val=get.Fr(Obj)
            % getter for Fr

            if isempty(Obj.Fr)
                [Obj, ~, Val] = Obj.estimateFnFr;
            else
                Val = Obj.Fr;
            end
            
        end

        function Val=get.BackN(Obj)
            % getter for BackN

            if isempty(Obj.BackN)
                [Obj] = Obj.estimateBackVar;
            end
            Val = Obj.BackN;
            
        end

        function Val=get.BackR(Obj)
            % getter for BackR

            if isempty(Obj.BackR)
                [Obj] = Obj.estimateBackVar;
            end
            Val = Obj.BackR;
            
        end

        function Val=get.VarN(Obj)
            % getter for VarN

            if isempty(Obj.VarN)
                [Obj] = Obj.estimateBackVar;
            end
            Val = Obj.VarN;
            
        end

        function Val=get.VarR(Obj)
            % getter for VarR

            if isempty(Obj.VarR)
                [Obj] = Obj.estimateBackVar;
            end
            Val = Obj.VarR;
            
        end


            % S_hat(Obj)
            % getter for S_hat

            % get D_hat
            % get_Pd_hat
            % cross correlate
            % normalize to sigma units

        

        % Need getters for:
        % Fn
        % Fr
        % Fd
        % SigmaN
        % SigmaR

        % need getters for:
        % D_den_hat
        % D_num_hat
        % D_denSqrt_hat
        % P_deltaNhat
        % P_deltaRhat
        % 
        % F_S
        % 
        % Zvec_hat

    end
    
    methods % read/write

    end

    methods % utilities
        % normS
        
        % normZ2
        
        % S2

        % cleanFFT (?)

        % fft(Obj, Fields) - store the results in the *_hat properties
        % 

        % ifft(Obj, Fields) from hat to non-hat

        % shift
        
        % shiftfft
        
        % resizePSF

    end

    methods % main functionality
        % loadRef

        % register
        function Obj=register(Obj, Args)
            % Register the New and Ref images in AstroDiff using their WCS.
            %   Use imProc.transIm.interp2wcs to register the Ref and New
            %   images.
            %   By default will register the Ref image into the New image
            %   (such that the New doesn't change) [controlled via the RegisterRef
            %   argument].
            %   
            % Input  : - An AstroDiff object.
            %            New and Ref must be populated and contains a WCS.
            %          * ...,key,val,...
            %            'ReRegister' - A logical indicating if to
            %                   re-register the images even if the IsRegistere
            %                   property is true.
            %                   Default is false.
            %            'RegisterRef' - A logical.
            %                   If true register the Ref into New.
            %                   If false register the New into Ref.
            %                   Default is true.
            %            'InterpMethod' - Interpolation method.
            %                   Default is 'cubic'.
            %            'InterpMethodMask' - Interpolation method for mask
            %                   images. Default is 'nearest'.
            %            'DataProp' - data properties in the AstroImage to
            %                   interpolate.
            %                   Default is {'Image','Mask'}.
            %            'ExtrapVal' - Extrapolation value. Default is NaN.
            %            'CopyPSF' - Copy PSF from input image. Default is true.
            %            'CopyWCS' - Copy WCS from input image. Default is true.
            %            'CopyHeader' - Copy Header from input image. Default is true.
            %                   If CopyWCS is true, then will update header by the
            %                   WCS.
            %            'Sampling' - AstroWCS/xy2refxy sampling parameter.
            %                   Default is 20.
            %            'SetNaNBitMask' - A logical indicating if to flag
            %                   NaN pixels (after registration) in the bit mask
            %                   image.
            %                   Default is true.
            %
            % Output : - An AstroDiff object in which the Ref and New are
            %            registered.
            % Author : Eran Ofek (Jan 2024)
            % Example: AD.Ref = AstroImage.readFileNamesObj('LAST.01.02.01_20230828.014050.716_clear_358+34_001_001_010_sci_proc_Image_1.fits');
            %          AD.New = AstroImage.readFileNamesObj('LAST.01.02.01_20230828.014710.841_clear_358+34_020_001_010_sci_proc_Image_1.fits');
            %          % Register the Ref image into the New image (New won't change)
            %          AD.register

            arguments
                Obj
                Args.ReRegister logical   = false;
                Args.RegisterRef logical  = true;

                Args.InterpMethod             = 'cubic';  % 'makima'
                Args.InterpMethodMask         = 'nearest';
                Args.DataProp                 = {'Image','Mask'};
                Args.ExtrapVal                = NaN;
                Args.CopyPSF logical          = true;
                Args.CopyWCS logical          = true;
                Args.CopyHeader logical       = true;
                Args.Sampling                 = 20;

                Args.SetNaNBitMask logical    = true;
            end

            Nobj = numel(Obj);
            for Iobj=1:1:Nobj

                if ~Obj(Iobj).IsRegistered || Args.ReRegister
                    if Args.RegisterRef

                        Obj(Iobj).Ref = imProc.transIm.interp2wcs(Obj(Iobj).Ref, Obj(Iobj).New,...
                                                                  'InterpMethod',Args.InterpMethod,...
                                                                  'InterpMethodMask',Args.InterpMethodMask,...
                                                                  'DataProp',Args.DataProp,...
                                                                  'ExtrapVal',Args.ExtrapVal,...
                                                                  'CopyPSF',Args.CopyPSF,...
                                                                  'CopyWCS',Args.CopyWCS,...
                                                                  'CopyHeader',Args.CopyHeader,...
                                                                  'Sampling',Args.Sampling,...
                                                                  'CreateNewObj',false);
                        % mask NaN pixels (typically at edges)
                        if Args.SetNaNBitMask
                            Obj(Iobj).Ref = imProc.mask.maskNaN(Obj(Iobj).Ref, 'CreateNewObj',false);
                        end

                    else
                        Obj(Iobj).New = imProc.transIm.interp2wcs(Obj(Iobj).New, Obj(Iobj).Ref,...
                                                                  'InterpMethod',Args.InterpMethod,...
                                                                  'InterpMethodMask',Args.InterpMethodMask,...
                                                                  'DataProp',Args.DataProp,...
                                                                  'ExtrapVal',Args.ExtrapVal,...
                                                                  'CopyPSF',Args.CopyPSF,...
                                                                  'CopyWCS',Args.CopyWCS,...
                                                                  'CopyHeader',Args.CopyHeader,...
                                                                  'Sampling',Args.Sampling,...
                                                                  'CreateNewObj',false);
                        % mask NaN pixels (typically at edges)
                        if Args.SetNaNBitMask
                            Obj(Iobj).New = imProc.mask.maskNaN(Obj(Iobj).New, 'CreateNewObj',false);
                        end
                    end
                    % set IsRegistered
                    Obj(Iobj).IsRegistered = true;
                end
            end % for Iobj=1:1:Nobj
        end


        % ready / Fn/Fr not tested
        function [Obj, Fn, Fr]=estimateFnFr(Obj, Args)
            % Estimate Fn/Fr (flux matching) and return matching factors such that Fn=1
            %   Restimate Fn/Fr using various methods.
            %   This function is automatically called by the Fn/Fr getters.
            %   Calling this function will recalculate Fr/Fn.
            % Input  : - An AstroDiff object.
            %          * ...,key,val,...
            %            'NewZP' - Either Zero Point (in mag or flux), or
            %                   header keyword name containing the ZP of the New image.
            %                   Default is 'PH_ZP'.
            %            'RefZP' - Like 'NewZP', but for the Ref image.
            %                   Default is 'PH_ZP'.
            %            'IsMagZP' - If true, then the units of the ZP is
            %                   mag, if false, then units are flux.
            %                   Default is true.
            %
            % Output : - An AstroDiff object in which the Fn and Fr flux
            %            matching values are populated.
            %          - The last value of Fn
            %          - The last value of Fr
            % Author : Eran Ofek (Jan 2024)
            % Example: AD.estimateFnFr

            arguments
                Obj
                %Args.Method           = 'header';
                Args.NewZP            = 'PH_ZP';
                Args.RefZP            = 'PH_ZP';
                Args.IsMagZP logical  = true;
                Args.Fn               = 1;
        
            end

            Nobj = numel(Obj);
            for Iobj=1:1:Nobj
                % get photometric zero point
                if ischar(Args.NewZP)
                    Fn = Obj(Iobj).New.HeaderData.getVal(Args.NewZP);
                else
                    Fn = Args.NewZP;
                end
                if ischar(Args.RefZP)
                    Fr = Obj(Iobj).Ref.HeaderData.getVal(Args.RefZP);
                else
                    Fr = Args.RefZP;
                end

                % convert to flx units
                if Args.IsMagZP
                    Fn     = 10.^(-0.4.*Fn);
                    Fr     = 10.^(-0.4.*Fr);
                end
               
                if isempty(Args.Fn)
                    % no normalization
                else
                    % Normalize by Fn value.
                    Fr = Args.Fn .* Fr./Fn;   % Fr = Fn./Fr; bug?
                    Fn = Args.Fn;
                end
                        
                % Its important not to use the Fn/Fr getters
                Obj(Iobj).Fr = Fr;
                Obj(Iobj).Fn = Fn;

            end


        end

        % ready
        function Obj=estimateBackVar(Obj, Args)
            % Estimate global background and variance of New and Ref images
            % and populate the BackN, BackR, VarN, VarR properties.
            %   The back/var will be calculated as the global "mean" of the
            %   Back/Var properties in the New/Ref AstroImage objects.
            %   If not populated, then the Back/Var properties will be
            %   first populated using: imProc.background.background
            %
            %   This function is automatically called by the BackN/BackR/VarN/VarR getters.
            %   Calling this function will recalculate BackN/BackR/VarN/VarR.
            %
            % Input  : - An AstroDiff object in which the New and Ref images are populated.
            %          * ...,key,val,...
            %            'FunBackImage' - Function handle to use for
            %                   calculation of the global "mean" background
            %                   from the Back property in the AstroImage of
            %                   the New and Ref images.
            %                   Default is @fast_median.
            %            'FunBackImageArgs' - A cell array of additional
            %                   arguments to pass to 'FunBackImage'.
            %                   Default is {}.
            %            'FunVarImage' - Like 'FunBackImage', but for the
            %                   variance.
            %                   Default is @fast_median.
            %            'FunVarImageArgs' - A cell array of additional
            %                   arguments to pass to 'FunVarImage'.
            %                   Default is {}.
            %
            %            'BackFun' - A function handle for the background (and
            %                   optionally variance) estimation.
            %                   The function is of the form:
            %                   [Back,[Var]]=Fun(Matrix,additional parameters,...),
            %                   where the output Variance is optional.
            %                   The additional parameters are provided by the
            %                   'BackFunPar' keyword (see next keyword).
            %                   Default is @imUtil.background.modeVar_LogHist
            %                   [other example: @median]
            %            'BackFunPar' - A cell array of additional parameters to pass
            %                   to the BackFun function.
            %                   Default is {'MinVal',1} (i.e., additional arguments
            %                   to pass to @imUtil.background.modeVar_LogHist).
            %            'VarFun' - A function handle for the background estimation.
            %                   The function is of the form:
            %                   [Var]=Fun(Matrix,additional parameters,...).
            %                   The additional parameters are provided by the
            %                   'VarFunPar' keyword (see next keyword).
            %                   If NaN, then will not calculate the variance.
            %                   If empty, then will assume the variance is returned as
            %                   the second output argument of 'BackFun'.
            %                   If a string then will copy Back value into the Var.
            %                   Default is empty (i.e., @imUtil.background.rvar returns
            %                   the robust variance as the second output argument).
            %            'VarFunPar' - A cell array of additional parameters to pass
            %                   to the VarFun function.
            %                   Default is {}.
            %            'SubSizeXY' - The [X,Y] size of the partitioned sub images.
            %                   If 'full' or empty, use full image.
            %                   Default is [].
            %            'Overlap' - The [X,Y] additional overlaping buffer between
            %                   sub images to add to each sub image.
            %                   Default is 16.
            %
            % Output : - An AstroDiff object in which the BackN, BackR,
            %            VarN, VarR properties are populated.
            % Author : Eran Ofek (Jan 2024)
            % Example: AD.estimateBackVar

            arguments
                Obj
                Args.FunBackImage           = @fast_median;
                Args.FunBackImageArgs cell  = {};
                Args.FunVarImage            = @fast_median;
                Args.FunVarImageArgs cell   = {};

                Args.BackFun                     = @imUtil.background.modeVar_LogHist; %@median;
                Args.BackFunPar cell             = {'MinVal',30, 'MaxVal',7000}; %{[1 2]};  % 5000 is the max vab. allowed in LAST images
        
                Args.VarFun                      = []; %@imUtil.background.rvar; % [];
                Args.VarFunPar cell              = {};
                Args.SubSizeXY                   = [];
                Args.Overlap                     = 16;
                

            end

            Nobj = numel(Obj);
            for Iobj=1:1:Nobj
                if any(isemptyImage(Obj(Iobj).New, {'Back','Var'}), 'all')
                    % Re calculate global back/var
                    Obj(Iobj).New = imProc.background.background(Obj(Iobj).New, 'BackFun',Args.BackFun,...
                                                                                'BackFunPar',Args.BackFunPar,...
                                                                                'VarFun',Args.VarFun,...
                                                                                'VarFunPar',Args.VarFunPar,...
                                                                                'SubSizeXY',Args.SubSizeXY,...
                                                                                'Overlap',Args.Overlap,...
                                                                                'SubBack',false);
                end

                if any(isemptyImage(Obj(Iobj).Ref, {'Back','Var'}), 'all')
                    % Re calculate global back/var
                    Obj(Iobj).Ref = imProc.background.background(Obj(Iobj).Ref, 'BackFun',Args.BackFun,...
                                                                                'BackFunPar',Args.BackFunPar,...
                                                                                'VarFun',Args.VarFun,...
                                                                                'VarFunPar',Args.VarFunPar,...
                                                                                'SubSizeXY',Args.SubSizeXY,...
                                                                                'Overlap',Args.Overlap,...
                                                                                'SubBack',false);
                end

                Obj(Iobj).BackN = Args.FunBackImage(Obj(Iobj).New.Back(:), Args.FunBackImageArgs{:});
                Obj(Iobj).VarN  = Args.FunBackImage(Obj(Iobj).New.Var(:), Args.FunVarImageArgs{:});

                Obj(Iobj).BackR = Args.FunBackImage(Obj(Iobj).Ref.Back(:), Args.FunBackImageArgs{:});
                Obj(Iobj).VarR  = Args.FunBackImage(Obj(Iobj).Ref.Var(:), Args.FunVarImageArgs{:});
                
            end

        end

        % ready
        function Obj=replaceNaN(Obj, Args)
            % Replace NaN pixels in New and Ref with Back value or other value.
            % Input  : - An AstroDiff object.
            %          * ...,key,val,...
            %            'ReplaceVal' - All the NaN pixels in the New and
            %                   Ref images will be replaced with this value.
            %                   If 'back', then will take the value from
            %                   the 'BackN' and 'BackR' properties.
            %                   Default is 'back'.
            % Output : - An updated AstroDiff object, in which the NaN
            %            values in the New and Ref images is replaced.
            % Author : Eran Ofek (Jan 2024)
            % Example: AD.replaceNaN

            arguments
                Obj
                Args.ReplaceVal  = 'back';  % or scalar
            end

            Nobj = numel(Obj);
            for Iobj=1:1:Nobj
                % New image
                if ischar(Args.ReplaceVal)
                    ValN = Obj(Iobj).BackN;
                else
                    ValN = Args.ReplaceVal;
                end
                Obj(Iobj).New = imProc.image.replaceVal(Obj(Iobj).New, NaN, ValN, 'CreateNewObj',false, 'UseOutRange',false);

                % Ref image
                if ischar(Args.ReplaceVal)
                    ValR = Obj(Iobj).BackR;
                else
                    ValR = Args.ReplaceVal;
                end
                Obj(Iobj).Ref = imProc.image.replaceVal(Obj(Iobj).Ref, NaN, ValR, 'CreateNewObj',false, 'UseOutRange',false);
            end
           
        end

        % ready
        function Obj=subtractionD(Obj, Args)
            % Calculate ZOGY D images and its PSF Pd.
            %   Given New and Ref images, this function will create the
            %   ZOGY subtraction image D (proper subtraction image) and
            %   populate it in AstroDiff along with its PSF Pd.
            %   By default, the D image will be normalized to units of flux
            %   using Fd.
            %   If needed the New and Ref will be registered prior to
            %   subtraction.
            %
            % Input  : - An AstroDiff object.
            %          * ...,key,val,...
            %            'AbsFun' - absolute value function.
            %                   Default is @(X) abs(X)
            %            'Eps' - A small value to add to the demoninators in order
            %                   to avoid division by zero due to roundoff errors.
            %                   Default is 0. (If needed set to about 100.*eps).
            %            'CleanPd' - A logical indicating if to clean Pd (zero low
            %                   frequencies).
            %                   Default is true.
            %            'ReplaceNaN' - A logical indicating if to replace
            %                   NaN's pixels in the New and Ref with thir
            %                   respective mean background levels.
            %                   Default is true.
            %            'ReplaceNaNArgs' - A cell array of additional
            %                   arguments to pass to replaceNaN.
            %                   Default is {}.
            %            'NormDbyFd' - A logical indicating if to normalize
            %                   D to units of flux, by dividing it by Fd.
            %                   Default is true.
            %
            %            'HalfSizePSF' - The size of the Pd PSF populated
            %                   in the AstroDiff PSFData property.
            %                   If 'full', then use a full size image (same
            %                   size as New and Ref).
            %                   If empty, then use the PSF size of New.
            %                   Otherwise [HalfSize] of the output PSF
            %                   size.
            %                   Default is [].
            %            'zeroConvArgs' - A cell array of arguments to pass
            %                   to zeroConv in imUtil.psf.full2stamp.
            %                   Default is {}.
            %            'NormPSF' - A logical indicating if to normalize
            %                   Pd PSF to unity. Default is true.
            %            'SuppressEdgesPSF' - A logical indicating if to supress the
            %                   edges of the PSF using imUtil.psf.suppressEdges
            %                   Default is true.
            %            'SuppressEdgesArgs' - A cell array of additional arguments
            %                   to pass to imUtil.psf.suppressEdges
            %                   Default is {}.
            %
            % Output : - An AstroDiff object with the populated
            %            D in the Image property.
            %            Pd in the PSFData property.
            %            New and Ref registered, and Fr, Fn, BackN, BackR,
            %            VarN, VarR populated.
            % Author : Eran Ofek (Jan 2024)
            % Example: AD.subtractionD

            arguments
                Obj
                
                Args.AbsFun              = @(X) abs(X);
                Args.Eps                 = 0;
                Args.CleanPd logical     = true;
                
                Args.ReplaceNaN logical  = true;
                Args.ReplaceNaNArgs cell = {};

                Args.NormDbyFd logical   = true;
                
                Args.HalfSizePSF         = [];  % []- use New PSF size; 'full', or size
                Args.zeroConvArgs cell   = {};
                Args.NormPSF logical     = true;
                Args.SuppressEdgesPSF logical  = true;
                Args.SuppressEdgesArgs cell    = {};
            end

            

            if Args.ReplaceNaN
                Obj.replaceNaN(Args.ReplaceNaNArgs{:});
            end

            Nobj = numel(Obj);
            for Iobj=1:1:Nobj

                if ~Obj(Iobj).IsRegistered
                    % register images if needed
                    Obj(Iobj).register;
                end


                [Obj(Iobj).D_hat, Obj(Iobj).Pd_hat, Obj(Iobj).Fd, Obj(Iobj).F_S,...
                                  Obj(Iobj).D_den_hat, Obj(Iobj).D_num_hat, Obj(Iobj).D_denSqrt_hat,...
                                  Obj(Iobj).P_deltaNhat, Obj(Iobj).P_deltaRhat] = imUtil.properSub.subtractionD(Obj(Iobj).N_hat,...
                                                                                               Obj(Iobj).R_hat,...
                                                                                               Obj(Iobj).Pn_hat,...
                                                                                               Obj(Iobj).Pr_hat,...
                                                                                               sqrt(Obj(Iobj).VarN),...
                                                                                               sqrt(Obj(Iobj).VarR),...
                                                                                               Obj(Iobj).Fn,...
                                                                                               Obj(Iobj).Fr,...
                                                                                               'AbsFun',Args.AbsFun,...
                                                                                               'Eps',Args.Eps,...
                                                                                               'IsFFT',true,...
                                                                                               'IsOutFFT',true,...
                                                                                               'CleanPd',Args.CleanPd);    
                % calculate D
                D = ifft2(Obj(Iobj).D_hat);
                if Args.NormDbyFd
                    D = D./Obj(Iobj).Fd;
                end
                Obj(Iobj).Image = D;

                % calculate Pd
                Pd = ifft2(Obj(Iobj).Pd_hat);
                if ischar(Args.HalfSizePSF)
                    % full - do not touch
                else
                    if isempty(Args.HalfSizePSF)
                        % use PSF size of new
                        [NPy, NPx] = size(Obj(Iobj).New.PSFData.getPSF);
                        if NPx~=NPy
                            error('Asymmetric PSF');
                        end
                        HalfSizePSF = (NPx - 1).*0.5;
                    else
                        HalfSizePSF = Args.HalfSizePSF;
                    end
                    HalfSizePSF = (HalfSizePSF(:).*ones(2,1)).';
                    Pd = imUtil.psf.full2stamp(Pd, 'StampHalfSize', HalfSizePSF,...
                                                   'IsCorner',true,...
                                                   'Recenter',false,...
                                                   'zeroConvArgs',Args.zeroConvArgs,...
                                                   'Norm',Args.NormPSF);
                    
                end
                if Args.SuppressEdgesPSF
                    Pd = imUtil.psf.suppressEdges(Pd, Args.SuppressEdgesArgs{:});
                end
                Obj(Iobj).PSFData.Data = Pd;

            end
        end

        % not tested - (check also norm by various methods)
        function Obj=subtractionS(Obj, Args)
            % Given D and Pd, populate S and S_hat
            % Input  : - An AstroDiff object in which the (D) Image and its 
            %            PSF (Pd) are populated.
            %          * ...,key,val,...
            %            'PopS_hat' - A logical indicating if to populate
            %                   S_hat. Default is true.
            %            'NormMethod' - A pre-defined method by which to normalize S
            %                   See imUtil.image.normalize for option.
            %                   This is the PreDef argument of imUtil.image.normalize
            %                   If empty, then do not normalize.
            %                   Options include
            %                   'norm','norm_robust','chi2_mean','chi2_median','chi2_std'.
            %                   Default is 'norm_robust'
            %
            % Output : - An AstroDiff object in which S and optional S_hat
            %            are normalize.
            %            Note that F_S is populated by subtractionD.
            % Author : Eran Ofek (Jan 2024)
            % Example: AD.subtractionS;

            arguments
                Obj
                Args.PopS_hat logical       = true;
                
                Args.NormMethod             = 'norm_robust';
                        %                   'norm_robust' - @fast_median, {}, 0,   @tools.math.stat.std_mad, {0,'all'}, 1
                        %                   'norm' - @mean, {}, 0,   @std, {0,'all'}, 1
                        %                   'chi2_mean' - Normalize to the mean of \chi^2 with K degrees of freedoms.
                        %                   'chi2_median' - Normalize to the median of \chi^2 with K degrees of freedoms.
                        %                   'chi2_var'    - Normalize to the variance of \chi^2 with K degrees of freedoms.
                
            end
            
            Nobj = numel(Obj);
            for Iobj=1:1:Nobj
                S_hat           = Obj(Iobj).D_hat.*conj(Obj(Iobj).Pd_hat);
                if Args.PopS_hat
                    Obj(Iobj).S_hat = S_hat;
                end
                Obj(Iobj).S     = ifft2(Obj(Iobj).S_hat);
            
                if ~isempty(Args.NormMethod)
                    % Normalize to units of significance
                    if Args.NormS2
                        Obj(Iobj).S = Obj(Iobj).S.^2;
                    end
                    
                    switch lower(Args.NormMethod(1:4)
                        case 'norm'
                            Obj(Iobj).S = imUtil.image.normalize(Obj(Iobj).S, 'PreDef',Args.NormMethod,...
                                                                      'K',1,...
                                                                      'Fun2Prob',...
                                                                      'Prob2Sig',false);
                        case 'chi2'
                            % Nomalize using S^2
                            Obj(Iobj).S = imUtil.image.normalize(Obj(Iobj).S.^2, 'PreDef',Args.NormMethod,...
                                                                      'K',1,...
                                                                      'Fun2Prob',@chi2cdf,...
                                                                      'Prob2Sig',true);
                        otherwise
                            error('Unknown NormMethod option');
                    end                                  
                    
                end
               
                
            end
        end


        % subtractionScorr

        % subtractionZ2
        function translient(Obj, Args)
            % Apply translient image subtraction to New and Ref in AstroFiff object.
            %   Using: imUtil.properSub.translient
            % Input  : - An AstroDiff object in which the New and Ref are
            %            populated. If IsRegistered is false, then the
            %            images will be registered.
            %          * ...,key,val,...
            

            arguments
                Obj

                Args.ReplaceNaN logical  = true;
                Args.ReplaceNaNArgs cell = {};

                Args.Eps              = 0;
                Args.SetToNaN         = [];
                Args.NormMethod       = 'analytical';
            end

            if Args.ReplaceNaN
                Obj.replaceNaN(Args.ReplaceNaNArgs{:});
            end

            Nobj = numel(Obj);
            for Iobj=1:1:Nobj

                if ~Obj(Iobj).IsRegistered
                    % register images if needed
                    Obj(Iobj).register;
                end

                [Obj(Iobj).Z2, Obj(Iobj).Zvec_hat,Norm] = imUtil.properSub.translient(Obj(Iobj).N_hat, Obj(Iobj).R_hat,...
                                                         Obj(Iobj).Pn_hat, Obj(Iobj).Pr_hat,...
                                                         Obj(Iobj).SigmaN, Obj(Iobj).SigmaR,...
                                                         'Fn',Obj(Iobj).Fn,...
                                                         'Fr',Obj(Iobj).Fr,...
                                                         'IsImFFT',true,...
                                                         'IsPsfFFT',true,...
                                                         'ShiftIm',false,...
                                                         'ShiftPsf',false,...
                                                         'Eps',Args.Eps,...
                                                         'SetToNaN',[],...
                                                         'NormaMethod',Args.NormMethod);
            end

        end

        % findTransients
        
    end
    
    methods % injection simulations
        % injectArtNew

    end

    
    methods % transients inspection and measurment
        % transientsCutouts

        % mergeTransients

        % searchSolarSystem

        % nearRedshift

        % nearGalaxy
        
        % nearStar

    end

    
    methods % display
        % ds9
        % Display Ref, New, D, S, Z2 in ds9 and mark transients
        
    end    
    
    
    methods (Static) % Unit-Test
        Result = unitTest()
    end
    
end
