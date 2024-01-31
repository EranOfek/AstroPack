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

        %
        Fn
        Fr
        Fd
        BackN
        BackR
        VarN
        VarR
        SigmaN
        SigmaR
       
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
        S2
        
        Zvec_hat
        
        % For artificial sources
        OrigImage    % Original New/Ref image before art source injection
        OrigIsNew    % Orig is New (true) or Ref (false)
        
    end

    properties (Hidden, Dependent)  % auxilary images
        % back subtracted New
        Nbs
        % Back subtracted Ref
        Rbs
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
            
            for Imax=Nmax:-1:1
                In = min(Imax, Nn);
                Ir = min(Imax, Nr);
                
                Obj(Imax).New = New(In);
                Obj(Imax).Ref = Ref(Ir);
            end
            
        end
    end
    
    methods % setters/getters
        function Val=get.Rbs(Obj)
            % getter for Rbs - Return background subtracted Ref image
    
            Val = Obj.Ref.Image - Obj.BackR;
        end

        function Val=get.Nbs(Obj)
            % getter for Mbs - Return background subtracted New image
    
            Val = Obj.New.Image - Obj.BackN;
        end

        function Val=get.R_hat(Obj)
            % getter for R_hat
            % Calculate fft2 of background subtracted Ref image

            if isempty(Obj.R_hat)
                % R_hat is not available - calculate
                if Obj.Ref.isemptyImage
                    error('Ref image is not populated');
                else
                    %Obj.R_hat = fft2(Obj.Ref.Image - Obj.BackR, Obj.ZeroPadRowsFFT, Obj.ZeroPadColsFFT);
                    Obj.R_hat = fft2(Obj.Rbs, Obj.ZeroPadRowsFFT, Obj.ZeroPadColsFFT);
                end
            else
                % R_hat is already available - use as is
            end
            Val = Obj.R_hat;
        end

        function Val=get.N_hat(Obj)
            % getter for N_hat
            % Calculate fft2 of background subtracted New image

            if isempty(Obj.N_hat)
                % N_hat is not available - calculate
                if Obj.New.isemptyImage
                    error('New image is not populated');
                else
                    %Obj.N_hat = fft2(Obj.New.Image - Obj.BackN, Obj.ZeroPadRowsFFT, Obj.ZeroPadColsFFT);
                    Obj.N_hat = fft2(Obj.Nbs, Obj.ZeroPadRowsFFT, Obj.ZeroPadColsFFT);
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


    end
    
    methods % read/write

    end

    methods % utilities
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
    end

    methods % utilities  % search/load images
        % loadRef


    end

    methods % registration and astrometry

        % astrometryRefine
        function Obj=astrometryRefine(Obj, Args)
            % Refine the astrometry of the New and Ref images using imProc.astrometry.astrometryRefine
            % Input  : - An AstroDiff object.
            %          * ...,key,val,...
            %            'RefineNew' - A logical indicating if to refine
            %                   the astrometric solution of the New image.
            %                   Default is true.
            %            'RefineRef' - A logical indicating if to refine
            %                   the astrometric solution of the Ref image.
            %                   Default is true.
            %            'astrometryRefineArgs' - A cell array of argumnets
            %                   to pass to imProc.astrometry.astrometryRefine
            %                   Default is {}.
            %            'CatName' - Astrometric catalog name, or AstroCatalog
            %                   containing catalog.
            %                   Default is 'GAIADR3'.
            %            'UseSameCat' - A logical indicating if to use the
            %                   same catalog for all elements of the AstroDiff
            %                   (i.e., the same field).
            %                   Default is false.
            % Output : - An AstroDiff object in which the New and Ref
            %            images astrometry is updated.
            % Author : Eran Ofek (Jan 2024)
            % Example: AD.astrometryRefine

            arguments
                Obj
                Args.RefineNew logical         = true;
                Args.RefineRef logical         = true;
                Args.astrometryRefineArgs cell = {};
                Args.CatName                   = 'GAIADR3';

                Args.UseSameCat logical        = false;
            end


            AstCat = Args.CatName;

            Nobj = numel(Obj);
            for Iobj=1:1:Nobj
                if Args.RefineNew
                    [~, Obj(Iobj).New, AstCat] = imProc.astrometry.astrometryRefine(Obj(Iobj).New, Args.astrometryRefineArgs{:}, 'CatName',AstCat);
                end
                if Args.RefineRef
                    [~, Obj(Iobj).Ref] = imProc.astrometry.astrometryRefine(Obj(Iobj).Ref, Args.astrometryRefineArgs{:}, 'CatName',AstCat);
                end
                if ~Args.UseSameCat
                    AstCat = Args.CatName;
                end
            end

        end

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
            %            'ReplaceNaN' - A logical indicating if to replace
            %                   NaN's pixels in the New and Ref with their
            %                   respective mean background levels.
            %                   Default is true.
            %            'ReplaceNaNArgs' - A cell array of additional
            %                   arguments to pass to replaceNaN.
            %                   Default is {}.
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
                Args.ReRegister logical       = false;
                Args.RegisterRef logical      = true;

                Args.InterpMethod             = 'cubic';  % 'makima'
                Args.InterpMethodMask         = 'nearest';
                Args.DataProp                 = {'Image','Mask'};
                Args.ExtrapVal                = NaN;
                Args.CopyPSF logical          = true;
                Args.CopyWCS logical          = true;
                Args.CopyHeader logical       = true;
                Args.Sampling                 = 20;

                Args.SetNaNBitMask logical    = true;

                Args.ReplaceNaN logical  = true;
                Args.ReplaceNaNArgs cell = {};

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

            if Args.ReplaceNaN
                Obj.replaceNaN(Args.ReplaceNaNArgs{:});
            end

        end

    end

    methods % estimate: Fn, Fr, Back, Var
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
                    % Note that there should be no "-" sign here
                    Fn     = 10.^(0.4.*Fn);
                    Fr     = 10.^(0.4.*Fr);
                end
               
                if isempty(Args.Fn)
                    % no normalization
                else
                    % Normalize by Fn value.
                    Fr = Args.Fn .* Fr./Fn;   
                    Fn = Args.Fn;
                end
                        
                % Its important not to use the Fn/Fr getters
                Obj(Iobj).Fr = Fr;
                Obj(Iobj).Fn = Fn;

            end


        end

        function Result=subAsFunFn(Obj, Args)
            % Return statistics of D image as a function of Fr
            %   Perform subtraction with variable Fr in order to find best
            %   Fn/Fr.
            %   This function will not generate a D and Pd in the
            %   AstroDiff.
            % Input  : - An AstroDiff object. If needed the New and Ref
            %            will be registered.
            %          * ...,key,val,...
            %            'UseNominalFr' - If true, then the test of Fr used
            %                   by this function are Obj.Fr * Args.RangeFr.
            %                   If false, then will use Args.RangeFr as is.
            %            'RangeFr' - A vector of Fr values to test.
            %                   Default is (0.9:0.01:1.1);
            %            
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
            %                   Default is false. (By default done in the
            %                   register step).
            %            'ReplaceNaNArgs' - A cell array of additional
            %                   arguments to pass to replaceNaN.
            %                   Default is {}.
            %            'NormDbyFd' - A logical indicating if to normalize
            %                   D to units of flux, by dividing it by Fd.
            %                   Default is true.
            %
            % Output : - A structure array with the D statistics as a
            %            function of Fr value.
            %            Each element corresponds to an AstroDiff element.
            %            The following fields are available:
            %            .VecFr - Vector of abs value of the Fr.
            %            .MeanD - Meran of D
            %            .MedianD - Median of D
            %            .StdD - Std of D
            %            .MadD - mad of D calculated using:
            %                   tools.math.stat.std_mad
            %            .OptMeanD - Absolute Fr corresponding to MeanD=0.
            %            .OptMedianD - Absolute Fr corresponding to MedianD=0.
            %            .OptStdD - Absolute Fr corresponding to min of StdD.
            %            .OptMadD - Absolute Fr corresponding to min of MadD.
            % Author : Eran Ofek (Jan 2024)
            % Example: RR=AD.subAsFunFn

            arguments
                Obj
                Args.UseNominalFr logical   = true;
                Args.RangeFr                = (0.9:0.01:1.1);

                Args.AbsFun              = @(X) abs(X);
                Args.Eps                 = 0;
                Args.CleanPd logical     = true;
                
                Args.ReplaceNaN logical  = false;
                Args.ReplaceNaNArgs cell = {};

                Args.NormDbyFd logical   = true;
                
            end

            Nfr = numel(Args.RangeFr);

            Nobj = numel(Obj);
            Result = struct('VecFr',cell(Nobj,1), 'MeanD',cell(Nobj,1), 'MedianD',cell(Nobj,1), 'StdD',cell(Nobj,1), 'MadD',cell(Nobj,1),...
                                                  'OptMeanD',cell(Nobj,1), 'OptMedianD',cell(Nobj,1), 'OptStdD',cell(Nobj,1), 'OptMadD',cell(Nobj,1));
            for Iobj=1:1:Nobj
                if ~Obj(Iobj).IsRegistered
                    % register images if needed
                    Obj(Iobj).register;
                end

                OriginalFr = Obj(Iobj).Fr;
                if Args.UseNominalFr
                    VecFr = OriginalFr.*Args.RangeFr;
                else
                    VecFr = Args.RangeFr;
                end

                Result(Iobj).VecFr   = VecFr;
                Result(Iobj).MeanD   = zeros(Nfr,1);
                Result(Iobj).MedianD = zeros(Nfr,1);
                Result(Iobj).StdD    = zeros(Nfr,1);
                Result(Iobj).MadD    = zeros(Nfr,1);
                for Ifr=1:1:Nfr

                    [D_hat, Pd_hat, Fd] = imUtil.properSub.subtractionD(Obj(Iobj).N_hat,...
                                                                                               Obj(Iobj).R_hat,...
                                                                                               Obj(Iobj).Pn_hat,...
                                                                                               Obj(Iobj).Pr_hat,...
                                                                                               sqrt(Obj(Iobj).VarN),...
                                                                                               sqrt(Obj(Iobj).VarR),...
                                                                                               Obj(Iobj).Fn,...
                                                                                               VecFr(Ifr),...
                                                                                               'AbsFun',Args.AbsFun,...
                                                                                               'Eps',Args.Eps,...
                                                                                               'IsFFT',true,...
                                                                                               'IsOutFFT',true,...
                                                                                               'CleanPd',Args.CleanPd);    
                    % calculate D
                    D = ifft2(D_hat);
                    if Args.NormDbyFd
                        D = D./Fd;
                    end
                    Result(Iobj).MeanD(Ifr)   = mean(D(:));
                    Result(Iobj).MedianD(Ifr) = fast_median(D(:));
                    Result(Iobj).StdD(Ifr)    = std(D(:));
                    Result(Iobj).MadD(Ifr)    = tools.math.stat.std_mad(D(:));

                end
                % search optimum Fr
                % MeanD 
                List = tools.find.find_local_zeros(Result(Iobj).VecFr(:), Result(Iobj).MeanD(:));
                if size(List,1)==1
                    Result(Iobj).OptMeanD = List(1,1);
                else
                    Result(Iobj).OptMeanD = NaN;
                end
                % MedianD
                List = tools.find.find_local_zeros(Result(Iobj).VecFr(:), Result(Iobj).MedianD(:));
                if size(List,1)==1
                    Result(Iobj).OptMedianD = List(1,1);
                else
                    Result(Iobj).OptMedianD = NaN;
                end
                % StdD
                List = tools.find.find_local_extremum(Result(Iobj).VecFr(:), Result(Iobj).StdD(:));
                if size(List,1)==1 && List(1,3)>0
                    Result(Iobj).OptStdD = List(1,1);
                else
                    Result(Iobj).OptStdD = NaN;
                end
                % MadD
                List = tools.find.find_local_extremum(Result(Iobj).VecFr(:), Result(Iobj).MadD(:));
                if size(List,1)==1 && List(1,3)>0
                    Result(Iobj).OptMadD = List(1,1);
                else
                    Result(Iobj).OptMadD = NaN;
                end



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
                Args.MeanVarFun function_handle   = @tools.math.stat.nanmean;
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

                Obj(Iobj).SigmaN = sqrt(Args.MeanVarFun(Obj(Iobj).VarN, 'all'));
                Obj(Iobj).SigmaR = sqrt(Args.MeanVarFun(Obj(Iobj).VarR, 'all'));
            end
        
        end
        

    end

    methods % Subtraction tools
        % ready for testing (including mask, header, wcs)
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
            %                   Default is false. (By default done in the
            %                   register step).
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
            %            'PopBackVar' - A logical indicating if to populate
            %                   the Back and Var images of the D subtraction
            %                   image. Default is true.
            %            'BackArgs' - A cell array of additional arguments
            %                   to pass to the background and variance
            %                   calculation function:
            %                   imProc.background.background.
            %                   Default is {'BackFun',@median, 'BackFunPar',{'all'}, 'VarFun',@imUtil.background.rvar, 'VarFunPar',{}, 'SubSizeXY',[]}
            %
            %            'CreateNewMask' - A logical indicating if to
            %                   create a new copy of the Mask image (or
            %                   using the New/Ref Mask image - i.e., will
            %                   modify their Mask).
            %                   If empty, then do not add Mask image.
            %                   Default is true.
            %            'CreateNewWCS' - A logical indicating if to create
            %                   a new copy of the WCS (or to use the
            %                   New/Ref WCS).
            %                   WCS will be copied from Ref image.
            %                   If empty, then do not add WCS.
            %                   Default is true.
            %            'CreateNewHeader' - A logical indicating if to create
            %                   a new copy of the Header (or to use the
            %                   New/Ref HeaderData).
            %                   Header will be copied fron New image.
            %                   If empty, then do not copy header.
            %                   Default is true.
            %            'AddHeaderInfo' - A logical indicating if to add
            %                   additional header keywords (e.g., from the Ref).
            %                   Default is true.
            %            'HeadKeysFromRef' - A two column cell array of the
            %                   name of the header keywords in Ref and
            %                   their name in the new generated D image.
            %                   If empty, then do not add any header
            %                   keywords from Ref.
            %                   Default is {'EXPTIME','REF_EXPT';
            %                               'JD','REF_JD'}
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
                
                Args.ReplaceNaN logical  = false;
                Args.ReplaceNaNArgs cell = {};

                Args.NormDbyFd logical   = true;
                
                Args.HalfSizePSF         = [];  % []- use New PSF size; 'full', or size
                Args.zeroConvArgs cell   = {};
                Args.NormPSF logical     = true;
                Args.SuppressEdgesPSF logical  = true;
                Args.SuppressEdgesArgs cell    = {};
                
                Args.PopBackVar logical        = true;
                Args.BackArgs cell             = {'BackFun',@median, 'BackFunPar',{'all'}, 'VarFun',@imUtil.background.rvar, 'VarFunPar',{}, 'SubSizeXY',[]};
                
                Args.CreateNewMask             = true;
                Args.CreateNewWCS              = true;
                Args.CreateNewHeader           = true;
                Args.AddHeaderInfo             = true;
                Args.HeadKeysFromRef           = {'EXPTIME','REF_EXPT'; 'JD','REF_JD'};
                
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
                
                % create mask image propgated from New and Ref
                if ~isempty(Args.CreateNewMask)
                    Obj(Iobj).MaskData = funBinary(Obj(Iobj).New.MaskData, Obj(Iobj).Ref.MaskData, @bitor, 'CreateNewObj',Args.CreateNewMask);
                end
                
                % copy WCS
                if ~isempty(Args.CreateNewWCS)
                    if Args.CreateNewWCS
                        Obj(Iobj).WCS = Obj(Iobj).Ref.WCS.copy;
                    else
                        Obj(Iobj).WCS = Obj(Iobj).Ref.WCS;
                    end
                end
                
                % Copy Header
                if ~isempty(Args.CreateNewHeader)
                    if Args.CreateNewHeader
                        Obj(Iobj).HeaderData = Obj(Iobj).New.HeaderData.copy;
                    else
                        Obj(Iobj).HeaderData = Obj(Iobj).New.HeaderData;
                    end
                end
                
                % Add Information to header
                if Args.AddHeaderInfo
                    % Add the following keyords:
                    % COMMENT: 'ZOGY Subtraction'
                    % COMMENT: 'Generated by AstroPack, AstroDiff class"
                    % REF_EXPT: Ref Exp Time
                    % REF_JD: Ref JD
                    CellKey = Obj(Iobj).Ref.HeaderData.getCellKey(Args.HeadKeysFromRef(:,1));
                    Obj(Iobj).HeaderData.replaceVal(Args.HeadKeysFromRef(:,2), CellKey);
                end
                
                % Normalize to flux units (according to Fn, Fr)
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
            
            % populate the Back and Var
            if Args.PopBackVar
                Obj = imProc.background.background(Obj, Args.BackArgs{:});
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
            %                   'norm','norm_robust','chi2_mean','chi2_median','chi2_std',
            %                   'none'.
            %                   Default is 'norm_robust'
            %            'PosS2' - Populate the S2 (S.^2) property.
            %                   Default is true.
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
               Args.PopS2 logical           = true;
                
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
                    
                    switch lower(Args.NormMethod(1:4))
                        case 'norm'
                            Obj(Iobj).S = imUtil.image.normalize(Obj(Iobj).S, 'PreDef',Args.NormMethod,...
                                                                      'K',1,...
                                                                      'Fun2Prob',[],...
                                                                      'Prob2Sig',false);
                        case 'chi2'
                            % Nomalize using S^2
                            Obj(Iobj).S = imUtil.image.normalize(Obj(Iobj).S, 'PreDef',Args.NormMethod,...
                                                                      'K',1,...
                                                                      'IfChi2_Sq',true,...
                                                                      'Fun2Prob',@chi2cdf,...
                                                                      'Prob2Sig',true);
                        case 'none'
                            % do nothing
                        otherwise
                            error('Unknown NormMethod option');
                    end      

                    if Args.PopS2
                        Obj(Iobj).S2 = Obj(Iobj).S.^2;
                    end
                    
                end
               
                
            end
        end


        % subtractionScorr

        % translient (almost ready?)
        function translient(Obj, Args)
            % Apply translient image subtraction to New and Ref in AstroFiff object.
            %   Using: imUtil.properSub.translient
            % Input  : - An AstroDiff object in which the New and Ref are
            %            populated. If IsRegistered is false, then the
            %            images will be registered.
            %          * ...,key,val,...
            %
            % Author : Eran Ofek (Jan 2024)
            % Example: AD.translient

            arguments
                Obj

                Args.ReplaceNaN logical  = true;
                Args.ReplaceNaNArgs cell = {};

                Args.Eps              = 0;
                Args.SetToNaN         = [];
                Args.NormZ2 logical   = true;  % analytical normalization
                Args.NormZsigma       = 'none'; %'chi_median'
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
                                                         sqrt(Obj(Iobj).VarN),...
                                                         sqrt(Obj(Iobj).VarR),...
                                                         'Fn',Obj(Iobj).Fn,...
                                                         'Fr',Obj(Iobj).Fr,...
                                                         'IsImFFT',true,...
                                                         'IsPsfFFT',true,...
                                                         'ShiftIm',false,...
                                                         'ShiftPsf',false,...
                                                         'Eps',Args.Eps,...
                                                         'SetToNaN',[],...
                                                         'NormMethod','none');
                if Args.NormZ2
                    % analytical normalization
                    Obj(Iobj).Z2 = Obj(Iobj).Z2./Norm; 
                end


                if ~isempty(Args.NormZsigma)
                    % Normalize to units of significance

                    switch lower(Args.NormZsigma(1:4))
                        case 'chi2'
                            % Nomalize using S^2
                            Obj(Iobj).Z2sigma = imUtil.image.normalize(Obj(Iobj).Z2, 'PreDef',Args.NormMethod,...
                                                                      'K',1,...
                                                                      'IfChi2_Sq',true,...
                                                                      'Fun2Prob',@chi2cdf,...
                                                                      'Prob2Sig',true);

                        case 'none'
                            % do nothing
                        otherwise
                            error('Unknown NormZsigma option');

                    end                                  

                end
            end

        end

        % findTransients
        % Search for positive and negative transients in S
        %   Only look for local max/min in S above detection threshold
        
        % measureTransients
        % For each transient candidate measure properties.
        %   Including, fit Pd PSF to D, return S, Zsig, mask, value in New,
        %   Ref, nearby source in New, Ref
        
        % cleanTransients
        % Select good transients using selection criteria
        
        % fitDT
        % Fit a variability + motion model to the D_T image
        
    end
    
    methods % injection simulations
        % injectArt
        % Inject artificial sources to the New/Ref images
        %   Will store original New/Ref images in the OrigImage property.

    end

    
    methods % transients inspection and measurment
        % transientsCutouts
        % Generate an AstroDiff of cutouts around transients

        % mergeTransients
        % Given multiple AstroDiff objects, search for transients that have similar positions
        %   and merge them [The meaning of the merged prodict is not clear:
        %       Is is a table? an AstroDiff with one element per merge?
        %       If so, then what should we do about the multiple diff and
        %       New images?]

        % searchSolarSystem
        % Match catalog to solar system objects and add information to CatData

        % nearRedshift
        % Match catalog to redshift catalogs and add information to CatData
        
        % nearGalaxy
        % Match catalog to galaxy catalogs and add information to CatData
        
        % nearStar
        % Match catalog to star/galaxy catalogs and add information to CatData
        

    end

    
    methods % display
        % ds9
        % Display Ref, New, D, S, Z2 in ds9 and mark transients
        
    end    
    
    
    methods (Static) % Unit-Test
        Result = unitTest()
    end
    
end
