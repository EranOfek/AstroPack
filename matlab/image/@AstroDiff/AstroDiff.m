% Class for astronomical difference/subtraction images and transients data
%
%

classdef AstroDiff < AstroImage
    
    properties (Dependent)
      
    end

    properties
        Ref AstroImage
        New AstroImage
        IsRegistered logical   = false;
        
        S ImageComponent  % with IsFFT
        Scorr ImageComponent
        Z2 ImageComponent

        %
        Fn
        Fr
        Fd
        BackN
        BackR
        VarN
        VarR

        ZeroPadRowsFFT   = [];
        ZeroPadColsFFT   = [];
    end
    
    properties (Hidden)  % auxilary images
       
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

        Zvec_hat
    end


    methods % constructor
       
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
        % norm

        % cleanFFT

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
                                                                
                    end
                    % set IsRegistered
                    Obj(Iobj).IsRegistered = true;
                end
            end % for Iobj=1:1:Nobj
        end


        % ready
        function Obj=estimateFnFr(Obj, Args)
            % Estimate Fn/Fr (flux matching) and return matching factors such that Fn=1
            %   Rstimate Fn/Fr using various methods.
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


        function Obj=subtractionD(Obj, Args)
            %
            % Input  : - 
            %          * ...,key,val,...
            %            'AbsFun' - absolute value function.
            %                   Default is @(X) abs(X)
            %            'Eps' - A small value to add to the demoninators in order
            %                   to avoid division by zero due to roundoff errors.
            %                   Default is 0. (If needed set to about 100.*eps).
            %            'CleanPd' - A logical indicating if to clean Pd (zero low
            %                   frequencies).
            %                   Default is true.
            

            arguments
                Obj
                
                Args.AbsFun            = @(X) abs(X);
                Args.Eps               = 0;
                Args.CleanPd logical   = true;Args.PopS_hat          = true;
            end

            Nobj = numel(Obj);
            for Iobj=1:1:Nobj
                [Obj(Iobj).D_hat, Obj(Iobj).Pd_hat, Obj(Iobj).Fd, Obj(Iobj).F_S, Obj(Iobj).D_den, Obj(Iobj).D_num, Obj(Iobj).D_denSqrt, Obj(Iobj).P_deltaNhat, Obj(Iobj).P_deltaRhat] = subtractionD(Obj(Iobj).N_hat,...
                                                                                                           Obj(Iobj).R_hat,...
                                                                                                           Obj(Iobj).Pn_hat,...
                                                                                                           Obj(Iobj).Pr_hat,...
                                                                                                           Obj(Iobj).SigmaN,...
                                                                                                           Obj(Iobj).SigmaR,...
                                                                                                           Obj(Iobj).Fn,...
                                                                                                           Obj(Iobj).Fr,...
                                                                                                           'AbsFun',Args.AbsFun,...
                                                                                                           'Eps',Args.Eps,...
                                                                                                           'IsFFT',true,...
                                                                                                           'IsOutFFT',true,...
                                                                                                           'CleanPd',Args.CleanPd);

    
            end
        end


        function Obj=subtractionS(Obj, Args)
            % Given D and Pd, populate S and S_hat
            %
            %            'PopS_hat' - A logical indicating if to populate
            %                   S_hat. Default is true.

            arguments
                Obj
                Args.PopS_hat logical    = true;
            end

            Nobj = numel(Obj);
            for Iobj=1:1:Nobj
                S_hat           = Obj(Iobj).D_hat.*conj(Obj(Iobj).Pd_hat);
                if Args.PopS_hat
                    Obj(Iobj).S_hat = S_hat;
                end
                Obj(Iobj).S     = ifft2(Obj(Iobj).S_hat);
            end
        end


        % subtractionScorr

        % subtractionZ2
        function translient(Obj, Args)
            %

            arguments
                Obj

                Args.Eps              = 0;
                Args.SetToNaN         = [];
                Args.NormMethod       = 'analytical';
            end

            Nobj = numel(Obj);
            for Iobj=1:1:Nobj
                [Z2,Zhat,Norm] = imUtil.properSub.translient(Obj(Iobj).N_hat, Obj(Iobj).R_hat,...
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