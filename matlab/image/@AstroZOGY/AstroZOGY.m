% Class for astronomical difference/subtraction images and transients data
% using the ZOGY image subtraction method.
%
% Example: 
%   cd /lastdata/LAST.01.01.01/2025/09/27/proc/195817v0
%   AI=AstroImage.readFileNamesObj('LAST.01.01.01_20250927.195807.180_clear_1240_000_001_010_sci_coadd_Image_1.fits');
%   cd /lastdata/LAST.01.01.01/2025/09/27/proc/210512v0
%   Ref=AstroImage.readFileNamesObj('LAST.01.01.01_20250927.210502.055_clear_1240_000_001_010_sci_coadd_Image_1.fits');
%   AD=AstroZOGY;
%   AD.New = AI;
%   AD.Ref = Ref;
%   AD.register;

classdef AstroZOGY < AstroDiff
   
    % See AstroDiff for main properties

    % ZOGY related
    properties
        S   % with IsFFT - do we need ImageComponent?
        Scorr 
    end
    
    properties (Hidden)  % auxilary images
        %ZeroPadRowsFFT   = [];
        %ZeroPadColsFFT   = [];

        %FFT
        R_hat
        Pr_hat
        N_hat
        Pn_hat

        D_hat
        Pd_hat
        S_hat
        
        S_delta   % S for delta function response
        S_ext     % S for extended PSF response
        
        DSDF
        %DScorrDF

        D_den_hat
        D_num_hat
        D_denSqrt_hat
        P_deltaNhat
        P_deltaRhat
        
        F_S
        
        Z2
        S2
        Sflux   % S in units of flux (prior to normalization
        
        Zvec_hat
    end


    methods % constructor
        % Use AstroDiff constructor
    end
    
    methods % setters/getters
       
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

                    Pr = Obj.Ref.PSFData.getPSF;
                    PrSize = size(Pr);
                    DiffSize = ImageSize - PrSize;
                    PadNeeded = DiffSize.';

                    if any(PadNeeded < 0)
                        error('Kernel is larger than image in at least one dimension.');
                    end
    
                    % Split asymmetrically per dim: floor goes to pre, remainder to post
                    PadPre = floor(PadNeeded/2);
                    PadPost = PadNeeded - PadPre;

                    Pr_padded = padarray(Pr, PadPre,  0, 'pre');
                    Pr_padded = padarray(Pr_padded, PadPost, 0, 'post');

                    AnchorPr = ceil(PrSize/2);

                    % Where that anchor lands after padding:
                    AnchorInPad = PadPre' + AnchorPr;
                    
                    % Shift so the anchor goes to (1,1)
                    Shift = 1 - AnchorInPad;             % vector [dr dc]
                    Pr_padded = circshift(Pr_padded, Shift);                    
                    Obj.Pr_hat = fft2(Pr_padded);
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

                    Pn = Obj.New.PSFData.getPSF;
                    PnSize = size(Pn);
                    DiffSize = ImageSize - PnSize;
                    PadNeeded = DiffSize.';

                    if any(PadNeeded < 0)
                        error('Kernel is larger than image in at least one dimension.');
                    end
    
                    % Split asymmetrically per dim: floor goes to pre, remainder to post
                    PadPre = floor(PadNeeded/2);
                    PadPost = PadNeeded - PadPre;

                    Pn_padded = padarray(Pn, PadPre,  0, 'pre');
                    Pn_padded = padarray(Pn_padded, PadPost, 0, 'post');

                    AnchorPn = ceil(PnSize/2);

                    % Where that anchor lands after padding:
                    AnchorInPad = PadPre' + AnchorPn;
                    
                    % Shift so the anchor goes to (1,1)
                    Shift = 1 - AnchorInPad;             % vector [dr dc]
                    Pn_padded = circshift(Pn_padded, Shift);

                    Obj.Pn_hat = fft2(Pn_padded);
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
                    %Pd = Obj.PSFData.getPSF('StampSize',ImageSize, 'fftshift','fftshift');
                    Pd = Obj.PSFData.getPSF;
                    PdSize = size(Pd);
                    DiffSize = ImageSize - PdSize;
                    PadNeeded = DiffSize.';

                    if any(PadNeeded < 0)
                        error('Kernel is larger than image in at least one dimension.');
                    end
    
                    % Split asymmetrically per dim: floor goes to pre, remainder to post
                    PadPre = floor(PadNeeded/2);
                    PadPost = PadNeeded - PadPre;

                    Pd_padded = padarray(Pd, PadPre,  0, 'pre');
                    Pd_padded = padarray(Pd_padded, PadPost, 0, 'post');

                    AnchorPd = ceil(PdSize/2);

                    % Where that anchor lands after padding:
                    AnchorInPad = PadPre' + AnchorPd;
                    
                    % Shift so the anchor goes to (1,1)
                    Shift = 1 - AnchorInPad;             % vector [dr dc]
                    Pd_padded = circshift(Pd_padded, Shift);                    
                    Obj.Pd_hat = fft2(Pd_padded);


                    %Obj.Pd_hat = fft2(Pd, Obj.ZeroPadRowsFFT, Obj.ZeroPadColsFFT);
                end
            else
                % Pd_hat is already available - use as is
            end
            Val = Obj.Pd_hat;

        end

        function Val=get.DSDF(Obj)
            % getter for dS/dFn (for Fn=1)

            if isempty(Obj.DSDF)
                % DSDF is not available - calculate
                Obj.DSDF = imUtil.properSub.dSdF(Obj.N_hat, Obj.R_hat, ...
                    Obj.Pn_hat, Obj.Pr_hat, Obj.VarN, Obj.VarR, ...
                    Obj.Fr, 'IsOutFFT',false);
            else
                % DSDF is already available - use as is
            end
            Val = Obj.DSDF;
        end

    
    end
    
    methods % read/write

    end

    methods (Static) % static utilities
        function Obj=convertFromAstroImage(AI)
            % convert AstroImage to AstroZOGY
            % Input  : - An AstroImage
            % Output : - An AstroZOGY
            % Author : Eran Ofek (May 2025)
            % Example: AD=AstroZOGY.convertFromAstroImage(AI);

            N = numel(AI);
            for I=N:-1:1
                Obj(I) = AstroZOGY;
                Obj(I).ImageData  = AI(I).ImageData;
                Obj(I).MaskData   = AI(I).MaskData;
                Obj(I).BackData   = AI(I).BackData;
                Obj(I).VarData    = AI(I).VarData;
                Obj(I).PSFData    = AI(I).PSFData;
                Obj(I).WCS        = AI(I).WCS;
                Obj(I).HeaderData = AI(I).HeaderData;
                Obj(I).CatData    = AI(I).CatData;
            end

        end
    end


    methods % utilities
      
    end

    methods % utilities  % search/load images
        % loadRef


    end

    methods % registration and astrometry


    end

    methods % estimate: Fn, Fr, Back, Var
        % ready / Fn/Fr not tested


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

                    [D_hat_Fr, ~, Fd] = imUtil.properSub.subtractionD(Obj(Iobj).N_hat,...
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
                    D = ifft2(D_hat_Fr);
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
    end

    methods % Subtraction tools
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
            %            'OverwriteFr' - A bool on whether to perform the
            %                   subtraction with a custom Fr value. Default
            %                   is false.
            %            'OverwriteFrVal' - Value of custom Fr to be used
            %                   in case OverwriteFr is true. Default is NaN.
            %            'UpdateHeader' - Update header with ZP/LimMag/Back info.
            %                   Default is true.
            %            'KeyLimMag' - Limiting mag header keyword.
            %                   Default is 'LIMMAG'.
            %            'NsigmaLimMag' - Number of sigma for lim. mag
            %                   calculation. Default is 5.
            %            'KeyZP' - Photometric ZP header keyword.
            %                   Default is 'PH_ZP'.
            %            'KeyBack' - Background header keyword.
            %                   Default is 'MEDBCK'.
            %            'KeyVar' - Variance header keyword.
            %                   Default is 'MEDVAR'.
            %            'KeyStd' - Std header keyword.
            %                   Default is 'STDBCK'.
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
                Args.BackArgs cell             = {'BackFun',@median, 'BackFunPar',{'all','omitnan'}, ...
                    'VarFun',@tools.math.stat.rvar, 'VarFunPar',{}, 'SubSizeXY',[]};
                
                Args.CreateNewMask             = true;
                Args.CreateNewWCS              = true;
                Args.CreateNewHeader           = true;
                Args.AddHeaderInfo             = true;
                Args.HeadKeysFromRef           = {'EXPTIME','REF_EXPT'; 'JD','REF_JD'};

                Args.OverwriteFr logical = false;
                Args.OverwriteFrVal = NaN;

                % Update header with ZP/LimMag/Back info:
                Args.UpdateHeader   = true;
                Args.KeyLimMag      = 'LIMMAG';
                Args.NsigmaLimMag   = 5;
                Args.KeyZP          = 'PH_ZP';
                Args.KeyBack        = 'MEDBCK';
                Args.KeyVar         = 'MEDVAR';
                Args.KeyStd         = 'STDBCK';
                
                
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

                FrVal = Obj(Iobj).Fr;
                if Args.OverwriteFr
                    FrVal = Args.OverwriteFrVal;
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
                                                                                               FrVal,...
                                                                                               'AbsFun',Args.AbsFun,...
                                                                                               'Eps',Args.Eps,...
                                                                                               'IsFFT',true,...
                                                                                               'IsOutFFT',true,...
                                                                                               'CleanPd',Args.CleanPd);    
                % calculate D
                D = ifft2(Obj(Iobj).D_hat);

                Obj(Iobj).ZpD = 2.5*log10(Obj(Iobj).Fd) + Obj(Iobj).ZpN;

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
                % ZOGY: Eq. 14 (division by Fd)
                if Args.NormDbyFd
                    D = D./Obj(Iobj).Fd;
                    Obj(Iobj).ZpD = Obj(Iobj).ZpN;
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
                    Pd = ifftshift(Pd);
                    PdSize = size(Pd);

                    if (mod(HalfSizePSF(1),2) > 0) || (mod(PdSize(1),2) > 0)
                        Pd = circshift(Pd, [-1, -1]);
                    end

                    CenterX = ceil(PdSize(1) / 2);
                    CenterY = ceil(PdSize(2) / 2);
                    
                    CropX1 = CenterX - HalfSizePSF(1);
                    CropX2 = CropX1 + NPx - 1;
                    CropY1 = CenterY - HalfSizePSF(2);
                    CropY2 = CropY1 + NPy - 1;
                    
                    Pd = Pd(CropX1:CropX2, CropY1:CropY2);
                    
                end
                if Args.SuppressEdgesPSF
                    Pd = imUtil.psf.suppressEdges(Pd, Args.SuppressEdgesArgs{:});
                end
                Obj(Iobj).PSFData.Data = Pd;

                BD_IM = BitDictionary('BitMask.Image.Default');

                % populate the Back and Var
                if Args.PopBackVar || Args.UpdateHeader
                    [ImageSizeX, ImageSizeY] = Obj(Iobj).sizeImage;

                    NaNMask = BD_IM.findBit(Obj(Iobj).MaskData.Image,'NaN');
                    DImage = Obj(Iobj).Image(~NaNMask);
                    NonNanDImage = DImage(~isnan(DImage));

                    Std_D = tools.math.stat.rstd(NonNanDImage(:), 1,1);
                    Var_D = Std_D.^2;
                    Med_D = median(NonNanDImage(:), 1);
                    
                    if Args.PopBackVar
                        Obj(Iobj).Back = repmat(Med_D, ImageSizeX, ImageSizeY);
                        Obj(Iobj).Var = repmat(Var_D, ImageSizeX, ImageSizeY);
                    end

                    % Updated ZP and LIMMAG of D image in header
                    if Args.UpdateHeader
                        if ~Args.NormDbyFd
                            % write Obj(Iobj).ZpD in PH_ZP
                            Obj(Iobj).HeaderData.replaceVal(Args.KeyZP, Obj(Iobj).ZpD);
                        end
    
                        Obj(Iobj).HeaderData.replaceVal(Args.KeyBack, Med_D);
                        Obj(Iobj).HeaderData.replaceVal(Args.KeyVar,  Std_D.^2);
                        Obj(Iobj).HeaderData.replaceVal(Args.KeyStd,  Std_D);
                    
                        % write D_LimMag in LIMMAG
                        MatchedFilterStdCorr = 1./sqrt(sum(Pd(:).^2)); % see issue #630
                        D_LimMag = Obj(Iobj).ZpD - 2.5.*log10(Args.NsigmaLimMag.*Std_D.*MatchedFilterStdCorr);
                        Obj(Iobj).HeaderData.replaceVal(Args.KeyLimMag, D_LimMag);
                    end
                end
            end 
        end

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
            %                   Default is 'norm_robust_rstd1'
            %            'PopS2' - Populate the S2 (S.^2) property.
            %                   Default is true.
            %            'PopSflux' - Populate Sflux (i.e., S prior to
            %                   normalization).
            %                   Default is true.
            %            'PosS_delta' - Populate the S_delta property
            %                   (i.e., the statistics for delta function).
            %                   Default is false.
            %            'PosS_ext' - - Populate the S_ext property
            %                   (i.e., the statistics for extended PSF).
            %                   Default is false.
            %            'ExtendedFun' - function handle for generating the
            %                   extending function (for S_ext calculation).
            %                   Of the form: Stamp = F(pars, sizeXY)
            %                   Default is @imUtil.kernel2.gauss
            %            'ExtendedFunArgs' - Parameters to pass to the
            %                   ExtendedFun.
            %                   Default is 0.5.
            %
            % Output : - An AstroDiff object in which S and optional S_hat
            %            are normalize.
            %            Note that F_S is populated by subtractionD.
            % Author : Eran Ofek (Jan 2024)
            % Example: AD.subtractionS;

            arguments
                Obj
                Args.PopS_hat logical       = true;
                
                Args.NormMethod             = 'norm_robust_rstd1';
                        
                Args.PopS2 logical           = true;
                Args.PopSflux logical        = true;
                
                Args.PopS_delta logical      = true;
                Args.DeltaWidth              = 0.1;
                Args.DeltaStampSize          = [3 3];
                Args.PopS_ext logical        = true;
                
                Args.ExtendedFun function_handle = @imUtil.kernel2.gauss;
                Args.ExtendedFunArgs             = [1.0];

                Args.Eps                    = 1e-5;
            end
            
            Nobj = numel(Obj);
            for Iobj=1:1:Nobj
                % ZOGY Eq 16:
                Obj(Iobj).S = Obj(Iobj).Fd .* imUtil.filter.filter2_fast(Obj(Iobj).Image, Obj(Iobj).PSF);
                
                if Args.PopS_delta
                    DeltaPSF = imUtil.kernel2.gauss(Args.DeltaWidth, Args.DeltaStampSize);
                    
                    Obj(Iobj).S_delta = Obj(Iobj).Fd .* imUtil.filter.filter2_fast(Obj(Iobj).Image, DeltaPSF);
                end

                if Args.PopS_ext
                    % likely faster to do it in real space
                    PSF         = Obj(Iobj).PSFData.getPSF;
                    ExtendedFun = Args.ExtendedFun(Args.ExtendedFunArgs, size(PSF));
                    ExtPSF      = conv2(PSF, ExtendedFun, 'same');
                    
                    Obj(Iobj).S_ext = Obj(Iobj).Fd .* imUtil.filter.filter2_fast(Obj(Iobj).Image, ExtPSF);
                end

                if Args.PopS_hat
                    
                    Obj(Iobj).S_hat = Obj(Iobj).Fd .* Obj(Iobj).D_hat.*conj(Obj(Iobj).Pd_hat);
                end

                if Args.PopSflux
                    Obj(Iobj).Sflux = Obj(Iobj).S./Obj(Iobj).F_S;
                end

                PreNormalizedS = Obj(Iobj).S;

                if ~isempty(Args.NormMethod)
                    % Normalize to units of significance
                    
                    switch lower(Args.NormMethod(1:4))
                        case 'norm'
                            Obj(Iobj).S = imUtil.image.normalize(Obj(Iobj).S, 'PreDef',Args.NormMethod,...
                                                                      'K',1,...
                                                                     'Fun2Prob',[],...
                                                                      'Prob2Sig',false);
                            if Args.PopS_delta
                                Obj(Iobj).S_delta = imUtil.image.normalize(Obj(Iobj).S_delta, 'PreDef',Args.NormMethod,...
                                                                      'K',1,...
                                                                      'Fun2Prob',[],...
                                                                      'Prob2Sig',false);
                            end
                            if Args.PopS_ext
                                Obj(Iobj).S_ext = imUtil.image.normalize(Obj(Iobj).S_ext, 'PreDef',Args.NormMethod,...
                                                                      'K',1,...
                                                                      'Fun2Prob',[],...
                                                                      'Prob2Sig',false);
                            end
                                
                        case 'chi2'
                            % Nomalize using S^2
                            Obj(Iobj).S = imUtil.image.normalize(Obj(Iobj).S, 'PreDef',Args.NormMethod,...
                                                                      'K',1,...
                                                                      'IfChi2_Sq',true,...
                                                                      'Fun2Prob',@chi2cdf,...
                                                                      'Prob2Sig',true);
                            if Args.PopS_delta                  
                                Obj(Iobj).S_delta = imUtil.image.normalize(Obj(Iobj).S_delta, 'PreDef',Args.NormMethod,...
                                                                      'K',1,...
                                                                      'IfChi2_Sq',true,...
                                                                      'Fun2Prob',@chi2cdf,...
                                                                      'Prob2Sig',true);
                            end
                            if Args.PopS_ext                  
                                Obj(Iobj).S_ext = imUtil.image.normalize(Obj(Iobj).S_ext, 'PreDef',Args.NormMethod,...
                                                                      'K',1,...
                                                                      'IfChi2_Sq',true,...
                                                                      'Fun2Prob',@chi2cdf,...
                                                                      'Prob2Sig',true);
                            end
                            
                        case 'none'
                            % do nothing
                        otherwise
                            error('Unknown NormMethod option');
                    end      

                    if Args.PopS2
                        Obj(Iobj).S2 = Obj(Iobj).S.^2;
                    end
                    
                end

                NormMap = Obj(Iobj).S./(PreNormalizedS+Args.Eps);
                
                Obj(Iobj).ThresholdImage = Obj(Iobj).S;
                Obj(Iobj).ThresholdImage_IsSet = true;

                Obj(Iobj).DSDF = imUtil.properSub.dSdF(Obj(Iobj).N_hat,...
                                    Obj(Iobj).R_hat, Obj(Iobj).Pn_hat, Obj(Iobj).Pr_hat, ...
                                    Obj(Iobj).VarN, Obj(Iobj).VarR, Obj(Iobj).Fr, ...
                                    'IsOutFFT', false);
                
                % Normalize the same way S was
                Obj(Iobj).DSDF = Obj(Iobj).DSDF.*NormMap;
            end
        end

        function [Kn_hat, Kr_hat, Kn, Kr]=knkr(Obj, Args)
            % Return kn_hat, kr_hat, kn, kr
            %   Using imUtil.properSub.knkr
            % Input  : - A single element AstroZOGY object.
            %          * ...,key,val,...
            %            'AbsFun' - Absolute value function - e.g., @(X) conj(X).*X or @(X) abs(X);
            %                   Default is @(x) abs(X)
            %            'Norm' - Normalize Kn and Kr to unity.
            %                   Default is false.
            %            'OverwriteFr' - A bool on whether to perform the
            %                   subtraction with a custom Fr value. Default
            %                   is false.
            %            'OverwriteFrVal' - Value of custom Fr to be used
            %                   in case OverwriteFr is true. Default is 1.            
            % Output : - kn_hat
            %          - kr_hat
            %          - kn
            %          - kr
            % Author : Eran Ofek (Feb 2024)
            % Example: [Kn_hat, Kr_hat, Kn, Kr]=AD.knkr;

            arguments
                Obj(1,1)
                Args.AbsFun   = @(x) abs(x);
                Args.Norm logical  = false;
                Args.OverwriteFr logical = false;
                Args.OverwriteFrVal = 1;       
            end

            Iobj = 1;

            Fr = Obj(Iobj).Fr;

            if Args.OverwriteFr
                Fr = Args.OverwriteFrVal;
            end

            if nargout>2
                [Kn_hat, Kr_hat, Kn, Kr] = imUtil.properSub.knkr(Obj(Iobj).Fn, Fr, Obj(Iobj).Pn_hat, Obj(Iobj).Pr_hat, Obj(Iobj).D_den_hat, Args.AbsFun);
            else
                [Kn_hat, Kr_hat] = imUtil.properSub.knkr(Obj(Iobj).Fn, Fr, Obj(Iobj).Pn_hat, Obj(Iobj).Pr_hat, Obj(Iobj).D_den_hat, Args.AbsFun);
            end
        end

        function Obj=subtractionScorr(Obj, Args)
            % Calculate the ZOGY Scorr statistics - S corrected for source noise and astrometric noise
            %   This function requires that New and Ref will be in units of
            %   electrons.
            % Input  : - An AstroZOGY object.
            %          * ...,key,val,...
            %            'IncludeSourceNoise' - A logical indicating if to
            %                   include source noise. Default is true.
            %            'IncludeAstromNoise' - A logical indicating if to
            %                   include astrometric noise. Default is false.
            %            'ExpTimeNewArr' - A 2 element cell array {a,b}.
            %                   a is the total exposure time of the
            %                   New image or the header key value under
            %                   which the total exposure time is saved. b
            %                   is the average single image exposure time
            %                   or the header key value under which the
            %                   average single image exposure time is
            %                   saved. Default is {'EXPTIME', 20}.
            %            'ExpTimeRefArr' - Same as ExpTimeNewArr but for
            %                   the Ref image. Default is {'EXPTIME', 20}.
            %            'NcoaddNew' - Number of images from which the New
            %                   image is compose (assuming the mean
            %                   operator was used).
            %                   This can be a scalar, or a characater array
            %                   with the header keyword name containing the
            %                   number of images (if key not available then
            %                   will be set to 1).
            %                   Default is 'NCOADD'.
            %            'NcoaddRef' - Like 'NcoaddNew' but for the Ref
            %                   image. Default is 'NCOADD'.
            %            'RN_New' - Readout noise [e] of the New image.
            %                       If this is a coadd, tghen use the RN of
            %                       a single image.
            %                   Default is 2.7
            %            'RN_New' - Readout noise [e] of the Ref image.
            %                   Default is 2.7
            %            'SigmaAstNew' - sigma of the single axis
            %                   astrometric noise of the New image [pix].
            %                   Default is 0.1.
            %            'SigmaAstRef' - sigma of the single axis
            %                   astrometric noise of the Ref image [pix].
            %                   Default is 0.1.
            %            'VarNormMethod' - FUnction handle by which to
            %                   normalize Vtotal (=Vsrc + Vast).
            %                   E.g., @median
            %                   If empty, do not normalize.
            %                   Default is [].
            %            'NormMethod' - Normalization method for Scorr.
            %                   Default is 'norm_robust'.
            %            'NormKnKr' - Normalize Kn and Kr to unity.
            %                   Default is false.
            %            'AbsFun' - Absolute value function - e.g., @(X) conj(X).*X or @(X) abs(X);
            %                   Default is @(x) abs(X)
            %            'OverwriteFr' - A bool on whether to perform the
            %                   subtraction with a custom Fr value. Default
            %                   is false.
            %            'OverwriteFrVal' - Value of custom Fr to be used
            %                   in case OverwriteFr is true. Default is 1.            
            % Output : - An AstroZOGY object in which the Scorr property is
            %            updated.
            % Author : Eran Ofek (Feb 2024)
            % Example: AD.subtractionScorr

            arguments
                Obj
                Args.IncludeSourceNoise logical    = true;
                Args.IncludeAstromNoise logical    = false;
                
                Args.ExpTimeNewArr  = {'EXPTIME', 20};
                Args.ExpTimeRefArr  = {'EXPTIME', 20};
                Args.NcoaddNew   = 'NCOADD';
                Args.NcoaddRef   = 'NCOADD';
                Args.RN_New      = 2.7;  % Read noise in electrons [single image]
                Args.RN_Ref      = 2.7;  % Read noise in electrons [single image]

                Args.SysNoiseN   = 0.015;
                Args.SysNoiseR   = 0.015;

                Args.SigmaAstNew = 0.1;   % astrometric noise in pixels.
                Args.SigmaAstRef = 0.1;   % astrometric noise in pixels.

                Args.VarNormMethod          = [];%@median;
                Args.NormMethod             = 'norm_robust_rstd1';

                Args.NormKnKr logical       = false;

                Args.AbsFun                 = @(x) abs(x);
                Args.OverwriteFr logical = false;
                Args.OverwriteFrVal = 1;
            end

            Nobj = numel(Obj);
            for Iobj=1:1:Nobj

                NcoaddNewSet = false;

                % Read Ncoadd
                if numel(Args.ExpTimeNewArr) == 2

                    if isnumeric(Args.ExpTimeNewArr{1})
                        ExpTimeNew = Args.ExpTimeNewArr{1};
                    elseif Obj(Iobj).New.HeaderData.isKeyExist(Args.ExpTimeNewArr{1})
                        ExpTimeNew = Obj(Iobj).New.HeaderData.getVal(Args.ExpTimeNewArr{1});
                    else
                        ExpTimeNew = 0;
                    end

                    if isnumeric(Args.ExpTimeNewArr{2})
                        MExpTimeNew = Args.ExpTimeNewArr{2};
                    elseif Obj(Iobj).New.HeaderData.isKeyExist(Args.ExpTimeNewArr{2})
                        MExpTimeNew = Obj(Iobj).New.HeaderData.getVal(Args.ExpTimeNewArr{2});
                    else
                        MExpTimeNew = 0;
                    end

                    if (ExpTimeNew > 0) && (MExpTimeNew > 0)
                        NcoaddNew = ExpTimeNew/MExpTimeNew;
                        NcoaddNewSet = true;
                    end
                end  

                if ~NcoaddNewSet
                    if isnumeric(Args.NcoaddNew)
                        NcoaddNew      = Args.NcoaddNew;
                    elseif Obj(Iobj).New.HeaderData.isKeyExist(Args.NcoaddNew)
                        % assumne Args.Ncoadd is a keyword name
                        NcoaddNew = Obj(Iobj).New.HeaderData.getVal(Args.NcoaddNew);
                    else
                        NcoaddNew = 1;
                    end
                end

                NcoaddRefSet = false;

                % Read Ncoadd
                if numel(Args.ExpTimeRefArr) == 2

                    if isnumeric(Args.ExpTimeRefArr{1})
                        ExpTimeRef = Args.ExpTimeRefArr{1};
                    elseif Obj(Iobj).Ref.HeaderData.isKeyExist(...
                            Args.ExpTimeRefArr{1})
                        ExpTimeRef = Obj(Iobj).Ref.HeaderData.getVal(...
                            Args.ExpTimeRefArr{1});
                    else
                        ExpTimeRef = 0;
                    end

                    if isnumeric(Args.ExpTimeRefArr{2})
                        MExpTimeRef = Args.ExpTimeRefArr{2};
                    elseif Obj(Iobj).Ref.HeaderData.isKeyExist(...
                            Args.ExpTimeRefArr{2})
                        MExpTimeRef = Obj(Iobj).Ref.HeaderData.getVal(...
                            Args.ExpTimeRefArr{2});
                    else
                        MExpTimeRef = 0;
                    end

                    if (ExpTimeRef > 0) && (MExpTimeRef > 0)
                        NcoaddRef = ExpTimeRef/MExpTimeRef;
                        NcoaddRefSet = true;
                    end
                end

                if ~NcoaddRefSet
                    if isnumeric(Args.NcoaddRef)
                        NcoaddRef      = Args.NcoaddRef;
                    elseif Obj(Iobj).Ref.HeaderData.isKeyExist(Args.NcoaddRef)
                        % assumne Args.Ncoadd is a keyword name
                        NcoaddRef = Obj(Iobj).Ref.HeaderData.getVal(Args.NcoaddRef);
                    else
                        NcoaddRef = 1;
                    end
                end
                
                RN_New = Args.RN_New;
                RN_Ref = Args.RN_Ref;

                [Kn_hat, Kr_hat, Kn, Kr] = knkr(Obj(Iobj), 'AbsFun',Args.AbsFun, ...
                                                           'Norm',Args.NormKnKr,'OverwriteFr',Args.OverwriteFr,...
                                                           'OverwriteFrVal',Args.OverwriteFrVal);

                % New and Ref should contain the images including
                % background in units of electrons - i.e., the variance
                % images.

                if Args.IncludeSourceNoise
                    % including background
                    VN = NcoaddNew.*(Obj(Iobj).New.Image + RN_New.^2);
                    VR = NcoaddRef.*(Obj(Iobj).Ref.Image + RN_Ref.^2);

                    if ~isempty(Args.SysNoiseN) && ~isempty(Args.SysNoiseR)
                        VN = VN + (Args.SysNoiseN.*VN).^2;
                        VR = VR + (Args.SysNoiseR.*VR).^2;
                    end

                    [Vsrc]      = abs(imUtil.properSub.sourceNoise(VN, VR, Kn, Kr));
                else
                    Vsrc = 0;
                end

                if Args.IncludeAstromNoise
                    [Vast] = imUtil.properSub.astrometricNoise(Obj(Iobj).N_hat, Obj(Iobj).R_hat, Kn_hat, Kr_hat, Args.SigmaAstNew, Args.SigmaAstRef);
                else
                    Vast = 0;
                end

                if ~Args.IncludeSourceNoise && ~Args.IncludeAstromNoise
                    error('Must use at least one of source noise or astrometric noise');
                end

                Vtotal = Vsrc + Vast;

                if ~isempty(Args.VarNormMethod)
                    % norm Vtotal
                    Vtotal = Vtotal./Args.VarNormMethod(Vtotal(:));
                end

                Obj(Iobj).Scorr = Obj(Iobj).Sflux./sqrt(Vtotal);
                
                switch lower(Args.NormMethod(1:4))
                    case 'norm'
                        Obj(Iobj).Scorr = imUtil.image.normalize(Obj(Iobj).Scorr, 'PreDef',Args.NormMethod,...
                                                                  'K',1,...
                                                                  'Fun2Prob',[],...
                                                                  'Prob2Sig',false);
                    case 'chi2'
                        % Nomalize using S^2
                        Obj(Iobj).Scorr = imUtil.image.normalize(Obj(Iobj).Scorr, 'PreDef',Args.NormMethod,...
                                                                  'K',1,...
                                                                  'IfChi2_Sq',true,...
                                                                  'Fun2Prob',@chi2cdf,...
                                                                  'Prob2Sig',true);
                    case 'none'
                        % do nothing
                    otherwise
                        error('Unknown NormMethod option');
                end      

               % DScorrDF is very slow compared to DSDF
               %{
                if Args.IncludeSourceNoise

                   Obj(Iobj).DScorrDF = imUtil.properSub.dScorrdF(Obj(Iobj).Sflux, ...
                        Obj(Iobj).N_hat, Obj(Iobj).R_hat, Obj(Iobj).Pn_hat, Obj(Iobj).Pr_hat, ...
                        Obj(Iobj).VarN, Obj(Iobj).VarR, Obj(Iobj).D_den_hat, ...
                        VN, VR, Obj(Iobj).Fr);
                
                end
               %}
            end

        end

        function Obj=translient(Obj, Args)
            % Apply translient image subtraction to New and Ref in AstroFiff object.
            %   Using: imUtil.properSub.translient
            %   This function can be exceuted only after subtractionD was
            %   executed.
            % Input  : - An AstroDiff object in which the New and Ref are
            %            populated. If IsRegistered is false, then the
            %            images will be registered.
            %          * ...,key,val,...
            %            'Eps' - A small value to add to the demoninators in order
            %                   to avoid division by zero due to roundoff errors.
            %                   Default is 0. (If needed set to about 100.*eps).
            %            'ReplaceNaN' - A logical indicating if to replace
            %                   NaN's pixels in the New and Ref with thir
            %                   respective mean background levels.
            %                   Default is false. (By default done in the
            %                   register step).
            %            'ReplaceNaNArgs' - A cell array of additional
            %                   arguments to pass to replaceNaN.
            %                   Default is {}.
            %            'NormZ2' - Normalize Z^2 using analytical
            %                   normalization.
            %                   Default is true.
            %            'NormZsigma' - Method to calculate Z_sigma from
            %                   Z^2. Options are:
            %                   'none' - Do not return Zsigma.
            %                   'chi2_median' - Using Z^2 median.
            %                   'chi2_mean' - Using Z^ mean.
            %                   'chi2_variance' - Using Z^2 variance.
            %                   Default is 'none'.
            %            'OverwriteFr' - A bool on whether to perform the
            %                   subtraction with a custom Fr value. Default
            %                   is false.
            %            'OverwriteFrVal' - Value of custom Fr to be used
            %                   in case OverwriteFr is true. Default is 1.            
            %
            % Author : Eran Ofek (Jan 2024)
            % Example: AD.translient

            arguments
                Obj

                Args.ReplaceNaN logical  = false;
                Args.ReplaceNaNArgs cell = {};

                Args.Eps              = 0;
                Args.SetToNaN         = [];
                Args.NormZ2 logical   = false;  % analytical normalization
                Args.NormZsigma       = 'none'; %'chi_median'
                Args.NormMethod       = 'empirical';

                Args.OverwriteFr logical = false;
                Args.OverwriteFrVal = 1;

                Args.PrecompKxKySize = [1726, 1726];
            end

            if Args.ReplaceNaN
                Obj.replaceNaN(Args.ReplaceNaNArgs{:});
            end

            Nobj = numel(Obj);

            Kx = [];
            Ky = [];

            if numel(Args.PrecompKxKySize) == 2
                % because the linearization of the translation phase (Delta*K), 
                % it is not 2*Pi periodic, use negative frequancies.
                FreqArrRows = fftshift(-ceil(Args.PrecompKxKySize(1)/2):...
                    (floor(Args.PrecompKxKySize(1)/2)-1));
                FreqArrCols = fftshift(-ceil(Args.PrecompKxKySize(2)/2):...
                    (floor(Args.PrecompKxKySize(2)/2)-1));
            
                [Kx,Ky] = meshgrid(FreqArrCols,FreqArrRows);

                Kx = Kx / Args.PrecompKxKySize(1);
                Ky = Ky / Args.PrecompKxKySize(2);
            end

            for Iobj=1:1:Nobj

                if ~Obj(Iobj).IsRegistered
                    % register images if needed
                    Obj(Iobj).register;
                end

                FrVal = Obj(Iobj).Fr;
                if Args.OverwriteFr
                    FrVal = Args.OverwriteFr;
                end               

                [Obj(Iobj).Z2, Obj(Iobj).Zvec_hat,Norm] = imUtil.properSub.translient(Obj(Iobj).N_hat, Obj(Iobj).R_hat,...
                                                         Obj(Iobj).Pn_hat, Obj(Iobj).Pr_hat,...
                                                         sqrt(Obj(Iobj).VarN),...
                                                         sqrt(Obj(Iobj).VarR),...
                                                         'Fn',Obj(Iobj).Fn,...
                                                         'Fr',FrVal,...
                                                         'IsImFFT',true,...
                                                         'IsPsfFFT',true,...
                                                         'ShiftIm',false,...
                                                         'ShiftPsf',false,...
                                                         'Eps',Args.Eps,...
                                                         'SetToNaN',[],...
                                                         'NormMethod',Args.NormMethod,...
                                                         'Kx',Kx,...
                                                         'Ky',Ky);
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
                                                                      'K',2,...
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

        function Obj=dSdFn(Obj, Args)
            % Populate DSDF - the dS/dFn (Fr=1) derivative for ZOGY subtraction.
            %   Obsoloete: USE getter DSDFn instead
            % Input  : - N_hat (Fourier Transform of new image).
            %            N, R, Pn, Pr can be either matrices or cubes in which the
            %            image index is in the 3rd dimension (see example).
            %            If you want to provide N instead of N_hat (and R, Pn, Pr),
            %            then set the IsFFT argument to false.
            %          - R_hat (Fourier Transform of ref image).
            %          - Pn_hat (Fourier Transform of new image PSF with size equal
            %            to the new image size).
            %            Prior to FT, the PSF should be in the image corner.
            %          - Pr_hat (like Pn_hat, but for the ref image).
            %          - VarN (var of new image background).
            %          - VarR (var of ref image background).
            %          - Fr (Ref image flux normalization). Default is 1.
            % Output : - An AstroZOGY objerct in which the DSDF property is
            %            populated with dS/dFn (for Fr=1).
            %            Be careful this is unnormalized.
            %            Normalization the same as S is needed.
            % Author : Eran Ofek (2024 Jun) 

            Fr = 1;
            Nobj = numel(Obj);
            for Iobj=1:1:Nobj
                Obj(Iobj).DSDF = imUtil.properSub.dSdF(Obj(Iobj).N_hat,...
                                                       Obj(Iobj).R_hat,...
                                                       Obj(Iobj).Pn_hat,...
                                                       Obj(Iobj).Pr_hat, ...
                                                       Obj(Iobj).VarN,...
                                                       Obj(Iobj).VarR, ...
                                                       Fr,...
                                                       'IsOutFFT',false);
            end
    
        end
    end

    methods % special PSFs
        % NOT NEEDED AS ALREADY POPULATED BU subtractionD
        % function psfDelta(Obj, Args)
        %     %
        % 
        %     arguments
        %         Obj
        %         Args.IsN        = true; % if false calculate for R
        %         Args.CalcFdelta = false;
        %         Args.Norm       = 1;
        %     end
        % 
        %     Nobj=numel(Obj);
        %     for Iobj=1:1:Nobj
        %         % for each element of AstroZOGY
        % 
        %         if Args.IsN
        %             if Args.CalcFdelta
        %                 FDN = Obj(Iobj).Fr./sqrt(Obj(Iobj).VarN.*Obj(Iobj).Fr.^2 + Obj(Iobj).VarR.*Obj(Iobj).Fn.^2);
        %             else
        %                 FDN = 1;
        %             end
        % 
        %             Obj(Iobj).Fr.*Obj(Iobj).Pr_hat./(FDN .*sqrt(Obj(Iobj).VarN.*Obj(Iobj).Fr.^2.*AbsPr.^2 + Obj(Iobj).VarR.*Obj(Iobj).Fn.^2.*AbsPn.^2 ))
        % 
        % 
        % 
        %     end
        % 
        % end
    
    end

    methods % catalog matching

        % matchCats

        % matchSolarSystemCat
        % Match catalog to solar system objects and add information to CatData

        % matchRedshiftCat
        % Match catalog to redshift catalogs and add information to CatData
        
        % matchGalaxyCat
        % Match catalog to galaxy catalogs and add information to CatData

        % matchStarCat
        % Match catalog to star/galaxy catalogs and add information to CatData

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

    end

    methods % display
        % ds9
        % Display Ref, New, D, S, Z2 in ds9 and mark transients
        
    end    

    methods % injection simulations
        % injectArt
        % Inject artificial sources to the New/Ref images
        %   Will store original New/Ref images in the OrigImage property.

    end
    
    methods (Static) % Unit-Test
        Result = unitTest()
    end
    
end
