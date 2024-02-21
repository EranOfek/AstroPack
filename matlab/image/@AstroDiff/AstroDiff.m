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
        ThresholdImage
        ThresholdImage_IsSet logical = false;
        
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
    end

  
    properties (Hidden, Dependent)  % auxilary images
        % back subtracted New
        Nbs
        % Back subtracted Ref
        Rbs
    end
    properties
        % For artificial sources
        OrigImage    % Original New/Ref image before art source injection
        OrigIsNew    % Orig is New (true) or Ref (false)
    end

    properties (Constant, Hidden)
        % List of common column names in the transients catalog
        DefaultColumnNames = {'XPEAK','YPEAK',...
                              'X1', 'Y1',...
                              'X2','Y2','XY',...
                              'SN','BACK_IM','VAR_IM',...  
                              'BACK_ANNULUS', 'STD_ANNULUS', ...
                              'FLUX_APER', 'FLUXERR_APER',...
                              'MAG_APER', 'MAGERR_APER'};
        
        
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

        function Val=get.Fn(Obj)
            % getter for Fn

            if isempty(Obj.Fn)
                [~, Val, ~] = Obj.estimateFnFr;
            else
                Val = Obj.Fn;
            end
            
        end

        function Val=get.Fr(Obj)
            % getter for Fr

            if isempty(Obj.Fr)
                [~, ~, Val] = Obj.estimateFnFr;
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

    end

    methods
       
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

    methods % transients search

        function findMeasureTransients(Obj,Args)
            %{
            Calls AD.findTransients, AD.matchTransients2Cats, AD.measureTransients, 
              and AD.flagNonTransients. 1) Derives a catalog of transients by
              thresholding the threshold image, 2) matches found transients 
              to external catalogs, 3) derives additional properties based
              on subtraction method, and 4) flags transients that are 
              likely not real transients.
            Input  : - An AstroDiff object in which the threshold image is
                        populated.
                     * ...,key,val,...
                       'findTransientsArgs' - Cell of arguments to be given
                              to AD.findTransients. Default is the same as
                              AD.findTransients.
                       'matchTransients2CatsArgs' - Cell of arguments to be given
                              to AD.matchTransients2Cats. Default is the same as
                              AD.matchTransients2Cats.
                       'measureTransientsArgs' - Cell of arguments to be given
                              to AD.measureTransients. Default is the same as
                              AD.measureTransients.
                       'flagNonTransientsArgs' - Cell of arguments to be given
                              to AD.flagNonTransients. Default is the same as
                              AD.flagNonTransients.
            Author : Ruslan Konno (Jan 2024)
            Example: AD.findMeasureTransients
            %}

            arguments
                Obj

                Args.findTransientsArgs = {...
                    'Threshold', 5,...
                    'findLocalMaxArgs', {},...
                    'includePsfFit', true,...
                    'HalfSizePSF', 7,...
                    'psfPhotCubeArgs', {}...
                    'includeBitMaskVal', true,...
                    'BitCutHalfSize', 3,...
                    'includeSkyCoord', true,...
                    'includeObsTime', true,...
                    };

                Args.matchTransients2CatsArgs = {...
                    'matchGalaxyCatArgs', {'ColDistName', 'GalaxyDist',...
                    'ColNmatchName','GalaxyMatches'},...
                    'matchStarCatArgs', {'ColDistName', 'StarDist',...
                    'ColNmatchName','StarMatches'},...
                    }

                Args.measureTransientsArgs = {...
                    'HalfSizeTS', 5,...
                    };
                
                Args.flagNonTransientsArgs = {...
                    'flagChi2', true,...
                    'Chi2dofLimits', [0.5 2],...
                    'flagMag', true,...
                    'MagLim', 21,...
                    'flagBadPix_Hard', true,...
                    'NewMask_BadHard', {'Saturated','NearEdge','FlatHighStd',...
                    'Overlap','Edge','CR_DeltaHT','Interpolated','NaN'},...
                    'RefMask_BadHard',{'Saturated','NearEdge','FlatHighStd',...
                    'Overlap','Edge','CR_DeltaHT','Interpolated','NaN'},...
                    'flagBadPix_Soft', true,...
                    'NewMask_BadSoft',{'HighRN', 'DarkHighVal', ...
                    'BiasFlaring', 'Hole', 'SrcNoiseDominated'},...
                    'RefMask_BadSoft', {'HighRN', 'DarkHighVal', ...
                    'BiasFlaring', 'Hole', 'SrcNoiseDominated'},...
                    };
                
            end

            Nobj = numel(Obj);

            for Iobj=1:1:Nobj
                Obj(Iobj).findTransients(Args.findTransientsArgs{:});
                Obj(Iobj).matchTransients2Cats(Args.matchTransients2CatsArgs{:});
                Obj(Iobj).measureTransients(Args.measureTransientsArgs{:});
                Obj(Iobj).flagNonTransients(Args.flagNonTransientsArgs{:});
            end
        end

        % findTransients
        function findTransients(Obj, Args)
            %{
            Search for positive and negative transients by selecting local
              minima and maxima with an absolute value above a set detection 
              threshold. Results are saved as an AstroCatalog under
              AD.CatData.
            Input  : - An AstroDiff object in which the threshold image is
                        populated.
                     * ...,key,val,...
                       'Threshold' - Threshold to be applied to the threshold image. 
                              Search for local maxima only above this threshold. 
                              Default is 5.
                       'findLocalMaxArgs' - Args passed into imUtil.sources.findLocalMax()
                              when looking for local maxima. Default is {}.
                       'includePsfFit' - Bool on whether to perform PSF photometry 
                              on images AD, AD.New, and AD.Ref. Include results in catalog.
                              Default is true.
                       'HalfSizePSF' - Half size of area on transients positions in 
                              image. Actual size will be 1+2*HalfSizePSF. Used to cut out 
                              an image area to perform PSF photometry on.
                              Default is 7.
                       'psfPhotCubeArgs' - Args passed into imUtil.sources.psfPhotCube when
                              performing PSF photometry on AD, AD.New, and AD.Ref cut outs.
                              Default is {}.
                       'includeBitMaskVal' - Bool on whether to retrieve bit mask
                              values from AD.New and AD.Ref, and add to catalog.
                              Default is true.
                       'BitCutHalfSize' - Half size of area on transients positions in 
                              image bit masks. Actual size will be 1+2*BitCutHalfSize. Used
                              to retrieve bit mask values around transient positions.
                              Default is 3.
                       'includeSkyCoord' - Bool on whether to retrieve sky
                              coordinates from AD.New and add to catalog. 
                              Default is true.
                       'includeObsTime' - Bool on whether to retrieve observation
                              times from AD.New and add to catalog. Default is true.
            Author : Ruslan Konno (Jan 2024)
            Example: AD.findTransients
            %}

            arguments
                Obj

                Args.Threshold                  = 5;
                Args.findLocalMaxArgs cell      = {};
        
                Args.includePsfFit logical      = true;
                Args.HalfSizePSF                = 7;
                Args.psfPhotCubeArgs cell       = {};
        
                Args.includeBitMaskVal logical  = true;
                Args.BitCutHalfSize             = 3;
        
                Args.includeSkyCoord logical    = true;
                Args.includeObsTime logical     = true;                
        
            end

            Nobj = numel(Obj);

            for Iobj=Nobj:-1:1
                % TODO: think a bit more on what to do when threshold image
                % is not set - warning, error, empty catalog,...?
                if ~Obj(Iobj).ThresholdImage_IsSet
                    continue
                end
                Obj(Iobj).CatData = imProc.sub.findTransients(Obj(Iobj), ...
                    'Threshold', Args.Threshold, ...
                    'findLocalMaxArgs', Args.findLocalMaxArgs,...
                    'includePsfFit',Args.includePsfFit,...
                    'HalfSizePSF', Args.HalfSizePSF,...
                    'psfPhotCubeArgs', Args.psfPhotCubeArgs,...
                    'includeBitMaskVal', Args.includeBitMaskVal,...
                    'BitCutHalfSize', Args.BitCutHalfSize,...
                    'includeSkyCoord', Args.includeSkyCoord,...
                    'includeObsTime', Args.includeObsTime...
                    );
            end
        end

        % matchTransients2Cats
        function matchTransients2Cats(Obj, Args)
            %{
            Matches transients candidates to external catalogs by calling
              AD.matchMergedCat, AD.matchGalaxyCat, AD.matchStarCat, and
              AD.matchSolarSystemCat.
            Input  : - An AstroDiff object in which CatData is populated.
                     * ...,key,val,...
                       'matchMergedCatArgs' - Cell of arguments to be given
                              to AD.matchMergedCat. Default is the same as
                              AD.matchMergedCat.
                       'matchGalaxyCatArgs' - Cell of arguments to be given
                              to AD.matchGalaxyCat. Default is the same as
                              AD.matchGalaxyCat.
                       'matchStarCatArgs' - Cell of arguments to be given
                              to AD.matchStarCat. Default is the same as
                              AD.matchStarCat.
                       'matchSolarSystemArgs' - Cell of arguments to be given
                              to AD.matchSolarSystemCat. Default is the same as
                              AD.matchSolarSystemCat.
            Author : Ruslan Konno (Feb 2024)
            Example: AD.matchTransients2Cats
            %}
            arguments
                Obj

                Args.matchMergedCatArgs = {'ColDistName', 'MergedDist',...
                    'ColNmatchName','MergedMatches'};
                Args.matchGalaxyCatArgs = {'ColDistName', 'GalaxyDist',...
                    'ColNmatchName','GalaxyMatches'};
                Args.matchStarCatArgs = {'ColDistName', 'StarDist',...
                    'ColNmatchName','StarMatches'};

                Args.matchSolarSystemCatArgs = {'ColDistName', 'SolarDist',...
                    'ColNmatchName','SolarMatches'};
            end
            
            Obj.matchMergedCat(Args.matchMergedCatArgs{:});
            Obj.matchGalaxyCat(Args.matchGalaxyCatArgs{:});
            Obj.matchStarCat(Args.matchStarCatArgs{:});

            Obj.matchSolarSystemCat(Args.matchSolarSystemCatArgs{:});
        end

        % measureTransients
        function measureTransients(Obj, Args)
            %{ 
            For each transient candidate measure further properties.
              These depend on the subtraction process, so a function is applied 
              that is determined by the object class.
            Input   : - An AstroDiff object in which CatData is populated.
                      * ...,key,val,...
                        --- AstroZOGY ---
                        'HalfSizeTS' - Half size of area on transients positions in 
                               test statistic images S2 and Z2. Actual size will be  
                               1+2*HalfSizeTS. Used to find peak S2 and Z2 values.
                               Default is 5.
            Author  : Ruslan Konno (Jan 2024)
            Example : AD.measureTransients
            %}
            arguments
                Obj
                
                % AstroZOGY
                Args.HalfSizeTS = 5;
            end
            
            Nobj = numel(Obj);

            for Iobj=1:1:Nobj
                Obj(Iobj).CatData = imProc.sub.measureTransients(Obj(Iobj),...
                    'HalfSizeTS', Args.HalfSizeTS);
            end

        end

        % flagNonTransients
        function flagNonTransients(Obj, Args)
            %{
            Flag transients candidates that are likely not real transients.
            Input  : - An AstroDiff object in which CatData is populated.
                     * ...,key,val,...
                       'flagChi2' - Bool on whether to flag transients candidates
                              based on Chi2 per degrees of freedom criterium. 
                              Default is true.
                       'Chi2dofLimits' - Limits on Chi2 per degrees of freedom. If
                              'filterChi2' is true, all transients candidates outside these
                              limits are flagged. Default is [0.5 2].
                       'flagMag' - Bool on whether to flag transients candidates
                              based on magnitude. Deault is true.
                       'MagLim' - Upper magnitude limit. If 'filterMag' is true,
                              all transients candidates below this limit are flagged. 
                              Default is 21.
                       'flagBadPix_Hard' - Bool on whether to flag transients
                              candidates based on hard bit mask criteria. 
                              Default is true.
                       'NewMask_BadHard' - Hard bit mask criteria for bad pixels in 
                              AD.New image. Default is {'Saturated', 'NearEdge', 'FlatHighStd',
                              'Overlap','Edge','CR_DeltaHT', 'Interpolated','NaN'}.
                       'RefMask_BadHard' - Hard bit mask criteria for bad pixels in 
                              AD.Ref image. Default is {'Saturated', 'NearEdge', 'FlatHighStd',
                              'Overlap','Edge','CR_DeltaHT', 'Interpolated','NaN'}.
                       'flagBadPix_Soft' - Bool on whether to flag transients
                              candidates based on soft bit mask criteria. 
                              Default is true.
                       'NewMask_BadSoft' - Soft bit mask criteria for bad pixels in 
                              AD.New Image. Default is {'HighRN', 'DarkHighVal',
                              'BiasFlaring', 'Hole', 'SrcNoiseDominated'}.
                       'RefMask_BadSoft' - Soft bit mask criteria for bad pixels in 
                              AD.Ref image. Default is {'HighRN', 'DarkHighVal',
                              'BiasFlaring', 'Hole', 'SrcNoiseDominated'}.
                       'flagStarMatches' - Bool on whether to flag transients
                               candidates that have matching star
                               positions. Default is true.
                       --- AstroZOGY ---
                       'flagTranslients' - Bool on whether to flag transients 
                              candidates which score higher in Z2 than S2.
                              Default is true.
            Author  : Ruslan Konno (Jan 2024)
            Example : AD.flagNonTransients
            %}

            arguments
                Obj
                
                Args.flagChi2 logical = true;
                Args.Chi2dofLimits         = [0.5 2];

                Args.flagMag logical = true;
                Args.MagLim = 21;

                Args.flagBadPix_Hard logical  = true;
                Args.NewMask_BadHard       = {'Saturated','NearEdge','FlatHighStd',...
                    'Overlap','Edge','CR_DeltaHT','Interpolated','NaN'};
                Args.RefMask_BadHard       = {'Saturated','NearEdge','FlatHighStd',...
                    'Overlap','Edge','CR_DeltaHT','Interpolated','NaN'};
                
                Args.flagBadPix_Soft logical  = true;
                Args.NewMask_BadSoft       = {'HighRN', 'DarkHighVal', ...
                    'BiasFlaring', 'Hole', 'SrcNoiseDominated'};
                Args.RefMask_BadSoft       = {'HighRN', 'DarkHighVal', ...
                    'BiasFlaring', 'Hole', 'SrcNoiseDominated'};

                Args.flagStarMatches logical = true;

                Args.flagTranslients logical = true;
        
            end

            Nobj = numel(Obj);

            for Iobj=1:1:Nobj
                Obj(Iobj).CatData = imProc.sub.flagNonTransients(Obj(Iobj),...
                    'flagChi2',Args.flagChi2,...
                    'Chi2dofLimits',Args.Chi2dofLimits,...
                    'flagMag', Args.flagMag,...
                    'MagLim', Args.MagLim,...
                    'flagBadPix_Hard', Args.flagBadPix_Hard,...
                    'NewMask_BadHard', Args.NewMask_BadHard,...
                    'RefMask_BadHard', Args.RefMask_BadHard,...
                    'flagBadPix_Soft', Args.flagBadPix_Soft,...
                    'NewMask_BadSoft', Args.NewMask_BadSoft,...
                    'RefMask_BadSoft', Args.RefMask_BadSoft);
            end
        end
        
        function [TranCat, NonTranCat] = separateNonTransients(Obj)
            %{
            Separates transients from likely non-transients into two
              AstroCat outputs.
            Input  : - An AstroDiff object with a CatData that is set and
                       flagged for likely non-transients.
            Output : - TranCat (AstroCat holding transients only).
                     - NonTranCat (AstroCat holding non-transients only).
            Author : Ruslan Konno (Feb 2024)
            Example: [TranCat, NonTranCat] = AD.separateNonTransients
            %}

            arguments
                Obj
            end

            Nobj = numel(Obj);
            for Iobj=Nobj:-1:1
                Transients = ~Obj(Iobj).CatData.getCol('LikelyNotTransient');
                TranCat(Iobj) = Obj(Iobj).CatData.selectRows(Transients);
                NonTranCat(Iobj) = Obj(Iobj).CatData.selectRows(~Transients);
            end
        end

        function AD = removeNonTransients(Obj)
            %{
            Removes likely non-transients from CatData.
            Input  : - An AstroDiff object with a CatData that is set and
                       flagged for likely non-transients.
            Output : - An AstroDiff copy of input AstroDiff but with likely
                       non-transients removed from CatData.
            Author : Ruslan Konno (Feb 2024)
            Example: ADnew = AD.removeNonTransients
            %}

            arguments
                Obj
            end

            Nobj = numel(Obj);
            for Iobj=Nobj:-1:1
                Transients = ~Obj(Iobj).CatData.getCol('LikelyNotTransient');
                AD(Iobj) = Obj(Iobj).copy();
                AD(Iobj).CatData = Obj(Iobj).CatData.selectRows(Transients);
            end

        end

        % fitDT
        % Fit a variability + motion model to the D_T image

    end
    
    methods % catalog matching

        % matchSolarSystemCat
        function matchSolarSystemCat(Obj, Args)
            %{
            Matches transients candidates to an external catalog for solar 
              system objects, then adds information to CatData.
            Input  : - An AstroDiff object in which CatData is populated.
                     * ...,key,val,...
                       'ColDistName' - Name of column with the angular 
                              distance to the closest match. Default is
                              'SolarDist.'
                       'ColNmatchName' - Name of columns with number of 
                              matches. Default is 'SolarMatches.'
            Author : Ruslan Konno (Feb 2024)
            Example: AD.matchSolarSystemCat
            %}

            arguments
                Obj

                Args.ColDistName = 'SolarDist';
                Args.ColNmatchName = 'SolarMatches';
            end

            Nobj = numel(Obj);

            for Iobj=1:1:Nobj
                [~, ~, Obj(Iobj)] = imProc.match.match2solarSystem(...
                    Obj(Iobj), 'InCooUnits', 'deg', ...
                    'ColDistName', Args.ColDistName,...
                    'ColNmatchName', Args.ColNmatchName);
            end

        end
        % matchRedshiftCat
        % Match catalog to redshift catalogs and add information to CatData

        function matchMergedCat(Obj, Args)
            %{
            Matches transients candidates to an external merged catalog,
              then adds information to CatData.
            Input  : - An AstroDiff object in which CatData is populated.
                     * ...,key,val,...
                       'ColDistName' - Name of column with the angular 
                              distance to the closest match. Default is
                              'MergedDist.'
                       'ColNmatchName' - Name of columns with number of 
                              matches. Default is 'MergedMatches.'
            Author : Ruslan Konno (Feb 2024)
            Example: AD.matchMergedCat
            %}

            arguments
                Obj

                Args.ColDistName = 'MergedDist';
                Args.ColNmatchName = 'MergedMatches';
            end         
            
            Nobj = numel(Obj);

            for Iobj=1:1:Nobj
                sizeCat = size(Obj(Iobj).CatData.Catalog,1);
                
                % If transients catalog is empty, continue.
                if sizeCat == 0
                    continue
                end                
                       
                Obj(Iobj).CatData = imProc.match.match_catsHTMmerged(Obj(Iobj).CatData);
                [Obj(Iobj).CatData, ~, ~, ~] = imProc.match.match_catsHTM(...
                    Obj(Iobj).CatData, 'MergedCat','ColDistName',Args.ColDistName,...
                    'ColNmatchName',Args.ColNmatchName);                
            end

        end
        
        % matchGalaxyCat
        function matchGalaxyCat(Obj, Args)
            %{
            Matches transients candidates to an external catalog of galaxies,
              then adds information to CatData.
            Input  : - An AstroDiff object in which CatData is populated.
                     * ...,key,val,...
                       'ColDistName' - Name of column with the angular 
                              distance to the closest match. Default is
                              'GalaxyDist.'
                       'ColNmatchName' - Name of columns with number of 
                              matches. Default is 'GalaxyMatches.'
            Author : Ruslan Konno (Feb 2024)
            Example: AD.matchGalaxyCat
            %}

            arguments
                Obj

                Args.ColDistName = 'GalaxyDist';
                Args.ColNmatchName = 'GalaxyMatches';
            end
            
            Nobj = numel(Obj);

            for Iobj=1:1:Nobj
                sizeCat = size(Obj(Iobj).CatData.Catalog,1);
                
                % If transients catalog is empty, continue.
                if sizeCat == 0
                    continue
                end                
            
                [Obj(Iobj).CatData, ~, ~, ~] = imProc.match.match_catsHTM(...
                    Obj(Iobj).CatData, 'GLADE','ColDistName',Args.ColDistName,...
                    'ColNmatchName',Args.ColNmatchName);
            end
        end
        
        % matchStarCat
        function matchStarCat(Obj, Args)
            %{
            Matches transients candidates to an external catalog of stars,
              then adds information to CatData.
            Input  : - An AstroDiff object in which CatData is populated.
                     * ...,key,val,...
                       'ColDistName' - Name of column with the angular 
                              distance to the closest match. Default is
                              'StarDist.'
                       'ColNmatchName' - Name of columns with number of 
                              matches. Default is 'StarMatches.'
            Author : Ruslan Konno (Feb 2024)
            Example: AD.matchStarCat
            %}

            arguments
                Obj
                
                Args.ColDistName = 'StarDist';
                Args.ColNMatchName = 'StarMatches';
            end

            Nobj = numel(Obj);

            for Iobj=1:1:Nobj
                sizeCat = size(Obj(Iobj).CatData.Catalog,1);
                
                % If transients catalog is empty, continue.
                if sizeCat == 0
                    continue
                end   
                
                [Obj(Iobj).CatData, ~, ~, ~] = imProc.match.match_catsHTM(...
                    Obj(Iobj).CatData, 'GAIADR3','ColDistName',...
                    Args.ColDistName, 'ColNmatchName',Args.ColNMatchName);
            end
        end
    end
    
    methods % transients inspection and measurment

        % transientsCutouts
        function ADc = cutoutTransients(Obj, Args)
            % Create cutouts of images around coordinates.
            %    Given an AstroZOGY/AstroDiff object, generate a new object
            %    (the same class as the input class), populated with image
            %    cutouts around selected coordinates.
            % Input  : - An AstroDiff/AstroZOGY object.
            %          * ...,key,val,...
            %            'XY' - A matrix of [X, Y] coordinates. The cutouts will be
            %                    generated around these positions.
            %                    If empty, then use getXY on the AstroDiff.CatData
            %                    AstroCatalog object.
            %                    Default is [].
            %            'HalfSize' - Half size of cutouts.
            %                    Default is 25.
            %            'CropNew' - Logical indicating if to populate the crop
            %                    of the New image. Default is true.
            %            'CropRef' - Logical indicating if to populate the crop
            %                    of the Ref image. Default is true.
            %            'CropSub' - Logical indicating if to populate the crop
            %                    of the difference image. Default is true.
            %            'CreateNewObj' - A logical indicating if to create
            %                    a new object of the crop object.
            %                    Default is true.
            %            'CropProp' - A cell array of additional properties
            %                    to populate and crop (will be done only if
            %                    property exist).
            %                    Default is {'Z2','S','Scorr'}.
            % Output : - An AstroDiff/AstroZOGY object with element per
            %            cutout.
            % Author : Eran Ofek (Feb 2024)
            % Example: ADc = AD.cutoutTransients;
            
            arguments
                Obj
                Args.XY                = [];
                Args.HalfSize          = 25;
                
                Args.CropNew logical   = true;
                Args.CropRef logical   = true;
                Args.CropSub logical   = true;
                
                Args.CreateNewObj logical   = true;
                
                Args.CropProp               = {'Z2','S','Scorr'};
            end
            
            NcropProp = numel(Args.CropProp);

            % Cast class of object to cutout objects
            ADc = feval(class(Obj));

            Nobj = numel(Obj);
            IndC = 0;
            for Iobj=Nobj:-1:1
                
                % Skip empty catalogs
                sizeCat = size(Obj(Iobj).CatData.Catalog,1);
                if sizeCat == 0
                    continue;
                end
                
                if isempty(Args.XY)
                    % get coordinates from AstroCatalog object
                    XY = Obj(Iobj).CatData.getXY;                   
                else
                    XY = Args.XY;
                end
                
                Nxy = size(XY, 1);
                for Ixy=1:1:Nxy
                    IndC = IndC + 1;
                    Pos = [XY(Ixy,:), Args.HalfSize, Args.HalfSize];
                    if Args.CropSub
                        ADc(IndC) = Obj(Iobj).crop(Pos,...
                                         'Type','center',...
                                         'UpdateCat',true,...
                                         'UpdateWCS',true,...
                                         'CreateNewObj',Args.CreateNewObj);
                    end
                    
                    if Args.CropNew
                        ADc(IndC).New = Obj(Iobj).New.crop(Pos,...
                                                     'Type','center',...
                                                     'UpdateCat',true,...
                                                     'UpdateWCS',true,...
                                                     'CreateNewObj',Args.CreateNewObj);
                    end
                    
                    if Args.CropRef
                        ADc(IndC).Ref = Obj(Iobj).Ref.crop(Pos,...
                                                     'Type','center',...
                                                     'UpdateCat',true,...
                                                     'UpdateWCS',true,...
                                                     'CreateNewObj',Args.CreateNewObj);
                    end
                    
                    % Cut additional properties
                    for Icp=1:1:NcropProp
                        if isprop(Obj(Iobj), Args.CropProp{Icp})
                            ADc(IndC).(Args.CropProp{Icp}) = ...
                                imUtil.cut.trim(Obj(Iobj).(Args.CropProp{Icp}), Pos, 'center');
                        end
                    end
                    
                end
                
            end
        end
        
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
