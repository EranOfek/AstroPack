function [Result,Template,FiltImage,FiltImageVar] = findSources(Image, Args)
    % Find sources, using matched filter template bank, in a 2D image
    % Input  : - A 2D image.
    %          * ...,key,val,...
    %            'Threshold' - Detection threshold above background in units of
    %                   the background std. 
    %                   Default is 5.
    %            'Psf' - A PSF stamp or a cube of PSFs. If a cube then the PSF
    %                   index is the third dimesnion.
    %                   The input image will be filtered with each PSF and 
    %                   local maxima will be searched in all the filtered
    %                   images.
    %                   If provided, this parameter overrid the PsfFun input
    %                   argument.
    %                   Default is [].
    %            'PsfFun' - A function handle to generate PSF or a cube of
    %                   PSFs.
    %                   Default is @imUtil.kernel2.gauss.
    %            'PsfFunPar' - A cell array of parameters to pass to the PsfFun
    %                   function.
    %                   Default is {[0.1;1.5;3]} (i.e., will generate a cuve of
    %                   templates with Gaussian PSF with sigmas of 0.1, 1.5 and
    %                   3 pixels).
    %
    %            'ForcedList' - An [X,Y] coordinates on which to perform forced
    %                   photometry measurments.
    %                   Forced photometry requestes have TEMP_ID=NaN.
    %                   Rounded coordinates must be within image boundries.
    %                   Default is [].
    %            'OnlyForced' - A logical flag indicating if to run only forced
    %                   photometry.
    %                   Default is false.
    %            'BackIm' - A background image. If provided, will overrid the
    %                   Back field in the input.
    %                   If empty, and background is not provided, then it will
    %                   be calculated using the imUtil.background.background
    %                   function.
    %                   Default is [].
    %            'VarIm' - A variance image. If provided, will overrid the
    %                   Var field in the input.
    %                   If empty, and variance is not provided, then it will
    %                   be calculated using the imUtil.background.background
    %                   function.
    %                   Default is [].
    %            'BackPar' - A cell array of additional parameters to pass to
    %                   the imUtil.background.background function.
    %                   Default is {}.
    %            'OutType' - Output type. Options are:
    %                   'table' - a table.
    %                   'struct' - a struct.
    %                   Default is 'struct'.
    %            'Conn' - Connectivity parameter for local maxima
    %                   identification.
    %                   Default is 8.
    %            'CleanSources' - A logocal indicating if to remove bad
    %                   sources (delta functions and near edge) using
    %                   imUtil.sources.cleanSources
    %                   Default is false.
    %            'cleanSourcesArgs' - A cell array of additional args to
    %                   pass to imUtil.sources.cleanSources.
    %                   Default is {}.
    %            'SortByY' - Sort sources by Y position.
    %                   Default is true.
    %
    %            'ImageField' - Image field. Default is 'Im'.
    %            'BackField' - Background field. Default is 'Back'.
    %            'VarField' - Variance field. Default is 'Var'.
    % Output : - Either a table, AstroCatalog, or struct with the following
    %            fields:
    %            .XPEAK - Source X position (whole pixel)
    %            .YPEAK - Source Y position (whole pixel)
    %            .TEMP_ID - Themplate bankd index for source maximal S/N.
    %            .SN - S/N.
    %            .FLUX_CONV - A Ntemplate column matrix with
    %                   convolution-based whole pixel flux for each one of the
    %                   templates.
    %            .BACK_IM - A vector of background at source position.
    %            .VAR_IM - A vector of variance at source position.
    %          - Cube of templates.
    %          - Filtered image.
    %          - Filtered image variance.
    % Author : Eran Ofek (Dec 2021)
    % Example: Image = randn(1700, 1700);
    %          Result = imUtil.sources.findSources(Image);
    
    arguments
        
        Image
        Args.Threshold                     = 5;
        Args.Psf                           = [];
        Args.PsfFun function_handle        = @imUtil.kernel2.gauss;
        Args.PsfFunPar cell                = {[0.1;1.5;3]};
        Args.ForcedList                    = [];
        Args.OnlyForced(1,1) logical       = false;
        Args.BackIm                        = [];
        Args.VarIm                         = [];
        Args.BackPar cell                  = {};
        Args.OutType                       = 'struct';   %  'struct' | 'table'
        Args.Conn                          = 8;
        Args.CleanSources logical          = false;
        Args.cleanSourcesArgs cell         = {};
        Args.SortByY logical               = true;
        Args.BackField char                = 'Back';
        Args.VarField char                 = 'Var';
    end
    
    if isstruct(Image)
        if isempty(Args.BackIm)
            Back = Image.(Args.BackField);
        end
        if isempty(Args.VarIm)
            Var  = Image.(Args.VarField);
        end
    else
        Back = Args.BackIm;
        Var  = Args.VarIm;
    end

    if isempty(Back) || isempty(Var)
        [Back,Var] = imUtil.background.background(Image,Args.BackPar{:});
    end

    if ~isempty(Args.Psf)
        Template = Args.Psf;
    else
        Template = Args.PsfFun(Args.PsfFunPar{:});
    end

    % filter the images with all the templates
    [SN,Flux,FiltImage,FiltImageVar] = imUtil.filter.filter2_snBank(Image,Back,Var,Template);
    if Args.OnlyForced
        Pos = zeros(0,4);
    else
        %[~,Pos]                       = imUtil.image.local_maxima(SN,1,Args.Threshold,Args.Conn);
        % much faster:
        [Pos] = imUtil.sources.findLocalMax(SN, 'Variance',1, 'Threshold',Args.Threshold,'Conn',Args.Conn, 'Algo','findlocalmex'); %findlocal');
        
        % Pos contains:  [X,Y,SN,index]
    end
    % Pos contains [X,Y,SN,IndexTemplate]


    Size = size(SN);
    Ntemplate = size(Template,3);

    % add forced photometry surces
    if ~isempty(Args.ForcedList)
        NsrcF = size(Args.ForcedList,1);
        PosF  = nan(NsrcF,5);
        % take the rounded positions
        PosF(:,1:2) = round(Args.ForcedList);
        % forced photomety are marked as arriving from template=NaN
        Pos = [Pos; PosF];
    end

    Nsrc = size(Pos,1);

    if Args.SortByY
        [Pos] = sortrows(Pos,2);
    end
    
    IndI = repmat(Pos(:,2),1,Ntemplate);
    IndJ = repmat(Pos(:,1),1,Ntemplate);
    IndT = (1:1:Ntemplate).*ones(Nsrc,1);
    %Ind  = sub2ind(Size,IndI,IndJ,IndT);
    Ind  = imUtil.image.sub2ind3d_fast(Size,IndI,IndJ,IndT);
    
    
    Src.XPEAK     = Pos(:,1);
    Src.YPEAK     = Pos(:,2);
    % S/N from local_maxima is not usedSrc.BACK_IM
    Src.TEMP_ID   = Pos(:,4);
    Src.SN        = SN(Ind);
    Src.FLUX_CONV = Flux(Ind);

            
    if numel(Back)==1
        Src.BACK_IM = Back;
        Src.VAR_IM  = Var;
    else
        Ind  = imUtil.image.sub2ind_fast(Size(1:2),IndI(:,1),IndJ(:,1));
        Src.BACK_IM   = Back(Ind);
        Src.VAR_IM    = Var(Ind);
    end

    if Args.CleanSources
        ImageSizeXY = fliplr(size(Image));
        [Src] = imUtil.sources.cleanSources(Src, Args.cleanSourcesArgs{:}, 'ImageSizeXY',ImageSizeXY);
    end
    
    switch lower(Args.OutType)
        case 'struct'
            Result = Src;
       
        case 'table'
            Result = struct2table(Src);
        otherwise
            error('Unknown OutType option');
    end
           
    
    
end