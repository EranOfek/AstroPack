function [Cat, ColCellOut, Res]=find_measure_sources(Image, Args)
    % find sources in an image
    % Package: imUtil.sources
    % Description: Find sources in an image using a matched filter of template
    %              bank and calculate basic properties for all the sources
    %              including first and second moment, aperture photometry, and
    %              PSF flux (using convolution).
    %              The program can filter the image simultanously with multiple
    %              filters, and the local maxima will be selected in the
    %              maximum of all the filtered images.
    % Input  : - A matrix (2D image).
    %            Alternatively, this can be a structure (or an imCl object)
    %            with image, background and variance fields. Field names can be
    %            changed using the 'ImageField', 'BackField', and 'VarField'
    %            arguments.
    %          * Pairs of ...,key,val,... The following keywords are available:
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
    %            'RemoveEdgeDist' - Indicating if to remove sources which
    %                   XPEAK/YPEAK are near the image edges.
    %                   If NaN, then no sources are removed. Otherwise, this is
    %                   the distance in pixels from the edge to remove the
    %                   sources. For example, 0 will remove sources located
    %                   exactly at the edge pixels.
    %                   Default is 0.
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
    %            'MomPar' - A cell array of additional parameters to pass to
    %                   the imUtil.image.moment2 function.
    %                   Default is {}.
    %            'OutType' - Output type. Options are:
    %                   'mat' - a matrix.
    %                   'table' - a table.
    %                   'catCl' - An catCl object.
    %                   Default is 'catCl'.
    %            'ColCell' - A cell array of column names to generate in the
    %                   output.
    %                   Default is
    %                   {'XPEAK','YPEAK','TEMP_ID','SN','FLUX_CONV','BACK_IM','VAR_IM',...           
    %                        'X', 'Y',...
    %                        'X2','Y2','XY',...
    %                        'FLUX_APER', 'APER_AREA', 'FLUX_BOX','BACK_ANNULUS', 'STD_ANNULUS', 'FLUX_WAPER'});
    %            'Conn' - Connectivity parameter for local maxima
    %                   identification.
    %                   Default is 8.
    %            'Gain' - Default is 1.
    %            'LupSoftPar' - Luptitude softening parameter. Default is 1e-10.
    %            'ZP' - ZP for magnitude. Default is 25.
    %            'ImageField' - Image field. Default is 'Im'.
    %            'BackField' - Background field. Default is 'Back'.
    %            'VarField' - Variance field. Default is 'Var'.
    % Output : - A catalog of sources and their properties.
    %            Forced photometry requestes will have TEMP_ID=NaN.
    %          - A cell array of column names in the output catalog.
    %          - A structure with additional calculated output (e.g., the
    %            filtered image).
    % License: GNU general public license version 3
    % Tested : Matlab R2015b
    %     By : Eran O. Ofek                    Apr 2016
    %    URL : http://weizmann.ac.il/home/eofek/matlab/
    % Example: [Cat,ColCell,Res]=imUtil.sources.find_measure_sources(I1.Im,'Threshold',5)
    %          Im=imUtil.kernel2.gauss(2,[128 128]);
    %          Im=Im.*1000 +randn(size(Im));        
    %          [Cat,ColCell,Res]=imUtil.sources.find_measure_sources(Im,'Threshold',5);
    % Reliable: 2
    %--------------------------------------------------------------------------

    arguments
        Image
        Args.Threshold                     = 5;
        Args.Psf                           = [];
        Args.PsfFun function_handle        = @imUtil.kernel2.gauss;
        Args.PsfFunPar cell                = {[0.1;1.5;3]};
        Args.RemoveEdgeDist                = 0;  % NaN for non removal
        Args.ForcedList                    = [];
        Args.OnlyForced(1,1) logical       = false;
        Args.BackIm                        = [];
        Args.VarIm                         = [];
        Args.BackPar cell                  = {};
        Args.MomPar cell                   = {};
        Args.OutType                       = 'AstroCatalog';   % 'mat', 'table', 'catcl', 'struct'
        Args.ColCell cell                  = {'XPEAK','YPEAK','TEMP_ID','SN','FLUX_CONV','BACK_IM','VAR_IM',...           
                                                'X1', 'Y1',...
                                                'X2','Y2','XY',...
                                                'FLUX_APER', 'APER_AREA', 'BACK_ANNULUS', 'STD_ANNULUS', ...
                                                'FLUXERR_APER',...
                                                'MAG_APER', 'MAGERR_APER', 'BACKMAG_ANNULUS',...
                                                'MAG_CONV', 'MAGERR_CONV'};
        Args.Conn                          = 8;
        
        Args.Gain                          = 1;      % only for errors calculation
        Args.LupSoftPar                    = 1e-10;
        Args.ZP                            = 25;
        
        Args.ImageField char               = 'Im';
        Args.BackField char                = 'Back';
        Args.VarField char                 = 'Var';
    end
    
    ZP_Flux = 10.^(0.4.*Args.ZP);

    Mom1Cell   = {'X1', 'Y1'};
    Mom2Cell   = {'X2','Y2','XY'};
    Mom3Cell   = {'FLUX_APER', 'APER_AREA', 'FLUX_BOX','BACK_ANNULUS', 'STD_ANNULUS', 'FLUX_WAPER', 'FLUXERR_APER', 'MAG_APER', 'MAGERR_APER', 'BACKMAG_ANNULUS'};

    
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

    

    [Src,Template] = imUtil.sources.findSources(Image, 'Threshold',Args.Threshold,...
                                            'Psf',Args.Psf,...
                                            'PsfFun',Args.PsfFun,...
                                            'PsfFunPar',Args.PsfFunPar,...
                                            'ForcedList',Args.ForcedList,...
                                            'OnlyForced',Args.OnlyForced,...
                                            'BackIm',Back,...
                                            'VarIm',Var,...
                                            'OutType','struct',...
                                            'Conn',Args.Conn,...
                                            'BackField',Args.BackField,...
                                            'VarField',Args.VarField);
    
    % Number of templates
    Ntemplate = size(Template,3);
    Nsrc      = numel(Src.XPEAK);
    %
    %if nargout>2
    %    varargout = cell(1:nargout-1);
    %    [varargout{1:nargout-1}] = imUtil.image.moment2(Image-Back,Pos(:,1),Pos(:,2),Args.MomPar{:});
    %end

    if any(ismember(Args.ColCell,Mom3Cell))
        %ImageSubBack = Image-Back;
        
        %[Cube, RoundX, RoundY, X, Y] = imUtil.cut.image2cutouts(Image-Back, Src.XPEAK,Src.YPEAK);
        
        % For aperture/PSF photometry use: imUtil.sources.aperPhotCube
        %
        %Aper = imUtil.sources.aperPhotCube(Cube, X, Y, 'PSF',PSF,'SubPixShift','fft')
        
        % old:
        [M1,M2,Aper] = imUtil.image.moment2(Image-Back,Src.XPEAK,Src.YPEAK,Args.MomPar{:});
        
    elseif any(ismember(Args.ColCell,Mom2Cell))
        [M1,M2] = imUtil.image.moment2(Image-Back,Src.XPEAK,Src.YPEAK,Args.MomPar{:});
        Aper    = [];
    elseif any(ismember(Args.ColCell,Mom1Cell))
        [M1] = imUtil.image.moment2(Image-Back,Src.XPEAK,Src.YPEAK,Args.MomPar{:});
        M2   = [];
        Aper = [];
    else
        % no need to call moment2
        M1   = [];
        M2   = [];
        Aper = [];
    end


    %--- construct the output array ---

    % calc number of requested columns
    Ncol = numel(Args.ColCell);

    NcolOut = Ncol;
    % properties that may have multiple columns:
    if any(strcmp(Args.ColCell,'SN'))
        NcolOut = NcolOut + Ntemplate - 1;
    end
    if any(strcmp(Args.ColCell,'FLUX_CONV'))
        NcolOut = NcolOut + Ntemplate - 1;
    end
    if any(strcmp(Args.ColCell,'MAG_CONV'))
        NcolOut = NcolOut + Ntemplate - 1;
    end
    if any(strcmp(Args.ColCell,'FLUX_APER'))
        NcolOut = NcolOut + numel(Aper.AperRadius) - 1;
    end
    if any(strcmp(Args.ColCell,'FLUXERR_APER'))
        NcolOut = NcolOut + numel(Aper.AperRadius) - 1;
    end
    if any(strcmp(Args.ColCell,'MAG_APER'))
        NcolOut = NcolOut + numel(Aper.AperRadius) - 1;
    end
    if any(strcmp(Args.ColCell,'MAGERR_APER'))
        NcolOut = NcolOut + numel(Aper.AperRadius) - 1;
    end
    if any(strcmp(Args.ColCell,'APER_AREA'))
        NcolOut = NcolOut + numel(Aper.AperRadius) - 1;
    end

    Cat  = nan(Nsrc,NcolOut);
    ColCellOut = cell(1,NcolOut);

    FluxErrAper = [];
    
    K    = 0;
    for Icol=1:1:Ncol
        K = K + 1;
        ColCellOut{K} = Args.ColCell{Icol};
        switch lower(Args.ColCell{Icol})
            case 'xpeak'
                Cat(:,K) = Src.XPEAK;
            case 'ypeak'
                Cat(:,K) = Src.YPEAK;
            case 'temp_id'
                Cat(:,K) = Src.TEMP_ID;
            case 'sn'
                % may have multiple columns
                NC = size(Src.SN,2);
                Cat(:,K:K+NC-1) = Src.SN;
                [ColCellOut(K:K+NC-1)] = deal(sprintf_cell(Args.ColCell{Icol},(1:1:NC)));
                K  = K + NC - 1;
            case 'flux_conv'
                % may have multiple columns
                NC = size(Src.FLUX_CONV,2);
                Cat(:,K:K+NC-1) = Src.FLUX_CONV;
                [ColCellOut(K:K+NC-1)] = deal(sprintf_cell(Args.ColCell{Icol},(1:1:NC)));
                K = K + NC - 1;
            case 'mag_conv'
                % may have multiple columns
                NC = size(Src.FLUX_CONV,2);
                Cat(:,K:K+NC-1) = convert.luptitude(Src.FLUX_CONV, ZP_Flux, Args.LupSoftPar);  
                %Cat(:,K:K+NC-1) = real(Cat(:,K:K+NC-1));
                [ColCellOut(K:K+NC-1)] = deal(sprintf_cell(Args.ColCell{Icol},(1:1:NC)));
                K = K + NC - 1;    
            case 'magerr_conv'
                % may have multiple columns
                NC = size(Src.FLUX_CONV,2);
                Cat(:,K:K+NC-1) = 1.086./Src.SN;
                [ColCellOut(K:K+NC-1)] = deal(sprintf_cell(Args.ColCell{Icol},(1:1:NC)));
                K = K + NC - 1;   
            case 'back_im'
                Cat(:,K) = Src.BACK_IM;
            case 'var_im'
                Cat(:,K) = Src.VAR_IM;
            case 'x1'
                Cat(:,K) = M1.X;
            case 'y1'
                Cat(:,K) = M1.Y;
            case 'x2'
                Cat(:,K) = M2.X2;
            case 'y2'
                Cat(:,K) = M2.Y2;
            case 'xy'
                Cat(:,K) = M2.XY;
            case 'flux_aper'
                % may have multiple columns
                NC = size(Aper.AperPhot,2);
                Cat(:,K:K+NC-1) = Aper.AperPhot;
                [ColCellOut(K:K+NC-1)] = deal(sprintf_cell('FLUX_APER',(1:1:NC)));
                K = K + NC - 1;
            case 'fluxerr_aper'
                % may have multiple columns
                NC = size(Aper.AperPhot,2);
                if isempty(FluxErrAper)
                    AperPhot    = Aper.AperPhot.*Args.Gain;
                    FluxErrAper = sqrt(abs(AperPhot) + Aper.AnnulusStd.^2)./AperPhot;
                end
                Cat(:,K:K+NC-1) = FluxErrAper;
                [ColCellOut(K:K+NC-1)] = deal(sprintf_cell('FLUXERR_APER',(1:1:NC)));
                K = K + NC - 1;
            case 'mag_aper'
                % may have multiple columns
                NC = size(Aper.AperPhot,2);
                Cat(:,K:K+NC-1) = convert.luptitude(Aper.AperPhot, ZP_Flux, Args.LupSoftPar);
                [ColCellOut(K:K+NC-1)] = deal(sprintf_cell('MAG_APER',(1:1:NC)));
                K = K + NC - 1;
            case 'magerr_aper'
                % may have multiple columns
                NC = size(Aper.AperPhot,2);
                if isempty(FluxErrAper)
                    AperPhot    = Aper.AperPhot.*Args.Gain;
                    FluxErrAper = sqrt(abs(AperPhot) + Aper.AnnulusStd.^2)./AperPhot;
                end
                Cat(:,K:K+NC-1) = 1.086 .* FluxErrAper;
                [ColCellOut(K:K+NC-1)] = deal(sprintf_cell('MAGERR_APER',(1:1:NC)));
                K = K + NC - 1;    
            case 'aper_area'
                % may have multiple columns
                NC = size(Aper.AperArea,2);
                Cat(:,K:K+NC-1) = Aper.AperArea.*ones(size(Cat(:,1)));
                [ColCellOut(K:K+NC-1)] = deal(sprintf_cell('APER_AREA',(1:1:NC)));
                K = K + NC - 1;
            case 'flux_box'
                % this is no longer in use
                Cat(:,K) = Aper.BoxPhot;
            case 'back_annulus'
                % need to add Src.BACK_IM because the background was subtract
                % (in moment2 input)
                Cat(:,K) = Aper.AnnulusBack + Src.BACK_IM;
            case 'backmag_annulus'
                % need to add Src.BACK_IM because the background was subtract
                % (in moment2 input)
                Cat(:,K) = convert.luptitude(Aper.AnnulusBack + Src.BACK_IM, ZP_Flux, Args.LupSoftPar);    
            case 'std_annulus'
                Cat(:,K) = Aper.AnnulusStd;
            case 'flux_waper'
                Cat(:,K) = Aper.WeightedAper;
            otherwise
                error('Unknown column in ColCell (%s)',Args.ColCell{Icol});

        end
    end

    if ~isnan(Args.RemoveEdgeDist)
        SizeIm = size(Image);
        FlagEdge = Src.XPEAK<=(Args.RemoveEdgeDist+1) | ...
                   Src.XPEAK>=(SizeIm(2)-Args.RemoveEdgeDist) | ...
                   Src.YPEAK<=(Args.RemoveEdgeDist+1) | ...
                   Src.YPEAK>=(SizeIm(1)-Args.RemoveEdgeDist);
        Cat = Cat(~FlagEdge,:);
    end

    if nargout>2
        Res.Src  = Src;
        Res.M1   = M1;
        Res.M2   = M2;
        Res.Aper = Aper;
        Res.FiltImage = FiltImage;
        Res.FiltImageVar = FiltImageVar;
    end

    % Convert to output table
    switch lower(Args.OutType)
        case 'astrocatalog'
            Tmp = AstroCatalog;
            Tmp.Catalog = Cat;
            Cat = Tmp;
            Cat.ColNames = ColCellOut;
            % obsolete
            %Cat.ColX = colname2ind(Cat,'X');
            %Cat.ColY = colname2ind(Cat,'Y');
            %Cat.CooType = 'pix';

        case 'mat'
            % do nothing
        case 'table'
            Cat = array2table(Cat,'VariableNames',ColCellOut);
        case 'catcl'
            Cat = catCl.array2catCl(Cat,ColCellOut);
        otherwise
            error('Unknown OutType option');
    end
end

%%%
function CellStr = sprintf_cell(Str,Ind)
    N = numel(Ind);
    CellStr = cell(1,N);
    for I=1:1:N
        CellStr{I} = sprintf('%s_%d',Str,Ind(I));
    end
end



