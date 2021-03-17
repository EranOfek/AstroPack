function [Cat,ColCellOut,Res]=find_sources(Image,varargin)
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
% Example: [Cat,ColCell,Res]=imUtil.sources.find_sources(I1.Im,'Threshold',5)
% Reliable: 2
%--------------------------------------------------------------------------

InPar = inputParser;

%addOptional(InPar,'BackFunOut',{'back','var'});  % back, var, std
addOptional(InPar,'Threshold',5);

addOptional(InPar,'Psf',[]);
addOptional(InPar,'PsfFun',@imUtil.kernel2.gauss);
addOptional(InPar,'PsfFunPar',{[0.1;1.5;3]});

addOptional(InPar,'RemoveEdgeDist',0);  % NaN for non removal

addOptional(InPar,'ForcedList',[]);
addOptional(InPar,'OnlyForced',false);

addOptional(InPar,'BackIm',[]);
addOptional(InPar,'VarIm',[]);

addOptional(InPar,'BackPar',{});
addOptional(InPar,'MomPar',{});
addOptional(InPar,'OutType','catCl');  % 'mat', 'table', 'catcl', 'struct'
addOptional(InPar,'ColCell',{'XPEAK','YPEAK','TEMP_ID','SN','FLUX_CONV','BACK_IM','VAR_IM',...           
                             'X', 'Y',...
                             'X2','Y2','XY',...
                             'FLUX_APER', 'APER_AREA', 'FLUX_BOX','BACK_ANNULUS', 'STD_ANNULUS', 'FLUX_WAPER'});
 
addOptional(InPar,'Conn',8);
addOptional(InPar,'ImageField','Im');
addOptional(InPar,'BackField','Back');
addOptional(InPar,'VarField','Var');


parse(InPar,varargin{:});
InPar = InPar.Results;


Mom1Cell   = {'X', 'Y'};
Mom2Cell   = {'X2','Y2','XY'};
Mom3Cell   = {'FLUX_APER', 'APER_AREA', 'FLUX_BOX','BACK_ANNULUS', 'STD_ANNULUS', 'FLUX_WAPER'};




if isstruct(Image)
    if isempty(InPar.BackIm)
        Back = Image.(InPar.BackField);
    end
    if isempty(InPar.VarIm)
        Var  = Image.(InPar.VarField);
    end
else
    Back = InPar.BackIm;
    Var  = InPar.VarIm;
end

if isempty(Back) || isempty(Var)
    [Back,Var] = imUtil.background.background(Image,InPar.BackPar{:});
end

if ~isempty(InPar.Psf)
    Template = InPar.Psf;
else
    Template = InPar.PsfFun(InPar.PsfFunPar{:});
end

% Template = single(Template);
% Back     = single(Back);
% Var      = single(Var);
% Image    = single(Image);
    
    
% filter the images with all the templates
[SN,Flux,FiltImage,FiltImageVar] = imUtil.filter.filter2_snBank(Image,Back,Var,Template);
if InPar.OnlyForced
    Pos = zeros(0,4);
else
    [~,Pos]                       = imUtil.image.local_maxima(SN,1,InPar.Threshold,InPar.Conn);
end
% Pos contains [X,Y,SN,IndexTemplate]


%{'X_PEAK','Y_PEAK','SN','TEMPLATE_ID','FLUX','BACK_IM','VAR_IM','XWIN_IMAGE','YWIN_IMAGE'
Size = size(SN);
Ntemplate = size(Template,3);

% add forced photometry surces
if ~isempty(InPar.ForcedList)
    NsrcF = size(InPar.ForcedList,1);
    PosF  = nan(NsrcF,4);
    % take the rounded positions
    PosF(:,1:2) = round(InPar.ForcedList);
    % forced photomety are marked as arriving from template=NaN
    Pos = [Pos; PosF];
end



Nsrc = size(Pos,1);

IndI = repmat(Pos(:,2),1,Ntemplate);
IndJ = repmat(Pos(:,1),1,Ntemplate);
IndT = (1:1:Ntemplate).*ones(Nsrc,1);
% consider writing sub2ind_fast for 3D
Ind  = sub2ind(Size,IndI,IndJ,IndT);

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




%if nargout>2
%    varargout = cell(1:nargout-1);
%    [varargout{1:nargout-1}] = imUtil.image.moment2(Image-Back,Pos(:,1),Pos(:,2),InPar.MomPar{:});
%end

if any(ismember(InPar.ColCell,Mom3Cell))
    [M1,M2,Aper] = imUtil.image.moment2(Image-Back,Src.XPEAK,Src.YPEAK,InPar.MomPar{:});
elseif any(ismember(InPar.ColCell,Mom2Cell))
    [M1,M2] = imUtil.image.moment2(Image-Back,Src.XPEAK,Src.YPEAK,InPar.MomPar{:});
    Aper    = [];
elseif any(ismember(InPar.ColCell,Mom1Cell))
    [M1] = imUtil.image.moment2(Image-Back,Src.XPEAK,Src.YPEAK,InPar.MomPar{:});
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
Ncol = numel(InPar.ColCell);

NcolOut = Ncol;
% properties that may have multiple columns:
if any(strcmp(InPar.ColCell,'SN'))
    NcolOut = NcolOut + Ntemplate - 1;
end
if any(strcmp(InPar.ColCell,'FLUX_CONV'))
    NcolOut = NcolOut + Ntemplate - 1;
end
if any(strcmp(InPar.ColCell,'FLUX_APER'))
    NcolOut = NcolOut + numel(Aper.AperRadius) - 1;
end
if any(strcmp(InPar.ColCell,'APER_AREA'))
    NcolOut = NcolOut + numel(Aper.AperRadius) - 1;
end

Cat  = nan(Nsrc,NcolOut);
ColCellOut = cell(1,NcolOut);

K    = 0;
for Icol=1:1:Ncol
    K = K + 1;
    ColCellOut{K} = InPar.ColCell{Icol};
    switch lower(InPar.ColCell{Icol})
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
            [ColCellOut(K:K+NC-1)] = deal(sprintf_cell(InPar.ColCell{Icol},(1:1:NC)));
            K  = K + NC - 1;
        case 'flux_conv'
            % may have multiple columns
            NC = size(Src.FLUX_CONV,2);
            Cat(:,K:K+NC-1) = Src.FLUX_CONV;
            [ColCellOut(K:K+NC-1)] = deal(sprintf_cell(InPar.ColCell{Icol},(1:1:NC)));
            K = K + NC - 1;
        case 'back_im'
            Cat(:,K) = Src.BACK_IM;
        case 'var_im'
            Cat(:,K) = Src.VAR_IM;
        case 'x'
            Cat(:,K) = M1.X;
        case 'y'
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
        case 'aper_area'
            % may have multiple columns
            NC = size(Aper.AperArea,2);
            Cat(:,K:K+NC-1) = Aper.AperArea;
            [ColCellOut(K:K+NC-1)] = deal(sprintf_cell('APER_AREA',(1:1:NC)));
            K = K + NC - 1;
        case 'flux_box'
            Cat(:,K) = Aper.BoxPhot;
        case 'back_annulus'
            % need to add Src.BACK_IM because the background was subtract
            % (in moment2 input)
            Cat(:,K) = Aper.AnnulusBack + Src.BACK_IM;
        case 'std_annulus'
            Cat(:,K) = Aper.AnnulusStd;
        case 'flux_waper'
            Cat(:,K) = Aper.WeightedAper;
        otherwise
            error('Unknown column in ColCell (%s)',InPar.ColCell{Icol});
            
    end
end

if ~isnan(InPar.RemoveEdgeDist)
    SizeIm = size(Image);
    FlagEdge = Src.XPEAK<=(InPar.RemoveEdgeDist+1) | ...
               Src.XPEAK>=(SizeIm(2)-InPar.RemoveEdgeDist) | ...
               Src.YPEAK<=(InPar.RemoveEdgeDist+1) | ...
               Src.YPEAK>=(SizeIm(1)-InPar.RemoveEdgeDist);
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




switch lower(InPar.OutType)
    case 'mat'
        % do nothing
    case 'table'
        Cat = array2table(Cat,'VariableNames',ColCellOut);
    case 'catcl'
        Cat = catCl.array2catCl(Cat,ColCellOut);
    otherwise
        error('Unknown OutType option');
end


%%%
function CellStr = sprintf_cell(Str,Ind)
N = numel(Ind);
CellStr = cell(1,N);
for I=1:1:N
    CellStr{I} = sprintf('%s_%d',Str,Ind(I));
end


