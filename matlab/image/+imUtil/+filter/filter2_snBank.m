function [SN,Flux,FiltImage,FiltImageVar,Info]=filter2_snBank(Image,Background,Variance,Template,varargin)
% Filter an image with a bank of PSFs and calculate the S/N and Flux estimators
% Package: imUtil.filter
% Description: Filter an image with a template/PSF bank and
%              calculate the signal-to-noise ratio (S/N) for
%              template-detection in each pixel, and the flux-estimator at
%              each pixel.
%              In order to find sources, you further need to identify
%              local maxima in the S/N
%              image (first output argument). The S/N of the source is the
%              S/N at the local maximum location, and the flux estimator of
%              the source is the Flux-image (second output argument) at
%              this local maximum.
%              See imUtil.background.<TAB> for background and variance
%              estimation functions.
%              See imUtil.image.local_maxima for source search options.
% Input  : - An image.
%          - A background map, or scalar. If the input image is background
%            subtracted then give here 0.
%            If empty, or not provided then use
%            imUtil.background.modeVar_LogHist
%            to calculate the robust background.
%            The mode is calculated with the Log=false option in order to
%            avoid the log of negative numbers.
%          - A variance image (or scalar).
%            If empty, or not provided use imUtil.background.mode
%            to calculate the robust variance.
%          - Template bank (no assumption on normalization / will be
%            normalized to have unity sum).
%            Template may be a single matrix, or a cube in which the third
%            dimension is the template index.
%            Alternativel, this can be a function handle that returns a
%            matrix or cube of templates.
%            Default is @imUtil.kernel2.gauss.
%          * Additional arguments to passt to the function handle that
%            generate the template bank.
%            Default is {}.
% Output : - S/N image or cube (detection statistics). This is the S/N per pixel
%            for detecting the template.
%            In order to find sources, identify local maxima above your
%            threshold S/N. Template = imUtil.kernel2.gauss(1.5);
%            The S/N image is give by FiltImage./sqrt(FiltImageVar)
%          - The flux estimator image or cube, given by: FiltImage./FiltImageVar.
%            This is the estimator for a template flux at each pixel
%            position. This is roughlu equivalent to a PSF photometry (the
%            main difference is sub-pixel position).
%          - Filtered image or cube (FiltImage).
%          - The variance image of the filtered image (FiltImageVar).
%          - Structure containing additional information.
%            The follwoing fields are available:
%            'Back' - Background image.
%            'Var' - Variance image.
%            'NormVar' -  The normalization factor you need to multiply
%                   the original image variance in order to get the
%                   variane of the filtred image.
%                   A vector, one element per template.
%            'Template' - The unit-sum-normalized template cube  used for
%            the
%                   detection.
% License: GNU general public license version 3
% Tested : Matlab R2015b
%     By : Eran O. Ofek                    Apr 2020
%    URL : http://weizmann.ac.il/home/eofek/matlab/
% See also: imUtil.filter.filter_sn
% Example: Image=randn(1024,1024); Variance=ones(1024,1024);
%          SN=imUtil.filter.filter2_snBank(Image);
%          SN=imUtil.filter.filter2_snBank(Image,[],[]);
%          An example for focusing a source
%          Image = imUtil.kernel2.gauss(2.4,[1024 1024]).*30 + randn(1024,1024);
%          Template = imUtil.kernel2.gauss((1:0.5:6).');
%          SN=imUtil.filter.filter2_snBank(Image,[],[],Template);
%          or equivalently:
%          SN=imUtil.filter.filter2_snBank(Image,[],[],@imUtil.kernel2.gauss,(1:0.5:6).');
%          plot((1:0.5:6).', squeeze(SN(513,513,:)))
% Reliable: 2
%--------------------------------------------------------------------------


%LogMode = true;  % true will not work if image contains negative numbers.

if nargin<4
    Template = @imUtil.kernel2.gauss;
    if nargin<3
        [Mode,Variance] =  imUtil.background.modeVar_LogHist(Image);
        if nargin<2
            if nargin<3
                Background = Mode;
            else
                Background =  imUtil.background.modeVar_LogHist(Image);
            end
        end
    end
end

if isempty(Template)
    Template = @imUtil.kernel2.gauss;
end

 if isa(Template, 'function_handle')
     Template = Template(varargin{:});
 end
 


% treat empty background/variance:
if isempty(Background)
    if isempty(Variance)
        [Background,Variance] =  imUtil.background.modeVar_LogHist(Image);
    else
        [Background] =  imUtil.background.modeVar_LogHist(Image);
    end
else
    if isempty(Variance)
        [~,Variance] =  imUtil.background.modeVar_LogHist(Image);
    end
end

% generate template bank
if ~isnumeric(Template)
    % assume Themplate is a function handle
    Template = Template(varargin{:});
end

Template = cast(Template, 'like',Image);

% normalize template to have unity sum
SumTemplate = sum(Template,[1 2]);
Template    = Template./SumTemplate;

% subtract background from image.
if numel(Background)==1 && Background==0
   % don't subtract background
else
    Image = Image - Background;
end


% filter the back-sub image with the template - initialization
[~,~,Nbank]  = size(Template);
SizeIm       = size(Image);
SizeVar      = size(Variance);
FiltImage    = zeros(SizeIm(1),SizeIm(2),Nbank,'like',Image);
NormVar      = zeros(Nbank,1);
FiltImageVar = zeros(SizeVar(1),SizeVar(2),Nbank,'like',Image);
%FiltImageStd = zeros(SizeVar(1),SizeVar(2),Nbank);
SN           = zeros(SizeIm(1),SizeIm(2),Nbank,'like',Image);
if nargout>1
    Flux         = zeros(SizeIm(1),SizeIm(2),Nbank,'like',Image);
end

% 20% slower
%for Ibank=1:1:Nbank
%    FiltImage(:,:,Ibank) = imUtil.filter.filter2_fast(Image,Template(:,:,Ibank));    
%end

FiltImage = imUtil.filter.filter2_fast(Image,Template);


% Convert the image variance to the filtred image variance
NormVar = sum(Template.^2,[1 2]);

FiltImageVar = Variance.*NormVar;

% S/N image
SN   = FiltImage./sqrt(FiltImageVar);
if nargout>1
    % flux estimator image [see Zackay & Ofek 2017 - Coaddition I paper]
    Flux = FiltImage./NormVar;  % possible bug (CHECK) - FiltImageVar(:,:,Ibank);
end



if nargout>4
    Info.Back     = Background;
    Info.Var      = Variance;
    Info.NormVar  = NormVar;
    Info.Template = Template;
end


