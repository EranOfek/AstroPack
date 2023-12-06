function [SN,Flux,FiltImage,FiltImageVar,Info]=filter2_sn(Image,Background,Variance,Template)
% Filter an image with a PSF and calculate the S/N and Flux estimators
% Package: imUtil.filter
% Description: Filter an image with a template/PSF and
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
%            If empty, or not provided then use imUtil.background.mode
%            to calculate the robust background.
%            The mode is calculated with the Log=false option in order to
%            avoid the log of negative numbers.
%          - A variance image (or scalar).
%            If empty, or not provided use imUtil.background.mode
%            to calculate the robust variance.
%          - Template (no assumption on normalization / will be normalized
%            to have unity sum).
% Output : - S/N image (detection statistics). This is the S/N per pixel
%            for detecting the template.
%            In order to find sources, identify local maxima above your
%            threshold S/N. 
%            The S/N image is give by FiltImage./sqrt(FiltImageVar)
%          - The flux estimator image, given by: FiltImage./FiltImageVar.
%            This is the estimator for a template flux at each pixel
%            position. This is roughlu equivalent to a PSF photometry (the
%            main difference is sub-pixel position).
%          - Filtered image (FiltImage).
%          - The variance image of the filtered image (FiltImageVar).
%          - Structure containing additional information.
%            The follwoing fields are available:
%            'Back' - Background image.
%            'Var' - Variance image.
%            'NormVar' -  The normalization factor you need to multiply
%                   the original image variance in order to get the
%                   variane of the filtred image.
%            'Template' - The unit-sum-normalized template used for the
%                   detection.
% License: GNU general public license version 3
% Tested : Matlab R2015b
%     By : Eran O. Ofek                    Apr 2020
%    URL : http://weizmann.ac.il/home/eofek/matlab/
% See also: imUtil.filter.filter_snBank
% Example: Image=randn(1024,1024); Variance=ones(1024,1024);
%          Template = imUtil.kernel2.gauss(1.5);
%          SN=imUtil.filter.filter2_sn(Image,0,Variance,Template);
%          Image = imUtil.kernel2.gauss(1.5,[1024 1024]).*300 + 10.*randn(1024,1024);
%          [SN,Flux]=imUtil.filter.filter2_sn(Image,0,Variance,Template);
%          % SN(513,513) contains max S/N of expectency value of 5.64: (=30./sqrt(4.*pi.*1.5.^2.*1))
%          % Flux(513,513) contains te flux estimator with expectency value of 30.
% Reliable: 2
%--------------------------------------------------------------------------


arguments
    Image
    Background           = [];
    Variance             = [];
    Template             = [];
end


%LogMode = false;  % don't use log option in mode - because of negative numbers...

if isempty(Template)
    Template = imUtil.kernel2.gauss;
end
if isempty(Variance)

    [Mode,Variance] =  imUtil.background.modeVar_LogHist(Image);
else
    Mode = [];
end
if isempty(Background)
    if isempty(Mode)

        Background =  imUtil.background.modeVar_LogHist(Image);

    else
        Background = Mode;
    end
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


% normalize template to have unity sum
SumTemplate = sum(Template,[1 2]);
Template    = Template./SumTemplate;

% subtract background from image.
if numel(Background)==1 && Background==0
   % don't subtract background
else
    Image = Image - Background;
end


% filter the back-sub image with the template
FiltImage = imUtil.filter.filter2_fast(Image,Template);

% Convert the image variance to the filtred image variance
NormVar = sum(Template.^2,[1 2]);
FiltImageVar = Variance.*NormVar;

% S/N image
SN   = FiltImage./sqrt(FiltImageVar);
if nargout>1
    % flux estimator image [see Zackay & Ofek 2017 - Coaddition I paper]
    Flux = FiltImage./NormVar; %FiltImageVar;
    
    if nargout>4
        Info.Back     = Background;
        Info.Var      = Variance;
        Info.NormVar  = NormVar;
        Info.Template = Template;
    end

end




