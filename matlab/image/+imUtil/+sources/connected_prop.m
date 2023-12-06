function [LabeledImage]=connected_prop(Image,varargin)
% 
% Package: imUtil.sources
% Description: 
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
% Example: 
% Reliable: 2
%--------------------------------------------------------------------------

error('under construction')

InPar = inputParser;

%addOptional(InPar,'BackFunOut',{'back','var'});  % back, var, std
addOptional(InPar,'ThresholdProp',5);
addOptional(InPar,'VarIm',[]);
addOptional(InPar,'CatXY',[]);
addOptional(InPar,'Conn',8);


parse(InPar,varargin{:});
InPar = InPar.Results;


if isempty(InPar.VarIm)
    % Var image is not provided
    % assume that the Image is normalized by the StD.
    ImageN = Image;
else
    ImageN = Image./sqrt(InPar.VarIm);
end

DetConn = ImageN > InPar.ThresholdProp;

% Generate pixel connectivity lists
% These are list of pixels of connected regions above the
% InPar.ThresholdProp parameter
CC = bwconncomp(DetConn,InPar.Conn);

% Connectivity labeled image
LabeledImage = labelmatrix(CC);

% These sources are not related to the sources found
% by the thresholding - so first we need to associate them with the
% sources found earlier
% i.e., only if CatXY is provided - otherwise return the list as is
if ~isempty(InPar.CatXY)

    Nsrc = size(InPar.CatXY);
    % Get the label index in CC for each peak index in PeaksInd
    PeaksNum     = (1:1:Nsrc).';           % serial number of peak
    IndexInCC    = LabeledImage(PeaksInd); % index of peak in CC
    Flag0        = IndexInCC==0;           % Peaks with no ID label matrix
    IndexInCC    = IndexInCC(~Flag0);      % index of peaks in CC (only with ID)
    IndexInPeaks = PeaksNum(~Flag0);       % index of peaks in PeaksInd (only with ID)

    % Implement the following in the prop calc case  block
    Nsrc_prop   = numel(IndexInCC);
    SizeIm      = imagesize(Sim(Isim));
    [MatX,MatY] = meshgrid((1:1:SizeIm(1)),(1:1:SizeIm(2)));
else
    %
    
end





