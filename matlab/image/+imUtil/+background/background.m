function [Back,Var]=background(Image,varargin) 
% Estimate the background and its variance for an astronomical image
% Package: @imUtil.background
% Description: A wrapper function for estimating the background and
%              background variance of an imaage in a matrix form.
%              The function partition the image into sub images 
%              (using imUtil.image.partition_subimage) and
%              estimate the background and variance in each sub image.
%              Next, it collect all the sub images into a full image
%              (using: imUtil.image.subimage2image).
%              The background and variance are calculated in each sub image
%              by calling a user supplied functions.
% Input  : - a 2D matrix.
%          * Pairs of ...,key,val,... The following keywords are available:
%            'BackFun' - A function handle for the background (and
%                   optionally variance) estimation.
%                   The function is of the form:
%                   [Back,[Var]]=Fun(Matrix,additional parameters,...),
%                   where the output Variance is optional.
%                   The additional parameters are provided by the
%                   'BackFunPar' keyword (see next keyword).
%                   Default is @median [other example:
%                   @imUtil.background.mode]
%            'BackFunPar' - A cell array of additional parameters to pass
%                   to the BackFun function.
%                   Default is {[1 2]} [other example: {true,true,0.1}]
%            'VarFun' - A function handle for the background estimation.
%                   The function is of the form:
%                   [Var]=Fun(Matrix,additional parameters,...).
%                   The additional parameters are provided by the
%                   'VarFunPar' keyword (see next keyword).
%                   If NaN, then will not calculate the variance.
%                   If empty, then will assume the variance is returned as
%                   the second output argument of 'BackFun'.
%                   Default is empty (i.e., @imUtil.background.rvar returns
%                   the robust variance as the second output argument).
%            'VarFunPar' - A cell array of additional parameters to pass
%                   to the VarFun function.
%                   Default is {}.
%            'SubSizeXY' - The [X,Y] size of the partitioned sub images.
%                   Default is [128 128].
%            'Overlap' - The [X,Y] additional overlaping buffer between
%                   sub images to add to each sub image.
%                   Default is 16.
%            'StitchMethod' - Stitching method.
%                   See imUtil.image.subimage2image for options.
%                   Another option is 'scalar'. If there is only one sub
%                   image, then the ouput will be a scalar.
%                   Default is 'IgnoreOverlap'.
%            'SmoothSigma' - Smooth the background image using a gaussian
%                   convolution kernel. 
%                   This is needed s the background calculation is done in
%                   sub images, and each sub image has its own background
%                   level. This is required in order to avoid sharp
%                   background transitions.
%                   If empty, then do not apply smoothing.
%                   If NaN, then use default smoothing.
%                   Otherwise should be [SigmaX, SigmaY], which are the
%                   sigmas in X and Y directions of the Gaussian kernel.
%                   Default is [SubSizeXY]./2.35.
% Output : - Background image.
%          - Variance image.
%      By: Eran O. Ofek                       Apr 2020             
% Example: [Back,Var]=imUtil.background.background(rand(1024,1024))



InPar = inputParser;

%addOptional(InPar,'BackFunOut',{'back','var'});  % back, var, std
addOptional(InPar,'BackFun',@median); % @imUtil.background.mode); % @median);
addOptional(InPar,'BackFunPar',{[1 2]}); %{true,true,0.1});      % {[1 2],'omitnan'});
addOptional(InPar,'VarFun',@imUtil.background.rvar); %[]);    % @imUtil.background.rvar);    % if empty, then will try to read var from second output of BackFun...
addOptional(InPar,'VarFunPar',{}); % {[1 2]});
addOptional(InPar,'SubSizeXY',[128 128]);  % or 'full'
addOptional(InPar,'Overlap',[16]);  % recomended ~1.7 FWHM
% imUtil.image.subimage2image parameters:
%addOptional(InPar,'StitchMethod','IgnoreOverlap');  % 'MeanOverlap' | 'IgnoreOverlap' | 'si'  | 'impaint'
%addOptional(InPar,'SmoothSigma',[]); %NaN);  % NaN - use SubSize./2.35  % otherwise sigma of Gaussian, empty - no smooth 

addOptional(InPar,'FieldName','Im');

parse(InPar,varargin{:});
InPar = InPar.Results;

if ~isa(InPar.BackFun,'function_handle')
    error('BackFun must be a function handle');
end
if ~isempty(InPar.VarFun)
    if ~isa(InPar.VarFun,'function_handle') && ~isnan(InPar.VarFun)
        error('VarFun must be a function handle, NaN, or empty');
    end
end

% partition the image into sub images
if ischar(InPar.SubSizeXY)
    switch lower(InPar.SubSizeXY)
        case 'full'
            SubImage(1).(InPar.FieldName) = Image;
            Size     = size(SubImage);
            CCDSEC   = [1 Size(2) 1 Size(1)];
            Center   = [mean(CCDSEC(:,1:2),2), mean(CCDSEC(:,3:4),2)];

        otherwise
            error('Unknown SubSizeXY option');
    end
else
    
    [SubImage,CCDSEC,Center]=imUtil.partition.image_partitioning(Image,InPar.SubSizeXY,'Overlap',InPar.Overlap,'FieldNameIm',InPar.FieldName);
    
    
%     [SubImage,CCDSEC,Center]=imUtil.image.partition_subimage(Image,[],...
%                                 'SubSizeXY',InPar.SubSizeXY,'OverlapXY',InPar.OverlapXY,...
%                                 'Output','struct','FieldName',FieldName);
end


% calc background for each sub image
Nsub = numel(SubImage);
for Isub=1:1:Nsub
    %
    SubI = SubImage(Isub).(InPar.FieldName); %(1:5:end,1:5:end);
   
    if isempty(InPar.VarFun)
        % assume the BackFun returns both background and variance
        [SubImage(Isub).Back, SubImage(Isub).Var] = InPar.BackFun(SubI,InPar.BackFunPar{:});
    else
         % different functions for background and variance
         SubImage(Isub).Back = InPar.BackFun(SubI,InPar.BackFunPar{:});
         if isa(InPar.VarFun,'function_handle')
             SubImage(Isub).Var = InPar.VarFun(SubI,InPar.VarFunPar{:});
         elseif isnan(InPar.VarFun)
             SubImage(Isub).Var = [];
         else
             % do nothing
         end
    end
    
end


% stitch the background image from the sub images
SizeSub = size(SubImage);

BackIm = reshape([SubImage.Back],SizeSub);

if numel(BackIm)==1
    Back = BackIm; % + zeros(size(Image));
else
    X = reshape([SubImage.CenterX],SizeSub);
    Y = reshape([SubImage.CenterY],SizeSub);
    [Back] = ImUtil.Back.interp_sparse2full(X,Y,BackIm,fliplr(size(Image)));
end

if isempty(SubImage(1).Var) || nargout<2
    Var = [];
else
    VarIm  = reshape([SubImage.Var],SizeSub);
    if numel(VarIm)==1
        Var = VarIm; % + zeros(size(Image));
    else
        X = reshape([SubImage.CenterX],SizeSub);
        Y = reshape([SubImage.CenterY],SizeSub);
        [Var]  = ImUtil.Back.interp_sparse2full(X,Y,VarIm,fliplr(size(Image)));
    end
end


% switch lower(InPar.StitchMethod)
%     case {'scalar'}
%         Back = SubImage.Back;
%     case {'ignoreoverlap','meanoverlap'}
%         Back = imUtil.image.subimage2image(SubImage,CCDSEC,'FieldName','Back','StitchMethod',InPar.StitchMethod);
%     case {'si','impaint'}
%         GridVal = [Center, [SubImage.Back].'];
%         Back = imUtil.background.fill_sparse(GridVal,max(CCDSEC(:,[2 4])));
%     otherwise
%         error('Unknwon StitchMethod');
% end
% 
% % smmoth background image
% % make sure that Kernel size is smaller than image
% if ~isempty(InPar.SmoothSigma)
%     if isnan(InPar.SmoothSigma)
%         SigmaX = InPar.SubSizeXY(1)./2.35;
%         SigmaY = InPar.SubSizeXY(2)./2.35;
%         
%     else
%         SigmaX = InPar.SmoothSigma(1);
%         SigmaY = InPar.SmoothSigma(2);
%     end
%     KerSizeX = min(InPar.SubSizeXY(1).*3, max(CCDSEC(:,2)));
%     KerSizeY = min(InPar.SubSizeXY(2).*3, max(CCDSEC(:,4)));
%     
%     SmKernel = Kernel2.gauss(SigmaX,SigmaY,0,KerSizeX,KerSizeY);
%     Back     = imUtil.filter.conv2_fast(Back,SmKernel);
% end
% 
% 
% % stitch the variance image
% if nargout>1
%     if isempty(SubImage(1).Var)
%         Var = [];
%     else
%         switch lower(InPar.StitchMethod)
%             case {'scalar'}
%                 Var = SubImage.Var;
%             case {'ignoreoverlap','meanoverlap'}
%                 Var  = imUtil.image.subimage2image(SubImage,CCDSEC,'FieldName','Var','StitchMethod',InPar.StitchMethod);
%             case {'si','impaint'}
%                 GridVal = [Center, [SubImage.Var].'];
%                 Var = imUtil.background.fill_sparse(GridVal,max(CCDSEC(:,[2 4])));
%             otherwise
%                 error('Unknwon StitchMethod');
%         end
%         % smotth variance imaeSubImageSubImage
%         if ~isempty(InPar.SmoothSigma)
%             Var     = imUtil.filter.conv2_fast(Var,SmKernel);
%         end
% 
%     end
% else
%     Var = [];
% end
