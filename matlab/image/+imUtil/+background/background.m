function [Back,Var]=background(Image,Args) 
% Estimate the background and its variance for an astronomical image
% Package: @imUtil.background
% Description: A wrapper function for estimating the background and
%              background variance of an imaage in a matrix form.
%              The function partition the image into sub images 
%              (using imUtil.image.partition_subimage) and
%              estimate the background and variance in each sub image.
%              Next, it collect all the sub images into a full image
%              (using: imUtil.image.subimages2image).
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
%                   If 'full' or empty use full image.
%                   Default is [128 128].
%            'Overlap' - The [X,Y] additional overlaping buffer between
%                   sub images to add to each sub image.
%                   Default is 16.
%            'ExtendFull' - A logical indicating if to extend the
%                   background map into a full-size image. Default is true.
%         Not relevent anymore:
%            'StitchMethod' - Stitching method.
%                   See imUtil.image.subimages2image for options.
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
% Example: [Back,Var]=imUtil.background.background(rand(1024,1024));

arguments
    Image
    Args.BackFun                 = @median;
    Args.BackFunPar cell         = {[1 2]};
    Args.VarFun                  = @imUtil.background.rvar;
    Args.VarFunPar cell          = {};
    Args.SubSizeXY               = [128 128];
    Args.Overlap                 = 16;
    Args.ExtendFull(1,1) logical = true;
    Args.ExtendMethod            = 'interp_sparse2full'; %'imresize';  % 'imresize' | 'interp_sparse2full'
    Args.FieldName               = 'Im';
end


if ~isa(Args.BackFun,'function_handle')
    error('BackFun must be a function handle');
end
if ~isempty(Args.VarFun)
    if ~isa(Args.VarFun,'function_handle') && ~isnan(Args.VarFun)
        error('VarFun must be a function handle, NaN, or empty');
    end
end

% partition the image into sub images
if isempty(Args.SubSizeXY)
    Args.SubSizeXY = 'full';
end
if ischar(Args.SubSizeXY)
    switch lower(Args.SubSizeXY)
        case 'full'
            SubImage(1).(Args.FieldName) = Image;
            Size     = size(SubImage);
            CCDSEC   = [1 Size(2) 1 Size(1)];
            Center   = [mean(CCDSEC(:,1:2),2), mean(CCDSEC(:,3:4),2)];
            Nxy      = [1 1];
        otherwise
            error('Unknown SubSizeXY option');
    end
else
    
    %[SubImage,CCDSEC,Center]=imUtil.partition.image_partitioning(Image,Args.SubSizeXY,'Overlap',Args.Overlap,'FieldNameIm',Args.FieldName);
    [SubImage, CCDSEC, Center, ~, ~, Nxy] = imUtil.image.partition_subimage(Image, [], 'SubSizeXY',Args.SubSizeXY, 'OverlapXY',Args.Overlap,...
                                                                               'Output','struct', 'FieldName',Args.FieldName);
    
%     [SubImage,CCDSEC,Center]=imUtil.image.partition_subimage(Image,[],...
%                                 'SubSizeXY',Args.SubSizeXY,'OverlapXY',Args.OverlapXY,...
%                                 'Output','struct','FieldName',FieldName);
end



% calc background for each sub image
Nsub = numel(SubImage);
for Isub=1:1:Nsub
    %
    SubI = SubImage(Isub).(Args.FieldName); %(1:5:end,1:5:end);
   
    if isempty(Args.VarFun)
        % assume the BackFun returns both background and variance
        [SubImage(Isub).Back, SubImage(Isub).Var] = Args.BackFun(SubI,Args.BackFunPar{:});
    else
         % different functions for background and variance
         SubImage(Isub).Back = Args.BackFun(SubI,Args.BackFunPar{:});
         if isa(Args.VarFun,'function_handle')
             SubImage(Isub).Var = Args.VarFun(SubI,Args.VarFunPar{:});
         elseif isnan(Args.VarFun)
             SubImage(Isub).Var = [];
         else
             % do nothing
         end
    end
    
end




if Args.ExtendFull
    %Args.ExtendMethod = 'interp_sparse2full'
    switch lower(Args.ExtendMethod)
        case 'imresize'
            BackIm = reshape([SubImage.Back],fliplr(Nxy));
            if isempty(BackIm)
                Back = [];
            else
                Back = imresize(BackIm,size(Image), 'lanczos3');
            end
            VarIm = reshape([SubImage.Var],fliplr(Nxy));
            if isempty(VarIm)
                Var = [];
            else
                Var = imresize(VarIm,size(Image), 'lanczos3');
            end

            
        case 'interp_sparse2full'
    
            % stitch the background image from the sub images
            SizeSub = size(SubImage);

            BackIm = reshape([SubImage.Back],SizeSub);

            if numel(BackIm)==1
                Back = BackIm; % + zeros(size(Image));
            else
                %X = reshape([SubImage.CenterX],SizeSub);
                X = reshape(Center(:,1),SizeSub);
                %Y = reshape([SubImage.CenterY],SizeSub);
                Y = reshape(Center(:,2),SizeSub);
                [Back] = imUtil.partition.interp_sparse2full(X,Y,BackIm,fliplr(size(Image)));
            end

            if isempty(SubImage(1).Var) || nargout<2
                Var = [];
            else
                VarIm  = reshape([SubImage.Var],SizeSub);
                if numel(VarIm)==1
                    Var = VarIm; % + zeros(size(Image));
                else
                    %X = reshape([SubImage.CenterX],SizeSub);
                    X = reshape(Center(:,1),SizeSub);
                    %Y = reshape([SubImage.CenterY],SizeSub);
                    Y = reshape(Center(:,2),SizeSub);
                    [Var]  = imUtil.partition.interp_sparse2full(X,Y,VarIm,fliplr(size(Image)));
                end
            end
        
        otherwise
            error('Unknown ExtendMethod option');
    end
else
    % do not interpolate/extrapolate sparse back/var to full image
    Back = reshape([SubImage.Back],fliplr(Nxy));
    Var  = reshape([SubImage.Var],fliplr(Nxy));
end

% switch lower(Args.StitchMethod)
%     case {'scalar'}
%         Back = SubImage.Back;
%     case {'ignoreoverlap','meanoverlap'}
%         Back = imUtil.image.subimages2image(SubImage,CCDSEC,'FieldName','Back','StitchMethod',Args.StitchMethod);
%     case {'si','impaint'}
%         GridVal = [Center, [SubImage.Back].'];
%         Back = imUtil.background.fill_sparse(GridVal,max(CCDSEC(:,[2 4])));
%     otherwise
%         error('Unknwon StitchMethod');
% end
% 
% % smmoth background image
% % make sure that Kernel size is smaller than image
% if ~isempty(Args.SmoothSigma)
%     if isnan(Args.SmoothSigma)
%         SigmaX = Args.SubSizeXY(1)./2.35;
%         SigmaY = Args.SubSizeXY(2)./2.35;
%         
%     else
%         SigmaX = Args.SmoothSigma(1);
%         SigmaY = Args.SmoothSigma(2);
%     end
%     KerSizeX = min(Args.SubSizeXY(1).*3, max(CCDSEC(:,2)));
%     KerSizeY = min(Args.SubSizeXY(2).*3, max(CCDSEC(:,4)));
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
%         switch lower(Args.StitchMethod)
%             case {'scalar'}
%                 Var = SubImage.Var;
%             case {'ignoreoverlap','meanoverlap'}
%                 Var  = imUtil.image.subimages2image(SubImage,CCDSEC,'FieldName','Var','StitchMethod',Args.StitchMethod);
%             case {'si','impaint'}
%                 GridVal = [Center, [SubImage.Var].'];
%                 Var = imUtil.background.fill_sparse(GridVal,max(CCDSEC(:,[2 4])));
%             otherwise
%                 error('Unknwon StitchMethod');
%         end
%         % smotth variance imaeSubImageSubImage
%         if ~isempty(Args.SmoothSigma)
%             Var     = imUtil.filter.conv2_fast(Var,SmKernel);
%         end
% 
%     end
% else
%     Var = [];
% end
