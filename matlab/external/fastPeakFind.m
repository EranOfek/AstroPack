function  [cent, varargout]=fastPeakFind(d, thres, filt ,edg, res, fid)
% Analyze noisy 2D images and find peaks using local maxima (1 pixel
% resolution) or weighted centroids (sub-pixel resolution).
% The code is designed to be as fast as possible, so I kept it pretty basic.
% The code assumes that the peaks are relatively sparse, test whether there
% is too much pile up and set threshold or user defined filter accordingly.
%
% How the code works:
% In theory, each peak is a smooth point spread function (SPF), like a
% Gaussian of some size, etc. In reality, there is always noise, such as
%"salt and pepper" noise, which typically has a 1 pixel variation.
% Because the peak's PSF is assumed to be larger than 1 pixel, the "true"
% local maximum of that PSF can be obtained if we can get rid of these
% single pixel noise variations. There comes medfilt2, which is a 2D median
% filter that gets rid of "salt and pepper" noise. Next we "smooth" the
% image using conv2, so that with high probability there will be only one
% pixel in each peak that will correspond to the "true" PSF local maximum.
% The weighted centroid approach uses the same image processing, with the
% difference that it just calculated the weighted centroid of each
% connected object that was obtained following the image processing.  While
% this gives sub-pixel resolution, it can miss peaks that are very close to
% each other, and runs slightly slower. Read more about how to treat these
% cases in the relevant code commentes.
%
% Inputs:
% d     The 2D data raw image - assumes a Double\Single-precision
%       floating-point, uint8 or unit16 array. Please note that the code
%       casts the raw image to uint16 if needed.  If the image dynamic range
%       is between 0 and 1, I multiplied to fit uint16. This might not be
%       optimal for generic use, so modify according to your needs.
% thres A number between 0 and max(raw_image(:)) to remove  background
% filt  A filter matrix used to smooth the image. The filter size
%       should correspond the characteristic size of the peaks
% edg   A number>1 for skipping the first few and the last few 'edge' pixels
% res   A handle that switches between two peak finding methods:
%       1 - the local maxima method (default).
%       2 - the weighted centroid sub-pixel resolution method.
%       Note that the latter method takes ~20% more time on average.
% fid   In case the user would like to save the peak positions to a file,
%       the code assumes a "fid = fopen([filename], 'w+');" line in the
%       script that uses this function.
%
%Optional Outputs:
% cent        a 1xN vector of coordinates of peaks (x1,y1,x2,y2,...
% [cent cm]   in addition to cent, cm is a binary matrix  of size(d)
%             with 1's for peak positions. (not supported in the
%             the weighted centroid sub-pixel resolution method)
%
%Example:
%
%   p=FastPeakFind(image);
%   imagesc(image); hold on
%   plot(p(1:2:end),p(2:2:end),'r+')
%
%   Adi Natan (natan@stanford.edu)
%   Ver 1.7 , Date: Oct 10th 2013
%
%% defaults
if (nargin < 1)
    d=uint16(conv2(reshape(single( 2^14*(rand(1,1024*1024)>0.99995) ),[1024 1024]) ,fspecial('gaussian', 15,3),'same')+2^8*rand(1024));
    imagesc(d);
end
if ndims(d)>2 %I added this in case one uses imread (JPG\PNG\...).
    d=uint16(rgb2gray(d));
end
if isfloat(d) %For the case the input image is double, casting to uint16 keeps enough dynamic range while speeds up the code.
    if max(d(:))<=1
        d =  uint16( d.*2^16./(max(d(:))));
    else
        d = uint16(d);
    end
end
if (nargin < 2)
    thres = (max([min(max(d,[],1))  min(max(d,[],2))])) ;
end
if (nargin < 3)
    filt = (fspecial('gaussian', 7,1)); %if needed modify the filter according to the expected peaks sizes
end
if (nargin < 4)
    edg =3;
end
if (nargin < 5)
    res = 1;
end
if (nargin < 6)
    savefileflag = false;
else
    savefileflag = true;
end
%% Analyze image
if any(d(:)) %for the case of non zero raw image
    
    d = medfilt2(d,[3,3]);
    
    % apply threshold
    if isa(d,'uint8')
        d=d.*uint8(d>thres);
    else
        d=d.*uint16(d>thres);
    end
    
    if any(d(:))    %for the case of the image is still non zero
        
        % smooth image
        d=conv2(single(d),filt,'same') ;
        
        % Apply again threshold (and change if needed according to SNR)
        d=d.*(d>0.9*thres);
        
        switch res % switch between local maxima and sub-pixel methods
            
            case 1 % peak find - using the local maxima approach - 1 pixel resolution
                
                % d will be noisy on the edges, and also local maxima looks
                % for nearest neighbors so edge must be at least 1. We'll skip 'edge' pixels.
                sd=size(d);
                [x, y]=find(d(edg:sd(1)-edg,edg:sd(2)-edg));
                
                % initialize outputs
                cent=[];%
                cent_map=zeros(sd);
                
                x=x+edg-1;
                y=y+edg-1;
                for j=1:length(y)
                    if (d(x(j),y(j))>d(x(j)-1,y(j)-1 )) &&...
                            (d(x(j),y(j))>d(x(j)-1,y(j))) &&...
                            (d(x(j),y(j))>d(x(j)-1,y(j)+1)) &&...
                            (d(x(j),y(j))>d(x(j),y(j)-1)) && ...
                            (d(x(j),y(j))>d(x(j),y(j)+1)) && ...
                            (d(x(j),y(j))>d(x(j)+1,y(j)-1)) && ...
                            (d(x(j),y(j))>d(x(j)+1,y(j))) && ...
                            (d(x(j),y(j))>d(x(j)+1,y(j)+1));
                        
                        %All these alternatives were slower...
                        %if all(reshape( d(x(j),y(j))>=d(x(j)-1:x(j)+1,y(j)-1:y(j)+1),9,1))
                        %if  d(x(j),y(j)) == max(max(d((x(j)-1):(x(j)+1),(y(j)-1):(y(j)+1))))
                        %if  d(x(j),y(j))  == max(reshape(d(x(j),y(j))  >=  d(x(j)-1:x(j)+1,y(j)-1:y(j)+1),9,1))
                        
                        cent = [cent ;  y(j) ; x(j)];
                        cent_map(x(j),y(j))=cent_map(x(j),y(j))+1; % if a binary matrix output is desired
                        
                    end
                end
                
            case 2 % find weighted centroids of processed image,  sub-pixel resolution.
                   % no edg requirement needed.
                
                % get peaks areas and centroids
                stats = regionprops(logical(d),d,'Area','WeightedCentroid');
                
                % find reliable peaks by considering only peaks with an area
                % below some limit. The weighted centroid method can be not
                % accurate if peaks are very close to one another, i.e., a
                % single peak will be detected, instead of the real number
                % of peaks. This will result in a much larger area for that
                % peak. At the moment, the code ignores that peak. If that
                % happens often consider a different threshold, or return to
                % the more robust "local maxima" method.
                % To set a proper limit, inspect your data with:
                % hist([stats.Area],min([stats.Area]):max([stats.Area]));
                % to see if the limit I used (mean+2 standard deviations)
                % is an appropriate limit for your data.
                
                rel_peaks_vec=[stats.Area]<=mean([stats.Area])+2*std([stats.Area]);
                cent=[stats(rel_peaks_vec).WeightedCentroid]';
                cent_map=[];
                
        end
        
        if savefileflag
            % previous version used dlmwrite, which can be slower than  fprinf
            %             dlmwrite([filename '.txt'],[cent],   '-append', ...
            %                 'roffset', 0,   'delimiter', '\t', 'newline', 'unix');+
            
            fprintf(fid, '%f ', cent(:));
            fprintf(fid, '\n');
            
        end
        
        else % in case image after threshold is all zeros
        cent=[];
        cent_map=zeros(size(d));
        if nargout>1 ;  varargout{1}=cent_map; end
        return
    end
    
else % in case raw image is all zeros (dead event)
    cent=[];
    cent_map=zeros(size(d));
    if nargout>1 ;  varargout{1}=cent_map; end
    return
end
%demo mode - no input to the function
if (nargin < 1); colormap(bone);hold on; plot(cent(1:2:end),cent(2:2:end),'rs');hold off; end
% return binary mask of centroid positions if asked for
if nargout>1 ;  varargout{1}=cent_map; end
