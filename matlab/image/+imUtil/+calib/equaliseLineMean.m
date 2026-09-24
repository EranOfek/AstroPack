function [Defects,EqImg]=equaliseLineMean(Img,Args)
% equalise an image by compensating, line by line, the average of the line
%  background. This is useful notably for PTF images, which have been
%  incorrectly debiased, causing a striped background disomogeneity.
% Algorithm: for each line, background pixels are selected as those with
%  values below some defined quantile level. The average of their
%  intensities is computed, and subtracted from the line values.
%  The global mean of all background pixels is added to the whole image,
%  to preserve approximately the image mean.
% Inputs:
%     Img    - an image matrix (any numeric type)
%     key,val arguments:
%        Level     - quantile level for cutoff [default 0.4]
%        StripeDim - 1 to equalise column background, 2 for row [1]
%        Plot      - show debug plots [false]
%      -- classification parameters --
%        JumpAmpltiude - empirical threshold to declare a jump [0.05]
%        Gradient      - slope threshold to declare an intensity gradient [0.01]
%        LightBackground - threshold intensity to declare the image
%                          "bright" (ADU) [500]
% Outputs:
%     EqImg   - the corrected image (double)
%     Defects - a strutcure reporting whether some common problems
%               have been found (e.g. intensity jumps, negative values,
%               vignetting), based on empiric thresholds
% Example:
%
%   Im=uint16(rand(120,160)*2^15);
%   Im(40:75,:)=Im(40:75,:)+uint16(3301);
%   [Defects,EqImg]=imUtil.calib.equaliseLineMean(Im,'Plot',true);
%
% Author: Enrico Segre, June 2023
    arguments
        Img = [];
        Args.Level =0.4;
        Args.StripeDim= 2;
        Args.Plot = false;
        Args.JumpAmplitude = 0.05;
        Args.Gradient = 0.01;
        Args.LightBackground = 500;
    end
    
    nr=size(Img,1);
    nc=size(Img,2);

    imMin=min(Img(:));
    % shift so that image minimum (which could be negative) is 0
    %  Not a good idea, large isolated negative values bias the quantile
    % a=img-imMin;

    % just disregard negative image values from the computation of the quantile
    % Do computations with double values, cast to the original type at the
    %  end
    a=double(Img);
    b=a;
    b(Img(:)<0)=NaN;

    % compute quantiles per line. Using a global quantile doesn't
    %  fully compensate images with large differences between lines
    if Args.StripeDim==2
        ql=quantile(b',Args.Level); % vector of size nr
        q2=b>repmat(ql',1,nc);
    else
        ql=quantile(b,Args.Level); % vector of size nr
        q2=b>repmat(ql,nr,1);
    end
    b(q2)=NaN;

    LineMean=mean(b,Args.StripeDim,"omitnan");
    LineStd=std(b,1,Args.StripeDim,"omitnan");
    bkgMean=mean(LineMean);
    
    if Args.Plot
        if Args.StripeDim==2
            subplot(1,3,1)
            plot(LineMean,1:nr)
            title('mean'); axis tight
            subplot(1,3,2)
            plot(LineStd,1:nr)
            title('std'); axis tight
            subplot(1,3,3)
            plot(LineStd,LineMean,'.')
            ylabel('mean'); xlabel('std'); axis tight
        else
            subplot(3,1,1)
            plot(LineMean)
            title('mean'); axis tight
            subplot(3,1,2)
            plot(LineStd)
            title('std'); axis tight
            subplot(3,1,3)
            plot(LineMean,LineStd,'.')
            xlabel('mean'); ylabel('std'); axis tight
        end
    end

    % image output (cast like the original Img)
    if nargout>1
        if Args.StripeDim==2
            EqImg=cast(a-repmat(LineMean,1,nc)+bkgMean,'like',Img);
        else
            EqImg=cast(a-repmat(LineMean,nr,1)+bkgMean,'like',Img);
        end
    end
    
    % classification:
    if nargout>0
        % jumps (excluding first and last rows which are always problemtic)
        Defects.jumps= any(diff(LineMean(3:end-2))>Args.JumpAmplitude*bkgMean);

        % brightness gradient
        % 0.01 means a trend of 40 e- over a span of 4096 rows
        if Args.StripeDim==2
            reg=[ones(nr,1),(1:nr)']\double(LineMean);
        else
            reg=[ones(nc,1),(1:nc)']\double(LineMean');
        end
        Defects.gradient= abs(reg(2))>Args.Gradient;

        % high background
        Defects.brightBackground=(bkgMean>Args.LightBackground);

        % vignetting
        blackcorner=0.5*bkgMean;
        Defects.vignetting= mean(b(1:10,1:10),'all')<blackcorner ||...
                            mean(b(end-10:end,1:10),'all')<blackcorner || ...
                            mean(b(1:10,end-10:end),'all')<blackcorner || ...
                            mean(b(end-10:end,end-10:end),'all')<blackcorner;

        % negative original values
        Defects.negative= (imMin<0);

        % correlated noise
        % TBD (peaks in power spectrum?)
    end
