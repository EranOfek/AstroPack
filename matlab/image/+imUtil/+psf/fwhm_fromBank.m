function [FWHM,Nstars]=fwhm_fromBank(Image,varargin)
% Measure the FWHM of an image by cross corr. with a Gaussian template bank
% Package: +imUtil.psf
% Description: Measure the FWHM of an image by cross corr. with a Gaussian
%              template bank and choose the best FWHM by the mode of most
%              detection above some S/N.
% Input  : - An image in matrix format.
%          * list of ...,key,val,...
%            'CCDSEC' - CCDSEC [Xmin Xmax Ymin Ymax] of region in which to
%                   measure FWHM. If empty use entire image. Default is [].
%            'HalfSize' - Image half size. If 'CCDSEC' is empty, and this
%                   argument is provided, then run this program on centeral
%                   image with this half size. Default is [].
%            'MinSN' - Minimum S/N to use. Default is 50.
%            'Background' - A background image. Default is [].
%            'Variance'   - A variance image. Default is [].
%            'SigmaVec'   - Vector of the Gaussian bank sigmas.
%                           This should not include a sharp object (such a
%                           sharp object is added by the code).
%                           Default is logspace(0,2,5).
%            'MinStars'   - Minimum numbre of stars needed to estimate
%                           FWHM. Default is 5.
%            'PixScale'   - Pixel scale ["/pix]. Default is 1.
%            'Method'     - Method: 
%                           'bisec' - Bi-sector search
%                           'MaxNdet' - Choose the filter with the max
%                                   number of detections.
%                           'MaxNdetInterp' - Same as 'MaxNdet', but with
%                                   interpolation over number of detections.
%                           Default is 'bisec'.
%            'MaxIter'    - Numbre of iterations for the 'bisec' method.
%                           Default is 6.
% Output : - FWHM [arcsec].
%          - Number of stars used for estimating the FWHM.
% Author : Eran Ofek (Mar 2021)
% Example: FWHM=imUtil.psf.fwhm_fromBank(AI.Image);


InPar = inputParser;
addOptional(InPar,'CCDSEC',[]); 
addOptional(InPar,'HalfSize',[]); 
addOptional(InPar,'MinSN',50); 
addOptional(InPar,'Background',[]); 
addOptional(InPar,'Variance',[]); 
addOptional(InPar,'SigmaVec',logspace(0,2,5));
addOptional(InPar,'MinStars',5);
addOptional(InPar,'PixScale',1);  % "/pix
addOptional(InPar,'Method','bisec');  % 10: 2.954; 20: 2.687; 40: 2.718; 80: 2.750; 160: 2.696; 320: 2.675
addOptional(InPar,'MaxIter',6);  
parse(InPar,varargin{:});
InPar = InPar.Results;

Image = single(Image);

if ~isempty(InPar.CCDSEC)
    Image = Image(InPar.CCDSEC(1,3):InPar.CCDSEC(1,4), InPar.CCDSEC(1,1):InPar.CCDSEC(1,2));
    
else
    if ~isempty(InPar.HalfSize)
        SizeIm   = size(Image);
        CenterIm = floor(SizeIm.*0.5);
        InPar.CCDSEC = [CenterIm(2)-InPar.HalfSize, CenterIm(2)+InPar.HalfSize, CenterIm(1)-InPar.HalfSize, CenterIm(1)+InPar.HalfSize];    
        Image = Image(InPar.CCDSEC(1,3):InPar.CCDSEC(1,4), InPar.CCDSEC(1,1):InPar.CCDSEC(1,2));
    end
end


switch lower(InPar.Method)
    case {'maxndet','maxndetinterp'}
        % Choose the template that maximize the SN for the largest number
        % of stars, ecluding the shaprpest star
        
        InPar.SigmaVec = [0.1; InPar.SigmaVec(:)];  % add a sharp object (always first) to bank of templates

        % filter image with filter bandk of gaussians with variable width
        SN = imUtil.filter.filter2_snBank(Image,InPar.Background,InPar.Variance,@imUtil.kernel2.gauss,InPar.SigmaVec);
        % Pos contains: [X,Y,SN,index]
        [~,Pos,MaxIsn]=imUtil.image.local_maxima(SN,1,InPar.MinSN);

        % remove sharp objects
        Pos = Pos(Pos(:,4)~=1,:);

        Nstars = size(Pos,1);

        if Nstars<InPar.MinStars
            FWHM = NaN;
        else
            
            switch lower(InPar.Method)
                case 'maxndet'
                    % Choose the templates (FWHM) that has the maximum number of max-detections
                    FWHM = 2.35.*InPar.PixScale.*InPar.SigmaVec(mode(Pos(:,4),'all'));
                case 'maxndetinterp'
                    % Choose the templates (FWHM) that has the maximum number of max-detections
                    % but interplate over the number of max-detections for each templates
                    Ntemp  = numel(InPar.SigmaVec);
                    Ncount = histcounts(Pos(:,4),(0.5:1:Ntemp+0.5)').';
                    LinSpace = (1:1:Ntemp)';
                    Extram = tools.find.find_local_extremum(LinSpace, Ncount);
                    Extram = Extram(Extram(:,3)<0,:);
                    if isempty(Extram)
                        FWHM = NaN;
                    else
                        [~,Imax]  = max(Extram(:,2));
                        TempInd   = Extram(Imax,1);
                        BestSigma = interp1(LinSpace, InPar.SigmaVec, TempInd, 'cubic');
                        FWHM      = 2.35.*InPar.PixScale.*BestSigma;
                    end
                otherwise
                    error('Unknown Method option');
            end
        end
    case 'bisec'
        % bi-sector method
        
        Nsigma   = numel(InPar.SigmaVec);
        SigmaVec = [0.1; InPar.SigmaVec(:)];  % add a sharp object (always first) to bank of templates
        
        if isempty(InPar.Background) || isempty(InPar.Variance)
            [InPar.Background, InPar.Variance] =  imUtil.background.mode(Image, true);
        end
        
        Cont = true;
        I = 0;
        while Cont
            I = I + 1;
            %for I=1:1:InPar.MaxIter
            % filter image with filter bandk of gaussians with variable width
            
            SN = imUtil.filter.filter2_snBank(Image, InPar.Background, InPar.Variance,@imUtil.kernel2.gauss, SigmaVec(:));
            % Pos contains: [X,Y,SN,index]
            [~,Pos,MaxIsn]=imUtil.image.local_maxima(SN, 1, InPar.MinSN);

            % remove sharp objects
            Pos = Pos(Pos(:,4)~=1,:);

            Nstars = size(Pos,1);

            if Nstars<InPar.MinStars
                Cont    = false;
                BestInd = NaN;
            else
                BestInd = mode(Pos(:,4),'all');
                if I<InPar.MaxIter
                    if BestInd>Nsigma
                        SigmaVec(end+1) = SigmaVec(end) + SigmaVec(end) - SigmaVec(end-1);
                    end
                    SigmaVec = [0.1, logspace(log10(SigmaVec(BestInd-1)), log10(SigmaVec(BestInd+1)), Nsigma)];
                end
            end
            
            if I==InPar.MaxIter
                Cont = false;
            end
        end
        
        if isnan(BestInd)
            FWHM = NaN;
        else
            FWHM = SigmaVec(BestInd).*2.35.*InPar.PixScale;
        end
        
        
    otherwise
        error('Unknown Method option');
end
