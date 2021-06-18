function Result = measureFocus(Obj, Args)
    % Estimate image seeing or focus state
    % Input  : - An AstroImage object (multi elements supported)
    %          * ...,key,val,...
    %            'CCDSEC' - A CCDSEC [xmin, xmax, ymin, ymax]
    %                   or 'center' [Xcenter, Ycenter, Xhalfsize, Yhalfsize].
    %                   If multiple lines then each line corresponding to
    %                   an AstroImage element.
    %                   If empty, then do not crop.
    %            'CropType' - ['ccdsec'] | 'center'
    %            'PixScale' - Pixel scale e.g., [arcsec/pix]. Default is 1.
    %            'Method' - Method by which to estimate focus state:
    %                   'BankVote' - Cross correlate the image with various
    %                           template bank. Start with Gaussian
    %                           templates, in which the first template is
    %                           always a delta function. The delta function
    %                           objects are removed, and the seeing is
    %                           selected based on majority vote. If the
    %                           seeing is equivalent to largest in the
    %                           bank, then divert to a second template bank
    %                           of large annuli.
    %            'SigmaVec' - A vector of Gaussian sigmas for the first
    %                   template bank. Default is logspace(0,1,25).
    %            'AnnulusRadVec' - A vector of annuli outer radii fot the
    %                   secondary bank. Default is logspace(1,3.5,25).
    %            'OuterInnerRatio' - Annulus inner radius in units of outer
    %                   radius. Default is 0.3.
    % Output : - A structure array (element per image)
    %            with the following fields
    %            'FWHMpix' - FWHM [pix]
    %            'FWHM'    - FWHM [arcsec]
    %            'AnnulusRpix' - If FWHM is the largest in the bank, then
    %                   also attempt estimating the best annulus radius
    %                   matched to the image [pix].
    %            'AnnulusR' - Like previous parameter, but in [arcsec].
    % Author : Eran Ofek
    % Example: problems...
    %          AI = AstroImage({imUtil.kernel2.annulus([100 300],[1000 1000]) + randn(1000,1000).*0.0000001});
    %          AI = AstroImage({imUtil.kernel2.gauss(4,[1000 1000]) + randn(1000,1000).*0.0000001});
    %          Result = imProc.sources.measureFocus(AI)       
    
    arguments
        Obj AstroImage
        Args.CCDSEC              = [];
        Args.CropType            = 'ccdsec';  % ccdsec | center
        Args.PixScale            = 1;
        Args.Method              = 'BankVote';
        
        Args.SigmaVec            = logspace(0,1,25);
        Args.AnnulusRadVec       = logspace(1,3.5,25);
        Args.OuterInnerRatio     = 0.3;
    end

    
    Nobj = numel(Obj);
    for Iobj=1:1:Nobj
        % for each image
        if isempty(Args.CCDSEC)
            % do not crop and do not make a new copy
            CroppedObj = Obj;
        else
            % crop and make a new copy
            CroppedObj = crop(Obj, 'Type',Args.CropType);
        end
        
        switch lower(Args.Method)
            case 'bankvote'
                % add a very narrow template
                Args.SigmaVec                = [0.1; Args.SigmaVec(:)];

                SN = imUtil.filter.filter2_snBank(CroppedObj(Iobj).Image,[],[],@imUtil.kernel2.gauss, Args.SigmaVec);
                [BW,Pos,MaxIsn]=imUtil.image.local_maxima(SN,1,5);

                % remove sharp objects
                Pos = Pos(Pos(:,4)~=1,:);
                Result(Iobj).Npeaks = size(Pos,1);
                if isempty(Pos)
                    Result(Iobj).FWHM    = NaN;
                    Result(Iobj).FWHMpix = NaN;
                    MostCommonInd = NaN;
                else
                    % instead one can check if the SN improves...
                    % search for most common template
                    MostCommonInd = mode(Pos(Pos(:,3)>50,4),'all');
                    Result(Iobj).FWHMpix = 2.35.*Args.SigmaVec(MostCommonInd);
                    Result(Iobj).FWHM    = Result(Iobj).FWHMpix.*Args.PixScale;
                end
                
                % check if solution is not near worst case PSF
                if isnan(MostCommonInd) || Args.SigmaVec(MostCommonInd)>=max(Args.SigmaVec)
                    % poor seeing - use a different method
                    AnnulusPar = [Args.AnnulusRadVec(:).*Args.OuterInnerRatio(:), Args.AnnulusRadVec(:)];
                    SN = imUtil.filter.filter2_snBank(CroppedObj(Iobj).Image,[],[],@imUtil.kernel2.annulus, AnnulusPar);
                    [BW,Pos,MaxIsn]=imUtil.image.local_maxima(SN,1,5);

                    if isempty(Pos)
                        Result(Iobj).AnnulusRpix    = NaN;
                        Result(Iobj).AnnulusR       = NaN;
                    else
                        % instead one can check if the SN improves...
                        % search for most common template
                        MostCommonInd = mode(Pos(Pos(:,3)>50,4),'all');
                        Result(Iobj).AnnulusRpix = 2.35.*Args.SigmaVec(MostCommonInd);
                        Result(Iobj).AnnulusR    = Result(Iobj).FWHMpix.*Args.PixScale;
                    end

                else
                    Result(Iobj).AnnulusRpix = NaN;
                    Result(Iobj).AnnulusR    = NaN;
                end
            otherwise
                error('Unknown Method option');
        end
        
    end
    
end
