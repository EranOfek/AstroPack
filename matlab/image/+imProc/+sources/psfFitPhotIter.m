function [Result,AISub] = psfFitPhotIter(AI,Args)
    % Perform a iterative psf photometry.
    %  The function will iteratively detect sources which has SN higher than user-provided
    %  threshold, measure their psf photometry, and substact them.
    %  The function assumes that the PSF is included in the input AstroImage.
    % Input  :  - An AstroImage object which contains a PSF.
    %           * ...,key,val,...
    %             'Threshold' - Array of numeric threshold for the SN in each
    %                   iteration. In case of single number, the function will
    %                   duplicate it 'Niter' (below) times.
    %                   Default is [100,50,5]. 
    %             'CreateNewObj' - A logical indicating if to copy the
    %                   content of the AI object.
    %                   Default is true.
    %             'Niter' - Number of iteration to perform. This value will
    %                   be used in case 'Threshold' contains only single number.
    %                   Default is 3.
    %             'ReCalcBack' - A logical indicating if to calculate the
    %                   background in each iteration.
    %                   Default is true.
    %             'ColNameIter' - A string indicating the name of column that
    %                   indicates the iteration in which the a specific sources
    %                   were detected in the output catalog. If empty (''),
    %                   the iteration nubmer column will not be added.
    %                   Default is 'PSF_ITER'.
    %             'OutType' - Output type. Options are:
    %                   'astroimage' | 'astrocatalog'
    %                   Default is 'astroimage'.
    %             'findSourcesArgs' - A cell array of additional args to
    %                   pass to imProc.sources.findSources.
    %                   Default is {}.
    %             'psfFitPhotArgs' - A cell array of additional args to
    %                   pass to imProc.sources.psfFitPhot.
    %                   Default is {}.
    %             'injectSourcesArgs' - A cell array of additional args to
    %                   pass to imUtil.art.injectSources.
    %                   Default is {}.
    %             'backgroundArgs' - A cell array of additional args to
    %                   pass to imProc.background.background.
    %                   Default is {}.
    %             'ColSN' - Column name containing the S/N to use for the
    %                   thresholding. Default is 'SN_2'.
    %
    % Output  :  -An AstroImage or AstroCatalog that conatin the new catalog.
    %            -An AstroImage contain the substracted images and the new catalog.
    % Author  :  Noam Segev (Jun 2023)
    % Example :  
    %            PSF = imUtil.kernel2.gauss; Cat = [rand(10,2)*1000,rand(10,1)*1e5];
    %            S = imUtil.art.injectSources(poissrnd(500,1000,1000),Cat,PSF);
    %            AI = AstroImage({S});AI.PSFData = AstroPSF(PSF);
    %            [Result,AISub] = imProc.sources.psfFitPhotIter(AI);
    


    arguments

        AI
        Args.Threshold              = [100,50,5];
        Args.CreateNewObj logical   = true;
        Args.Niter                  = 3;
        Args.ReCalcBack logical     = true;
        Args.ColNameIter            = 'PSF_ITER';
        Args.OutType                = 'AstroImage'; %|AstroImage or AstroCatalog
        Args.findSourcesArgs cell   = {};
        Args.psfFitPhotArgs cell    = {};
        Args.injectSourcesArgs cell = {};
        Args.backgroundArgs cell    = {};
        
        Args.ColSN                  = 'SN_2';
    end

    if numel(Args.Threshold)==1
        Threshold = Args.Threshold.*ones(Args.Niter,1);
    else
        Threshold =  Args.Threshold ;
    end
    Niter         = numel(Threshold);
    
    
    if Args.CreateNewObj
        AISub = AI.copy();
    else
        AISub = AI;
    end

    
    [Imsz1,Imsz2] = AISub.sizeImage;

    for Iiter = 1:1:Niter
        if Args.ReCalcBack % recalculate background
            AISub = imProc.background.background(AISub,Args.backgroundArgs{:});
        end
        % Source detection using the psf from AISub 
        AISub = imProc.sources.findSources(AISub,'Psf',AISub.PSF,'Threshold',Threshold(Iiter),Args.findSourcesArgs{:});
        
        % why is this needed?
        AISub.CatData.Catalog = double(AISub.CatData.Catalog);
        
        if AISub.CatData.isemptyCatalog
            continue;
        end
        % psf photometry. The use of SN_1 isn't robust.
        [AISub] = imProc.sources.psfFitPhot(AISub, 'PSF',PSF, 'ColSN',Args.ColSN, 'HalfSize',floor(numel(PSF(:,1))/2), Args.psfFitPhotArgs{:});
        if ~isempty(Args.ColNameIter) % Add iter number to the catalog.
            AISub.CatData.insertCol((Iiter).*ones(numel(AISub.CatData.Catalog(:,1)),2) ,Inf,{Args.ColNameIter},{''});
        end
        SrcCat = AISub.CatData.getCol({'X','Y','FLUX_PSF'});
        flagnan = ~any(isnan(SrcCat),2);
        S = imUtil.art.injectSources([Imsz1,Imsz2] ,SrcCat(flagnan,:),AISub.PSF,Args.injectSourcesArgs{:});
        AISub = AISub- S;
        Cat(Iiter)= AISub.astroImage2AstroCatalog;
        AISub.CatData=AstroCatalog;

    end

    Cat = Cat.merge;
    switch lower(Args.OutType)
        case 'astroimage'
            Result = AI;
            Result.CatData = Cat;

        case 'astrocatalog'
            Result = Cat;
        otherwise
            error('Unknown OutType option');
    end
end




