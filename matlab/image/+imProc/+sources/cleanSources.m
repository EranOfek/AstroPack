function [Result, Flag] = cleanSources(Obj, Args)
    % Clean sources found by findMeasureSources (bad S/N and CR).
    %   Cleaning include - flaging or removal of CRs based on hypothesis
    %   testing between a delta function and some PSF, and finding sources
    %   which S/N based on the annulus std is low (i.e. identify cases in
    %   which the local background/variance estimation is not reliable).
    % Input  : - An AstroCatalog or AstroImage (w/ Astrocatalog) object.
    %            The AstroCatalog must contains at least two S/N columns
    %            measured using two PSF matched filters,
    %            an 'STD_ANNULUS' column of the std measured in an annulus
    %            around each source, and at least one flux column.
    %          * ...,key,val,...
    %            'ColNamsSN' - A cell array of at least two column names containg the
    %                   S/N measured for PSFs with different width.
    %                   The first column name must corresponds to a sharp
    %                   PSF (i.e., delta function), and the second column
    %                   is for some wider PSF.
    %                   The CR flag is calculated by SN(1)>SN(2).
    %                   Default is {'SN_1','SN_2'}.
    %            'ColNameFlux' - A cell array of column names containing
    %                   some flux measurments. These, along with the annulus
    %                   std, are used to calculate the annulus-std-based
    %                   S/N. Default is {'FLUX_CONV_2','FLUX_CONV_3'}.
    %            'SigmaPSF' - The width of the PSF (i.e., unit of sigma).
    %                   This is a vector which must have the same number of
    %                   elements as ColNameFlux. This is used to calculate
    %                   the PSF effective area (4\pi\sigma^2).
    %                   Default is [].
    %            'ColNameStdAnn' - Annulus std column name.
    %                   Default is 'STD_ANNULUS'.
    %            'ThresholdSN' - S/N threshold. annulus-std S/N smaller than
    %                   this threshold are declared as bad.
    %                   Default is 5.
    %            'MaskCR' - A logical indicating if to update the mask
    %                   image with cosmic rays found using 'CR_DeltaHT'.
    %                   This will be activated only if the input is an
    %                   AstroImage. Default is true.
    %            'BitNameCR' - A bit name in the Mask image for flagging
    %                   the CR (cosmic rays). Default is 'CR_DeltaHT'.
    %            'RemoveBadSources' - A logical indicating if to remove bad
    %                   sources from the output catalog. Default is true.
    %            'CreateNewObj' - A logical indicating if to create a new
    %                   copy of (only) the AstroCatalog object.
    %                   Default is false.
    % Output : - An AstroCatalog/AstroImage with the updated catalog.
    %          - A structure array with the flaged sources. Abailable
    %            fields are:
    %            .CR - possible CR.
    %            .BadSN - std-annulud-based S/N smaller than threshold.
    % Author : Eran Ofek (Oct 2021)
    % Example: [Result, Flag] = imProc.sources.cleanSources(AI,'SigmaPSF',[1.2 1.5])
   
    
    arguments
        Obj
        Args.ColNamsSN                 = {'SN_1','SN_2'}; % two column names: sharp and non-sharp
        %Args.ColNameFlux               = {'FLUX_CONV_2','FLUX_CONV_3'};   % PSFs for checking annulus S/N
        Args.SigmaPSF                  = [];              % corresponding PSFs sigmas 
        Args.ColNameStdAnn             = 'STD_ANNULUS';
        
        Args.ColNameBackAnn            = 'BACK_ANNULUS';
        Args.ColNameBackIm             = 'BACK_IM';
        Args.ColNameVarIm              = 'VAR_IM';
        Args.ColNamePos                = {'X1','Y1','XPEAK','YPEAK'};
        
        Args.ThresholdSN               = 5;
        
        Args.MaskCR logical            = true;
        Args.BitNameCR                 = 'CR_DeltaHT';
        
        Args.RemoveBadSources logical  = true;
        Args.CreateNewObj logical      = false;
    end
    
    %AreaPSF    = 4.*pi.*Args.SigmaPSF.^2;
    %NsigmaPSF  = numel(Args.SigmaPSF);
    %if NsigmaPSF~=numel(Args.ColNameFlux)
    %    error('SigmaPSF must have the same length as ColNameFlux');
    %end
    
    Nobj = numel(Obj);
    
    Result = Obj;
    Flag   = struct('CR',cell(Nobj,1), 'BadSN',cell(Nobj,1)); % allocate the Flag structure
    for Iobj=1:1:Nobj
        if isa(Obj, 'AstroImage')
            if Args.CreateNewObj
                Cat = Obj(Iobj).CatData.copy;
            else
                Cat = Obj(Iobj).CatData;
            end
        elseif isa(Obj, 'AstroCatalog')
            if Args.CreateNewObj
                Cat = Obj(Iobj).copy;
            else
                Cat = Obj(Iobj);
            end
        else
            error('First input argument must be an AstroImage or AstroCatalog object');
        end
        
        SN      = getCol(Cat, Args.ColNamsSN);
        StdAnn  = getCol(Cat, Args.ColNameStdAnn);
        %Flux    = getCol(Cat, Args.ColNameFlux);
        
        BackAnn  = getCol(Cat, Args.ColNameBackAnn);
        BackIm   = getCol(Cat, Args.ColNameBackIm);
        VarIm    = getCol(Cat, Args.ColNameVarIm);
        
        BackAnn  = max(BackAnn, BackIm);
        StdAnn   = max(StdAnn, sqrt(VarIm));
        
        XY = getCol(Cat, Args.ColNamePos);
        Dist = sqrt((XY(:,1) - XY(:,3)).^2 + (XY(:,2) - XY(:,4)).^2);
        
        Flag(Iobj).BadLocation = Dist>1;
        
        F = (SN.*sqrt(VarIm) + BackIm - BackAnn) < (Args.ThresholdSN.*StdAnn);
        %(1 + 20.*log( max(BackAnn./BackIm,1) )));
        Flag(Iobj).BadSN = all(F,2);
        
        % Flag CR
        Flag(Iobj).CR = SN(:,1) > SN(:,2);
        
        % Flag bad S/N
        %SN_FluxAnn = Flux./(StdAnn.*sqrt(AreaPSF(:).'));
        %Flag(Iobj).BadSN = all(SN_FluxAnn < Args.ThresholdSN, 2);
       
        % flag bad sources in Mask image
        Args.MaskCR = false;
        if Args.MaskCR && isa(Obj, 'AstroImage')
            warning('VERIFY that maskSet works properly');
            SizeImage = size(Obj(Iobj).Image);
            FlagImage = false(SizeImage);
            FXY  = round(XY(:,1:2));
            Find = imUtil.image.sub2ind_fast(SizeImage, FXY(:,2), FXY(:,1));
            FlagImage(Find) = true;
            Result(Iobj).MaskData = maskSet(Obj(Iobj).MaskData, FlagImage, Args.BitNameCR, 1);
        end
            
        % remove bad sources
        if Args.RemoveBadSources
            Cat = selectRows(Cat, ~Flag(Iobj).CR & ~Flag(Iobj).BadLocation & ~Flag(Iobj).BadSN);
        
            % save in Result
            if isa(Obj, 'AstroImage')
                Result(Iobj).CatData = Cat;
            else
                Result(Iobj)         = Cat;
            end
        end
    end    
    
end
