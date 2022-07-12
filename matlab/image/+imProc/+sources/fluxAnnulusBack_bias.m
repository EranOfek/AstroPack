function [Result, Obj] = fluxAnnulusBack_bias(Obj, Args)
    % Estimate and correct the flux-annulus-background bias.
    %   The background in annulus around bright sources may be overestimated due to
    %   leakage of flux into the annulus.
    %   This function estimate the bias in the background per pixel and
    %   optionaly correct the measured flux and annulus background.
    %   This is estimating by binning (and interpolating) in the log-log
    %   space of the two parameters.
    %   This can be done safely to aperture photometry values.
    % Input  : - An AstroImage or AstroCatalog object, contraining the
    %            catalogs of sources.
    %          * ...,key,val,...
    %            'FLUX_ColName' - Column name containing the flux to use
    %                   for the estimation. 
    %                   Default is 'FLUX_APER_3'.
    %            'BACK_ColName' -  Column name containing the annulus background
    %                   to use for the estimation. 
    %                   Default is 'BACK_ANNULUS'.
    %            'CorrectKey' - A cell array of column names for which to
    %                   correct the bias (in addition to the BACK_ColName
    %                   column).
    %                   Only flux-like values can be corrected.
    %                   Default is {'FLUX_APER_1','FLUX_APER_2','FLUX_APER_3'}
    %            'CorrectRadius' - A vector of effective aperture radius to
    %                   use in the background in aperture calculation.
    %                   Default is [2 4 6].
    %            'BinningLog' - Binning logarithmic step size.
    %                   Default is 0.3.
    %            'InterpMethod' - Interpolation method used in the
    %                   correction. Default is 'spline'.
    % Output : - A structure array with element per each image or catalog
    %            element, and the following fields:
    %            .Binning - [Flux, AnnulusBackBiasPerPix]
    %                       The AnnulusBackBias is the bias per pixel in
    %                       the background estimation as a function of
    %                       flux.
    % Author : Eran Ofek (Jul 2022)
   
    arguments
        Obj   % AstroImage or AstroCatalog
        Args.FLUX_ColName     = 'FLUX_APER_3';
        Args.BACK_ColName     = 'BACK_ANNULUS';
        Args.CorrectKey cell  = {'FLUX_APER_1','FLUX_APER_2','FLUX_APER_3'};
        Args.CorrectRadius    = [2 4 6];
        Args.BinningLog       = 0.3;
        Args.InterpMethod     = 'spline';
    end
    
    if nargout>1
        if numel(Args.CorrectKey)~=numel(Args.CorrectRadius)
            error('Number of elements in CorrectKey must be equal to the number of elements in CorrectRadius');
        end
    end
    NcorrKey = numel(Args.CorrectKey);
            
    
    Nobj = numel(Obj);
    for Iobj=1:1:Nobj
        if isa(Obj, 'AstroImage')
            Cat = Obj(Iobj).CatData;
        elseif isa(Obj, 'AstroCatalog')
            Cat = Obj(Iobj);
        else
            error('First input argument must be an AstroImage or AstroCatalog object');
        end
        
        [Data,~,ColInd] = getCol(Cat, {Args.FLUX_ColName, Args.BACK_ColName});
        
        % fit bias model
        % remove NaNs and negative values
        FlagGood = all(Data>0 & ~isnan(Data), 2);
        LogData  = log10(Data(FlagGood,:));
        
        % binning
        B  = timeseries.binning(LogData,Args.BinningLog,[],{MeanBin, @median});
        NN = ~isnan(B(:,2));
        B  = B(NN,:);
        
        Result(Iobj).Binning = 10.^B;
        Result(Iobj).Binning = Result(Iobj).Binning - min(Result(Iobj).Binning);
        Result(Iobj).Data    = Data;
        
        if nargout>1
            [CorrData,~,CorrColInd] = getCol(Obj(Iobj), Args.CorrectKey);
            
            BackCorrPerPix = interp1(Result(Iobj).Binning(:,1),Result(Iobj).Binning(:,2), Data(:,1), Args.InterpMethod, 'linear');
            BackCorr       = BackCorrPerPix.*pi.*[Args.CorrectRadius(:).'].^2; % column per aperture
            CorrData       = CorrData + BackCorr;  % add the bias
            
            CorrBack       = Data(:,2) - BackCorrPerPix;  % subtract the bias from the background            

            % update Cat
            Cat.Catalog(:,ColInd(2))  = CorrBack;
            Cat.Catalog(:,CorrColInd) = CorrData;
        end
    end
end