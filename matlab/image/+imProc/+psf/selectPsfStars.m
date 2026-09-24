function [PsfXY, Flag, Flux, Back] = selectPsfStars(Obj, Args)
    % Select PSF stars from AstroCatalog
    %   Selection is based on:
    %   1. S/N in range 
    %   2. Reject delta function by DeltaSN
    %   3. Select sources with second moment around the mode
    % Input  : - A single element AstroImage or AstroCatalog object.
    %          * ...,key,val,...
    %            'ColSN' - Column name for S/N. Default is 'SN_2'.
    %            'RangeSN' - Select S/N in range. Default is [30 500].
    %            'ColSNdelta' - Column name for S/N of delta fun.
    %                   Default is 'SN_1'.
    %            'MinDeltaSN' - Select sources with (SN_2-SN_1)>MinDeltaSN
    %                   Default is 1.
    %            'ColMom2' - Column names for second moments in X,Y,XY.
    %                   Default is {'X2','Y2','XY'}.
    %            'DeltaSigma' - Select sources with 2nd moment in this
    %                   half0range around mode of second moment.
    %                   Default is 0.5.
    %            'ColMom1' - Column names for source position in X,Y.
    %                   Default is {'X','Y'}.
    %            'ColFluxNorm' - Column name for normalization flux.
    %                   Default is 'FLUX_APER_3'.
    %            'ColBack' - Column name for background flux.
    %                   E.g., 'BACK_ANNULUS' | 'BACK_IM'
    %                   Default is 'BACK_ANNULUS'.
    % Outout : - A two column matrix of [X,Y] of selected sources.
    %          - A logical vector of flags (true for selected object).
    %          - The flux of each selected source.
    % Author : Eran Ofek (Jan 2022)
    % Example: XY = imProc.psf.selectPsfStars(AI);
    
    arguments
        Obj(1,1)
        Args.ColSN             = 'SN_2';
        Args.RangeSN           = [30 500];
        Args.ColSNdelta        = 'SN_1';
        Args.MinDeltaSN        = 1;                 % (SN-SNdelta)>MinDeltaSN
        Args.ColMom2           = {'X2','Y2','XY'};
        Args.DeltaSigma        = 0.5;
        Args.ColMom1           = {'X1','Y1'};
        Args.ColFluxNorm       = 'FLUX_APER_3';  % column of flux for normalization
        Args.ColBack           = 'BACK_ANNULUS';
    end
    
    if isa(Obj, 'AstroImage')
        Cat = Obj.CatData;
    elseif isa(Obj, 'AstroCatalog')
        Cat = Obj;
    else
        error('First input argument must be of AstroImage or AstroCatalog type');
    end
    
    SN       = getCol(Cat, {Args.ColSNdelta, Args.ColSN});
    Mom2     = getCol(Cat, Args.ColMom2);
    XY       = getCol(Cat, Args.ColMom1);
    
    FlagSN   = SN(:,2)>Args.RangeSN(1) & SN(:,2)<Args.RangeSN(2) & (SN(:,2)-SN(:,1))>Args.MinDeltaSN;
    Sigma    = sqrt(abs(Mom2(:,1))+abs(Mom2(:,2)));
    %MedSigma = median(Sigma, 1, 'omitnan');

    MedSigma = imUtil.background.modeVar_QuantileHist(Sigma(FlagSN));
    
    FlagSig  = Sigma>(MedSigma - Args.DeltaSigma) & Sigma<(MedSigma + Args.DeltaSigma);
    
    Flag     = FlagSN & FlagSig;
    
    % get PSF sources
    PsfXY    = XY(Flag,:);
    
    if nargout>2
        Flux     = getCol(Cat, Args.ColFluxNorm);
        Flux     = Flux(Flag);
    
        if nargout>3
            Back     = getCol(Cat, Args.ColBack);
            Back     = Back(Flag);
        end
    end
    
end