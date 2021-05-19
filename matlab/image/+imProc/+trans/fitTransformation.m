function [Param, Res] = fitTransformation(ObjCat, ObjRef, Args)
    %
    % Example: 
   
    arguments
        ObjCat
        ObjRef
        Args.Tran Tran2D                   % optional Tran2d object - override Fun... 
%         Args.FunX         = {@(x,y,c,AM,PA) ones(size(x)),...
%                              @(x,y,c,AM,PA) x,...
%                              @(x,y,c,AM,PA) y,...
%                              @(x,y,c,AM,PA) 2.*x.^2-1,...
%                              @(x,y,c,AM,PA) 2.*y.^2-1,...
%                              @(x,y,c,AM,PA) x.*y};
%         Args.FunY         = {@(x,y,c,AM,PA) ones(size(x)),...
%                              @(x,y,c,AM,PA) x,...
%                              @(x,y,c,AM,PA) y,...
%                              @(x,y,c,AM,PA) 2.*x.^2-1,...
%                              @(x,y,c,AM,PA) 2.*y.^2-1,...
%                              @(x,y,c,AM,PA) x.*y};
%         Args.FunNX        = @(x,nx1,nx2) (x-nx1)./nx2;
%         Args.FunNY        = @(y,ny1,ny2) (y-ny1)./ny2;
        Args.MaxIter      = 0;
        Args.ErrPos       = 1e-5;  % total error in one axis - all contributions
        Args.Norm         = NaN;   % empty - do nothing, NaN - auto, or [centerX, RangeX, centerY, RangeY]
        Args.FitMethod    = 'lscov'; % 'lscov' | '\'
        Args.Algo         = 'chol'; % 'orth' | 'chol'
        Args.ColCatX      = AstroCatalog.DefNamesX;
        Args.ColCatY      = AstroCatalog.DefNamesY;
        Args.ColCatM      = {'MAG','mag','MAG_PSF','MAG_CONV'};
        Args.ColRefX      = AstroCatalog.DefNamesX;
        Args.ColRefY      = AstroCatalog.DefNamesY;
        Args.ColRefM      = {'MAG','mag','MAG_PSF','MAG_CONV'};
        Args.ColRefC      = {'Color'};
        Args.ColRefAM     = {'AIRMASS'};
        Args.ColRefPA     = {'ParAng'};
        Args.ColRefModX   = {'ModX'};
        Args.ColRefModY   = {'ModY'};
        
        % resid_vs_mag arguments
        Args.MagRange     = [];  % empty - no limit
        Args.MaxResid     = 1;   % pixels
        Args.BinMethod    = 'bin'; % 'bin' | 'poly'
        Args.PolyDeg      = 3;
        Args.BinSize      = 1;
        Args.FunMean      = @nanmedian;
        Args.FunStd       = @imUtil.background.rstd;
        Args.InterpMethod = 'linear';
        Args.ThresholdSigma = 3;
        
        
    end
    DefColX = 'X';
    DefColY = 'Y';
    
    if isnumeric(ObjCat)
        Ncat = 1;
    else
        Ncat = numel(ObjCat);
    end
    if isnumeric (ObjRef)
        Nref = 1;
    else
        Nref = numel(ObjRef);
    end
    Nmax = max(Mcat, Nref);
    for Imax=1:1:Nmax
        %
        Icat = min(Imax, Ncat);
        Iref = min(Imax, Nref);
        
        if isa(ObjCat,'AstroImage')
            Cat = ObjCat(Icat).CatData;
        elseif isa(ObjCat,'AstroCatalog')
            Cat = ObjCat(Icat);
        elseif isnumeric(ObjCat)
            Cat = AstroCatalog({ObjCat});
            Cat.ColNames = {DefColX, DefColY};
        else
            error('ObjCat must be of AstroImage or AstroCatalog or numeric type');
        end
        if isa(ObjRef,'AstroImage')
            Ref = ObjRef(Iref).CatData;
        elseif isa(ObjRef,'AstroCatalog')
            Ref = ObjRef(Iref);
        elseif isnumeric(ObjRef)
            Ref = AstroCatalog({ObjRef});
            Ref.ColNames = {DefColX, DefColY};
        else
            error('ObjRef must be of AstroImage or AstroCatalog or numeric type');
        end
        
        % columns indices
        ColCatX = colnameDict2ind(Cat, Args.ColCatX);
        ColCatY = colnameDict2ind(Cat, Args.ColCatY);
        ColCatM = colnameDict2ind(Cat, Args.ColCatM);
        
        ColRefX = colnameDict2ind(Ref, Args.ColRefX);
        ColRefY = colnameDict2ind(Ref, Args.ColRefY);
        ColRefM = colnameDict2ind(Ref, Args.ColRefM);
        ColRefC = colnameDict2ind(Ref, Args.ColRefC);
        ColRefAM = colnameDict2ind(Ref, Args.ColRefAM);
        ColRefPM = colnameDict2ind(Ref, Args.ColRefPM);
        ColRefModX = colnameDict2ind(Ref, Args.ColRefModX);
        ColRefModY = colnameDict2ind(Ref, Args.ColRefModY);
        
        
        % fit the transformation
        [Param(Imax), Res(Imax), ResLoop] = imUtil.patternMatch.fit_astrometric_tran(Cat, Ref, ...
                                                                         'Tran',Args.Tran,...
                                                                         'MaxIter',Args.MaxIter,...
                                                                         'ErrPos',Args.ErrPos,...
                                                                         'Norm',Args.Norm,...
                                                                         'FitMethod',Args.FitMethod,...
                                                                         'Algo',Args.Algo,...
                                                                         'ColCatX',ColCatX, 'ColCatY',ColCatY, 'ColCatM',ColCatM,...
                                                                         'ColRefX',ColRefX, 'ColRefY',ColRefY, 'ColRefM',ColRefM,...
                                                                         'ColRefC',ColRefC, 'ColRefAM',ColRefAM, 'ColRefPM', ColRefPM,...
                                                                         'ColRefModX',ColRefModX, 'ColRefModY',ColRefModY,...
                                                                         'MagRange',Args.MagRange,...
                                                                         'MaxResid',Args.MaxResid,...
                                                                         'BinMethod',Args.BinMethod,...
                                                                         'PolyDeg',Args.PolyDeg,...
                                                                         'BinSize',Args.BinSize,...
                                                                         'FunMean',Args.FunMean,...
                                                                         'FunStd',Args.FunStd,...
                                                                         'InterpMethod',Args.InterpMethod,...
                                                                         'ThresholdSigma',Args.ThresholdSigma);
                                                                         

        %
        
        
        
    end
    
end