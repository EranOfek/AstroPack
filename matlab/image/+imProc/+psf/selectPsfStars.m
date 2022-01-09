function [PsfXY, Flag, Cube] = selectPsfStars(Obj, Args)
    % Select PSF stars from AstroCatalog
    %
    
    arguments
        Obj(1,1)
        Args.ColSN             = 'SN_2';
        Args.RangeSN           = [30 500];
        Args.ColSNdelta        = 'SN_1';
        Args.MinDeltaSN        = 1;                 % (SN-SNdelta)>MinDeltaSN
        Args.ColMom2           = {'X2','Y2','XY'};
        Args.DeltaSigma        = 0.5;
        Args.ColMom1           = {'X','Y'};
        
        Args.StampHalfSize     = 7;
        Args.mexCutout logical    = true;
        Args.Circle logical       = false;
    end
    
    if isa(Obj, 'AstroImage')
        Cat = Obj.CatData;
    elseif isa(Obj, 'AstroCatalog')
        if nargout>2
            error('In order to return Cube of PSFs, AstroImage must be provided');
        end
        Cat = Obj;
    else
        error('First input argument must be of AstroImage or AstroCatalog type');
    end
    
    SN       = getCol(Cat, {Args.ColSNdelta, Args.ColSN});
    Mom2     = getCol(Cat, Args.ColMom2);
    XY       = getCol(Cat, Args.ColMom1);
    
    FlagSN   = SN(:,2)>Args.RangeSN(1) & SN(:,2)<Args.RangeSN(2) & (SN(:,2)-SN(:,1))>Args.MinDeltaSN;
    Sigma    = sqrt(Mom2(:,1)+Mom2(:,2));
    MedSigma = median(Sigma);
    FlagSig  = Sigma>(MedSigma - Args.DeltaSigma) & Sigma<(MedSigma + Args.DeltaSigma);
    
    Flag     = FlagSN & FlagSig;
    
    % get PSF sources
    PsfXY    = XY(Flag,:);
    
    if nargout>2
        [Cube, RoundX, RoundY, X, Y] = image2cutouts(Obj(Obj.Image, PsfXY(:,1), PsfXY(:,2),...
                                                                    Args.StampHalfSize,...
                                                                    'mexCutout',Args.mexCutout,...
                                                                    'Circle',Args.Circle);
    end
    
end