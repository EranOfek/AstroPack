function Obj = constructPSF(Obj, Args)
    % Select PSF stars and construct a PSF for an AstroImage
    % Input  : - An AstroImage object. The CatData must be popualted.
    %          * ...,key,val,...
    %            'HalfSize' - PSF stamp half size. Default is 8.
    %            'selectPsfStarsArgs' - A cell array of arguments to pass
    %                   to imProc.psf.selectPsfStars. Default is {}.
    %            'constructPSF_cutoutsArgs' - A cell array of arguments to pass
    %                   to imProc.psf.constructPSF_cutouts.
    %                   Defaukt is {}.
    %            'ReCenter' - Recenter the sources in the stamps.
    %                   Default is false (i.e., X,Y in catalogs are good
    %                   enough).
    % Output : - The input AstroImage object in which the PSFData is
    %            populated with the measured pixelated PSF.
    % Author : Eran Ofek (Jan 2022)
    % Example: 
   
    arguments
        Obj AstroImage   % CatData must be populated
        Args.HalfSize                       = 8;
        Args.selectPsfStarsArgs cell        = {};
        Args.constructPSF_cutoutsArgs cell  = {};
        Args.ReCenter logical               = false;
       
    end
    
    Nobj = numel(Obj);
    for Iobj=1:1:Nobj
        % select PSF stars
        [PsfXY] = imProc.psf.selectPsfStars(Obj(Iobj).CatData, Args.selectPsfStarsArgs{:});
             
        
        [Mean, Var, Nim] = imProc.psf.constructPSF_cutouts(Obj(Iobj).Image, PsfXY, Args.constructPSF_cutoutsArgs{:},...
                                                           'ReCenter',Args.ReCenter,...
                                                           'MomRadius',Args.HalfSize);
                                                       
        % populate the PSFData
        Obj(Iobj).PSFData
        
    end
end