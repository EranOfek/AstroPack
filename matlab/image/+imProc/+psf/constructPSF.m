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
    % Example: AI=AstroImage('PTF_201411204943_i_p_scie_t115144_u023050379_f02_p100037_c02.fits');
    %          AI=imProc.background.background(AI);
    %          AI=imProc.sources.findMeasureSources(AI);
    %          AI=imProc.psf.constructPSF(AI);

   
    arguments
        Obj AstroImage   % CatData must be populated
        Args.HalfSize                       = 8;
        Args.ColFluxNorm                    = 'FLUX_APER_3';  % column of flux for normalization
        Args.ColBack                        = 'BACK_ANNULUS';
        Args.SumMethod                      = 'sigclip';
        
        Args.selectPsfStarsArgs cell        = {};
        Args.constructPSF_cutoutsArgs cell  = {};
        Args.ReCenter logical               = false;
       
        
    end
    
    Nobj = numel(Obj);
    for Iobj=1:1:Nobj
        % select PSF stars
        [PsfXY, ~, Flux, Back] = imProc.psf.selectPsfStars(Obj(Iobj).CatData,...
                                                     Args.selectPsfStarsArgs{:},...
                                                     'ColFluxNorm',Args.ColFluxNorm,...
                                                     'ColBack',Args.ColBack);
        
        Nsrc = size(PsfXY,1);
        
        % get flux for normalization
        Norm = 1./Flux;
        
        % remove sources with non unity flux (maybe neighboors?)
        hist(squeeze(sum(ShiftedCube,[1 2])),1000)
        
        % constructPSF_cutouts
        Args.SumMethod = 'median';
        [Mean, Var, Nim] = imUtil.psf.constructPSF_cutouts(Obj(Iobj).Image, PsfXY, Args.constructPSF_cutoutsArgs{:},...
                                                           'Norm',Norm,...
                                                           'Back',Back,...
                                                           'SumMethod',Args.SumMethod,...
                                                           'ReCenter',Args.ReCenter,...
                                                           'MomRadius',Args.HalfSize);
                    
        
        % populate the PSFData
        Obj(Iobj).PSFData
        
    end
end