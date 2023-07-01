function [Result, Summary] = constructPSF(Obj, Args)
    % Select PSF stars and construct a PSF for an AstroImage
    %       A new vesion: imProc.psf.populatePSF
    % Input  : - An AstroImage object. The CatData must be popualted.
    %          * ...,key,val,...
    %            'OutType' - Output type: 'AstroImage' | 'AstroPSF'.
    %                   Default is 'AstroImage'.
    %            'HalfSize' - PSF stamp half size. Default is 8.
    %            'selectPsfStarsArgs' - A cell array of arguments to pass
    %                   to imProc.psf.selectPsfStars. Default is {}.
    %            'constructPSF_cutoutsArgs' - A cell array of arguments to pass
    %                   to imProc.psf.constructPSF_cutouts.
    %                   Defaukt is {}.
    %            'ReCenter' - Recenter the sources in the stamps.
    %                   Default is false (i.e., X,Y in catalogs are good
    %                   enough).
    %            'TypePSF' - Type of output PSF (e.g., @single).
    %                   If empty, do not change. Default is [].
    % Output : - The input AstroImage object in which the PSFData is
    %            populated with the measured pixelated PSF.
    %          - A structure array, with:
    %            .Nsrc - number of sources.
    % Author : Eran Ofek (Jan 2022)
    % Example: AI=AstroImage('PTF_201411204943_i_p_scie_t115144_u023050379_f02_p100037_c02.fits');
    %          AI=imProc.background.background(AI);
    %          AI=imProc.sources.findMeasureSources(AI);
    %          AI=imProc.psf.constructPSF(AI);

   
    arguments
        Obj AstroImage   % CatData must be populated
        Args.OutType                        = 'AstroImage'; % 'AstroImage' | 'AstroPSF'
        Args.HalfSize                       = 8;
        Args.ColFluxNorm                    = 'FLUX_APER_3';  % column of flux for normalization
        Args.ColBack                        = 'BACK_ANNULUS';
        Args.SumMethod                      = 'sigclip';
        
        Args.selectPsfStarsArgs cell        = {};
        Args.constructPSF_cutoutsArgs cell  = {};
        Args.SmoothWings logical            = false; %true;
        Args.SuppressEdges logical          = true;
        Args.suppressEdgesArgs cell         = {};
        Args.ReCenter logical               = false;
        Args.TypePSF                        = []; % or '@single', '@double',...
        
    end
    
    Nobj = numel(Obj);
    
    switch lower(Args.OutType)
        case 'astroimage'
            Result = Obj;
            OutIsImage = true;
        case 'astropsf'
            Result = AstroPSF;
            OutIsImage = false;
        otherwise
            error('Unknown OutType option');
    end
    
    for Iobj=1:1:Nobj
        % select PSF stars
        [PsfXY, ~, Flux, Back] = imProc.psf.selectPsfStars(Obj(Iobj).CatData,...
                                                     Args.selectPsfStarsArgs{:},...
                                                     'ColFluxNorm',Args.ColFluxNorm,...
                                                     'ColBack',Args.ColBack);
        
        Nsrc = size(PsfXY,1);
        Summary(Iobj).Nsrc = Nsrc;
        
        Result(Iobj).PSFData.Nstars = Nsrc;

        if Nsrc==0
            % NO PSF candidates found - skip PSF
        else

            % get flux for normalization
            Norm = 1./Flux;
                    
            % constructPSF_cutouts
            Args.SumMethod = 'median';
            [Mean, Var, Nim] = imUtil.psf.constructPSF_cutouts(Obj(Iobj).Image, PsfXY, Args.constructPSF_cutoutsArgs{:},...
                                                               'Norm',Norm,...
                                                               'Back',Back,...
                                                               'SmoothWings',Args.SmoothWings,...
                                                               'SumMethod',Args.SumMethod,...
                                                               'ReCenter',Args.ReCenter,...
                                                               'MomRadius',Args.HalfSize);
                        
            if ~isempty(Args.TypePSF)
                Mean = Args.TypePSF(Mean);
                Var  = Args.TypePSF(Var);
            end
            % populate the PSFData
            if OutIsImage
                Result(Iobj).PSFData.DataPSF = Mean;
                Result(Iobj).PSFData.DataVar = Var;
            else
                Result(Iobj).DataPSF = Mean;
                Result(Iobj).DataVar = Var;
            end
            if Args.SuppressEdges
                Result(Iobj).PSFData = Result(Iobj).PSFData.suppressEdges(Args.suppressEdgesArgs{:});
            end
        end
    end
end