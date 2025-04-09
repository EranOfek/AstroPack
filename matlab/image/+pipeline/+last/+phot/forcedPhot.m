function [Result] = forcedPhot(Image, Coo, Args)
    % Specilaized forced photometry for LAST images and diff images
    % Input  : - 
    %          - 
    %          * ...,key,val,... 
    % Output : - 
    % Author : Eran Ofek (2025 Apr) 
    % Example: pipeline.last.phot.forcedPhot

    arguments
        Image
        Coo
        Args.CooUnits          = 'deg';

        Args.MomentMaxIter           = 0;       % 0 - no iterations
        Args.UseMomCoo logical       = false;
        Args.AperRadius              = [2 4 6];
        Args.Annulus                 = [10 12];
    
        Args.HalfSizePSF             = 6;
        Args.FitRadius               = 3;
        Args.SmallStep               = 1e-3;
        Args.MaxStep                 = 0.2;
        Args.ConvThresh              = 1e-4;
        Args.MaxIter                 = 10;      % use 1 for no itrations
        Args.UseSourceNoise          = 'off';
        Args.ZP                      = 25; 
        Args.HeaderZP                = false;   

    end
    RAD = 180./pi;



    if isa(Image, 'AstroImage')
        AI = Image;
    else
        % read Image
    end

    % convert coordinates to pixel position:
    [PX,PY]  = sky2xy(AI.WCS, Coo(:,1), Coo(:,2), 'InUnits',Args.CooUnits);

    % populate Back and Var in image:
    AI.Back = median(AI.Image(:));
    AI.Var  = tools.math.stat.rstd(AI.Image(:)).^2;

    R=imProc.sources.forcedPhot(AI, 'Coo',Coo,...
                                    'CooUnits',Args.CooUnits,...
                                    'Moving',false,...
                                    'AddRefStarsDist',0,...
                                    'UseBack',true,...
                                    'ReconstructPSF',false,...                                
                                    'MomentMaxIter',Args.MomentMaxIter,...
                                    'UseMomCoo',Args.UseMomCoo,...
                                    'AperRadius',Args.AperRadius,...
                                    'Annulus',Args.Annulus,...
                                    'HalfSizePSF',Args.HalfSizePSF,...
                                    'FitRadius',Args.FitRadius,...
                                    'SmallStep',Args.SmallStep,...
                                    'ConvThresh',Args.ConvThresh,...
                                    'MaxIter',Args.MaxIter,...
                                    'UseSourceNoise',Args.UseSourceNoise,...
                                    'ZP',Args.ZP,...
                                    'HeaderZP',Args.HeaderZP);


    


end
