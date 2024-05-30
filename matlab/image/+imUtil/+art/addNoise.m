function Image = addNoise(Image, NoiseModel, Args)
    % Add noise to an image matrix
    %     NB: the noise models employed below should be used for count images (electrons, photons), 
    %     but not for count rate images (el/s, ph/s)!
    % Input  : - Image (counts, electrons, photons) 
    %          - NoiseModel (currently only 'normal' or 'poisson')
    %          * ...,key,val,... 
    %        'NoisePar' - parameters of a noise model 
    % Output : - 
    % Author : A.M. Krassilchtchikov (2024 May) 
    % Example: Image = imUtil.art.addNoise(Image,'normal');

    arguments
        Image
        NoiseModel             = 'normal'
        Args.NoisePar          = [];      % currently nor used      
    end
    %
    ImSize = size(Image);
    
    switch lower(NoiseModel)
        case 'normal'
            Image = normrnd( Image, sqrt(Image), ImSize(1), ImSize(2) ); 
        case 'poisson'
            Image = poissrnd(Image, ImSize(1), ImSize(2)); 
        otherwise
            error('Unknown noise model requested')
    end
end
