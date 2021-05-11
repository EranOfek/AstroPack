function [PassedThreshold, FracIdentical] = identifySimilarImages(Obj, Args)
    % Search for sucessive images with a fraction of identical pixel values
    %   This is useful in order to identify problems with the
    %   detector firmware (i.e., some regions of the detector in
    %   two surcessive images are identical due to a readout
    %   problem).
    % Input  : - An AstroImage or ImageComonent object.
    %          * ...,key,val,...
    %            'DataProp' - Data property containing the image.
    %                   Default is 'Image'.
    %            'MaxAllowedFrac' - The fraction of identical
    %                   pixels above to set the output argument
    %                   PassedThreshold to true.
    %                   Default is 0.2.
    % Output : - PassedThreshold. True if the fraction of identical
    %            pixels in two sucessive images is larger than the
    %            MaxAllowedFrac threshold.
    %          - An array of the fraction of identical pixels in
    %            and image and the sucessive image.
    %            The last value is always NaN.
    % Author : Eran Ofek
    % Example: AI=AstroImage({rand(1000,1000), rand(1000,1000)});
    %          [a,b]=imProc.dark.identifySimilarImages(AI) 

    arguments
        Obj AstroImage
        Args.DataProp                = 'Image';
        Args.MaxAllowedFrac          = 0.2;
    end

    Nobj = numel(Obj);
    FracIdentical = nan(size(Obj));
    for Iobj=1:1:Nobj-1
        Nidentical          = sum(Obj(Iobj).(Args.DataProp) == Obj(Iobj+1).(Args.DataProp),'all');
        Npix                = numel(Obj(Iobj).(Args.DataProp));
        FracIdentical(Iobj) = Nidentical./Npix;
    end
    if any(FracIdentical>Args.MaxAllowedFrac)
        PassedThreshold = true;
    else
        PassedThreshold = false;
    end

end
