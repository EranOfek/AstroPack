function Result = forcedPhot(Obj, Args)
    % Perform forced photometry on images in an AstroImage object.
    %       Given a s et of coordinates [X,Y] or [RA,Dec] perform forced
    %       photometry on images.
    %       This can be either aperture photometry, or PSF photometry, with
    %       or without position refinment.
    %       The output is written either to an AstroCatalaog object or
    %       added to the AstroCatalog in the AstroImage.
    % Input  : - An AstroImage object.
    %          * ...,key,val,...
    %
    
    arguments
        Obj AstroImage
        Args.Coo
        Args.CooUnits          = 'deg';   % 'pix'|'deg'|'rad
        Args.CalcPSF logical   = true;
    end

    switch lower(Args.CooUnits)
        case 'pix'
            IsSpherical = false;
        case 'deg'
            IsSpherical = true;
        case 'rad'
            IsSpherical = true;
            
        otherwise
            error('Unknown CooUnits option');
    end

    if IsSpherical
        Args.Coo = convert.angular(Args.CooUnits,'deg',Args.Coo);
    end

    Nobj = numel(Obj);
    for Iobj=1:1:Nobj
        if IsSpherical
            [X,Y] = Obj(Iobj).WCS.sky2xy(Arg.Coo(:,1), Args.Coo(:,2), 'InUnits','deg');
        end

        % check if sources are in footprint
        
        % force photometry on sources

    end
end
   
