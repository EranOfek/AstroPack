function [FluxMatch,ZP0] = getFluxMatch(Obj, Args)
    % Return the flux matching factor required to multiply images to bring them into common flux. 
    % Input  : - AstroImage/AstroDiff/AstroZOGY object.
    %          * ...,key,val,... 
    %            'ZP' - Either a string/char array containing an header
    %                   keyword name from which to extract the ZP of each
    %                   image, or a numeric array with ZP per image.
    %                   This ZP will be converted (with 'ZP0') to flux
    %                   matching factor.
    %                   Default is 'PH_ZP'.
    %            'FluxZP' - Either a string/char array containing an header
    %                   keyword name from which to extract the flux matching factor of each
    %                   image, or a numeric array with flux matching factor
    %                   per image. If not empty, then this parameter
    %                   superceed the 'ZP' argument. Default is [].
    %            'ZP0' - Magnitude reference for the ZP (can be used in
    %                   order to avoid flux matching factor which is far from
    %                   unity).
    %                   The FluxMatch is 10.^(-0.4.*(ZP-ZP0))
    %                   This can be either a number or a function handle to
    %                   apply to array of ZP to get a scalar ZP0 (e.g.,
    %                   @median). Default is 25.
    %
    % Output : - An array of flux matching factor. Multiply this by each
    %            image to bring all the images to a common flux system.
    %          - ZP0 used (NaN for FluxZP option).
    % Author : Eran Ofek (2026 Mar) 
    % Example: [FluxMatch,ZP0] = imProc.stack.getFluxMatch(AI)

    arguments
        Obj
        Args.ZP                = 'PH_ZP';
        Args.FluxZP            = [];
        Args.ZP0               = 25;  % @median
    end

    
    if isempty(Args.FluxZP)
        if isnumeric(Args.ZP)
            ZP = Args.ZP;
        else
            % get ZP from header
            StZP = Obj.getStructKey(Args.ZP);
            ZP   = [StZP.(Args.ZP)].';
            if any(isnan(ZP))
                error('ZP is NaN or not in header');
            end
            if isnumeric(Args.ZP0)
                ZP0 = Args.ZP0;
            else
                % Args.ZP0 is assumed to be a function handle
                ZP0 = Args.ZP0(ZP);
            end
        end
        % convert ZP to flux matching parameter
        FluxMatch = 10.^(-0.4.*(ZP-ZP0));

    else
        ZP0 = NaN;
        if isnumeric(Args.FluxZP)
            FluxMatch = Args.FluxZP;
        else
            % get flux match from header
            StFM = Obj.getStructKey(Args.FluxZP);
            FluxMatch   = [StFM.(Args.FluxZP)].';
            if any(isnan(FluxMatch))
                error('FluxZP is NaN or not in header');
            end
        end
    end

end
