function [FLon, FLat] = funOnCosDir(Lon, Lat, Fun, Args)
    % Given Lon/Lat, apply a function on the cosined and convert back to Lon/Lat.
    % Input  : - Lon
    %          - Lat
    %          - Function handle to apply. Default is @mean.
    %          * ...,key,val,... 
    %            'FunArgs' - A cell array of additional arguments to pass
    %                   to the function. Default is {1, 'omitnan'}.
    %            'InUnits' - Input coordinate units. Default is 'rad'.
    %            'OutUnits' - Output coordinate units. If empty, then use
    %                   the same as 'InUnits'. Default is [].
    % Output : - Value of Lon after function applied.
    %          - Value of Lat after function applied.
    % Author : Eran Ofek (2024 Mar) 
    % Example: [FLon, FLat] = celestial.coo.funOnCosDir(rand(5,1), rand(5,1))

    arguments
        Lon
        Lat
        Fun               = @mean;
        Args.FunArgs cell = {1,'omitnan'};
        Args.InUnits      = 'rad';
        Args.OutUnits     = [];
    end

    Conv = convert.angular(Args.InUnits,'rad');
    
    [X,Y,Z] = celestial.coo.coo2cosined(Lon.*Conv, Lat.*Conv);

    FX = Fun(X, Args.FunArgs{:});
    FY = Fun(Y, Args.FunArgs{:});
    FZ = Fun(Z, Args.FunArgs{:});

    if isempty(Args.OutUnits)
        Args.OutUnits = Args.InUnits;
    end

    [FLon, FLat] = celestial.coo.cosined2coo(FX, FY, FZ);

    Conv = convert.angular('rad',Args.OutUnits);
    FLon = FLon.*Conv;
    FLat = FLat.*Conv;

end
