function [UniquePix,Result, PixRA, PixDec] = statHealpix(NSide, RA, Dec, Vals, Args)
    % Calculate statistics of values in healpix.
    %   Given RA, Dec, and some values sort the positions into healpix and
    %   for each unique pixel, calculate the user defined statistics within
    %   this pixel.
    % Input  : - NSide.
    %          - RA
    %          - Dec
    %          - A matrix of values, row per RA,Dec.
    %            The values are used to calculate the returned statistics.
    %          * ...,key,val,... 
    %            'Units' - Units of imput and output coordinates.
    %                   Default is 'deg'.
    %            'Type' - 'nested'|'ring'. Default is 'nested'.
    %            'UniqueID' - Use uniqueID. Default is false.
    %            --- functions ---
    %            'Funs' - A cell array of function handles to run on the
    %                   values within each pixel.
    %                   Default is {@median, @std}
    %            'FunsCol' - A cell array. Each element corresponds to a
    %                   function in 'Funs', and contains the indices of
    %                   columns to send the function as input.
    %                   The columns will be sent to the function in one
    %                   block.
    %                   Default is {1, 1}
    %            'FunsArgs' - A cell array. Each element corresponds to a
    %                   function in 'Funs', and contains additional
    %                   arguments to send to the function.
    %                   Default is {{1,'omitnan'}, {[],1,'omitnan'}}
    % Output : - Pixel ID for each pixel with data.
    %          - A matrix with the statistics.
    %            Row per pixel and column per function.
    %          - The RA of the pixel.
    %          - THe Dec of the pixel.
    % Author : Eran Ofek (2026 Jul) 
    % Example: [i,s]=celestial.healpix.statHealpix(2.^3,rand(1000,1),rand(1000,1),randn(1000,1))

    arguments
        NSide
        RA
        Dec
        Vals
        Args.Units             = 'deg';
        Args.Type              = 'nested';
        Args.UniqueID          = false;
        Args.Funs              = {@median, @std};
        Args.FunsCol           = {1, 1};
        Args.FunsArgs          = {{1,'omitnan'}, {[],1,'omitnan'}};
    end

    Nfun = numel(Args.Funs);

    Pix = celestial.healpix.ang2pix(NSide, RA, Dec, 'Type',Args.Type, 'CooUnits',Args.Units, 'UniqueID',Args.UniqueID);

    UniquePix = unique(Pix);
    Nup       = numel(UniquePix);
    Result    = nan(Nup, Nfun);
    for Iup=1:1:Nup
        Flag = (Pix == UniquePix(Iup));
        
        for Ifun=1:1:Nfun
            Result(Iup, Ifun) = Args.Funs{Ifun}(Vals(Flag, Args.FunsCol{Ifun}), Args.FunsArgs{Ifun}{:});
        end
    end

    [PixRA, PixDec] = celestial.healpix.pix2ang(NSide, UniquePix, 'CooUnits',Args.Units, 'Type',Args.Type, 'UniqueID',Args.UniqueID);

end
