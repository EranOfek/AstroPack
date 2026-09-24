function [T] = insertHealpixIndex2table(T, Args)
    % Use RA, Dec to add healpix indices to table
    % Input  : - A table.
    %          * ...,key,val,... 
    %            'ColRA' - RA column name in table. Default is 'RA'.
    %            'ColDec' - Dec column name in table. Default is 'DEC'.
    %            'CooUnits' - Coordinate units. Default is 'deg'.
    %            'HealpixType' - Healpix type: 'nested'|'ring'.
    %                   Default is 'nested'.
    %            'HealpixLevel' - Healpix levels to add.
    %                   Will add one column per level.
    %                   Default is 2.^[3, 8, 16]
    %            'ColHealpix' - String array of cell array of column names
    %                   to to add to table (one name per healpix level).
    %                   Default is ["NSIDE_PARTITION", "NSIDE_LOW", "NSIDE_HIGH"]
    %            'UniqueID' - A logical flag indicating if to return a
    %                   unique ID (using celestial.healpix.pix2uniqueId).
    %                   Default is true.
    % Output : - The input table in which Args.ColHealpix column names were
    %            added with the healpix indices corresponding to Args.HealpixLevel
    % Author : Eran Ofek (2024 Oct) 
    % Example: T=db.util.insertHealpixIndex2table(T)

    arguments
        T
        % Healpix indexing
        Args.ColRA         = 'RA';
        Args.ColDec        = 'DEC';
        Args.CooUnits      = 'deg';
        Args.HealpixType   = 'nested';
        Args.HealpixLevel  = 2.^[3, 8, 16];   % diamater ~ 13 deg, 0.4 deg, 5.7"
        Args.ColHealpix    = ["NSIDE_PARTITION", "NSIDE_LOW", "NSIDE_HIGH"];
        Args.UniqueID logical = true;

    end

    % add healpix ID
    Nlevel = numel(Args.HealpixLevel);
    for Ilevel=1:1:Nlevel
        T.(Args.ColHealpix{Ilevel}) = celestial.healpix.ang2pix(Args.HealpixLevel(Ilevel), T.(Args.ColRA), T.(Args.ColDec), 'Type',Args.HealpixType, 'CooUnits',Args.CooUnits, 'UniqueID', Args.UniqueID);
    end

end
