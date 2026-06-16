function PixRanges = coneSearch2PixRanges(RA, Dec, SearchRadius, NSideCat, Args)
    % coneSearch2PixRanges  Convert cone search to HEALPix nested pixel ranges.
    % Package: celestial.healpix
    % Description: Given a cone-search center and radius, choose a HEALPix
    %              NSide whose pixel size approximately covers the search
    %              radius, find the central pixel, find its neighbors, and
    %              convert these low-NSide pixels into ranges of nested pixel
    %              IDs at NSideCat.
    %              The function assumes NESTED ordering.
    % Input  : - Right ascension [radians].
    %          - Declination [radians].
    %          - Search radius [radians].
    %          - Catalog HEALPix NSide.
    %          * ...,key,val,... 
    %          'Algo' - search algorithm: 'neighb' or 'cone' 
    % Output : - An array of [Npix x 2] ranges of nested pixel IDs at NSideCat.
    %            The first column is the low end of the range, and the second
    %            column is the high end of the range.
    %            The ranges are inclusive.
    %            Npix is typically 9, including the central pixel and its
    %            unique neighbors. In cases where one neighbor appears twice,
    %            duplicate pixels are removed.
    % Author : ChatGPT + Eran Ofek (Jun 2026)
    % Example: PixRanges = celestial.healpix.coneSearch2PixRanges(1, 0.5, 1./1024, 2.^16);
    
    arguments
        RA
        Dec
        SearchRadius
        NSideCat
        Args.Algo = 'neighb';  % 'neighb' or 'cone'
    end
    
    RAD = 180/pi;
    
    if ~isPowerOfTwo(NSideCat)
        error('NSideCat must be a positive power of 2.');
    end
    
    if any(SearchRadius <= 0, 'all')
        error('SearchRadius must be positive.');
    end
    
    if numel(RA) ~= 1 || numel(Dec) ~= 1 || numel(SearchRadius) ~= 1
        error('RA, Dec, and SearchRadius must be scalar.');
    end
    
    % Choose search NSide.
    % Approximate enclosing radius of a HEALPix pixel is ~1/NSide radians.
    % Therefore choose the largest NSide for which:
    %
    %   1/NSideSearch >= SearchRadius
    %
    % but never larger than NSideCat.
    NSideSearch = 2.^floor(log2(1./SearchRadius));  % actually, it should be 2.^floor(log2(sqrt(3)/SearchRadius));
    NSideSearch = max(NSideSearch, 1);
    NSideSearch = min(NSideSearch, NSideCat);
    
    if strcmpi(Args.Algo,'neighb')        
        % Central pixel at the search NSide
        PixSearch = celestial.healpix.ang2pix(NSideSearch, RA, Dec, ...
            'Type', 'nested', ...
            'CooUnits', 'rad');
        
        % Find neighbors, including the central pixel
        PixList = celestial.healpix.findNeighbors(NSideSearch, PixSearch, ...
            'IncludeSelf', true);
        
    elseif strcmpi(Args.Algo,'cone')        
        PixList = celestial.healpix.mex.coneSearch(NSideSearch,RA*RAD,Dec*RAD,SearchRadius*RAD);
        
    else
        error('Unknown algorithm')
    end
    
    % Remove duplicates.
    % In some special HEALPix locations, one neighbor may appear twice.
    PixList = unique(PixList(:));
    
    % Convert low-NSide nested pixels into ranges at NSideCat.
    % In NESTED ordering, all children of a parent pixel are contiguous.
    Factor = NSideCat ./ NSideSearch;
    Nchild = Factor.^2;
    
    Low  = double(PixList) .* Nchild;
    High = Low + Nchild - 1;
    
    PixRanges = int64([Low, High]);
end


function Flag = isPowerOfTwo(X)
% isPowerOfTwo  True for positive integer powers of two.
Flag = X > 0 & X == round(X) & 2.^round(log2(double(X))) == X;
end