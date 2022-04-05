function [FlagOverlap, N_SearchInRect, N_RectInSearch] = rectOverlap(RectLon, RectLat, SearchRectLon, SearchRectLat, Crit)
    % Test if a spherical rectangles are overlapping/intersecting.
    % Input  : - A 4 rows array of rectangle longitude [rad]. Each column is for
    %            a single rectangle. The function search for intersection
    %            between each one of this rectangles and the single
    %            rectangle defined by the 3rd and 4th input argumnets.
    %          - A 4 rows array of rectangle latitude [rad]. Each column is for
    %            a single rectangle.
    %          - A 4 element column vector of longitude [rad] of rectangle
    %            to search.
    %          - A 4 element column vector of latitude [rad] of rectangle
    %            to search.
    %          - Flag indicating if to use ">" or ">=" in in_halfspace.m.
    %            If 1 then use (N dot R > C),
    %            If 2 then use (N dot R) >= C.
    %            Default is 2.
    % Output : - A row vector of logicals indicatibg if the rectangles and
    %            the search rectangle are overlapping. Entry per each input
    %            rectangle (i.e., number of columns in the first input
    %            argument).
    %          - A row vector indicating, for each rectangle, the number of
    %            corners of search rectangle that are found within the
    %            rectangle.
    %          - A row vector indicating, for each rectangle, the number of
    %            corners of rectangle that are found within the
    %            search rectangle.
    % Author : Eran Ofek (Apr 2022)
    % Example: [FlagOverlap, N_SearchInRect, N_RectInSearch] = celestial.search.rectOverlap([1 1 1.1 1.1]'+[0 1],[1 1.1 1.1 1]'+[0 -1] ,[1 1 1.01 1.01]', [1 1.01 1.01 1]');
    
    
    
    arguments
        RectLon(4,:)
        RectLat(4,:)
        SearchRectLon(4,1)
        SearchRectLat(4,1)
        Crit                = 2;
    end
    
    
    
    [~,Nrect] = size(RectLon);
    N_SearchInRect = zeros(1, Nrect);
    N_RectInSearch = zeros(1, Nrect);
    for Irect=1:1:Nrect
        Flag1 = celestial.htm.in_polysphere([SearchRectLon, SearchRectLat], [RectLon(:,Irect), RectLat(:,Irect)], Crit);
        Flag2 = celestial.htm.in_polysphere([RectLon(:,Irect), RectLat(:,Irect)], [SearchRectLon, SearchRectLat], Crit);
        N_SearchInRect(Irect) = sum(Flag1);
        N_RectInSearch(Irect) = sum(Flag2);
    end
    FlagOverlap = N_SearchInRect>0 | N_RectInSearch>0;
    
end
