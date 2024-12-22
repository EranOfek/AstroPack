function [Colors,Markers] = colorMarkerOrder(LineTypes, Colors)
    % Generate a set of colors and marker types for plotting
    % Input  : - String array of markers type or line style.
    %            Default is ["-", "--", ":", "-."]
    %          - A 3 column matrix of colors. Default is colororder.
    %          * ...,key,val,... 
    % Output : - A 3 column matrix of colors.
    %          - A  string array of marker types.
    % Author : Eran Ofek (2024 Dec) 
    % Example: [Colors,Markers] = plot.colorMarkerOrder;

    arguments
        LineTypes = ["-", "--", ":", "-."]
        Colors    = colororder;
    end

    Ncol  = size(Colors,1);
    Nline = numel(LineTypes);

    Colors  = repmat(Colors, Nline, 1);
    Markers = repmat(LineTypes(:).', Ncol, 1);
    Markers = Markers(:);

end
