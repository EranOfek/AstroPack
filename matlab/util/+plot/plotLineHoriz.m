function [varargout] = plotLineHoriz(Y, varargin)
    % Plot horizontal line over current axis
    % Input  : - Y position.
    %          * ...,key,val,... of arguments to pass to the plot function.
    % Output : - Handle.
    % Author : Eran Ofek (2024 Dec) 
    % Example: plot.plotLineHoriz(0.2)

    H = gca;
    [varargout{1}] = plot(H.XLim, repmat(Y,1,2), varargin{:});

end
