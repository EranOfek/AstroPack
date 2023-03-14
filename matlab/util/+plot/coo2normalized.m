function [NX,NY]=coo2normalized(X,Y)
    % Convert plot coordinates to normalized coordinates (between 0 to 1)
    % Input  : - X
    %          - Y
    % Output : - Normalized X.
    %          - Normalized Y.
    % Author : Eran Ofek (Mar 2023)
    % Example: [NX,NY]=plot.coo2normalized(1.5,-2);

    H = gca;
  
    Pos = H.Position;
    Xn  = [Pos(1),Pos(1)+Pos(3)];
    Yn  = [Pos(2),Pos(2)+Pos(4)];
    NX  = interp1(H.XLim, Xn, X, 'linear');
    NY  = interp1(H.YLim, Yn, Y, 'linear');

end
