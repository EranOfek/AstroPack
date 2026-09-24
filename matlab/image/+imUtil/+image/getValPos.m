function [Val] = getValPos(Image, X, Y)
    % Get value of image at list of pixel positions.
    % Input  : - 2D Image.
    %          - X positions in which to extract value.
    %          - Y positions in which to extract value.
    % Output : - Value at X,Y pixel positions (always a column vector).
    % Author : Eran Ofek (2025 Apr) 
    % Example: V=imUtil.image.getValPos(rand(10,10),[1 2;3 4],[0 11;2,3])

    RX = round(X(:));
    RY = round(Y(:));
    [NY, NX] = size(Image);
    FlagIn = RX>0 & RY>0 & RX<=NX & RY<=NY;
    Val    = nan(size(RX));
    Ind = sub2ind([NY NX], RY(FlagIn), RX(FlagIn));
    Val(FlagIn) = Image(Ind);

end
