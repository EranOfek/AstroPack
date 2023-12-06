function [TrimedData, CCDSEC]=trim(Data, CCDSEC, Type, FillVal)
% Trim an image or a cube using CCDSEC coordinates.
% Pacakge: imUtilimage
% Description: Trim an image or a cube using CCDSEC coordinates.
% Input  : - An image or a cube in which the 3rd dimension is the image
%            index. If empty, then will return only the CCDSEC vector.
%          - CCDSEC vector.
%            Either: [Xmin, Xmax, Ymin, Ymax] if type = 'ccdsec'
%                    [Xcenter, Ycenter, Xhalfsize, Yhalfsize] if
%                    type='center'
%                 or [Xhalfsize, Yhalfsize] around central pixel if type =
%                 'center'.
%          - Type. Either 'ccdsec', or 'center'.
%            Default is 'ccdsec'.
%          - Fill value. In case that the trim section is near the edge,
%            this is the fill value to insert into the edge, such that the
%            trim section will have the requires size. If empty, then
%            return only the overlap region. Default is [].
% Output : - A trimmed image or cube.
%          - A CCDSEC vector.
% License: GNU general public license version 3
% Tested : Matlab R2015b
%     By : Eran O. Ofek                    Sep 2016
%    URL : http://weizmann.ac.il/home/eofek/matlab/
% Example: D=rand(100,100);
%          imUtil.cut.trim(D,[2 10 2 10])
% Reliable: 2
%--------------------------------------------------------------------------

arguments
    Data
    CCDSEC
    Type      = 'ccdsec';
    FillVal   = [];
end

Size = size(Data);

switch lower(Type)
    case 'ccdsec'
        % [Xmin, Xmax, Ymin, Ymax]
        X1 = CCDSEC(1);
        X2 = CCDSEC(2);
        Y1 = CCDSEC(3);
        Y2 = CCDSEC(4);
    case 'center'
        
        if numel(CCDSEC)==4
            % [Xcenter, Ycenter, Xhalfsize, Yhalfsize]
            X1 = CCDSEC(1) - CCDSEC(3);
            X2 = CCDSEC(1) + CCDSEC(3);
            Y1 = CCDSEC(2) - CCDSEC(4);
            Y2 = CCDSEC(2) + CCDSEC(4);
        elseif numel(CCDSEC)==2
            % [Xhalfsize, Yhalfsize] around centeral pixel
            Xc = floor(Size(2).*0.5);
            Yc = floor(Size(1).*0.5);
            X1 = Xc - CCDSEC(1);
            X2 = Xc + CCDSEC(1);
            Y1 = Yc - CCDSEC(2);
            Y2 = Yc + CCDSEC(2);
        else
            error('Uknown CCDSEC format - CCDSEC must contain 2 or 4 elements');
        end
    otherwise
        error('Unknown Type option');
end

if isempty(Data)
    TrimedData = [];
else
    if isempty(FillVal)
        % remove boundries from trimed image
        if Y1<1
            Y1 = 1;
        end
        if X1<1
            X1 = 1;
        end
        if Y2>Size(1)
            Y2 = Size(1);
        end
        if X2>Size(2)
            X2 = Size(2);
        end
    else
        error('FillVal different than [] is not yet supported');
    end
        
    TrimedData = Data(Y1:Y2,X1:X2,:);
end

CCDSEC = [X1, X2, Y1, Y2];
        