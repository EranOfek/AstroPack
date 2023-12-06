function SubIm=trim2d(Image,CCDSEC)
    % Trim a 2D matrix using CCDSEC (simple version of trim)
    % Input  : - A 2D matrix.
    %          - CCDSEC [Xmin Xmax Ymin Ymax]
    % Output : - Sub matrix
    % Author : Eran Ofek (Jun 2023)
    % Example: D=rand(1700,1700);
    %          B=imUtil.cut.trim2d(D,[102 120 202 220]);

    
    SubIm = Image(CCDSEC(3):CCDSEC(4), CCDSEC(1):CCDSEC(2));

    % This is slower:
    %IndI = (CCDSEC(3):CCDSEC(4));
    %IndJ = (CCDSEC(1):CCDSEC(2));
    %Ind = tools.array.sub2ind_fast(size(Image), IndI.', IndJ);
    %Ind = tools.array.sub2ind_fast(size(Image), (CCDSEC(3):CCDSEC(4)).', (CCDSEC(1):CCDSEC(2)));
    
    %SubIm = Image(Ind);

end