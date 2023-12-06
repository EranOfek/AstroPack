function K = box(SizeBox, SizeXY, PosXY)
    % Generate a box kernel
    % Input  : - Size of box [X, Y].
    %          - Size of stamp [X,Y].
    %            If empty, use size of box.
    %            Default is [].
    %          - Position of box [X, Y].
    %            If empty use size of stamp.
    %            Default is [].
    % Output : - A cube of kernels.
    % Author : Eran Ofek (Nov 2021)
    % Example: K = imUtil.kernel2.box([5 5]);
    
    arguments
        SizeBox     = [5 5];
        SizeXY      = [];
        PosXY       = [];
    end
    
    if isempty(SizeXY)
        SizeXY = SizeBox;
    end
    if isempty(PosXY)
        PosXY  =  SizeXY.*0.5;
    end
    
    if all(SizeBox==SizeXY)
        % all ones
        K = ones(SizeXY(2), SizeXY(1));
    else
        % pad with zeros
        error('Multiple kernels is not supported yet for box');
        Nk = size(SizeBox,1);
        K  = zeros(SizeXY(2), SizeXY(1), Nk);
        for Ik=1:1:Nk
        
        end
    end
            
    
    
    