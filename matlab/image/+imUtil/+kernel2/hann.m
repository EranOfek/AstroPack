function K=hann(Sigma,SizeXY,PosXY, Norm)
    % create a matrix or a cube of 2D hann function (peak normalized to 1).
    % Package: +imUtil.kernel2
    % Input  : - A vector which length corresponds to the number of images
    %            in the cube. The actual value is not important.
    %            Default is 1.
    %          - Stamp size [X,Y]. Default is [15 15].
    %          - [X,Y] Position of the Gaussian center in the stamp.
    %            Default is the ceil(stamp_size/2).
    %          - Normalize the sum of the kernel by this value.
    %            If empty, do not normalize.
    %            Default is [].
    % Output : - A matrix or a cube with the 2D Hann functions which peak is
    %            normalized to 1.
    %            If a cube, the third dimension corresponds to the template
    %            index.
    % Author : Eran Ofek (Jun 2023)
    % Example: K=imUtil.kernel2.hann

    arguments
        Sigma   = 1;
        SizeXY  = [15 15];
        PosXY   = [];
        Norm    = []; 
    end

    if isempty(PosXY)
        PosXY = ceil(SizeXY.*0.5);
    end

    L     = SizeXY(1);
    VecX  = (1:1:SizeXY(1)) - PosXY(1);
    VecY  = (1:1:SizeXY(2)) - PosXY(2);
    
    MatR = sqrt(VecX.^2 + VecY(:).^2);
    
    K = cos(pi.*MatR./L).^2 ./L;
    K(MatR>(L.*0.5)) = 0;
    
    if ~isempty(Norm)
        K = K./sum(K,'all');
    end
    
    Ncube = numel(Sigma);
    if Ncube>1
        K = repmat(K, 1, 1, Ncube);
    end
    
end