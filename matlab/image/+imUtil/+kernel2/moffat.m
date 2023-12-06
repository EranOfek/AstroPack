function K=moffat(AlphaBeta,SizeXY,PosXY)    
    % create a matrix or a cube of 2D normalized Moffat function 
    % Package: +imUtil.kernel2
    % Input  : - (AlphaBeta) A two column matrix of [Alpha, Beta]
    %            parameters of the Moffat distribution.
    %            The number of rows will dictates the number of kernels
    %            generated.
    %            Default is [0.5 2].
    %          - Stamp size [X,Y]. Default is [15 15].
    %          - [X,Y] Position of the Gaussian center in the stamp.
    %            Default is the ceil(stamp_size/2).
    % Output : - A matrix or a cube with the 2D Moffat which sum is
    %            normalized to 1.
    %            If a cube, the third dimension corresponds to the template
    %            index.
    %      By: Eran O. Ofek                         Apr 2020
    % Example: imUtil.kernel2.moffat
    %          imUtil.kernel2.gauss([1;2;3;4]); % a template bank of Gaussians.

    arguments
        AlphaBeta = [0.5 2];
        SizeXY    = [15 15];
        PosXY     = [];
    end


    if isempty(PosXY)
        PosXY = ceil(SizeXY.*0.5);
    end

    [NrowSigma, NcolSigma] = size(AlphaBeta);
    if NcolSigma~=2
        error('1st input argument must have two columns');
    end    

    Alpha = reshape(AlphaBeta(:,1),1,1,NrowSigma);
    Beta  = reshape(AlphaBeta(:,2),1,1,NrowSigma);
    
    
    [MatX,MatY] = meshgrid( (1:1:SizeXY(1))-PosXY(1), (1:1:SizeXY(2))-PosXY(2) );
    
    K = (Beta - 1)./(pi.*Alpha.^2) .* (1 - (MatX.^2 + MatY.^2)./(Alpha.^2)).^(-Beta);
    
    %K = zeros(SizeXY(2),SizeXY(1),NrowSigma);
   
    % doing direct normalization
    % otherwise bug when sigma is small
    K = K./sum(K,[1 2]);
    
end