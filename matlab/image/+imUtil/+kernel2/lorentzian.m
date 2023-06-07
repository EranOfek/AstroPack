function K=lorentzian(Gamma,SizeXY,PosXY)    
    % create a matrix or a cube of 2D normalized Moffat function 
    % Package: +imUtil.kernel2
    % Input  : - (Gamma) A vector of the Lorentzian Gamma parameter.
    %            Will generate a stamp for each element in the vector.
    %            Default is 1.
    %          - Stamp size [X,Y]. Default is [15 15].
    %          - [X,Y] Position of the Gaussian center in the stamp.
    %            Default is the ceil(stamp_size/2).
    % Output : - A matrix or a cube with the 2D Lorentzian which sum is
    %            normalized to 1.
    %            If a cube, the third dimension corresponds to the template
    %            index.
    % Author : Eran Ofek (Jun 2023)
    % Example: imUtil.kernel2.lorentzian
    %          imUtil.kernel2.lorentzian([1;2;3;4]); % a template bank of Gaussians.

    arguments
        Gamma     = [1];
        SizeXY    = [15 15];
        PosXY     = [];
    end


    if isempty(PosXY)
        PosXY = ceil(SizeXY.*0.5);
    end

    NrowSigma = numel(Gamma); 

    Gamma = reshape(Gamma(:),1,1,NrowSigma);    
    
    [MatX,MatY] = meshgrid( (1:1:SizeXY(1))-PosXY(1), (1:1:SizeXY(2))-PosXY(2) );
    
    K = (Gamma./(MatX.^2 + MatY.^2 + Gamma.^2))./pi;
    
    %K = zeros(SizeXY(2),SizeXY(1),NrowSigma);
   
    % doing direct normalization
    % otherwise bug when sigma is small
    K = K./sum(K,[1 2]);
    
end