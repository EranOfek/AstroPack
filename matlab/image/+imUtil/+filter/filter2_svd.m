function [Result, SUV] = filter2_svd(Array, Kernel, N, Tol, Shape)
    % Fast 2-D filter using SVD approximation.
    %   For low-rand approximation of the small kernel, this function is
    %   about 10% faster compared with filter2.
    %   For a convolution with a Gaussian kernel, 1st order will provide an
    %   accurate results.
    %   Note that the MATLAB built in filter2 function already uses SVD,
    %   but this is slighly faster.
    % Input  : - A 2-D array.
    %          - A 2-D kernel.
    %          - SVD approximation order. Default is 1.
    %          - A singular value tolerance. If empty, use the order given
    %            by the previous argument. If provided, than choose singular
    %            values above this tolerance.
    %          - Shape (see conv2). Default is 'same'.
    % Output : - A 2-D array of the convolution result.
    %          - A cell array of the {S, U, V} decomposition.
    % Author : Eran Ofek (May 2023)
    % Reference: https://dercuano.github.io/notes/svd-convolution.html#:~:text=SVD%20convolution%20can%20be%20applied,the%20performance%20of%20optical%20systems.
    %            Inspired by a code by David Young
    % Example : Kernel=imUtil.kernel2.gauss;
    %           Array = rand(1700,1700);
    %           R = imUtil.filter.filter2_svd(Array, Kernel);
    %           % compare with regular filter2
    %           R1 = filter2(Array, Kernel,'same'); sum(abs(R-R1),'all') 
    %           R2 = imUtil.filter.filter2_fft(Array, Kernel); sum(abs(R-R2),'all') 
    
    arguments
        Array
        Kernel
        N       = 1;
        Tol     = [];
        Shape   = 'same';
    end
   
    % Not contributing to speedup
    %if iscell(Kernel)
    %    S = Kernel{1};
    %    U = Kernel{2};
    %    V = Kernel{3};
    %else
    
    Kernel = rot90(Kernel,2);
    
    % Descompose Kernel
    [U, S, V] = svd(Kernel);
    S         = diag(S);
    
    
    if isempty(Tol)
        % use N
    else
        % use Tol
        N = sum(S>Tol);
    end
    
    % approximate kernel is given by:
    % S(1,1).*U(:,1)*V(:,1).'
    
    
    for I=1:1:N
        % Convolve with rows and columns seperatly
        if I==1
            %Result = conv2(conv2(Array, S(I)*U(:,I), Shape), V(:,I).', Shape);
            Result = conv2(S(I)*U(:,I), V(:,I).', Array, Shape);
        else
            Result = Result + conv2(conv2(Array, S(I)*U(:,I), Shape), V(:,I).', Shape);
        end
    end
    
    if nargout>1
        SUV = {S, U, V};
    end
    
end