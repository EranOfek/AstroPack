function Margin=arrayMarginalization(Cube, Args)
    % For each dimension in an array, marginalize over all other dimensions.
    %   Also apply function prior and following the marginalization.
    % Input  : - A N-D array.
    %          * ...,key,val,...
    %            'Fun' - Fun to apply on the cube prior to marginalization.
    %                   Default is @(X) exp(X)
    %            'InvFun' - Fun to apply on the marginalized vector.
    %                   default is @(X) log(X)
    % Output : - A cell array of marganlized (column) vectors for each dimension in
    %            the input array.
    % Author : Eran Ofek (Oct 2023)
    % Example: M=tools.array.arrayMarginalization(rand(11,9,11,17))
    
    arguments
        Cube
        Args.Fun             = @(X) exp(X);
        Args.InvFun          = @(X) log(X);
    end

    Ndim     = ndims(Cube);
    %SizeChi2 = size(Cube);

    VecDim = (1:1:Ndim);
    
    FC           = Args.Fun(Cube);
    for Idim=1:1:Ndim
        Flag        = VecDim~=Idim;
        VecDimClean = VecDim(Flag);
        
        Margin{Idim} = squeeze(Args.InvFun(sum(FC, VecDimClean)));
        Margin{Idim} = Margin{Idim}(:);
    end
    
end
