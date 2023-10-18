function Chi2cuts=arrayAllCuts(Chi2, Args)
    % Cut a cube to all possible 2D cuts (1 by 2, 1 by 3, ..., 2 by 3, etc.
    % Input  : - A cube containing e.g., chi^2.
    %          * ...,key,val,...
    %            'AxesCenter' - A vector of axes center indices.
    %                   If empty, use central index in each dimension.
    %                   Default is [].
    % Output : - A 2D cell array in which each element contains a 2D
    %            matrix. Each cell element corresponds to a 2-D cut of the
    %            cube. For example, 2,3 corresponds to dim 2 vs dim 3.
    % Author : Eran Ofek (Oct 2023)
    % Example: Cuts=tools.array.arrayAllCuts(rand(13,11,9,11,9))

    arguments
        Chi2
        Args.AxesCenter      = [];
    end

    Ndim     = ndims(Chi2);
    SizeChi2 = size(Chi2);

    if isempty(Args.AxesCenter)
        Args.AxesCenter = (SizeChi2 + 1).*0.5;
    end

    VecDim = (1:1:Ndim);
    
    for Idim1=1:1:Ndim
        for Idim2=1:1:Ndim
            if Idim2>Idim1
                Flag = ~(VecDim==Idim1 | VecDim==Idim2);
                VecDimClean = VecDim(Flag);
                PermutedChi2 = permute(Chi2, [Idim1 Idim2 VecDimClean]);
    
                DimC = num2cell(Args.AxesCenter(VecDimClean));
                Chi2cuts{Idim1, Idim2} = PermutedChi2(:,:,DimC{:});

            end
        end
    end
end