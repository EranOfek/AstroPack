function [Y, PosX, PosY] = subImageFun(X, S, Fun)
    % subImageFun  Apply a reducer over S-sized blocks of X (N-D), return block grid and XY positions.
    %   Works with edge blocks (last block in each dim may be smaller).
    %   [Y, PosX, PosY] = subImageFun(X, S, Fun)
    % Input  : - numeric/logical array (N-D)
    %          - Block size per dimension [I,J] i.e., [Y,X].
    %            Scalar expands to all dims; if numel(S)<ndims(X),
    %            it is padded with ones.
    %          - Function handle that maps a vector -> scalar (e.g., @sum, @mean, @median).
    %            The function must return a scalar.
    % Output : - Per-block scalar output, size ceil(size(X)./S)
    %          - This is the blocks centers X position in the input image grid.
    %          - This is the blocks centers Y position in the input image grid.
    % Author : ChatGPT + Eran Ofek (Oct 2025)
    % Example: [Y,PoxX, PosY]=imUtil.cut.subImageFun(Image, [256 256], @median);
    
    arguments
        X  %{mustBeNumericOrLogical, mustBeNonempty}
        S  %{mustBeNumeric, mustBeVector, mustBePositive, mustBeInteger}
        Fun (1,1) function_handle
    end
    
    % Normalize S to match ndims(X)
    szX = size(X);
    nd  = ndims(X);
    if isscalar(S)
        S = repmat(S,1,nd);
    end
    if numel(S) < nd
        S(end+1:nd) = 1;
    end
    if numel(S) > nd
        % allow trailing singleton expansion of X
        szX(end+1:numel(S)) = 1;
        nd = numel(S);
    end
    
    % Output block grid size
    sizeY   = ceil(szX./S);
    nBlocks = prod(sizeY);
    
    % Choose a compact integer class for block IDs
    if nBlocks <= intmax('uint32')
        castI = @uint32;
    elseif nBlocks <= intmax('uint64')
        castI = @uint64;
    else
        error('Too many blocks to index.');
    end
    
    % Build per-dimension maps from element indices -> block index
    I_block = cell(1,nd);
    for k = 1:nd
        % e.g., for size 9 and S=3: [1 1 1 2 2 2 3 3 3]
        I_block{k} = castI( ceil((1:szX(k)) / S(k)) );
    end
    
    % Map each element to its linear block ID using an ID "cube" (size = sizeY)
    IDcube = castI( reshape(1:nBlocks, sizeY) );
    linID  = IDcube(I_block{:});   % size(linID) == size(X)
    
    % Reduce with accumarray (fun must return scalar)
    Y = accumarray(linID(:), X(:), [nBlocks 1], Fun);
    Y = reshape(Y, sizeY);
    
    % Efficient PosX / PosY (2-D block grid indices)
    % Handle 1-D gracefully by treating missing dimension as 1
    if isscalar(sizeY)
        sizeY = [sizeY 1];
    end

    % start-row and start-column (I/Y first, J/X second) in original X
    row0 = 1 + (0:sizeY(1)-1)*S(1);   % 1, 1+S(1), 1+2*S(1), ...
    col0 = 1 + (0:sizeY(2)-1)*S(2);   % 1, 1+S(2), 1+2*S(2), ...
    %[PosY, PosX] = ndgrid(row0, col0);

    row1 = min(row0 + S(1) - 1, szX(1));                 % block end rows (clamped at image edge)
    col1 = min(col0 + S(2) - 1, szX(2));                 % block end cols
    rowC = floor((row0 + row1)/2);                       % center rows
    colC = floor((col0 + col1)/2);                       % center cols
    [PosY, PosX] = ndgrid(rowC, colC);

    %[PosY, PosX] = ndgrid(1:sizeY(1), 1:sizeY(2));
end
