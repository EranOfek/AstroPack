function B = blockCirculant(blocks)
    % Generate a right block-circulant matrix from blocks.
    % Input  : - A cell array of blocks: {C0, C1, ..., C_{n-1}}
    % Output : - A right block-circulant matrix
    % Author : Claude + Eran Ofek (Jul 2026)
    % Example: tools.math.matrix.blockCirculant({1, 2, 3})
    n = numel(blocks);
    row = blocks([1, n:-1:2]);   % first block-row
    B = [];
    for k = 1:n
        B = [B; cat(2, row{:})]; %#ok<AGROW>
        row = circshift(row, 1); % cyclic shift of blocks
    end
end

