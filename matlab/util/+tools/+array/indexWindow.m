function [WindowLeftIndex, WindowRightIndex, ArrayLeftIndex, ArrayRightIndex] = indexWindow(ArrayLength, WindowCenter, WindowSize)
    % Return the in bound indices of a window in an array of some length.
    % Input  : - Array length.
    %          - Winddow center.
    %          - Window size. Must be smaller than array length,
    %            and must be odd size.
    % Output : - In bound window left index.
    %          - In bound window right index.
    %          - Corresponding array left index.
    %          - Corresponding array right index.
    % Author : Eran Ofek (Jan 2022)
    % [A,B,C,D] = tools.array.indexWindow(1700,[1;500; 1700],15)
    
    % check that Array is larger than window
    if ArrayLength<WindowSize
        error('Works only for ArrayLength>WindowSize');
    end
    
    Ncenter  = numel(WindowCenter);
    HalfSize = (WindowSize - 1).*0.5;
    
    ArrayLeftIndex   = WindowCenter - HalfSize;
    ArrayRightIndex  = WindowCenter + HalfSize;
    
    WindowLeftIndex  = ones(Ncenter,1);
    WindowRightIndex = WindowSize + zeros(Ncenter,1);
    
    % fix the indices of the window to match the truncation due to out of bound
    WindowLeftIndex(ArrayLeftIndex<1) = abs(ArrayLeftIndex(ArrayLeftIndex<1  )) + 2;
    WindowEnd = WindowRightIndex - ArrayRightIndex + ArrayLength;
    WindowRightIndex(ArrayRightIndex>ArrayLength) = WindowEnd(ArrayRightIndex>ArrayLength);
    
    % fix the indices of the window in the array such that they will be inbound
    ArrayLeftIndex(ArrayLeftIndex<1)             = 1;
    ArrayRightIndex(ArrayRightIndex>ArrayLength) = ArrayLength;
    
end