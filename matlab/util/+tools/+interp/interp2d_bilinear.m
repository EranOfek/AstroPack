function [Result] = interp2d_bilinear(V, MatX, MatY)
    % 2D bi-linear interpolation - fast MEX for single and double inputs
    %       (x2.6 times faster than interp2).
    % Input  : - A 2D array which coordinates are the pixel indices.
    %          - A 2D matrix of X interpolated position.
    %          - A 2D matrix of Y interpolated position.
    % Output : - Interpolated matrix
    % Author : Eran Ofek (2025 Jan) 
    % Compilation: mex interp2d_bilinear_single.cpp CXXFLAGS="\$CXXFLAGS -O3 -march=native -fopenmp" LDFLAGS="\$LDFLAGS -fopenmp"
    % Example:  V=single(rand(1700,1700));                                                             
    %           [MatX,MatY]=meshgrid((1:1700),(1:1700)); MatX1=single(MatX+0.1); MatY1=single(MatY+0.2);
    %           tic;for I=1:1:100, Vq=tools.interp.interp2d_bilinear(V,MatX1,MatY1);end,toc
    %           tic;for I=1:1:100, Vq1=interp2(V,MatX1,MatY1);end,toc                          
    %           max(abs(Vq-Vq1),[],'all')

    switch class(V)
        case 'single'
            Result = tools.interp.mex.interp2d_bilinear_single(V, MatX, MatY);
        case 'double'
            Result = tools.interp.mex.interp2d_bilinear_double(V, MatX, MatY);
        otherwise
            error('Unsupported data type');
    end
end
