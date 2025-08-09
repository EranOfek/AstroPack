function [Result] = interp2d_mex(X, Y, D, Xq, Yq, Method)
    % Fast 2D interpolation with mex functions
    % Input  : - X
    %          - Y
    %          - 2D matrix.
    %          - Matrix of interpolation X coordinates.
    %          - Matrix of interpolation Y coordinates.
    %          - Interolation method: 'single','double','uint32' (for
    %            nearest only).
    % Output : - Interpolated matrix.
    % Author : Eran Ofek (2025 Aug) 
    % Compilation:
    % mex -R2018a CXXFLAGS="\$CXXFLAGS -O3 -march=native -ffast-math -fno-exceptions -fno-rtti -fopenmp" LDFLAGS="\$LDFLAGS -fopenmp" interp2d_mex_float_all.cpp 
    % mex -R2018a CXXFLAGS="\$CXXFLAGS -O3 -march=native -ffast-math -fno-exceptions -fno-rtti -fopenmp" LDFLAGS="\$LDFLAGS -fopenmp" interp2d_mex_float_wlanczos.cpp
    % mex -R2018a CXXFLAGS="\$CXXFLAGS -O3 -march=native -ffast-math -fno-exceptions -fno-rtti -fopenmp" LDFLAGS="\$LDFLAGS -fopenmp" interp2d_mex_uint32_nearest.cpp
    % Example: X=(1:1:1726); Y=(1:1:1726).'; [MatX,MatY]=meshgrid(X,Y); D=rand(1726,1726);
    %          tic;for I=1:1:10, A=interp2(X,Y,D,MatX,MatY,'nearest');end,toc 
    %          tic;for I=1:1:10, Out=tools.interp.mex.interp2d_mex(X,Y,D,MatX,MatY,'nearest');end,toc 
    %          max(abs(A(5:end-5,5:end-5)-Out(5:end-5,5:end-5)),[],'all')
    %
    %          tic;for I=1:1:10, A=interp2(X,Y,D,MatX,MatY,'linear');end,toc                           
    %          tic;for I=1:1:10, Out=tools.interp.mex.interp2d_mex(X,Y,D,MatX,MatY,'linear');end,toc  
    %          max(abs(A(5:end-5,5:end-5)-Out(5:end-5,5:end-5)),[],'all')
    %
    %          tic;for I=1:1:10, A=interp2(X,Y,D,MatX,MatY,'cubic');end,toc                           
    %          tic;for I=1:1:10, Out=tools.interp.mex.interp2d_mex(X,Y,D,MatX,MatY,'cubic');end,toc  
    %          max(abs(A(5:end-5,5:end-5)-Out(5:end-5,5:end-5)),[],'all')
    
    if isa(D, "single") 
        X  = single(X);
        Y  = single(Y);
        Xq = single(Xq);
        Yq = single(Yq);
        if contains(Method, 'lanczos')
            Result = tools.interp.mex.interp2d_mex_float_wlanczos(X, Y, D, Xq, Yq, Method);
        else
            Result = tools.interp.mex.interp2d_mex_float_all(X, Y, D, Xq, Yq, Method);
        end
    elseif isa(D, "double")
        X  = double(X);
        Y  = double(Y);
        Xq = double(Xq);
        Yq = double(Yq);
        if contains(Method, 'lanczos')
            Result = tools.interp.mex.interp2d_mex_float_wlanczos(X, Y, D, Xq, Yq, Method);
        else
            Result = tools.interp.mex.interp2d_mex_float_all(X, Y, D, Xq, Yq, Method);
        end
    else
        Result = tools.interp.mex.interp2d_mex_uint32_nearest(X, Y, D, Xq, Yq);
    end

end
