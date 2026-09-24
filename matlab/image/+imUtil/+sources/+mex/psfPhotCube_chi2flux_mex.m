% A fast mex helper function for imUtil.sources.psfPhotCube chi^2 calculations
% Input  : See below
% output : See below
% Compilation: mex -R2018a CXXFLAGS='$CXXFLAGS -O3 -fopenmp' LDFLAGS='$LDFLAGS -fopenmp' psfPhotCube_chi2flux_mex.cpp
% Author : ChatGPT + Eran Ofek (Apr 2026)
% Example: [Chi2, Flux, Dof, FluxErr] = psfPhotCube_chi2flux_mex(Cube, Std, ShiftedPSF, DX, DY, VecXrel, VecYrel, FitRadius2);
% Notes  : This function performs the following block:
%{
        if isempty(FitRadius2)
            Flag = true(Ny, Nx, Nim);
        else
            MatX = reshape(VecXrel, 1, Nx, 1) - reshape(DX, 1, 1, []);
            MatY = reshape(VecYrel, Ny, 1, 1) - reshape(DY, 1, 1, []);
            MatR2 = MatX.^2 + MatY.^2;
            Flag  = MatR2 < FitRadius2;
        end
    
        W = 1 ./ max(Std.^2, eps(class(Std)));
    
        Num = sum(Flag .* W .* Cube .* ShiftedPSF, [1 2], 'omitnan');
        Den = sum(Flag .* W .* ShiftedPSF.^2,      [1 2], 'omitnan');
    
        Den = max(Den, eps(class(Den)));
        Flux = squeeze(Num ./ Den);
    
        Flux3 = reshape(Flux, 1, 1, []);
        Resid = Cube - ShiftedPSF .* Flux3;
    
        ResidStd = Flag .* Resid ./ Std;
        Chi2 = squeeze(sum(ResidStd.^2, [1 2], 'omitnan'));
    
        Dof = squeeze(sum(Flag, [1 2]) - 3);
        FluxErr = sqrt(1 ./ squeeze(Den));
%}