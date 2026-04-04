% A fast mex helper function for imUtil.sources.psfPhotCube 2D gradient descent
% Input  : See below
% output : See below
% Compilation: mex -R2018a CXXFLAGS='$CXXFLAGS -O3 -fopenmp' LDFLAGS='$LDFLAGS -fopenmp' psfPhotCube_step2d_mex.cpp
% Author : ChatGPT + Eran Ofek (Apr 2026)
% Example: [StepX, StepY] = psfPhotCube_step2d_mex(F0, Fxp, Fxm, Fyp, Fym, Fpp, Fpm, Fmp, Fmm, sx, sy, MaxStep);
% Notes  : This function performs the following block:
%{
        Gx  = (Fxp - Fxm) ./ (2 .* sx);
        Gy  = (Fyp - Fym) ./ (2 .* sy);
    
        Hxx = (Fxp - 2 .* F0 + Fxm) ./ (sx.^2);
        Hyy = (Fyp - 2 .* F0 + Fym) ./ (sy.^2);
        Hxy = (Fpp - Fpm - Fmp + Fmm) ./ (4 .* sx .* sy);
    
        StepX = zeros(1, Nim);
        StepY = zeros(1, Nim);
    
        for Iim = 1:Nim
            Hmat = [Hxx(Iim), Hxy(Iim); Hxy(Iim), Hyy(Iim)];
            Gvec = [Gx(Iim); Gy(Iim)];
    
            if all(isfinite(Hmat), 'all') && all(isfinite(Gvec))
                if det(Hmat) > 0 && Hxx(Iim) > 0 && Hyy(Iim) > 0
                    Step = -Hmat \ Gvec;
                else
                    Step = zeros(2,1);
                    if isfinite(Hxx(Iim)) && Hxx(Iim) ~= 0
                        Step(1) = -Gx(Iim) ./ Hxx(Iim);
                    end
                    if isfinite(Hyy(Iim)) && Hyy(Iim) ~= 0
                        Step(2) = -Gy(Iim) ./ Hyy(Iim);
                    end
                end
            else
                Step = [0; 0];
            end
    
            Step(1) = sign(Step(1)) .* min(abs(Step(1)), MaxStep);
            Step(2) = sign(Step(2)) .* min(abs(Step(2)), MaxStep);
    
            if ~isfinite(Step(1)); Step(1) = 0; end
            if ~isfinite(Step(2)); Step(2) = 0; end
    
            StepX(Iim) = Step(1);
            StepY(Iim) = Step(2);
        end
    end
%}