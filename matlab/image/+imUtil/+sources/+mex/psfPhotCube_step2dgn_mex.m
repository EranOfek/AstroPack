% A fast mex helper function for imUtil.sources.psfPhotCube 2D gauss-newton solver 
% Input  : See below
% output : See below
% Compilation: mex -R2018a CXXFLAGS='$CXXFLAGS -O3 -fopenmp' LDFLAGS='$LDFLAGS -fopenmp' psfPhotCube_step2dgn_mex.cpp
% Author : ChatGPT + Eran Ofek (Apr 2026)
% Example: 
%{
            [StepX, StepY] = psfPhotCube_step2dgn_mex( ...
                                Cube, Std, ShiftedPSF, PSF_Xp, PSF_Xm, PSF_Yp, PSF_Ym, ...
                                SmallStep(:), SmallStep(:), Flux(:), DX(:), DY(:), VecXrel(:), VecYrel(:), ...
                                FitRadius2, MaxStep);
%}
% Notes  : This function performs the following block:
%{
        dPdx = (PSF_Xp - PSF_Xm) ./ (2 .* SX);
        dPdy = (PSF_Yp - PSF_Ym) ./ (2 .* SY);
    
        [Ny, Nx, ~] = size(Cube);
        if isempty(FitRadius2)
            Flag = true(Ny, Nx, Nim);
        else
            MatX = reshape(VecXrel, 1, Nx, 1) - reshape(DX, 1, 1, []);
            MatY = reshape(VecYrel, Ny, 1, 1) - reshape(DY, 1, 1, []);
            MatR2 = MatX.^2 + MatY.^2;
            Flag = MatR2 < FitRadius2;
        end
    
        W = 1 ./ max(Std.^2, eps(class(Std)));
        if size(W,1)==1 && size(W,2)==1
            W = repmat(W, size(Cube,1), size(Cube,2), 1);
        end
        
        Flux3 = reshape(Flux, 1, 1, []);
        Resid = Cube - ShiftedPSF .* Flux3;
        
        Jx = Flux3 .* dPdx;
        Jy = Flux3 .* dPdy;
    
       
    
        StepX = zeros(1, Nim, 'like', Flux);
        StepY = zeros(1, Nim, 'like', Flux);
    
        for Iim = 1:Nim
            Wi  = W(:,:,Iim);
            Fi  = Flag(:,:,Iim);
    
            Ri  = Resid(:,:,Iim);
            Jxi = Jx(:,:,Iim);
            Jyi = Jy(:,:,Iim);
    
            Wi  = Wi(Fi);
            Ri  = Ri(Fi);
            Jxi = Jxi(Fi);
            Jyi = Jyi(Fi);
    
            Good = isfinite(Wi) & isfinite(Ri) & isfinite(Jxi) & isfinite(Jyi);
            Wi   = Wi(Good);
            Ri   = Ri(Good);
            Jxi  = Jxi(Good);
            Jyi  = Jyi(Good);
    
            if isempty(Wi)
                continue;
            end
    
            A11 = sum(Wi .* Jxi .* Jxi);
            A12 = sum(Wi .* Jxi .* Jyi);
            A22 = sum(Wi .* Jyi .* Jyi);
    
            B1  = sum(Wi .* Jxi .* Ri);
            B2  = sum(Wi .* Jyi .* Ri);
    
            A = [A11, A12; A12, A22];
            B = [B1; B2];
    
            if all(isfinite(A), 'all') && all(isfinite(B)) && rcond(A) > 1e-10
                Step = A \ B;
            else
                Step = [0; 0];
                if isfinite(A11) && A11 > 0
                    Step(1) = B1 ./ A11;
                end
                if isfinite(A22) && A22 > 0
                    Step(2) = B2 ./ A22;
                end
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