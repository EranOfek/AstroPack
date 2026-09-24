% (MEX) Lomb-Scargle periodogram
%   NOT TESTED CAREFULLY - MAYBE A PROBLEM WITH NORMALIZATION
%   Assumes Y is already mean-subtracted.
%   Normalization:
%   P(w) = 1/(2*Sig2) * [ (Σ Y*cos(w*(T-τ)))^2 / Σ cos^2(w*(T-τ))  +  (Σ Y*sin(w*(T-τ)))^2 / Σ sin^2(w*(T-τ)) ]
%
% Input : - T   : Nx1 time samples
%         - Y   : Nx1 values (mean-subtracted)
%         - F   : Mx1 frequencies (Hz). Internally uses W = 2πF (rad/s)
%
% Output: - Mx1 Lomb–Scargle power at each frequency
%         - Mx1 phase offsets τ(F) used to orthogonalize sin/cos
%         - A amplitude LS coefficients for cos/sin in y ≈ A*cos(w*(t-τ)) + B*sin(w*(t-τ))
%         - B amplitude LS coefficients for cos/sin in y ≈ A*cos(w*(t-τ)) + B*sin(w*(t-τ))
%         - R - total amplitude sqrt(A^2+B^2)
%         - Phi : phase where y ≈ R*cos(w*(t-τ) - Phi); R = sqrt(A^2+B^2), Phi = atan2(B,A)
% Authors: ChatGPT + Eran Ofek (Oct 2025)
% Compilation: mex -O CXXFLAGS="$CXXFLAGS -std=c++17 -march=native -mavx2 -mfma -fopenmp" LDFLAGS="$LDFLAGS -fopenmp" periodScargle_mex.cpp
% Example: [P1,Tau1,A1,B1,R1,Phi1]=timeSeries.period.mex.periodScargle_mex(T,Y,Freq);