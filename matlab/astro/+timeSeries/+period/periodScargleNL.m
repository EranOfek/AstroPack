function [P, Tau, A, B, R, Phi] = periodScargleNL(T, Y, F)
    % Vectorized Lomb–Scargle periodogram (no loops).
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
    % Example: [P1,Tau1,A1,B1,R1,Phi1]=timeSeries.period.periodScargle(T,Y,Freq);
    
    T   = T(:);
    Y   = Y(:);
    F   = F(:);
    
    % Variance (since Y is mean-subtracted)
    Sig2 = mean(Y.^2);
    if Sig2 == 0
        P   = zeros(size(F));
        Tau = zeros(size(F));
        if nargout > 2
            Z = zeros(size(F));
            A = Z; B = Z; R = Z; Phi = Z;
        end
        return
    end
    
    W   = 2*pi*F;         % Mx1
    Wt  = W.*T.';         % MxN (implicit expansion)
    
    % τ via tan(2W*τ) = Σ sin(2W*T) / Σ cos(2W*T)
    S2  = sum(sin(2*Wt), 2);     % Mx1
    C2  = sum(cos(2*Wt), 2);     % Mx1
    Tau = 0.5 * atan2(S2, C2) ./ max(W, eps(class(W)));
    
    % Phases shifted by τ
    PhiShift = Wt - W.*Tau;      % MxN
    C        = cos(PhiShift);
    S        = sin(PhiShift);
    
    % Numerators (Σ Y*cos(...), Σ Y*sin(...))
    Yc  = C * Y;                 % (MxN)*(Nx1) -> Mx1
    Ys  = S * Y;                 % Mx1
    
    % Denominators (Σ cos^2(...), Σ sin^2(...))
    Cc2 = sum(C.^2, 2);          % Mx1
    Ss2 = sum(S.^2, 2);          % Mx1
    Cc2s = max(Cc2, eps(class(Cc2)));
    Ss2s = max(Ss2, eps(class(Ss2)));
    
    % Power
    P   = (Yc.^2 ./ Cc2s + Ys.^2 ./ Ss2s) / (2*Sig2);
    
    % Optional LS coefficients and amplitude/phase
    if nargout > 2
        A   = Yc ./ Cc2s;                % cos coefficient
        B   = Ys ./ Ss2s;                % sin coefficient
        R   = hypot(A, B);               % amplitude
        Phi = atan2(B, A);               % phase in y ≈ R*cos(w*(t-τ) - Phi)
    end

end
