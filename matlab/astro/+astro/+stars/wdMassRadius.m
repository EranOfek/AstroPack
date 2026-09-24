function [Result] = wdMassRadius(Type, Val, Mu_e, Method)
    % Approximate WD mass-radius relation
    % Input  : - Specify the input: 'M' (mass) or 'R' (radius).
    %            Default is M.
    %          - Star mass (in solar mass), or radius in (solar radii).
    %          - Mu_e. Default is 2.
    %          - Formulae to use:
    %            'simple' - simple analytic relation (default)
    % Output : - Mass (solar mass) or radius (solar radius).
    % Author : Eran Ofek (2025 Sep) 
    % Example: M=[0.1;0.5;1;1.4];
    %          R=astro.stars.wdMassRadius('M',M);
    %          M1=astro.stars.wdMassRadius('R',R);


    arguments
        Type
        Val
        Mu_e     = 2;
        Method   = 'simple';
    end

    switch lower(Method)
        case 'simple'
            Mc = 1.454;
            switch lower(Type)
                case 'm'
                    M_ch = Val./Mc;
                    %Result = 0.01125.*sqrt(M_ch.^(-2./3) - M_ch.^(2./3));  % radius in solar radius
                    Result = (0.0225./Mu_e) .*sqrt(1 - (M_ch).^(4./3)) .* (M_ch).^(-1./3);
                case 'r'

                    %M_ch = (0.05:0.05:1).';
                    %R = (0.0225./Mu_e) .*sqrt(1 - (M_ch).^(4./3)) .* (M_ch).^(-1./3);
                    %Result = 1.454.*interp1(R, M_ch, Val, 'cubic');

                    %syms M_ch Mu_e R
                    %vectorize(simplify(solve(R==(0.0225./Mu_e) .*sqrt(1 - (M_ch).^(4./3)) .* (M_ch).^(-1./3), M_ch)))

                    R = Val;
                    Result = Mc.*real(((6400000000.*Mu_e.^4.*R.^4 + 6561).^(1./2) - 80000.*Mu_e.^2.*R.^2).^(3./2)./729);


                otherwise
                    error('Unknown Type option');
            end
        otherwise
            error('Unknown Method option');
    end

end
