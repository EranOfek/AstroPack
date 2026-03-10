function [Y] = funPowerLawExpCutoff(Par,X)
    % Generate a power-law function with exponential cutoff
    %   The function is of the form:
    %       Y = Npeak.*X.^Slope;
    %       F    = X>Xpeak;
    %       Y(F) = exp((Xpeak-X(F))./Xpeak).*Npeak.*Xpeak.^Slope;
    %
    % Input  : - Function parameters [Xpeak, Npeak, SlopePL]
    %          - Values at which to evaluate the function.
    % Output : - Function values.
    % Author : Eran Ofek (2026 Mar) 
    % Example: Y=tools.math.fun.funPowerLawExpCutoff([1e38, 1, 1],logspace(35,40,100))

    arguments
        Par   % [Xpeak, Npeak, SlopePL]
        X
    end

    Y    = Par(2).*X.^Par(3);
    F    = X>Par(1);
    Y(F) = exp((Par(1)-X(F))./Par(1)).*Par(2).*Par(1).^Par(3);

end
