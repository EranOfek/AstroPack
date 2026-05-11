function [Y] = fermiFun(X, Mu, Width, Args)
    % Generate Fermi-Dirac function
    %   Function of the form: Args.Norm./(1 + exp(Args.Dir.*(X-Mu))./Width);
    % Input  : - X
    %          - Mu
    %          - Width
    %          * ...,key,val,... 
    %            'Norm' - Normalization. Default is 1.
    %            'Dir' - Direction: 1: decreasing, -1: rising.
    %                   Default is -1.
    % Output : - Y(X)
    % Author : Eran Ofek (2026 May) 
    % Example: tools.math.fun.fermiFun((1:1:20),10, 2) 

    arguments
        X
        Mu
        Width
        Args.Norm   = 1;
        Args.Dir    = -1;
    end
  
    Y = Args.Norm./(1 + exp(Args.Dir.*(X-Mu))./Width);

end
