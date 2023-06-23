function Result=cosbellOneSided(In, Out, Length)
    % One sided (e.g., radial) cosine vell function
    % Input  : - Position below function is 1.
    %          - Position below function is 0.
    %          - Length. Actual length will be this + 1, as the output
    %            start at 0.
    % Output : - A two column marix of [pos, fun].
    % Author : Eran Ofek (Jun 2023)
    % Example: F=tools.math.fun.cosbellOneSided(5,10,15)
    
    Vec = (0:1:Length);
    F = ones(Length,1);
    F(Vec>Out)  = 0;
    SpanQ = Out - In;
    Ind = find(Vec>In & Vec<=Out);
    F(Ind) = 0.5.*(1+cos( pi.*(Vec(Ind) - In)./SpanQ ));
    
    Result = [Vec(:), F(:)];
    
end
