function [Mid] = mid(V)
    % Return the midpoint between successive data points in a vector.
    % Input  : - A vector.
    % Output : - Vector of mid points.
    % Author : Eran Ofek (2025 Apr) 
    % Example: M=tools.hist.mid((1:1:5))

    Mid = (V(1:end-1) + V(2:end)).*0.5;

end
