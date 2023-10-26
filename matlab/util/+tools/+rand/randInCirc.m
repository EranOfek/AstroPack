function [X,Y,R,Theta]=randInCirc(Radius,varargin)
    % Generate random numbers uniformly within a circle.
    % Input  : - Circle radius
    %          * Size of output
    % Output : - Random numbers
    % Author : Eran Ofek (Oct 2023)
    % Example: [X,Y,R,Theta]=tools.rand.randInCirc(2,10,2)
    
    R     = Radius.*sqrt(rand(varargin{:}));
    Theta = 2.*pi.*rand(varargin{:});
    
    X = R.*cos(Theta);
    Y = R.*sin(Theta);
       
end

