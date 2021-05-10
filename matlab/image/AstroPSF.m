% BaseImage handle class - all images inherits from this class
% Package: @BaseImage
% Description: 
% Tested : Matlab R2018a
% Author : Eran O. Ofek (Mar 2021)
% Dependencies: @convert, @celestial
% Example : 
% Reliable: 2
%--------------------------------------------------------------------------

classdef AstroPSF < Component
    % ImageComponent contains:
    % Data
    % Scale
    % ScaleMethod
    
    
    properties (Dependent) % Access image data directly    
        Data
        Var
    end
    
    properties (SetAccess = public)
        FunPSF                  % e.g., Map = Fun(Data, X,Y, Color, Flux)
        FunVar
        DataPSF                 % The fun parameters, or an image
        DataVar
        ArgNames                % {'X','Y','Color','Flux'}
        Type                    % 'image' | 'fun'
    end
    
    methods % Constructor
       
        function Obj = AstroPSF
            
            
        end

    end
 

 
    methods % Setters/Getters
       
    end
    
    methods (Static)  % static methods
        function multiGaussianPSF(DataPSF, X, Y, Color, Flux)
            %
            % I = G1(x,y, sigmaX(X,Y,Color,Flux), sigmaY(X,Y,Color,Flux), rho(X,Y,Color,Flux) )
            
            
            
            
        end
            
    end
    
    
    
    methods % functionality
        function Result = fun_unary(Obj, OperatorOperatorArgs, OutType, DataProp, DataPropOut)
            %
           
            Nobj = numel(Obj)
            
            
        end
        
    end
    

    methods % Unit-Test
        function Result = unitTest()
            Astro = AstroImage;
            Result = true;
        end
    end
    

end

            
