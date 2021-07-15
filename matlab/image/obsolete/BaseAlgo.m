% AstroCatalog handle class
% Package: @AstroCatalog
% Description: 
% Tested : Matlab R2018a
% Author : Eran O. Ofek (Mar 2021)
% Dependencies: @convert, @celestial
% Example : 
% Reliable: 2
%--------------------------------------------------------------------------

classdef BaseAlgo < handle %Component or Base
    
    methods % Constructor
       
        function Obj = BaseAlgo()
        end  
               
    end
    

    methods (Static)  % Unit-Test
        function Result = unitTest()
 
            Result = true;
        end
    end
    

end

            
