classdef SuperClass < matlab.mixin.Copyable

   properties(Access = private)
      super_prop	  
   end

   
   methods
      % ...
 
      function cpObj = copyElement(obj)
            % ...
			
         cpObj = copyElement@matlab.mixin.Copyable(obj); 
            % ...
      end
   end
end
