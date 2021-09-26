% https://www.mathworks.com/help/matlab/matlab_oop/custom-copy-behavior.html

classdef HandleCopy < matlab.mixin.Copyable

   properties
      Prop1 % Shallow copy
      Prop2 % Handle copy
   end
   
   
   methods (Access = protected)
      function cp = copyElement(obj)
	  
         % Shallow copy object
         cp = copyElement@matlab.mixin.Copyable(obj);
		 
         % Get handle from Prop2
         hobj = obj.Prop2;
		 
         % Create default object
         new_hobj = eval(class(hobj));
		 
         % Add public property values from orig object
         HandleCopy.propValues(new_hobj,hobj);
		 
         % Assign the new object to property
         cp.Prop2  = new_hobj;
		 
      end
   end
   
   
   methods (Static)
      function propValues(newObj,orgObj)
	  
         pl = properties(orgObj);
         for k = 1:length(pl)
            if isprop(newObj,pl{k})
               newObj.(pl{k}) = orgObj.(pl{k});
            end
         end
		 
      end
   end
end

