
classdef SpecializedCopy < matlab.mixin.Copyable

   properties   
      Prop1
      Prop2 = datestr(now)
   end
   
   
   methods(Access = protected)
   
      function cp = copyElement(obj)
         cp = SpecializedCopy;
         cp.Prop1 = obj.Prop1;
         cp.Prop2 = datestr(now);
      end
	  
   end
end


