
classdef ContainsHandles < matlab.mixin.Copyable

   properties
      Prop1
      Prop2
      DeepObj     % Contains a DeepCp object
      ShallowObj  % Contains a ShallowCp object
   end
   
   
   methods
   
      function obj = ContainsHandles(val1, val2, deepobj, shallowobj)
         if nargin > 0
            obj.Prop1 = val1;
            obj.Prop2 = val2;
            obj.DeepObj = deepobj;
            obj.ShallowObj = shallowobj;
         end
      end
   end
   
   
   methods(Access = protected)
   
      % Override copyElement method:
      function cpObj = copyElement(obj)
	  
         % Make a shallow copy of all four properties
         cpObj = copyElement@matlab.mixin.Copyable(obj);
		 
         % Make a deep copy of the DeepCp object
         cpObj.DeepObj = copy(obj.DeepObj);
		 
      end
   end
end
