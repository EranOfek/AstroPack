classdef SubClass1 < SuperClass

   properties(Access=private)
      sub_prop1
   end
   
   
   methods
     
      function cpObj = copyElement(obj)
	  
         % Copy super_prop
         cpObj = copyElement@SuperClass(obj);
		 
         % Copy sub_prop1 in subclass
         % Assignment can introduce side effects
         cpObj.sub_prop1 = obj.sub_prop1;
		 
      end
   end
end
