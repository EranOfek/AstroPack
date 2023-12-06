% https://www.mathworks.com/matlabcentral/answers/431064-changing-variable-properties-in-class-by-method


% Your class is a value class as opposed to a handle class. 
% See this documentation page for a description of the differences between the two. 
% The "Modifying Value Objects in Functions" and "Modifying Handle Objects in Functions" 
% sections on that documentation page describe what happens when you modify properties of 
% an instance of each kind of class in a function or method. So you can either:
% 
% Define the methods in your value class to return the modified object, 
% and call the methods with the object as both input and output.
% Change your class to a handle class.


classdef ClassName

   properties
      PropertyName
   end
   
   methods
      function [obj ] = ClassName()
         obj.PropertyName = 0;
      end
      function  [ ] = MethodName(obj, arg1,...)
          obj.PropertyName=obj.PropertyName+5;
      end
      function  [ ] = MethodName2(obj, arg1,...)
       obj.PropertyName=obj.PropertyName/2;
      end
   end
   
end

