% https://www.mathworks.com/matlabcentral/answers/183246-updating-property-of-an-object-without-creating-new-object

% When you create a class in MATLAB, it is by default a value class. 
% If you want to update objects by reference, you should make this a handle 
% class by changing the first line to:
%
% 	classdef testClass < handle


classdef testClass
	properties
		count
	end
	
	methods
		function obj = testClass
			obj.count = 1;
		end
		
		function obj = increment(obj)
			obj.count = obj.count + 1;
		end
	end
end
