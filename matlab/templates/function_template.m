
function Result = myFunction()
% Summary
% Detailed description

Result = 1;

end

%--------------------------------------------------------------------------
function Result = testMyFunction() % Unit-Test

x = myFunction();

if x == 1
	Result = true;
else
	Result = false;
end

end

