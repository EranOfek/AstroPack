function Result = unitTest(Obj)
	%
	
    if abs(constant.c-299792458e2)>eps
        error('Problem with constant class');
    end

	Result = true;
end
