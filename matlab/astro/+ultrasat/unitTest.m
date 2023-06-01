% Package Unit-Test
%
% ### Requirements:
%
%
%


function Result = unitTest()
    % Package Unit-Test   
	io.msgStyle(LogLevel.Test, '@start', 'test started');
    
    % testing usim simulation utility:
    
    SimA = ultrasat.usim('InCat',1000, 'Tile', 'A');
    SimB = ultrasat.usim('InCat',1000, 'Tile', 'B');
    SimC = ultrasat.usim('InCat',1000, 'Tile', 'C');
    SimD = ultrasat.usim('InCat',1000, 'Tile', 'D');
    
    MergedImage = ultrasat.umergeTileImages ();
    
    
%     func_unitTest();
    
	io.msgStyle(LogLevel.Test, '@passed', 'test passed');
	Result = true;
end

%--------------------------------------------------------------------------


function Result = func_unitTest()
	% Function Unit-Test
	io.msgStyle(LogLevel.Test, '@start', 'test started');
   
	io.msgStyle(LogLevel.Test, '@passed', 'passed');
	Result = true;
end


%--------------------------------------------------------------------------

