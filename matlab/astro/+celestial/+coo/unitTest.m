% Package Unit-Test
%
% ### Requirements:
%
%
%


function unitTest
    % Package Unit-Test   
	io.msgStyle(LogLevel.Test, '@start', 'test started');
    
    % Check conversion for altitude to hour angle, and the inverse.
    Dec=0.5;
    Phi = 30/180*pi;
    Alt = (20:1:60)/180*pi;
    HA=celestial.coo.alt2ha(Alt,Dec,Phi);
    [Alt_inv,AM]=celestial.coo.ha2alt(HA,Dec,Phi);
    assert(all(abs(Alt_inv-Alt)<1e-12));
    
    
    
    
    
    func_unitTest();
    
	io.msgStyle(LogLevel.Test, '@passed', 'test passed');
end

%--------------------------------------------------------------------------


function Result = func_unitTest()
	% Function Unit-Test
	io.msgStyle(LogLevel.Test, '@start', 'test started');
   
	io.msgStyle(LogLevel.Test, '@passed', 'passed');
	Result = true;
end


%--------------------------------------------------------------------------

