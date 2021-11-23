% Package Unit-Test
%
% ### Requirements:
%
%
%


function unitTest
    % Package Unit-Test   
	io.msgStyle(LogLevel.Test, '@start', 'test started');
    RAD = 180/pi;
    % Check conversion for altitude to hour angle, and the inverse.
    Dec=0.5;
    Phi = 30/180*pi;
    Alt = (20:1:60)/180*pi;
    HA=celestial.coo.alt2ha(Alt,Dec,Phi);
    [Alt_inv,AM]=celestial.coo.ha2alt(HA,Dec,Phi);
    assert(all(abs(Alt_inv-Alt)<1e-12));
    
    % Check the azalt2hadec and the inverse
    
    Lat = (-90:1:90)'/RAD;
    Az = (0:180)'/RAD;
    Alt = linspace(10,80,numel(Az))'/RAD;
    [HA,Dec]=celestial.coo.azalt2hadec(Az,Alt,Lat);
    
    [Az2,Alt2]=celestial.coo.hadec2azalt(HA,Dec,Lat);
    % Check consistency up to 1e-13 accuracy
    assert(all(abs(Az2-Az)<1e-13))
    assert(all(abs(Alt2-Alt)<1e-13))
    
    
    % Check spherre distance ??
        
    
    % Check convert_coo- focus on two common transformation:
    %                     (1) ecliptic to Galactic 
    %                     (2) J2000.0 to horizontal
    
    %ecliptic to galactic 
    Long =(0:0.2:2*pi)';
    Lat= linspace(-80,80,numel(RA))'/RAD; 
    JD = celestial.time.julday;
    [Long_ge,Lat_ge]= celestial.coo.convert_coo(Long,Lat,'g','e',JD);
    [Long_ge_inv,Lat_ge_inv]= celestial.coo.convert_coo(Long_ge,Lat_ge,'e','g',JD);
    
    % Check consistency up to 1e-9 accuracy - typically 1e-10 accuracy
    assert(all(abs(Long_ge_inv-Long)<1e-9))
    assert(all(abs(Lat_ge_inv-Lat)<1e-9))
    
    
    % j2000.0 to horizontal
    NeotCoo = [35.0281,30.0491]/RAD;
    [Long_jh,Lat_jh]= celestial.coo.convert_coo(Long,Lat,'j2000.0','h',JD,NeotCoo );
    [Long_jh_inv,Lat_jh_inv]= celestial.coo.convert_coo(Long_jh,Lat_jh,'h','j2000.0',JD,NeotCoo );
    
    
    assert(all(abs(Long_jh_inv-Long)<1e-13))
    assert(all(abs(Lat_jh_inv-Lat)<1e-13))

    
    % Check coo2cosined
    Long =(0:0.2:2*pi)';
    Lat= linspace(-80,80,numel(RA))'/RAD; 
    
    [CD1,CD2,CD3]=celestial.coo.coo2cosined(Long,Lat);
    [Long_cos_inv,Lat_cos_inv]=celestial.coo.cosined2coo(CD1,CD2,CD3);
    
    assert(all(abs(Long_cos_inv-Long)<1e-13));
    assert(all(abs(Lat_cos_inv-Lat)<1e-13));
    
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

