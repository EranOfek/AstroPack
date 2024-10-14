function Result = unitTest()
    % unitTest for celestial.time
    % Example: celestial.time
    
    
	%io.msgStyle(LogLevel.Test, '@start', 'test started');
    
    jd = celestial.time.julday;
    Date = celestial.time.jd2date(jd);
    jd_inv = celestial.time.date2jd(Date([3,2,1,4]));
    %celestial.time.date2jd
    assert(abs(jd_inv-jd)<1e-16);
    
    % test year2jd with respect to matlab datetime
    jd2001_ref= juliandate(datetime([2021,1,1]));
    jd2001 = celestial.time.year2jd(2021);
    
    assert(abs(jd2001-jd2001_ref)<1e-16);
    
    
    Date_str= '2015:04:22 11:11:11.111';
    vec_str2date = celestial.time.str2date(Date_str);
    jd_str2date = celestial.time.date2jd(vec_str2date );
    vec_vec = celestial.time.date_str2vec(Date_str);
    jd_str2vec = celestial.time.date2jd(vec_vec);
    
    assert(abs(jd_str2date-jd_str2vec)<1e-16);
    
    JD = 2451500;
    BJD = celestial.time.barycentricJD(JD,0,0,'Object','Ear','GeoPos',[0,0,0]);
    Delt = BJD-JD;
    DeltAstropy = 0.0032572676399179193;

    assert(abs(Delt-DeltAstropy)<1e-5);
    
    JD = celestial.time.date2jd([2010 08 01 10 30 0]);
    [BJD, BVel] = celestial.time.barycentricJD(JD,180,-20,'CooUnits','deg','InTimeScale','UTC','VelOutUnits','km/s','GeoPos',[0,0,0]);
    BJD1 = 2455409.935445962; % from https://astroutils.astronomy.osu.edu/time/utc2bjd.html
    assert(abs(BJD-BJD1)<1e-5);    
    BVel1 = -23.43236813668; % test python script based on astropy time library: see https://gist.github.com/StuartLittlefair/5aaf476c5d7b52d20aa9544cfaa936a1#file-corrs-py
    try
        assert(abs(BVel-BVel1)<1e-3);
    catch
        fprintf('Velocity discrepancy: %.1f cm\n',abs(BVel-BVel1)*1e3);
    end    
    
    JD = celestial.time.date2jd([2014 05 07 12 00 0]);
    [BJD, BVel] = celestial.time.barycentricJD(JD,50,50,'CooUnits','deg','InTimeScale','UTC','VelOutUnits','km/s','GeoPos',[0,0,0]);
    BJD1 = 2456784.995914159; % from https://astroutils.astronomy.osu.edu/time/utc2bjd.html
    assert(abs(BJD-BJD1)<1e-5);    
    BVel1 = -6.7555816846; % test python script based on astropy time library: see https://gist.github.com/StuartLittlefair/5aaf476c5d7b52d20aa9544cfaa936a1#file-corrs-py
    try
        assert(abs(BVel-BVel1)<1e-3);
    catch
        fprintf('Velocity discrepancy: %.1f cm/s\n',abs(BVel-BVel1)*1e3);
    end
  
    
    func_unitTest();    
    
    
    Result = true;
	%io.msgStyle(LogLevel.Test, '@passed', 'test passed');
end

%--------------------------------------------------------------------------


function Result = func_unitTest()
	% Function Unit-Test
	%io.msgStyle(LogLevel.Test, '@start', 'test started');
   
	%io.msgStyle(LogLevel.Test, '@passed', 'passed');
	Result = true;
end


%--------------------------------------------------------------------------

