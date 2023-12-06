% celestial.time package Unit-Test
%
% ### Requirements:
%
%
%


function Result = unitTest()
    % Package Unit-Test   
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

