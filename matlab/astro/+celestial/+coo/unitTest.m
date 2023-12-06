function Result = unitTest()
    % Unit Test for celestial.coo
    
	%io.msgStyle(LogLevel.Test, '@start', 'test started');
    
    %test_sphere_dist_fast();
    
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
    Lat= linspace(-80,80,numel(Long))'/RAD; 
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
    
    DiffLong = Long_jh_inv-Long;
    Flag = DiffLong>pi;
    DiffLong(Flag) = DiffLong(Flag)-2.*pi; 
    if max(abs(DiffLong))>1e-11
        error('Error in convert_coo');
    end
    assert(all(abs(Lat_jh_inv-Lat)<1e-11))

    
    % Check coo2cosined
    Long =(0:0.2:2*pi)';
    Lat= linspace(-80,80,numel(Long))'/RAD; 
    
    [CD1,CD2,CD3]=celestial.coo.coo2cosined(Long,Lat);
    [Long_cos_inv,Lat_cos_inv]=celestial.coo.cosined2coo(CD1,CD2,CD3);
    
    assert(all(abs(Long_cos_inv-Long)<1e-13));
    assert(all(abs(Lat_cos_inv-Lat)<1e-13));
    
    func_unitTest();    
    
	%io.msgStyle(LogLevel.Test, '@passed', 'test passed');
	Result = true;	
end

%--------------------------------------------------------------------------


function Result = func_unitTest()
	% Function Unit-Test
	%io.msgStyle(LogLevel.Test, '@start', 'test started');
   
	%io.msgStyle(LogLevel.Test, '@passed', 'passed');
	Result = true;
end


%--------------------------------------------------------------------------

function Result = test_sphere_dist_fast()
    io.msgLog(LogLevel.Test, 'celestial.coo.sphere_dist_fast test started');
    
    % Checking basic functionality and comparing mex and matlab
    UseMex = 0;
    UseMP = 0;
    
    ra1 = rand(1, 10) * 2 * pi;
    ra2 = rand(1, 10) * 2 * pi;
    dec1 = rand(1, 10) * 2 * pi;
    dec2 = rand(1, 10) * 2 * pi;
    
    matlab_res =  celestial.coo.sphere_dist_fast(ra1(1),dec1(1),ra2(1),dec2(1),UseMex,UseMP);
    
    UseMex = 1;
    mex_res = celestial.coo.sphere_dist_fast(ra1(1),dec1(1),ra2(1),dec2(1),UseMex,UseMP);
    
%     assert(isequal(matlab_res, mex_res));
    
    
    iters = 50;
    
    for arr_sizes=1:3
        
        arr_size = power(10,arr_sizes);
        
        for var_types=5:6

            MatlabTimeTotal = 0;
            MexTimeTotal = 0;
            MexMPTimeTotal = 0;            
            MatlabTime = 0;
            MexTime = 0;
            MexMPTime = 0;

            for iter=1:iters

                ra1 = rand(1, 10) * 2 * pi;
                ra2 = rand(1, 10) * 2 * pi;
                dec1 = rand(1, 10) * 2 * pi;
                dec2 = rand(1, 10) * 2 * pi;
    
                switch var_types
                    case 1
                        ra1 = int8(ra1);
                        ra2 = int8(ra2);
                        dec1 = int8(dec1);
                        dec2 = int8(dec2);
                        var_name = 'int8';
                    case 2
                        ra1 = int16(ra1);
                        ra2 = int16(ra2);
                        dec1 = int16(dec1);
                        dec2 = int16(dec2);
                        var_name = 'int16';
                    case 3
                        ra1 = int32(ra1);
                        ra2 = int32(ra2);
                        dec1 = int32(dec1);
                        dec2 = int32(dec2);
                        var_name = 'int32';
                    case 4                        
                        ra1 = int64(ra1);
                        ra2 = int64(ra2);
                        dec1 = int64(dec1);
                        dec2 = int64(dec2);
                        var_name = 'int64';
                    case 5                        
                        ra1 = single(ra1);
                        ra2 = single(ra2);
                        dec1 = single(dec1);
                        dec2 = single(dec2);
                        var_name = 'single';
                    case 6                        
                        ra1 = double(ra1);
                        ra2 = double(ra2);
                        dec1 = double(dec1);
                        dec2 = double(dec2);
                        var_name = 'double';
                end

                UseMex = 0;
                UseMP = 0;
                t = tic;
                matlab_res =  celestial.coo.sphere_dist_fast(ra1(1),dec1(1),ra2(1),dec2(1),UseMex,UseMP);                
                MatlabTime = toc(t);
                MatlabTimeTotal = MatlabTimeTotal + MatlabTime;

                UseMex = 1;
                UseMP = 0;
                t = tic;
                mex_res = celestial.coo.sphere_dist_fast(ra1(1),dec1(1),ra2(1),dec2(1),UseMex,UseMP);
                MexTime = toc(t);
                MexTimeTotal = MexTimeTotal + MexTime;

                UseMex = 1;
                UseMP = 1;
                t = tic;
                mex_mp_res = celestial.coo.sphere_dist_fast(ra1(1),dec1(1),ra2(1),dec2(1),UseMex,UseMP);
                MexMPTime = toc(t);
                MexMPTimeTotal = MexMPTimeTotal + MexMPTime;                
                                        
%                 assert(isequal(matlab_res, mex_res));
            end

            MatlabTime = MatlabTimeTotal / iters;
            MexTime = MexTimeTotal / iters;
            MexMPTime = MexMPTimeTotal / iters;

            fprintf('Array_size: %d, Var_type: %s, Matlab: %.6f, Mex: %.6f, MexMP: %.6f, Ratio: %0.2f, MP_Ratio: %0.2f\n', arr_size, var_name, MatlabTime, MexTime, MexMPTime, MatlabTime/MexTime, MatlabTime/MexMPTime);

        end
    end
    
    io.msgStyle(LogLevel.Test, '@passed', 'celestial.coo.sphere_dist_fast passed')
    Result = true;    

end
