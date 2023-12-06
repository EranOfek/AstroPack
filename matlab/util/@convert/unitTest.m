function Result = unitTest(Obj)
	%
	io.msgStyle(LogLevel.Test, '@start', 'convert test started');
    
    % O.S. just testing that all functions run, testing all the units is
    % futile because its just a dictionary of values, I think its better to
    % add the list of units to a config file.
    Iters = 100;
    for Iter=1:Iters
        x = rand(1);
        flerr = 1e-10;

        [f1, y] = convert.units('mm','km',x);
        [f2, z] = convert.units('km','mm',y);
        assert(abs((x - z)) < flerr);
        assert(f1 == 1/f2);
        
        y = convert.angular('rad', 'arcmin',x);
        z = convert.angular('arcmin', 'rad',y);
        assert(abs((x - z)) < flerr);

        y = convert.timeUnits('s', 'hr',x);
        z = convert.timeUnits('hr', 's',y);
        assert(abs((x - z)) < flerr);

        y = convert.length('mm','km',x);
        z = convert.length('km','mm',y);
        assert(abs((x - z)) < flerr);

        y = convert.velocity('km/hr', 'm/s', x);
        z = convert.velocity('m/s', 'km/hr', y);
       assert(abs((x - z)) < flerr);

        convert.proper_motion('arcmin/hr', 'rad/s', 1);
        y = convert.proper_motion('arcmin/hr', 'rad/s', x);
        z = convert.proper_motion('rad/s', 'arcmin/hr', y);
        assert(abs((x - z)) < flerr);

        assert(convert.minusPi2Pi(1+2*pi, 'rad') == 1);
        assert(convert.minusPi2Pi(1+180, 'deg') == -179);
        y = convert.minusPi2Pi(x, 'rad');
        z = convert.minusPi2Pi(y, 'deg');
        assert(abs((x - z)) < flerr);

        y = convert.mass('kg','gr',x);
        z = convert.mass('gr','kg',y);
        assert(abs((x - z)) < flerr);

        y = convert.energy('erg', 'j', z);
        z = convert.energy('j', 'erg', y);
        assert(abs((x - z)) < flerr);

        y = convert.flux(x, 'mJy', 'AB', x, 'Hz');
        z = convert.flux(y, 'AB', 'mJy', x, 'Hz');
        assert(abs((x - z)) < flerr);

        y = convert.temp(x, 'K', 'F');
        z = convert.temp(y, 'F', 'K');
        assert(abs((x - z)) < flerr);

        y = convert.luptitude(x*1e6, 1);
        z = convert.flux2mag(x*1e6);
        assert(abs((y - z)) < flerr);       

        y = convert.sum_mag([x 100], 2);
        assert(abs((y - x)) < flerr);

        convert.hms2angle([1, 0, 0], 'deg');
        convert.dms2angle('+01 00 00.000');

        d = randi(29);
        m = randi(12);
        y = randi(3000);
        s = convert.date2str([d m y]);
        a = convert.str2date(s);
        assert(all(a(1:3) == [y, m, d]));

        convert.hour_str2frac('01:01:01.000');

        convert.time(x,'J','B');    
        y = convert.time(x,'J','B');    
        z = convert.time(y,'B','J');    
        assert(abs((x - z)) < flerr);
    end
    
	io.msgStyle(LogLevel.Test, '@passed', 'convert test passed');
	Result = true;
end
