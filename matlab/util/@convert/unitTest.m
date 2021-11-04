function Result = unitTest(Obj)
	%
	io.msgStyle(LogLevel.Test, '@start', 'convert test started');
    
    % O.S. just testing that all functions run, testing all the units is
    % futile because its just a dictionary of values, I think its better to
    % add the list of units to a config file.
    convert.units('mm','km',1)
    convert.angular('rad', 'arcmin1', 1)
    convert.timeUnits('s', 'hr', 1)
    convert.length('mm','km',1)
    convert.velocity('km/hr', 'm/s', 1)
	convert.proper_motion('arcmin/hr', 'rad/s', 1)
    assert(convert.minusPi2Pi(1+2*pi, 'rad') == 1)
    assert(convert.minusPi2Pi(1+180, 'deg') == -179)
    convert.mass('kg','gr', 1)
    convert.energy('erg', 'j', 1)
    convert.flux(1, 'mJy', 'AB', 1, 'Hz')
    convert.temp(1, 'K', 'F')
    convert.luptitude(1,1,1)
    convert.flux2mag(10,1)
    convert.sum_mag([1 10 5], 2)
    convert.hms2angle([1, 0, 0], 'deg')
    convert.dms2angle('+01 01 01.000')
    convert.str2date(convert.date2str([1 1 1]))
    convert.hour_str2frac('01:01:01.000')
    convert.time(1,'J','B')
    

	io.msgStyle(LogLevel.Test, '@passed', 'convert test passed');
	Result = true;
end
