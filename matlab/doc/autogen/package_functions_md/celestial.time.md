# Package: celestial.time


### celestial.time.date2jd

Convert Julian/Gregorian date to Julian Day Package: @Time Description: Convert Julian/Gregorian date to Julian Day.


    
    Convert Julian/Gregorian date to Julian Day  
    Package: @Time  
    Description: Convert Julian/Gregorian date to Julian Day.  
    Input  : - Gregorian of Julian date in one of the following  
    formats:  
    [Y, M, D, Frac]  
    [Y, M, D]  
    [Y] - first day of year  
    [Y, M] - first day of month  
    [Y, M, D, H M S]  
    [Day, Month, Year, Day_Fraction]  
    or [Day, Month, Year, Hour, Min, Sec]  
    Alternatively this can be a string or a cell array of strings  
    in which each string contains the date in the format:  
    'yyyy-mm-ddTHH:MM:SS' (e.g., '2010-08-11T15:01:56')  
    If argument is not provided then the program will calculate  
    the JD for now using the clock UTC computer (time zone  
    included).  
    - Output type. Options are:  
    'JD'  - Julian days (default).  
    'MJD' - Modified JD (JD-2400000.5).  
    Output : - Row vector of Julian days.  
    Tested : Matlab 3.5  
    By : Eran O. Ofek                    Jan 1994  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: celestial.time.date2jd([1 1 2000 10 30 0]);  
    celestial.time.date2jd([1 1 2000; 2 2 2000]);  
    celestial.time.date2jd({'20101012T101010.111'});  
    celetial.time.date2jd;   JD of now (UTC)  
    Reliable: 1  
      
      
### celestial.time.date_str2vec

date_str2vec function                                            General Description: Convert a string or a cell array of string containing date and time in the format 'YYYY-MM-DD HH:MM:SS.frac' or 'YYYY-MM-DD', to a matrix of dates with the following


    
      
    date_str2vec function                                            General  
    Description: Convert a string or a cell array of string containing date  
    and time in the format 'YYYY-MM-DD HH:MM:SS.frac'  
    or 'YYYY-MM-DD', to a matrix of dates with the following  
    columns [Y M D H M S].  
    OBSOLETE: Use convert.str2date instead.  
    Input  : - A string or a cell array of string containing date  
    and time in the format 'YYYY-MM-DD HH:MM:SS.frac'  
    or 'YYYY-MM-DD'  
    Output : - A matrix of dates with the following columns [Y M D H M S].  
    Tested : Matlab R2014a  
    By : Eran O. Ofek                    Dec 2014  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: DateVec=date_str2vec({'2010-10-11T10:10:10.11','2014-12-12 10:23:59.1'});  
    Reliable: 2  
      
      
### celestial.time.days_in_month

Return the number of days in month Package: @Time, adapted from +celestial.time Description: Calculate the number of days in a given Gregorian or Julian month.


    
    Return the number of days in month  
    Package: @Time, adapted from +celestial.time  
    Description: Calculate the number of days in a given Gregorian or Julian  
    month.  
    Input  : - Year (array)  
    - Month (array, the same size as Year).  
    Output : - Number of days in month.  
    Tested : Matlab 5.3  
    By : Eran O. Ofek                    Jan 2003  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: Nd=celestial.time.days_in_month(2000,2);  
    Reliable: 2  
      
### celestial.time.delta_t

Return \Delta{T} Package: celestial.time Description: Return \Delta{T} at a vector of Julian days. DeltaT is defined as ET-UT prior to 1984, and TT-UT1 after 1984 (= 32.184+(TAI-UTC)-(UT1-UTC)).


    
    Return \Delta{T}  
    Package: celestial.time  
    Description: Return \Delta{T} at a vector of Julian days.  
    DeltaT is defined as ET-UT prior to 1984, and  
    TT-UT1 after 1984 (= 32.184+(TAI-UTC)-(UT1-UTC)).  
    Input  : - Vector of JD.  
    Output : - DeltaT [seconds].  
    Return NaN if DeltaT is not available (out of range).  
    Tested : Matlab R2014a  
    By : Eran O. Ofek                    Jun 2014  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: DeltaT=celestial.time.delta_t([2451545;celestial.time.julday])  
    Reliable: 2  
      
      
      
### celestial.time.easter_date

Calculate the date of Easter Package: celestial.time Description: Calculate the date of Easter for any Gregorian year.


    
    Calculate the date of Easter  
    Package: celestial.time  
    Description: Calculate the date of Easter for any Gregorian year.  
    Input  : - Year (integer).  
    Output : - Date of Easter, [D M Y].  
    - Date of Easter, [JD].  
    Tested : Matlab 7.0  
    By : Eran O. Ofek                    May 2006  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Reference: Oudin (1940)  
    Example: easter_date(2010);   will return [4 4 2010].  
    Reliable: 2  
      
### celestial.time.get_atime

Get current time, date, JD and LST. Package: celestial.time Description: Get current time, date, JD and LST.


    
    Get current time, date, JD and LST.  
    Package: celestial.time  
    Description: Get current time, date, JD and LST.  
    Input  : - Column vector of date in [JD],  
    or [D M Y H M S] formats and in UTC time system.  
    If empty matrix then use current time and date.  
    - Geodetic east longitude [radians].  
    - UT1-UTC [s], default is 0.  
    Output : - Structure of astronomical times, contains the following  
    fields:  
    .JD     - Julian day (UTC)  
    .Day    - Day in month (UTC)  
    .Month  - Month (UTC)  
    .Year   - Year (UTC)  
    .Hour   - Hour (UTC)  
    .Min    - Minutes (UTC)  
    .Sec    - Seconds (UTC)  
    .Frac   - Fraction of day (UTC)  
    .LST    - Local mean sidereal time [fraction of day]  
    .ISO    - String containing UTC date and time in  
    standard ISO format.  
    Tested : Matlab 7.3  
    By : Eran O. Ofek                    Jun 2008  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: T=celestial.time.get_atime(convert.date2jd,35./RAD);  
    Reliable: 1  
    -  
      
### celestial.time.jd2date

Convert Julian days to Gregorian/Julian date Package: @Time Description: Convert Julian days to Gregorian/Julian date.


    
    Convert Julian days to Gregorian/Julian date  
    Package: @Time  
    Description: Convert Julian days to Gregorian/Julian date.  
    Input  : - Row vector of (positive) Julian Days.  
    - Output format:  
    'f'  - [Day Month Year, Day_Fraction(UT)] (default).  
    'H'  - [Day Month Year, H M S]  
    - Output type: 'DMY' | 'YMD'. Default is 'DMY'  
    Output : - Matrix of dates.  
    e.g., [Day, Month, Year, Day_Fraction(UT)].  
    Tested : Matlab 5.2  
    By : Eran O. Ofek                    Sep 1999  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: Time.jd2date(convert.date2jd([1 1 2000; 2 2 2000]))  
    Time.jd2date(2451545.*ones(2,1),'h','YMD')  
    Time.jd2date(2451545.*ones(2,1),'f','YMD')  
    Reliable: 1  
      
### celestial.time.jd2mjd

Convert JD to MJD Package: celestial.time Description: Convert JD to MJD. See also: convert.time


    
    Convert JD to MJD  
    Package: celestial.time  
    Description: Convert JD to MJD. See also: convert.time  
    Input  : - JD  
    Output : - MJD (i.e., JD - 2400000.5)  
    See also: julday.m, jd2date.m, mjd2jd.m  
    Tested : Matlab R2011b  
    By : Eran O. Ofek                    Dec 2013  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: celestial.time.jd2mjd(2450000)  
    Reliable: 1  
    -  
      
### celestial.time.jd2year

Convert JD to year Package: celestial.time Description: Convert Julian day to Julian or Besselian years. OBSOLETE: Use convert.time instead.


    
    Convert JD to year  
    Package: celestial.time  
    Description: Convert Julian day to Julian or Besselian years.  
    OBSOLETE: Use convert.time instead.  
    Input  : - Vector of Julian days.  
    - Type of output years:  
    'J'  - Julian year (default).  
    'B'  - Besselian year.  
    Output : - Vector of (Julian or Besselian) years with decimal years.  
    Tested : Matlab 7.0  
    By : Eran O. Ofek                    Apr 2007  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: celestial.time.jd2year(2451545,'B')  
    Reliable: 1  
      
### celestial.time.julday

Convert Julian/Gregorain date to JD Package: celestial.time Description: Convert Julian/Gregorian date to Julian Day. OBSOLETE: Use convert.date2jd instead. See also: convert.time


    
    Convert Julian/Gregorain date to JD  
    Package: celestial.time  
    Description: Convert Julian/Gregorian date to Julian Day.  
    OBSOLETE: Use convert.date2jd instead.  
    See also: convert.time  
    Input  : - Gregorian of Julian date in one of the following formats  
    [Day, Month, Year, Day_Fraction]  
    or [Day, Month, Year, Hour, Min, Sec]  
    or [Day, Month, Year] - in this case set Day_Fraction to 0.  
    Alternatively this can be a string or a cell array of strings  
    in which each string contains the date in the format:  
    'yyyy-mm-ddTHH:MM:SS' (e.g., '2010-08-11T15:01:56') or:  
    'yyyy-mm-dd HH:MM:SS'.  
    If argument is not provided then the program will calculate  
    the JD for now using the clock UTC computer (time zone  
    included).  
    - Output type. Options are:  
    'JD'  - Julian days (default).  
    'MJD' - Modified JD (JD-2400000.5).  
    Output : - Row vector of Julian days.  
    Tested : Matlab 3.5  
    By : Eran O. Ofek                    Jan 1994  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: celestial.time.julday([1 1 2000 10 30 0]);  
    celestial.time.julday([1 1 2000; 2 2 2000]);  
    celestial.time.julday('2010-10-12 10:10:10.111');  
    celestial.time.julday({'2010-10-12 10:10:10.111'});  
    celestial.time.julday;   JD of now  
    Reliable: 1  
      
      
### celestial.time.julday1

Convert Gregorian date in the range 1901 to 2099 to JD Package: celestial.time Description: Convert Gregorian date in the range 1901 to 2099 to Julian days (see also: julday.m). See also: convert.date2jd, celestial.time.julday


    
    Convert Gregorian date in the range 1901 to 2099 to JD  
    Package: celestial.time  
    Description: Convert Gregorian date in the range 1901 to 2099 to  
    Julian days (see also: julday.m).  
    See also: convert.date2jd, celestial.time.julday  
    Input  : - Gregorian date in the range 1901 to 2099 in one of the  
    following formats  
    [Day, Month, Year, Day_Fraction]  
    or [Day, Month, Year, Hour, Min, Sec]  
    or [Day, Month, Year] (in this case set Day_Fraction to 0.  
    Output : Column vector of Julian Days.  
    Tested : Matlab 3.5  
    By : Eran O. Ofek                   January 1994  
    URL : http://wise-obs.tau.ac.il/~eran/matlab.html  
    Example: celestial.time.julday1([1 1 2000 10 30 0]);  
    celestial.time.julday1([1 1 2000; 2 2 2000]);  
    Reliable: 1  
      
      
### celestial.time.lst

Local Sidereal Time Package: celestial.time Description: Local Sidereal Time, (mean or apparent), for vector of JDs and a given East Longitude.


    
    Local Sidereal Time  
    Package: celestial.time  
    Description: Local Sidereal Time, (mean or apparent), for vector of  
    JDs and a given East Longitude.  
    Input  : - Vector of JD [days], in UT1 time scale.  
    - East Longitude in radians. Default is 0.  
    - Sidereal Time Type,  
    'm' - Mean (default).  
    'a' - apparent.  
    Output : - vector of LST in fraction of day.  
    Tested : Matlab 5.3  
    By : Eran O. Ofek                    Aug 1999  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: LST=celestial.time.lst(2451545+[0:1:5]',0);   LST at Greenwhich 0 UT1  
    Reliable: 1  
      
      
### celestial.time.mjd2jd

Convert MJD to JD Package: celestial.time Description: Convert MJD to JD. See also: convert.time


    
    Convert MJD to JD  
    Package: celestial.time  
    Description: Convert MJD to JD.  
    See also: convert.time  
    Input  : - MJD  
    Output : - JD (i.e., MJD + 2400000.5)  
    See also: julday.m, jd2date.m, jd2mjd.m  
    Tested : Matlab R2011b  
    By : Eran O. Ofek                    Dec 2013  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: celestial.time.mjd2jd(2450000)  
    Reliable: 1  
      
      
### celestial.time.month_name

Convert month number to name Package: celestial.time Description: Given a month number return a string with a month name.


    
    Convert month number to name  
    Package: celestial.time  
    Description: Given a month number return a string with a month name.  
    Input  : - Vector of month number.  
    Output : - Cell vector of month full name (9 chars).  
    - Cell vector of month short name (3 chars).  
    Tested : Matlab 5.3  
    By : Eran O. Ofek                    Jan 2003  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: [FullName,ShortName]=celestial.time.month_name([1;2])  
    Reliable: 1  
      
      
### celestial.time.str2date

Convert a date string (usinf datevec) to date vector


    
    Convert a date string (usinf datevec) to date vector  
    Input : - Date string (or cell of strings) that may have one of the following formats:  
    yyyymmddTHHMMSS.FFF  
    yyyy:mm:dd HH:MM:SS.FFF  
    yyyy-mm-dd HH:MM:SS.FFF  
    Output - A date vector [y m d, H M S]  
    Example: Date=celestial.time.str2date({'2020:02:01 10:10:23.1123'})  
      
### celestial.time.tdb_tdt

Approximate TDB-TT Package: celestial.time Description: Calculate approximate difference between TDT and TDB time scales.


    
    Approximate TDB-TT  
    Package: celestial.time  
    Description: Calculate approximate difference between TDT and TDB  
    time scales.  
    Input  : - Vector of julian days.  
    Output : - TDB-TDT (TDB-TT) [seconds].  
    Tested : Matlab 5.3  
    By : Eran O. Ofek                    Jun 2000  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: Diff=celestial.time.tdb_tdt(julday([1 1 2000]));  
    Reliable: 2  
    -  
      
### celestial.time.tt_utc

Get TT-UTC and TT-UT1 time Description: TT continues Terrestrial Dynamical Time (TDT or TD), which succeeded ephemeris time (ET). \Delta{T} = TT - UT1 TAI − GPS time = +19 seconds


    
    Get TT-UTC and TT-UT1 time  
    Description: TT continues Terrestrial Dynamical Time (TDT or TD),  
    which succeeded ephemeris time (ET).  
    \Delta{T} = TT - UT1  
    TAI − GPS time = +19 seconds  
    Input  : - Vector of JDs  
    * ...,key,val,...  
    'WhereToGet' -  
    'get' - get the latest EOP data from the IERS website and  
    update the local version.  
    'use' - use local version of the EOP data (default).  
    'FillVal' - Fill value if not available. Default is NaN.  
    'SourceFile' - Source file - options are:  
    '1992' - 1992 till now+3 month (default).  
    '1962' - 1962 till now.  
    'NearFutureInterp' - A logical indicating if to perform  
    interpolation into the near future.  
    Default is false.  
    This should be used only if you need TT-UT1 in the  
    present or up to a few months into the future.  
    Output : - TT - UTC [s]  
    - TT - UT1 [s] = \Delta{T}  
    - UT1 - TAI [s]  
    - UT1 - UTC [s]  
    Author : Eran Ofek (Sep 2021)  
    Example: [TTmUTC, TTmUT1, UT1mTAI, UT1mUTC]=celestial.time.tt_utc([0;2451545;celestial.time.julday+10]);  
      
### celestial.time.ut1_tai

Return the UT1-TAI time (available from 1846 till now).


    
    Return the UT1-TAI time (available from 1846 till now).  
    Input  : - A vector of JD  
    * ...,key,val,...  
    'WhereToGet' -  
    'get' - get the latest EOP data from the IERS website and  
    update the local version.  
    'use' - use local version of the EOP data (default).  
    'FillVal' - Fill value if not available. Default is NaN.  
    Output : - UT1-TAI [seconds].  
    - EOP full table (see Installer/readIERS_EOP)  
    Aujthor : Eran Ofek (Sep 2021)  
    Example: [UT1mTAI,EOP]=celestial.time.ut1_tai(2451545);  
    [UT1mTAI,EOP]=celestial.time.ut1_tai(2451545,'get',0);  
      
### celestial.time.ut1_utc

Return UT1-UTC (DUT1) - read from IERS EOP file (1992 to present day) Package: celestial.time Description: Return UT1-UTC (also known as DUT1).


    
    Return UT1-UTC (DUT1) - read from IERS EOP file (1992 to present day)  
    Package: celestial.time  
    Description: Return UT1-UTC (also known as DUT1).  
    Input  : - Vector of Julian days (valid only after 1 1 1961).  
    * ...,key,val,...  
    'WhereToGet' -  
    'get' - get the latest EOP data from the IERS website and  
    update the local version.  
    'use' - use local version of the EOP data (default).  
    'FillVal' - Fill value if not available. Default is NaN.  
    'SourceFile' - Source file - options are:  
    '1992' - 1992 till now+3 month (default).  
    '1962' - 1962 till now.  
    Output : - UT1-UTC [seconds].  
    - EOP full table (see Installer/readIERS_EOP)  
    Aujthor : Eran Ofek (Sep 2021)  
    Example: [UT1mUTC,EOP]=celestial.time.ut1_utc(2451545);  
    [UT1mUTC,EOP]=celestial.time.ut1_utc(2451545,'WhereToGet','get','FillVal',0);  
    [UT1mUTC]=celestial.time.ut1_utc(2451545,'SourceFile','1962')  
    Reliable: 2  
      
      
### celestial.time.wget_eop

Read or get the Earth orientation parameters file from IERS Package: celestial.time OBSOLETE: see Installer/readIERS_EOP class. Description: Get the table of historical and predicted Earth orientation parameters (EOP) from the IERS web site.


    
    Read or get the Earth orientation parameters file from IERS  
    Package: celestial.time  
    OBSOLETE: see Installer/readIERS_EOP class.  
    Description: Get the table of historical and predicted Earth orientation  
    parameters (EOP) from the IERS web site.  
    Input  : - 'get' - get the latest EOP data from the IERS website and  
    update the local version.  
    'use' - use local version of the EOP data (default).  
    Output : - A structure containing the EOP data. The structure contains  
    the following fields:  
    .Cat  - The catalog.  
    .Col  - A structure describing the catalog columns.  
    .UnitsCell - A cell array of the units of each column.  
    Tested : Matlab R2014a  
    By : Eran O. Ofek                    Jun 2014  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: EOP=celestial.time.wget_eop;  
    Reliable: 2  
      
      
### celestial.time.wget_tai_utc

Get TAI-UTC from file or IERS website Package: celestial.time Description: Get the table of historical TAI-UTC time differences (leap second) from the IERS web site.


    
    Get TAI-UTC from file or IERS website  
    Package: celestial.time  
    Description: Get the table of historical TAI-UTC time differences  
    (leap second) from the IERS web site.  
    Input  : - 'get' - get the latest TAI-UTC data from the IERS website and  
    update the local version.  
    'use' - use local version of the TAI-UTC data (default).  
    Output : - A structure containing the TAI-UTC data. The structure  
    contains the following fields:  
    .Cat  - The catalog.  
    .Col  - A structure describing the catalog columns.  
    .UnitsCell - A cell array of the units of each column.  
    Tested : Matlab R2014a  
    By : Eran O. Ofek                    Jun 2014  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: TAI_UTC=celestial.time.wget_tai_utc;  
    Reliable: 2  
      
      
      
### celestial.time.year2jd

Convert year to JD Package: celestial.time Description: Return the Julian day at Jan 1 st of a given list of years. See also: convert.time instead.


    
    Convert year to JD  
    Package: celestial.time  
    Description: Return the Julian day at Jan 1 st of a given list of years.  
    See also: convert.time instead.  
    Input  : - Column vector of years.  
    - Year type. Options are:  
    '1'  - January 1st of the year. Default  
    'by','b' - Bessilian year.  
    'jy','year,'yr',j' - Julian year.  
    'jd' - Input is julian days - return input as is.  
    Output : - Vector of JDs.  
    Tested : Matlab R2014a  
    By : Eran O. Ofek                    Jun 2014  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: JD=celestial.time.year2jd(2000);  
    Reliable: 2  
      
      
