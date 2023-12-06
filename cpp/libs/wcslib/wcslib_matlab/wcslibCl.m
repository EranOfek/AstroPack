% wcslib functions

classdef wcslibCl < handle   
    
    properties
        wcs
    end
    
    %--------------------------------------- START of Auto-Generated Code
    %<props>
    %%% Auto generated %%%
    properties
        flag
        naxis
        crpix
        pc
        cdelt
        crval
        lonpole
        latpole
        restfrq
        restwav
        npv
        npvmax
        nps
        npsmax
        cd
        crota
        altlin
        velref
        alt
        colnum
        colax
        crder
        csyer
        czphs
        cperi
        wcsname
        timesys
        trefpos
        trefdir
        plephem
        timeunit
        dateref
        mjdref
        timeoffs
        dateobs
        datebeg
        dateavg
        dateend
        mjdobs
        mjdbeg
        mjdavg
        mjdend
        jepoch
        bepoch
        tstart
        tstop
        xposure
        telapse
        timsyer
        timrder
        timedel
        timepixr
        obsgeo
        obsorbit
        radesys
        equinox
        specsys
        ssysobs
        velosys
        zsource
        ssyssrc
        velangl
        ntab
        nwtb
        lngtyp
        lattyp
        lng
        lat
        spec
        cubeface
        types
        m_flag
        m_naxis
        m_crpix
        m_pc
        m_cdelt
        m_crval
        m_cd
        m_crota
        m_colax
        m_crder
        m_csyer
        m_czphs
        m_cperi
    end

    %</props>
    %--------------------------------------- END of Auto-Generated Code
    
    % Constructor
    methods    
        function obj = wcslibCl()
            addpath('D:\Ultrasat\wcs\wcslib\wcs_matlab')
            %obj.wcs = clib.wcslibPkg.wcslibUtil();
            
            %filename = 'D:\Ultrasat\wcs\Images\ztf_20190709485764_000600_zg_c04_o_q4_sciimg.fits';
            %this.wcs.openFits(filename);
        end        
    end
  
    % Methods
    methods
        % Load header from FITS file
        function result = readFits(obj, filename)
            arguments
                obj wcslibCl
                filename string
            end
            
            %result = obj.wcs.readFits(filename);
            clib.wcslibPkg.wcsReadFits(filename)
        end
              
        
        function [sx, sy] = pixToSky(obj, x, y)
        % Convert pixel coordinates to sky coordinates
        % Usage: , including units
        % Add: Matrix
            arguments
                obj wcslibCl
                x double
                y double
            end
            
            %wx = obj.wcs.pix2sky(x, y);
            %wy = obj.wcs.getskyy();
            [sx, sy] = clib.wcslibPkg.wcsPixToSky(x, y);
        end
        
        
        % Convert world to pixel
        function [wx, wy] = skyToPix(obj, x, y)
            arguments
                obj wcslibCl
                x double
                y double
            end
            
            %wx = obj.wcs.sky2pix(x, y);
            %wy = obj.wcs.getpixy();
            [sx, sy] = clib.wcslibPkg.wcsSkyToPix(x, y);
        end

    end
    
    %--------------------------------------- START of Auto-Generated Code
    %<props_methods>
    %%% Auto generated %%%
    methods
        % Property getter: flag (int)
        function value = get.flag(obj)
            value = wcsGetProp_flag();
        end

        % Property setter: flag (int)
        function set.flag(obj, value)
            wcsSetProp_flag(value);
        end

        % Property getter: naxis (int)
        function value = get.naxis(obj)
            value = wcsGetProp_naxis();
        end

        % Property setter: naxis (int)
        function set.naxis(obj, value)
            wcsSetProp_naxis(value);
        end

        % Property getter: crpix (double*)
        function value = get.crpix(obj)
            len = wcsGetPropLen_crpix();
            value = wcsGetProp_crpix();
        end

        % Property setter: crpix (double*)
        function set.crpix(obj, value)
            len = wcsGetPropLen_crpix();
            wcsSetProp_crpix(value, len);
        end

        % Property getter: pc (double*)
        function value = get.pc(obj)
            len = wcsGetPropLen_pc();
            value = wcsGetProp_pc();
        end

        % Property setter: pc (double*)
        function set.pc(obj, value)
            len = wcsGetPropLen_pc();
            wcsSetProp_pc(value, len);
        end

        % Property getter: cdelt (double*)
        function value = get.cdelt(obj)
            len = wcsGetPropLen_cdelt();
            value = wcsGetProp_cdelt();
        end

        % Property setter: cdelt (double*)
        function set.cdelt(obj, value)
            len = wcsGetPropLen_cdelt();
            wcsSetProp_cdelt(value, len);
        end

        % Property getter: crval (double*)
        function value = get.crval(obj)
            len = wcsGetPropLen_crval();
            value = wcsGetProp_crval();
        end

        % Property setter: crval (double*)
        function set.crval(obj, value)
            len = wcsGetPropLen_crval();
            wcsSetProp_crval(value, len);
        end

        % Property getter: lonpole (double)
        function value = get.lonpole(obj)
            value = wcsGetProp_lonpole();
        end

        % Property setter: lonpole (double)
        function set.lonpole(obj, value)
            wcsSetProp_lonpole(value);
        end

        % Property getter: latpole (double)
        function value = get.latpole(obj)
            value = wcsGetProp_latpole();
        end

        % Property setter: latpole (double)
        function set.latpole(obj, value)
            wcsSetProp_latpole(value);
        end

        % Property getter: restfrq (double)
        function value = get.restfrq(obj)
            value = wcsGetProp_restfrq();
        end

        % Property setter: restfrq (double)
        function set.restfrq(obj, value)
            wcsSetProp_restfrq(value);
        end

        % Property getter: restwav (double)
        function value = get.restwav(obj)
            value = wcsGetProp_restwav();
        end

        % Property setter: restwav (double)
        function set.restwav(obj, value)
            wcsSetProp_restwav(value);
        end

        % Property getter: npv (int)
        function value = get.npv(obj)
            value = wcsGetProp_npv();
        end

        % Property setter: npv (int)
        function set.npv(obj, value)
            wcsSetProp_npv(value);
        end

        % Property getter: npvmax (int)
        function value = get.npvmax(obj)
            value = wcsGetProp_npvmax();
        end

        % Property setter: npvmax (int)
        function set.npvmax(obj, value)
            wcsSetProp_npvmax(value);
        end

        % Property getter: nps (int)
        function value = get.nps(obj)
            value = wcsGetProp_nps();
        end

        % Property setter: nps (int)
        function set.nps(obj, value)
            wcsSetProp_nps(value);
        end

        % Property getter: npsmax (int)
        function value = get.npsmax(obj)
            value = wcsGetProp_npsmax();
        end

        % Property setter: npsmax (int)
        function set.npsmax(obj, value)
            wcsSetProp_npsmax(value);
        end

        % Property getter: cd (double*)
        function value = get.cd(obj)
            len = wcsGetPropLen_cd();
            value = wcsGetProp_cd();
        end

        % Property setter: cd (double*)
        function set.cd(obj, value)
            len = wcsGetPropLen_cd();
            wcsSetProp_cd(value, len);
        end

        % Property getter: crota (double*)
        function value = get.crota(obj)
            len = wcsGetPropLen_crota();
            value = wcsGetProp_crota();
        end

        % Property setter: crota (double*)
        function set.crota(obj, value)
            len = wcsGetPropLen_crota();
            wcsSetProp_crota(value, len);
        end

        % Property getter: altlin (int)
        function value = get.altlin(obj)
            value = wcsGetProp_altlin();
        end

        % Property setter: altlin (int)
        function set.altlin(obj, value)
            wcsSetProp_altlin(value);
        end

        % Property getter: velref (int)
        function value = get.velref(obj)
            value = wcsGetProp_velref();
        end

        % Property setter: velref (int)
        function set.velref(obj, value)
            wcsSetProp_velref(value);
        end

        % Property getter: alt (char)
        function value = get.alt(obj)
            value = wcsGetProp_alt();
        end

        % Property setter: alt (char)
        function set.alt(obj, value)
            wcsSetProp_alt(value);
        end

        % Property getter: colnum (int)
        function value = get.colnum(obj)
            value = wcsGetProp_colnum();
        end

        % Property setter: colnum (int)
        function set.colnum(obj, value)
            wcsSetProp_colnum(value);
        end

        % Property getter: colax (int*)
        function value = get.colax(obj)
            len = wcsGetPropLen_colax();
            value = wcsGetProp_colax();
        end

        % Property setter: colax (int*)
        function set.colax(obj, value)
            len = wcsGetPropLen_colax();
            wcsSetProp_colax(value, len);
        end

        % Property getter: crder (double*)
        function value = get.crder(obj)
            len = wcsGetPropLen_crder();
            value = wcsGetProp_crder();
        end

        % Property setter: crder (double*)
        function set.crder(obj, value)
            len = wcsGetPropLen_crder();
            wcsSetProp_crder(value, len);
        end

        % Property getter: csyer (double*)
        function value = get.csyer(obj)
            len = wcsGetPropLen_csyer();
            value = wcsGetProp_csyer();
        end

        % Property setter: csyer (double*)
        function set.csyer(obj, value)
            len = wcsGetPropLen_csyer();
            wcsSetProp_csyer(value, len);
        end

        % Property getter: czphs (double*)
        function value = get.czphs(obj)
            len = wcsGetPropLen_czphs();
            value = wcsGetProp_czphs();
        end

        % Property setter: czphs (double*)
        function set.czphs(obj, value)
            len = wcsGetPropLen_czphs();
            wcsSetProp_czphs(value, len);
        end

        % Property getter: cperi (double*)
        function value = get.cperi(obj)
            len = wcsGetPropLen_cperi();
            value = wcsGetProp_cperi();
        end

        % Property setter: cperi (double*)
        function set.cperi(obj, value)
            len = wcsGetPropLen_cperi();
            wcsSetProp_cperi(value, len);
        end

        % Property getter: wcsname (char)
        function value = get.wcsname(obj)
            value = wcsGetProp_wcsname();
        end

        % Property setter: wcsname (char)
        function set.wcsname(obj, value)
            wcsSetProp_wcsname(value);
        end

        % Property getter: timesys (char)
        function value = get.timesys(obj)
            value = wcsGetProp_timesys();
        end

        % Property setter: timesys (char)
        function set.timesys(obj, value)
            wcsSetProp_timesys(value);
        end

        % Property getter: trefpos (char)
        function value = get.trefpos(obj)
            value = wcsGetProp_trefpos();
        end

        % Property setter: trefpos (char)
        function set.trefpos(obj, value)
            wcsSetProp_trefpos(value);
        end

        % Property getter: trefdir (char)
        function value = get.trefdir(obj)
            value = wcsGetProp_trefdir();
        end

        % Property setter: trefdir (char)
        function set.trefdir(obj, value)
            wcsSetProp_trefdir(value);
        end

        % Property getter: plephem (char)
        function value = get.plephem(obj)
            value = wcsGetProp_plephem();
        end

        % Property setter: plephem (char)
        function set.plephem(obj, value)
            wcsSetProp_plephem(value);
        end

        % Property getter: timeunit (char)
        function value = get.timeunit(obj)
            value = wcsGetProp_timeunit();
        end

        % Property setter: timeunit (char)
        function set.timeunit(obj, value)
            wcsSetProp_timeunit(value);
        end

        % Property getter: dateref (char)
        function value = get.dateref(obj)
            value = wcsGetProp_dateref();
        end

        % Property setter: dateref (char)
        function set.dateref(obj, value)
            wcsSetProp_dateref(value);
        end

        % Property getter: mjdref (double*)
        function value = get.mjdref(obj)
            len = wcsGetPropLen_mjdref();
            value = wcsGetProp_mjdref();
        end

        % Property setter: mjdref (double*)
        function set.mjdref(obj, value)
            len = wcsGetPropLen_mjdref();
            wcsSetProp_mjdref(value, len);
        end

        % Property getter: timeoffs (double)
        function value = get.timeoffs(obj)
            value = wcsGetProp_timeoffs();
        end

        % Property setter: timeoffs (double)
        function set.timeoffs(obj, value)
            wcsSetProp_timeoffs(value);
        end

        % Property getter: dateobs (char)
        function value = get.dateobs(obj)
            value = wcsGetProp_dateobs();
        end

        % Property setter: dateobs (char)
        function set.dateobs(obj, value)
            wcsSetProp_dateobs(value);
        end

        % Property getter: datebeg (char)
        function value = get.datebeg(obj)
            value = wcsGetProp_datebeg();
        end

        % Property setter: datebeg (char)
        function set.datebeg(obj, value)
            wcsSetProp_datebeg(value);
        end

        % Property getter: dateavg (char)
        function value = get.dateavg(obj)
            value = wcsGetProp_dateavg();
        end

        % Property setter: dateavg (char)
        function set.dateavg(obj, value)
            wcsSetProp_dateavg(value);
        end

        % Property getter: dateend (char)
        function value = get.dateend(obj)
            value = wcsGetProp_dateend();
        end

        % Property setter: dateend (char)
        function set.dateend(obj, value)
            wcsSetProp_dateend(value);
        end

        % Property getter: mjdobs (double)
        function value = get.mjdobs(obj)
            value = wcsGetProp_mjdobs();
        end

        % Property setter: mjdobs (double)
        function set.mjdobs(obj, value)
            wcsSetProp_mjdobs(value);
        end

        % Property getter: mjdbeg (double)
        function value = get.mjdbeg(obj)
            value = wcsGetProp_mjdbeg();
        end

        % Property setter: mjdbeg (double)
        function set.mjdbeg(obj, value)
            wcsSetProp_mjdbeg(value);
        end

        % Property getter: mjdavg (double)
        function value = get.mjdavg(obj)
            value = wcsGetProp_mjdavg();
        end

        % Property setter: mjdavg (double)
        function set.mjdavg(obj, value)
            wcsSetProp_mjdavg(value);
        end

        % Property getter: mjdend (double)
        function value = get.mjdend(obj)
            value = wcsGetProp_mjdend();
        end

        % Property setter: mjdend (double)
        function set.mjdend(obj, value)
            wcsSetProp_mjdend(value);
        end

        % Property getter: jepoch (double)
        function value = get.jepoch(obj)
            value = wcsGetProp_jepoch();
        end

        % Property setter: jepoch (double)
        function set.jepoch(obj, value)
            wcsSetProp_jepoch(value);
        end

        % Property getter: bepoch (double)
        function value = get.bepoch(obj)
            value = wcsGetProp_bepoch();
        end

        % Property setter: bepoch (double)
        function set.bepoch(obj, value)
            wcsSetProp_bepoch(value);
        end

        % Property getter: tstart (double)
        function value = get.tstart(obj)
            value = wcsGetProp_tstart();
        end

        % Property setter: tstart (double)
        function set.tstart(obj, value)
            wcsSetProp_tstart(value);
        end

        % Property getter: tstop (double)
        function value = get.tstop(obj)
            value = wcsGetProp_tstop();
        end

        % Property setter: tstop (double)
        function set.tstop(obj, value)
            wcsSetProp_tstop(value);
        end

        % Property getter: xposure (double)
        function value = get.xposure(obj)
            value = wcsGetProp_xposure();
        end

        % Property setter: xposure (double)
        function set.xposure(obj, value)
            wcsSetProp_xposure(value);
        end

        % Property getter: telapse (double)
        function value = get.telapse(obj)
            value = wcsGetProp_telapse();
        end

        % Property setter: telapse (double)
        function set.telapse(obj, value)
            wcsSetProp_telapse(value);
        end

        % Property getter: timsyer (double)
        function value = get.timsyer(obj)
            value = wcsGetProp_timsyer();
        end

        % Property setter: timsyer (double)
        function set.timsyer(obj, value)
            wcsSetProp_timsyer(value);
        end

        % Property getter: timrder (double)
        function value = get.timrder(obj)
            value = wcsGetProp_timrder();
        end

        % Property setter: timrder (double)
        function set.timrder(obj, value)
            wcsSetProp_timrder(value);
        end

        % Property getter: timedel (double)
        function value = get.timedel(obj)
            value = wcsGetProp_timedel();
        end

        % Property setter: timedel (double)
        function set.timedel(obj, value)
            wcsSetProp_timedel(value);
        end

        % Property getter: timepixr (double)
        function value = get.timepixr(obj)
            value = wcsGetProp_timepixr();
        end

        % Property setter: timepixr (double)
        function set.timepixr(obj, value)
            wcsSetProp_timepixr(value);
        end

        % Property getter: obsgeo (double*)
        function value = get.obsgeo(obj)
            len = wcsGetPropLen_obsgeo();
            value = wcsGetProp_obsgeo();
        end

        % Property setter: obsgeo (double*)
        function set.obsgeo(obj, value)
            len = wcsGetPropLen_obsgeo();
            wcsSetProp_obsgeo(value, len);
        end

        % Property getter: obsorbit (char)
        function value = get.obsorbit(obj)
            value = wcsGetProp_obsorbit();
        end

        % Property setter: obsorbit (char)
        function set.obsorbit(obj, value)
            wcsSetProp_obsorbit(value);
        end

        % Property getter: radesys (char)
        function value = get.radesys(obj)
            value = wcsGetProp_radesys();
        end

        % Property setter: radesys (char)
        function set.radesys(obj, value)
            wcsSetProp_radesys(value);
        end

        % Property getter: equinox (double)
        function value = get.equinox(obj)
            value = wcsGetProp_equinox();
        end

        % Property setter: equinox (double)
        function set.equinox(obj, value)
            wcsSetProp_equinox(value);
        end

        % Property getter: specsys (char)
        function value = get.specsys(obj)
            value = wcsGetProp_specsys();
        end

        % Property setter: specsys (char)
        function set.specsys(obj, value)
            wcsSetProp_specsys(value);
        end

        % Property getter: ssysobs (char)
        function value = get.ssysobs(obj)
            value = wcsGetProp_ssysobs();
        end

        % Property setter: ssysobs (char)
        function set.ssysobs(obj, value)
            wcsSetProp_ssysobs(value);
        end

        % Property getter: velosys (double)
        function value = get.velosys(obj)
            value = wcsGetProp_velosys();
        end

        % Property setter: velosys (double)
        function set.velosys(obj, value)
            wcsSetProp_velosys(value);
        end

        % Property getter: zsource (double)
        function value = get.zsource(obj)
            value = wcsGetProp_zsource();
        end

        % Property setter: zsource (double)
        function set.zsource(obj, value)
            wcsSetProp_zsource(value);
        end

        % Property getter: ssyssrc (char)
        function value = get.ssyssrc(obj)
            value = wcsGetProp_ssyssrc();
        end

        % Property setter: ssyssrc (char)
        function set.ssyssrc(obj, value)
            wcsSetProp_ssyssrc(value);
        end

        % Property getter: velangl (double)
        function value = get.velangl(obj)
            value = wcsGetProp_velangl();
        end

        % Property setter: velangl (double)
        function set.velangl(obj, value)
            wcsSetProp_velangl(value);
        end

        % Property getter: ntab (int)
        function value = get.ntab(obj)
            value = wcsGetProp_ntab();
        end

        % Property setter: ntab (int)
        function set.ntab(obj, value)
            wcsSetProp_ntab(value);
        end

        % Property getter: nwtb (int)
        function value = get.nwtb(obj)
            value = wcsGetProp_nwtb();
        end

        % Property setter: nwtb (int)
        function set.nwtb(obj, value)
            wcsSetProp_nwtb(value);
        end

        % Property getter: lngtyp (char)
        function value = get.lngtyp(obj)
            value = wcsGetProp_lngtyp();
        end

        % Property setter: lngtyp (char)
        function set.lngtyp(obj, value)
            wcsSetProp_lngtyp(value);
        end

        % Property getter: lattyp (char)
        function value = get.lattyp(obj)
            value = wcsGetProp_lattyp();
        end

        % Property setter: lattyp (char)
        function set.lattyp(obj, value)
            wcsSetProp_lattyp(value);
        end

        % Property getter: lng (int)
        function value = get.lng(obj)
            value = wcsGetProp_lng();
        end

        % Property setter: lng (int)
        function set.lng(obj, value)
            wcsSetProp_lng(value);
        end

        % Property getter: lat (int)
        function value = get.lat(obj)
            value = wcsGetProp_lat();
        end

        % Property setter: lat (int)
        function set.lat(obj, value)
            wcsSetProp_lat(value);
        end

        % Property getter: spec (int)
        function value = get.spec(obj)
            value = wcsGetProp_spec();
        end

        % Property setter: spec (int)
        function set.spec(obj, value)
            wcsSetProp_spec(value);
        end

        % Property getter: cubeface (int)
        function value = get.cubeface(obj)
            value = wcsGetProp_cubeface();
        end

        % Property setter: cubeface (int)
        function set.cubeface(obj, value)
            wcsSetProp_cubeface(value);
        end

        % Property getter: types (int*)
        function value = get.types(obj)
            len = wcsGetPropLen_types();
            value = wcsGetProp_types();
        end

        % Property setter: types (int*)
        function set.types(obj, value)
            len = wcsGetPropLen_types();
            wcsSetProp_types(value, len);
        end

        % Property getter: m_flag (int)
        function value = get.m_flag(obj)
            value = wcsGetProp_m_flag();
        end

        % Property setter: m_flag (int)
        function set.m_flag(obj, value)
            wcsSetProp_m_flag(value);
        end

        % Property getter: m_naxis (int)
        function value = get.m_naxis(obj)
            value = wcsGetProp_m_naxis();
        end

        % Property setter: m_naxis (int)
        function set.m_naxis(obj, value)
            wcsSetProp_m_naxis(value);
        end

        % Property getter: m_crpix (double*)
        function value = get.m_crpix(obj)
            len = wcsGetPropLen_m_crpix();
            value = wcsGetProp_m_crpix();
        end

        % Property setter: m_crpix (double*)
        function set.m_crpix(obj, value)
            len = wcsGetPropLen_m_crpix();
            wcsSetProp_m_crpix(value, len);
        end

        % Property getter: m_pc (double*)
        function value = get.m_pc(obj)
            len = wcsGetPropLen_m_pc();
            value = wcsGetProp_m_pc();
        end

        % Property setter: m_pc (double*)
        function set.m_pc(obj, value)
            len = wcsGetPropLen_m_pc();
            wcsSetProp_m_pc(value, len);
        end

        % Property getter: m_cdelt (double*)
        function value = get.m_cdelt(obj)
            len = wcsGetPropLen_m_cdelt();
            value = wcsGetProp_m_cdelt();
        end

        % Property setter: m_cdelt (double*)
        function set.m_cdelt(obj, value)
            len = wcsGetPropLen_m_cdelt();
            wcsSetProp_m_cdelt(value, len);
        end

        % Property getter: m_crval (double*)
        function value = get.m_crval(obj)
            len = wcsGetPropLen_m_crval();
            value = wcsGetProp_m_crval();
        end

        % Property setter: m_crval (double*)
        function set.m_crval(obj, value)
            len = wcsGetPropLen_m_crval();
            wcsSetProp_m_crval(value, len);
        end

        % Property getter: m_cd (double*)
        function value = get.m_cd(obj)
            len = wcsGetPropLen_m_cd();
            value = wcsGetProp_m_cd();
        end

        % Property setter: m_cd (double*)
        function set.m_cd(obj, value)
            len = wcsGetPropLen_m_cd();
            wcsSetProp_m_cd(value, len);
        end

        % Property getter: m_crota (double*)
        function value = get.m_crota(obj)
            len = wcsGetPropLen_m_crota();
            value = wcsGetProp_m_crota();
        end

        % Property setter: m_crota (double*)
        function set.m_crota(obj, value)
            len = wcsGetPropLen_m_crota();
            wcsSetProp_m_crota(value, len);
        end

        % Property getter: m_colax (int*)
        function value = get.m_colax(obj)
            len = wcsGetPropLen_m_colax();
            value = wcsGetProp_m_colax();
        end

        % Property setter: m_colax (int*)
        function set.m_colax(obj, value)
            len = wcsGetPropLen_m_colax();
            wcsSetProp_m_colax(value, len);
        end

        % Property getter: m_crder (double*)
        function value = get.m_crder(obj)
            len = wcsGetPropLen_m_crder();
            value = wcsGetProp_m_crder();
        end

        % Property setter: m_crder (double*)
        function set.m_crder(obj, value)
            len = wcsGetPropLen_m_crder();
            wcsSetProp_m_crder(value, len);
        end

        % Property getter: m_csyer (double*)
        function value = get.m_csyer(obj)
            len = wcsGetPropLen_m_csyer();
            value = wcsGetProp_m_csyer();
        end

        % Property setter: m_csyer (double*)
        function set.m_csyer(obj, value)
            len = wcsGetPropLen_m_csyer();
            wcsSetProp_m_csyer(value, len);
        end

        % Property getter: m_czphs (double*)
        function value = get.m_czphs(obj)
            len = wcsGetPropLen_m_czphs();
            value = wcsGetProp_m_czphs();
        end

        % Property setter: m_czphs (double*)
        function set.m_czphs(obj, value)
            len = wcsGetPropLen_m_czphs();
            wcsSetProp_m_czphs(value, len);
        end

        % Property getter: m_cperi (double*)
        function value = get.m_cperi(obj)
            len = wcsGetPropLen_m_cperi();
            value = wcsGetProp_m_cperi();
        end

        % Property setter: m_cperi (double*)
        function set.m_cperi(obj, value)
            len = wcsGetPropLen_m_cperi();
            wcsSetProp_m_cperi(value, len);
        end

    end

    %</props_methods>        
    %--------------------------------------- END of Auto-Generated Code

    methods
        
        % UnitTest
        function result = runUnitTest(obj)
            
            % Test conversion: pixel -> sky -> pixel
            iters = 10;
            count = 0;
            for i=1:1:iters
                x = 10 * i;
                y = 5 * i;
                [sx,sy] = obj.pix2sky(x, y);              
                [px,py] = obj.sky2pix(sx, sy);
                fprintf('x: %.3f, y: %.3f, Sx: %.3f, Sy: %.3f, Px: %.3f, Py: %.3f\n', ...
                    x, y, sx, sy, px, py);                
                
                if ((int32(px) == x) && (int32(py) == y))
                    count = count + 1;
                end
            end
            
            if (count == iters)
                fprintf('PASS, %d / %d\n', count, iters);
            else
                fprintf('FAILED, %d / %d\n', count, iters);
            end
            
            % Properties test
            fprintf('LONPOLE: %.3f\n', obj.LONPOLE);
            fprintf('LONPOLE: %.3f\n', obj.LATPOLE);
            
            result = true;        
        end
    end
    
    

    % Static methods
    methods (Static)           
        function result = unitTest()
            w = wcslibCl;
            filename = 'D:\Ultrasat\wcs\Images\ztf_20190709485764_000600_zg_c04_o_q4_sciimg.fits';
            w.readFits(filename);
            %result = runUnitTest(w);
            x = [100,200,300]
            y = [400,500,600]
            [sx, sy] = w.pixToSky(x, y)
            result = true;
        end
        
   end
     
end

            
