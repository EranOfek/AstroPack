from astropy import units as u
from astropy.coordinates import Angle
from astropy.time import Time
from wisegcn.observing_tools import is_night, next_sunset, next_sunrise, is_observable_in_interval, change_iers_url
from configparser import ConfigParser
from schedulertml import rtml
from wisegcn.email_alert import send_mail
from wisegcn import tile
import logging

config = ConfigParser(inline_comment_prefixes=';')
config.read("config.ini")


def process_galaxy_list(galaxies, alertname='GW', ra_event=None, dec_event=None, log=None):
    """Get the full galaxy list, and find which are good to observe at Wise"""

    if log is None:
        log = logging.getLogger(__name__)

    log.info("Event most probable RA={}, Dec={}.".format(
        ra_event.to_string(unit=u.hourangle, sep=':', precision=2, pad=True),
        dec_event.to_string(sep=':', precision=2, alwayssign=True, pad=True)))

    eventname = alertname.split('#')[1]
    eventname = eventname.split('-')[0]

    t = Time.now()
    if not is_night(lat=config.getfloat('WISE', 'LAT')*u.deg,
                    lon=config.getfloat('WISE', 'LON')*u.deg,
                    alt=config.getfloat('WISE', 'ALT')*u.m,
                    t=t,
                    sun_alt_twilight=config.getfloat('OBSERVING', 'SUN_ALT_MAX')*u.deg):
        log.info("Daytime at Wise! Preparing a plan for next sunset.")
        t = next_sunset(lat=config.getfloat('WISE', 'LAT')*u.deg,
                        lon=config.getfloat('WISE', 'LON')*u.deg,
                        alt=config.getfloat('WISE', 'ALT')*u.m,
                        t=t,
                        sun_alt_twilight=config.getfloat('OBSERVING', 'SUN_ALT_MAX')*u.deg)
    else:
        log.info("It's nighttime at Wise! Preparing a plan for NOW.")

    t_sunrise = next_sunrise(lat=config.getfloat('WISE', 'LAT')*u.deg,
                             lon=config.getfloat('WISE', 'LON')*u.deg,
                             alt=config.getfloat('WISE', 'ALT')*u.m,
                             t=t,
                             sun_alt_twilight=config.getfloat('OBSERVING', 'SUN_ALT_MAX')*u.deg)
    log.debug("Now/sunset = {}, sunrise = {}".format(t, t_sunrise))

    telescopes = config.get('WISE', 'TELESCOPES').split(',')
    max_galaxies = config.getint('GALAXIES', 'MAXGALAXIESPLAN')  # maximal number of galaxies to use in observation plan

    # change IERS table URL (to fix URL timeout problems)
    change_iers_url(url=config.get('IERS', 'URL'))

    nothing_to_observe = True
    for tel in range(0, len(telescopes)):
        log.info("Writing a plan for the {}".format(telescopes[tel]))
        root = rtml.init(name=config.get('OBSERVING', 'USER'),
                         email=config.get('OBSERVING', 'EMAIL'))

        log.debug("Index\tGladeID\tRA\t\tDec\t\tAirmass\tHA\tLunarDist\tDist\tBmag\tScore\t\tDist factor")

        csv_filename = f"{telescopes[tel]}_GalaxyList.csv"
        fid = open(csv_filename, "w")
        fid.write("Index,GladeID,RA,Dec,Airmass,HA,LunarDist,Dist,Bmag,Score,Dist factor\n")

        n_galaxies_in_plan = 0
        for i in range(tel, galaxies.shape[0], len(telescopes)):

            ra = Angle(galaxies[i, 1] * u.deg)
            dec = Angle(galaxies[i, 2] * u.deg)
            is_observe, airmass, ha, lunar_dist = is_observable_in_interval(ra=ra, dec=dec, lat=config.getfloat('WISE', 'LAT')*u.deg,
                                                    lon=config.getfloat('WISE', 'LON')*u.deg,
                                                    alt=config.getfloat('WISE', 'ALT')*u.m,
                                                    t1=t, t2=t_sunrise,
                                                    ha_min=config.getfloat(telescopes[tel], 'HOURANGLE_MIN')*u.hourangle,
                                                    ha_max=config.getfloat(telescopes[tel], 'HOURANGLE_MAX')*u.hourangle,
                                                    airmass_min=config.getfloat(telescopes[tel], 'AIRMASS_MIN'),
                                                    airmass_max=config.getfloat(telescopes[tel], 'AIRMASS_MAX'),
                                                    min_lunar_distance=config.getfloat(telescopes[tel], 'MIN_LUNAR_DIST')*u.deg,
                                                    return_values=True)

            if is_observe:
                nothing_to_observe = False
                n_galaxies_in_plan += 1
                log.debug(
                    "{}:\t{:.0f}\t{}\t{}\t{:+.2f}\t{:+.2f}\t{:.2f}\t{:.2f}\t{:.2f}\t{:.6g}\t\t{:.2f}\t\tadded to plan!".format(
                        i + 1, galaxies[i, 0],
                        ra.to_string(unit=u.hourangle, sep=':', precision=2, pad=True),
                        dec.to_string(sep=':', precision=2, alwayssign=True, pad=True),
                        airmass, ha, lunar_dist, galaxies[i, 3], galaxies[i, 4], galaxies[i, 5], galaxies[i, 6]))
                fid.write("{},{:.0f},{},{},{:+.2f},{:+.2f},{:.2f},{:.2f},{:.2f},{:.6g},{:.2f}\n".format(
                        i + 1, galaxies[i, 0],
                        ra.to_string(unit=u.hourangle, sep=':', precision=2, pad=True),
                        dec.to_string(sep=':', precision=2, alwayssign=True, pad=True),
                        airmass, ha, lunar_dist, galaxies[i, 3], galaxies[i, 4], galaxies[i, 5], galaxies[i, 6]))

                root = rtml.add_request(root,
                                        request_id="GladeID_{:.0f}".format(galaxies[i, 0]),
                                        bestefforts=config.get('OBSERVING', 'BESTEFFORTS'),
                                        user=config.get('OBSERVING', 'USER'),
                                        description=config.get('OBSERVING', 'DESCRIPTION'),
                                        project=alertname,
                                        airmass_min=config.get(telescopes[tel], 'AIRMASS_MIN'),
                                        airmass_max=config.get(telescopes[tel], 'AIRMASS_MAX'),
                                        hourangle_min=config.get(telescopes[tel], 'HOURANGLE_MIN'),
                                        hourangle_max=config.get(telescopes[tel], 'HOURANGLE_MAX'),
                                        priority=str(min(max_galaxies, galaxies.shape[0])-n_galaxies_in_plan+1))

                rtml.add_target(root,
                                request_id="GladeID_{:.0f}".format(galaxies[i, 0]),
                                ra=ra.to_string(unit=u.degree, decimal=True),
                                dec=dec.to_string(unit=u.degree, decimal=True, alwayssign=True),
                                name="GladeID_{:.0f}".format(galaxies[i, 0]))

                rtml.add_picture(root,
                                 filt=config.get(telescopes[tel], 'FILTER'),
                                 target_name="GladeID_{:.0f}".format(galaxies[i, 0]),
                                 exptime=config.get(telescopes[tel], 'EXPTIME'),
                                 binning=config.get(telescopes[tel], 'BINNING'))
            else:
                log.debug(
                    "{}:\t{:.0f}\t{}\t{}\t{:+.2f}\t{:+.2f}\t{:+.2f}\t{:.2f}\t{:.2f}\t{:.6g}\t\t{:.2f}".format(
                        i + 1, galaxies[i, 0],
                        ra.to_string(unit=u.hourangle, sep=':', precision=2, pad=True),
                        dec.to_string(sep=':', precision=2, alwayssign=True, pad=True),
                        airmass, ha, lunar_dist, galaxies[i, 3], galaxies[i, 4], galaxies[i, 5], galaxies[i, 6]))

            if n_galaxies_in_plan >= max_galaxies:
                # maximal number of galaxies per plan has been reached
                break

        if nothing_to_observe:
            log.info("Nothing to observe.")
            send_mail(subject=f"[GW@Wise] {eventname} {telescopes[tel]} observing plan",
                      text="Nothing to observe for alert {}.\nEvent most probable at RA={}, Dec={}."
                      .format(alertname,
                              ra_event.to_string(unit=u.hourangle, sep=':', precision=2, pad=True),
                              dec_event.to_string(sep=':', precision=2, alwayssign=True, pad=True)))

        else:
            rtml_filename = config.get('WISE', 'PATH') + alertname + '_' + telescopes[tel] + '.xml'
            rtml.write(root, rtml_filename)

            fid.close()

            log.info(f"Created observing plan for alert {alertname}.")
            send_mail(subject=f"[GW@Wise] {eventname} {telescopes[tel]} observing plan",
                      text="{} observing plan for alert {}.\nEvent most probable at RA={}, Dec={}."
                      .format(telescopes[tel], alertname,
                              ra_event.to_string(unit=u.hourangle, sep=':', precision=2, pad=True),
                              dec_event.to_string(sep=':', precision=2, alwayssign=True, pad=True)),
                      files=[rtml_filename, csv_filename])

            # upload to remote Scheduler
            if not config.get(telescopes[tel], 'HOST'):
                log.info("No host name was provided, skipping plan upload.")
            else:
                result = rtml.import_to_remote_scheduler(rtml_filename,
                                                         username=config.get(telescopes[tel], 'USER'),
                                                         remote_host=config.get(telescopes[tel], 'HOST'),
                                                         remote_path=config.get(telescopes[tel], 'PATH'),
                                                         cygwin_path=config.get(telescopes[tel], 'CYGWIN_PATH'))
                log.info(result)

    return


def process_tiles(skymap_path, alertname='GW', log=None):
    if log is None:
        log = logging.getLogger(__name__)

    eventname = alertname.split('#')[1]
    eventname = eventname.split('-')[0]

    t = Time.now()
    if not is_night(lat=config.getfloat('WISE', 'LAT')*u.deg,
                    lon=config.getfloat('WISE', 'LON')*u.deg,
                    alt=config.getfloat('WISE', 'ALT')*u.m,
                    t=t,
                    sun_alt_twilight=config.getfloat('OBSERVING', 'SUN_ALT_MAX')*u.deg):
        log.info("Daytime at Wise! Preparing a plan for next sunset.")
        t = next_sunset(lat=config.getfloat('WISE', 'LAT')*u.deg,
                        lon=config.getfloat('WISE', 'LON')*u.deg,
                        alt=config.getfloat('WISE', 'ALT')*u.m,
                        t=t,
                        sun_alt_twilight=config.getfloat('OBSERVING', 'SUN_ALT_MAX')*u.deg)
    else:
        log.info("It's nighttime at Wise! Preparing a plan for NOW.")

    t_sunrise = next_sunrise(lat=config.getfloat('WISE', 'LAT')*u.deg,
                             lon=config.getfloat('WISE', 'LON')*u.deg,
                             alt=config.getfloat('WISE', 'ALT')*u.m,
                             t=t,
                             sun_alt_twilight=config.getfloat('OBSERVING', 'SUN_ALT_MAX')*u.deg)
    log.debug("Now/sunset = {}, sunrise = {}".format(t, t_sunrise))

    telescopes = config.get('WISE', 'TELESCOPES').split(', ')

    # change IERS table URL (to fix URL timeout problems)
    change_iers_url(url=config.get('IERS', 'URL'))

    nothing_to_observe = True
    for tel in range(0, len(telescopes)):
        log.info("Writing a plan for the {}".format(telescopes[tel]))
        root = rtml.init(name=config.get('OBSERVING', 'USER'),
                         email=config.get('OBSERVING', 'EMAIL'))

        # Tile the credible region
        ra, dec, probability = tile.tile_region(skymap_path, credzone=config.getfloat("TILE", "CREDZONE"),
                                                tile_area=config.getfloat("TILE", "SIZE")*config.getfloat(telescopes[tel], "FOV"), log=log)

        log.debug("Index\tRA\t\tDec\tAirmass\tHA\tLunarDist\tProbability")

        csv_filename = f"{telescopes[tel]}_TileList.csv"
        fid = open(csv_filename, "w")
        fid.write("Index,RA,Dec,Airmass,HA,LunarDist,Probability\n")

        for i in range(len(ra)):
            is_observe, airmass, ha, lunar_dist = is_observable_in_interval(
                                                    ra=ra[i], dec=dec[i], lat=config.getfloat('WISE', 'LAT')*u.deg,
                                                    lon=config.getfloat('WISE', 'LON')*u.deg,
                                                    alt=config.getfloat('WISE', 'ALT')*u.m,
                                                    t1=t, t2=t_sunrise,
                                                    ha_min=config.getfloat(telescopes[tel], 'HOURANGLE_MIN')*u.hourangle,
                                                    ha_max=config.getfloat(telescopes[tel], 'HOURANGLE_MAX')*u.hourangle,
                                                    airmass_min=config.getfloat(telescopes[tel], 'AIRMASS_MIN'),
                                                    airmass_max=config.getfloat(telescopes[tel], 'AIRMASS_MAX'),
                                                    min_lunar_distance=config.getfloat(telescopes[tel], 'MIN_LUNAR_DIST')*u.deg,
                                                    return_values=True)

            if is_observe:
                nothing_to_observe = False
                log.debug(
                    "{}:\t{}\t{}\t{:+.2f}\t{:+.2f}\t{:.2f}\t{:.6g}\t\tadded to plan!".format(
                        i + 1,
                        ra[i].to_string(unit=u.hourangle, sep=':', precision=2, pad=True),
                        dec[i].to_string(sep=':', precision=2, alwayssign=True, pad=True),
                        airmass, ha, lunar_dist, probability[i]))
                fid.write("{},{},{},{:+.2f},{:+.2f},{:.2f},{:.6g}\n".format(
                        i + 1,
                        ra[i].to_string(unit=u.hourangle, sep=':', precision=2, pad=True),
                        dec[i].to_string(sep=':', precision=2, alwayssign=True, pad=True),
                        airmass, ha, lunar_dist, probability[i]))

                root = rtml.add_request(root,
                                        request_id="Tile_{:.0f}".format(i+1),
                                        bestefforts=config.get('OBSERVING', 'BESTEFFORTS'),
                                        user=config.get('OBSERVING', 'USER'),
                                        description=config.get('OBSERVING', 'DESCRIPTION'),
                                        project=alertname,
                                        airmass_min=config.get(telescopes[tel], 'AIRMASS_MIN'),
                                        airmass_max=config.get(telescopes[tel], 'AIRMASS_MAX'),
                                        hourangle_min=config.get(telescopes[tel], 'HOURANGLE_MIN'),
                                        hourangle_max=config.get(telescopes[tel], 'HOURANGLE_MAX'),
                                        priority=str(len(ra)-i))

                rtml.add_target(root,
                                request_id="Tile_{:.0f}".format(i+1),
                                ra=ra[i].to_string(unit=u.degree, decimal=True),
                                dec=dec[i].to_string(unit=u.degree, decimal=True, alwayssign=True),
                                name="Tile_{:.0f}".format(i+1))

                rtml.add_picture(root,
                                 filt=config.get(telescopes[tel], 'FILTER'),
                                 target_name="Tile_{:.0f}".format(i+1),
                                 exptime=config.get(telescopes[tel], 'EXPTIME'),
                                 binning=config.get(telescopes[tel], 'BINNING'))
            else:
                log.debug(
                    "{}:\t{}\t{}\t{:+.2f}\t{:+.2f}\t{:+.2f}\t{:.6g}".format(
                        i + 1,
                        ra.to_string(unit=u.hourangle, sep=':', precision=2, pad=True),
                        dec.to_string(sep=':', precision=2, alwayssign=True, pad=True),
                        airmass, ha, lunar_dist, probability[i]))

        if nothing_to_observe:
            log.info("Nothing to observe.")
            send_mail(subject=f"[GW@Wise] {eventname} {telescopes[tel]} observing plan",
                      text=f"Nothing to observe for alert {alertname}.")

        else:
            rtml_filename = config.get('WISE', 'PATH') + alertname + '_' + telescopes[tel] + '.xml'
            rtml.write(root, rtml_filename)

            fid.close()

            log.info(f"Created observing plan for alert {alertname}.")
            send_mail(subject=f"[GW@Wise] {eventname} {telescopes[tel]} observing plan",
                      text="{} observing plan for alert {}."
                      .format(telescopes[tel], alertname),
                      files=[rtml_filename, csv_filename])

            # upload to remote Scheduler
            if not config.get(telescopes[tel], 'HOST'):
                log.info("No host name was provided, skipping plan upload.")
            else:
                result = rtml.import_to_remote_scheduler(rtml_filename,
                                                         username=config.get(telescopes[tel], 'USER'),
                                                         remote_host=config.get(telescopes[tel], 'HOST'),
                                                         remote_path=config.get(telescopes[tel], 'PATH'),
                                                         cygwin_path=config.get(telescopes[tel], 'CYGWIN_PATH'))
                log.info(result)

    return
