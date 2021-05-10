import gcn.notice_types
from astropy.utils.data import download_file
from astropy.io import ascii
import shutil
import ntpath
from wisegcn.email_alert import send_mail, format_alert, format_html
from wisegcn import galaxy_list
from wisegcn import wise
from wisegcn import mysql_update
from wisegcn.utils import get_sky_area
from configparser import ConfigParser
import voeventparse as vp
import logging
import os

config = ConfigParser(inline_comment_prefixes=';')
config.read("config.ini")


def init_log(filename="log.log"):
    log_path = config.get('LOG', 'PATH')  # log file path
    console_log_level = config.get('LOG', 'CONSOLE_LEVEL')  # logging level
    file_log_level = config.get('LOG', 'FILE_LEVEL')  # logging level

    # create log folder
    if not os.path.exists(log_path):
        os.makedirs(log_path)

    log = logging.getLogger(__name__)
    log.setLevel(logging.DEBUG)

    # console handler
    h = logging.StreamHandler()
    h.setLevel(logging.getLevelName(console_log_level))
    formatter = logging.Formatter("%(asctime)s - %(levelname)s - %(message)s", "%Y-%m-%d %H:%M:%S")
    h.setFormatter(formatter)
    log.addHandler(h)

    # log file handler
    h = logging.FileHandler(log_path + filename + ".log", "w", encoding=None, delay="true")
    h.setLevel(logging.getLevelName(file_log_level))
    formatter = logging.Formatter(
        "%(asctime)s - %(levelname)s [%(filename)s:%(lineno)s]: %(message)s", "%Y-%m-%d %H:%M:%S")
    h.setFormatter(formatter)
    log.addHandler(h)

    return log


def close_log(log):
    handlers = list(log.handlers)
    for h in handlers:
        log.removeHandler(h)
        h.flush()
        h.close()


# Function to call every time a GCN is received.
# Run only for notices of type
# LVC_PRELIMINARY, LVC_INITIAL, LVC_UPDATE, or LVC_RETRACTION.
@gcn.handlers.include_notice_types(
    gcn.notice_types.LVC_PRELIMINARY,
    gcn.notice_types.LVC_INITIAL,
    gcn.notice_types.LVC_UPDATE,
    gcn.notice_types.LVC_RETRACTION)
def process_gcn(payload, root):
    alerts_path = config.get('ALERT FILES', 'PATH')  # event alert file path
    fits_path = config.get('EVENT FILES', 'PATH')  # event FITS file path
    is_test = config.getboolean('GENERAL', 'TEST') if config.has_option('GENERAL', 'TEST') else False

    # Respond only to 'test'/'observation' events
    if is_test:
        role = 'test'
    else:
        role = 'observation'

    if root.attrib['role'] != role:
        logging.info('Not {}, aborting.'.format(role))
        return

    ivorn = root.attrib['ivorn']
    filename = ntpath.basename(ivorn).split('#')[1]
    log = init_log(filename)

    # Is retracted?
    if gcn.handlers.get_notice_type(root) == gcn.notice_types.LVC_RETRACTION:
        # Save alert to file
        with open(alerts_path + filename + '.xml', "wb") as f:
            f.write(payload)
        log.info("Event {} retracted, doing nothing.".format(filename))
        send_mail(subject="[GW@Wise] {}".format(filename.split('-')[0]),
                  text="GCN/LVC retraction {} received, doing nothing.".format(filename),
                  html=format_html("<b>Alert retracted.</b><br>"),
                  files=[alerts_path + filename + '.xml'])
        return

    v = vp.loads(payload)

    # Read all of the VOEvent parameters from the "What" section
    params = {elem.attrib['name']:
              elem.attrib['value']
              for elem in v.iterfind('.//Param')}

    # Respond only to 'CBC' (compact binary coalescence candidates) events.
    # Change 'CBC' to 'Burst' to respond to only unmodeled burst events.
    if params['Group'] != 'CBC':
        log.info('Not CBC, aborting.')
        return

    # Respond only to specific merger types
    if ((config.getfloat("GENERAL", "BNS_MIN") < float(params["BNS"])) |
            (config.getfloat("GENERAL", "NSBH_MIN") < float(params["NSBH"])) |
            (config.getfloat("GENERAL", "MASSGAP_MIN") < float(params["MassGap"])) |
            (config.getfloat("GENERAL", "BBH_MIN") < float(params["BBH"])) |
            (config.getfloat("GENERAL", "HASNS_MIN") < float(params["HasNS"])) |
            (config.getfloat("GENERAL", "HASREMNANT_MIN") < float(params["HasRemnant"]))) & \
            (config.getfloat("GENERAL", "TERRESTRIAL_MAX") >= float(params["Terrestrial"])) & \
            (config.getfloat("GENERAL", "FAR_MAX") >= float(params["FAR"])*60*60*24*365):
        pass
    else:
        log.info("Uninteresting alert, aborting.")
        return

    # Save alert to file
    with open(alerts_path+filename+'.xml', "wb") as f:
        f.write(payload)
    log.info("GCN/LVC alert {} received, started processing.".format(ivorn))

    # Read VOEvent attributes
    keylist = ['ivorn', 'role', 'version']
    for key in keylist:
        params[key] = v.attrib[key]

    # Read Who
    params['author_ivorn'] = v.Who.Author.contactName
    params['date_ivorn'] = v.Who.Date

    # Read WhereWhen
    params['observatorylocation_id'] = v.WhereWhen.ObsDataLocation.ObservatoryLocation.attrib['id']
    params['astrocoordsystem_id'] = v.WhereWhen.ObsDataLocation.ObservationLocation.AstroCoordSystem.attrib['id']
    params['isotime'] = v.WhereWhen.ObsDataLocation.ObservationLocation.AstroCoords.Time.TimeInstant.ISOTime

    # Read How
    description = ""
    for item in v.How.iterfind('Description'):
        description = description + ", " + item
    params['how_description'] = description

    # Insert VOEvent to the database
    mysql_update.insert_voevent('voevent_lvc', params, log)

    # Download the HEALPix sky map FITS file.
    tmp_path = download_file(params['skymap_fits'], cache=False)
    skymap_path = fits_path + filename + "_" + ntpath.basename(params['skymap_fits'])
    shutil.move(tmp_path, skymap_path)

    # Respond only to alerts with reasonable localization
    credzones = [0.5, 0.9, config.getfloat("GENERAL", "AREA_CREDZONE"), config.getfloat("TILE", "CREDZONE")]
    area = get_sky_area(skymap_path, credzone=credzones)
    if area[2] > config.getfloat("GENERAL", "AREA_MAX"):
        log.info(f"""{credzones[2]} area is {area[2]} > {config.get("GENERAL", "AREA_MAX")} deg^2, aborting.""")
        send_mail(subject="[GW@Wise] {}".format(params["GraceID"]),
                  text=f"""Attached {filename} GCN/LVC alert received, but {credzones[2]} area is {area[2]} > \
                          {config.get("GENERAL", "AREA_MAX")} deg^2, aborting.""",
                  html=format_alert(params, area[0:1]),
                  files=[alerts_path + filename + '.xml'],
                  log=log)
        return

    # Send alert email
    send_mail(subject="[GW@Wise] {}".format(params["GraceID"]),
              text="Attached {} GCN/LVC alert received, started processing.".format(filename),
              html=format_alert(params, area[0:2]),
              files=[alerts_path+filename+'.xml'],
              log=log)

    if area[3] > config.getfloat("TILE", "AREA_MAX"):
        # Create the galaxy list
        galaxies, ra, dec = galaxy_list.find_galaxy_list(skymap_path, log=log)
        # Save galaxy list to csv file and send it
        ascii.write(galaxies, "galaxy_list.csv", format="csv", overwrite=True,
                    names=["GladeID", "RA", "Dec", "Dist", "Bmag", "Score", "Distance factor"])
        send_mail(subject="[GW@Wise] {} Galaxy list".format(params["GraceID"]),
                  text="{} GCN/LVC alert galaxy list is attached.".format(filename),
                  files=["galaxy_list.csv"],
                  log=log)

        # Create Wise plan
        wise.process_galaxy_list(galaxies, alertname=ivorn.split('/')[-1], ra_event=ra, dec_event=dec, log=log)
    else:
        # Tile the credible region
        wise.process_tiles(skymap_path, alertname=ivorn.split('/')[-1], log=log)

    # Finish and delete logger
    log.info("Done.")
    close_log(log)
