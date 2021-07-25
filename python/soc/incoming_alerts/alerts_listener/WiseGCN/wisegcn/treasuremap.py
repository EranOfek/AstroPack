import os
import requests
from configparser import ConfigParser
from ccdproc import ImageFileCollection
from astropy import units as u
from astropy.coordinates import SkyCoord
from astropy.time import Time

config = ConfigParser(inline_comment_prefixes=';')
config.read("config.ini")


def get_nightly_image_list(date, telescope="C28"):
    path = os.path.join(config.get("WISE", "OBS_PATH"), config.get(telescope, "OBS_DIR"))
    if telescope == "C28":
        path = os.path.join(path, date + "c28")
    elif telescope == "C18":
        path = os.path.join(path, date + "c18")
    else:
        path = os.path.join(path, date)
    imlist = ImageFileCollection(path)

    return imlist


def get_observed_target_list(date, telescope="C28"):
    imlist = get_nightly_image_list(date, telescope)
    idx = ["GladeID" in obj for obj in imlist.summary["object"]]

    jd = imlist.summary[idx]["jd"] * u.day
    jd = jd + imlist.summary[idx]["exptime"]/2 * u.s  # get mid-exposure time
    # fix JD for RBI flood delay (C28):
    if telescope == "C28":
        rbi_delay = 27 * u.s
        rbi_idx = ["RBI Flood" in readout for readout in imlist.summary[idx]["readoutm"]]
        jd[rbi_idx] = jd[rbi_idx] + rbi_delay
    t = Time(jd, format="jd")
    t.format = "fits"  # convert to string
    t = t.value

    coo = SkyCoord(ra=imlist.summary[idx]["ra"], dec=imlist.summary[idx]["dec"], unit=(u.hourangle, u.deg))
    ra = coo.ra.value
    dec = coo.dec.value
    return ra, dec, t


def prepare_pointing(ra, dec, time, band="other", instrumentid=57, depth=21, depth_unit="ab_mag",
                     pos_angle=0, status="completed"):
    pointing = {
                "ra": f"{ra}",
                "dec": f"{dec}",
                "band": f"{band}",
                "instrumentid": f"{instrumentid}",
                "depth": f"{depth}",
                "depth_unit": f"{depth_unit}",
                "time": f"{time}",
                "pos_angle": f"{pos_angle}",
                "status": f"{status}"
    }
    return pointing


def prepare_json_data(graceid, api_token, ra, dec, time,
                      band="other", instrumentid=57, depth=21, depth_unit="ab_mag",
                      pos_angle=0, status="completed"):

    pointings = []
    for i in range(len(ra)):
        pointings.append(prepare_pointing(ra[i], dec[i], time[i], band, instrumentid, depth, depth_unit,
                                          pos_angle, status))
    json_data = {
        "graceid": f"{graceid}",
        "api_token": f"{api_token}",
        "pointings": pointings
    }
    return json_data


def post(json_data, base="http://treasuremap.space/api/v0/", target="pointings"):
    r = requests.post(url=base + target, json=json_data)
    print(r.text)


def submit_nightly_pointings(date, graceid, api_token=config.get("TREASUREMAP", "APITOKEN"),
                             telescope="C28", instrumentid=57,
                             band="other", depth=21, depth_unit="ab_mag",
                             pos_angle=0, status="completed",
                             base=config.get("TREASUREMAP", "BASE"), target=config.get("TREASUREMAP", "TARGET")):
    ra, dec, t = get_observed_target_list(date, telescope)
    if telescope == "C28":
        instrumentid = 57
    elif telescope == "C18":
        instrumentid = 58

    json_data = prepare_json_data(graceid, api_token, ra, dec, t,
                                  band, instrumentid, depth, depth_unit,
                                  pos_angle, status)
    post(json_data, base, target)
