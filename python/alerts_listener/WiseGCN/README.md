# WiseGCN

A GCN/TAN (Gamma-ray Coordinates Network/Transient Astronomy Network) handler for use at the Wise Observatory in case of gravitational-wave alerts.

## Getting started

### Prerequisites

* `miniconda` or `anaconda` with `python 3`
* `mysql`
* `git`

### Installing

Create and activate a `conda` environment (named `gw`) with the necessary modules:
```
$ conda create -p /path/to/gw python=3.7.1
$ source activate /path/to/gw
$ pip install pygcn healpy configparser voevent-parse pymysql lxml ccdproc
$ pip install git+https://github.com/naamach/schedulertml.git
$ pip install git+https://github.com/naamach/wisegcn.git
```
Setup the `mysql` database according to [these instructions](docs/mysql.md).


#### Upgrading
To upgrade `wisegcn` run:
```
$ pip install git+https://github.com/naamach/wisegcn.git --upgrade
```

#### The configuration file

Finally, you will have to provide `wisegcn` with the database credentials and point it to the catalog file and to the directory where you want it to store the event `FITS` files.
To do so, you will need to have a `config.ini` file in the working directory (the directory from which you run the script).
The file should look like that (see `config.ini.example` in the main directory):
```
; config.ini
[GENERAL]
TEST = False ; True - listen ONLY test alerts, change to False to listen to real alerts
BNS_MIN = 0 ; minimal allowed probability of binary neutron star alerts (not including)
NSBH_MIN = 0 ; minimal allowed probability of black hole-neutron star alerts (not including)
BBH_MIN = 999 ; minimal allowed probability of black hole-black hole alerts (not including)
MASSGAP_MIN = 0 ; minimal allowed probability of mass-gap merger alerts (not including)
HASNS_MIN = 0 ; minimal allowed probability the event involved a neutron star (not including)
HASREMNANT_MIN = 0 ; minimal allowed probability the system ejected a non-zero amount of neutron star matter (not including)
TERRESTRIAL_MAX = 999 ; maximal allowed probability of terrestrial alerts (including)
FAR_MAX = 999 ; maximal allowed false alarm rate (including) [1/yr]
AREA_MAX = 999999 ; [deg^2] maximal allowed sky area (including; if the localization is worse, drop the alert)
AREA_CREDZONE = 0.9 ; localization probability to consider credible for AREA_MAX

[LOG]
PATH = /path/to/log/
CONSOLE_LEVEL = DEBUG ; DEBUG, INFO, WARNING, ERROR, CRITICAL
FILE_LEVEL = DEBUG ; DEBUG, INFO, WARNING, ERROR, CRITICAL

[CATALOG]
PATH = /path/to/catalog/
NAME = glade_2.3_RA_Dec

[EMAIL]
FROM = root@example.com
TO = user@example.com
CC = 
BCC = 
SERVER = localhost

[DB]
HOST = localhost
USER = gcn
PASSWD = password
DB = gw
SOCKET = /var/run/mysqld/mysqld.sock

[ALERT FILES]
PATH = /path/to/alerts/

[EVENT FILES]
PATH = /path/to/ligoevent_fits/

[GALAXIES]
CREDZONE = 0.99
RELAXED_CREDZONE = 0.99995
NSIGMAS_IN_D = 3
RELAXED_NSIGMAS_IN_D = 5
COMPLETENESS = 0.5
MINGALAXIES = 100
MAXGALAXIES = 500 ; number of best galaxies to use
MAXGALAXIESPLAN = 100 ; maximal number of galaxies per observation plan
MINMAG = -12 ; magnitude of event in r-band
MAXMAG = -17 ; magnitude of event in r-band
SENSITIVITY = 22
MINDISTFACTOR = 0.01 ; reflecting a small chance that the theory is completely wrong and we can still see something
ALPHA = -1.07 ; Schechter function parameters
MB_STAR = -20.7 ; Schechter function parameters, random slide from https://www.astro.umd.edu/~richard/ASTRO620/LumFunction-pp.pdf but not really...?

[TILE]
CREDZONE = 0.9  ; credible region to cover in tiles
AREA_MAX = 30  ; [deg^2] maximal credible area to tile (including; if larger, observe individual galaxies instead)
SIZE = 0.9  ; tile size in percentage of FOV

[OBSERVING]
SUN_ALT_MAX = -12
BESTEFFORTS = 1
USER = New Observer
EMAIL = user@example.com
DESCRIPTION = 
SOLVE = 1

[WISE]
LAT = 30.59583333333333
LON = 34.763333333333335
ALT = 875
UTC_OFFSET = -2
TELESCOPES = C28  ; C28, C18, 1m
PATH = /path/to/plans/
OBS_PATH = /path/to/observed_fits_images

[C28]
FOV = 1  ; [deg^2] FLI field of view
AIRMASS_MIN = 1.02  ; the shutter blocks the CCD above 80deg
AIRMASS_MAX = 3
HOURANGLE_MIN = -4.83
HOURANGLE_MAX = 4.83
MIN_LUNAR_DIST = 30 ; [deg] minimal lunar distance
FILTER = Luminance
EXPTIME = 300
BINNING = 1
HOST = c28_computer_name ; leave blank to skip plan upload to remote host
USER = username
CYGWIN_PATH = C:\cygwin64\home\username\
PATH = /home/username/
OBS_DIR = C28backup

[C18]
FOV = 1  ; [deg^2] SBIG field of view
AIRMASS_MIN = 1
AIRMASS_MAX = 3
HOURANGLE_MIN = -5.3
HOURANGLE_MAX = 5.3
MIN_LUNAR_DIST = 30 ; [deg] minimal lunar distance
FILTER = clearx
EXPTIME = 300
BINNING = 1
HOST = c18_computer_name ; leave blank to skip plan upload to remote host
USER = username
CYGWIN_PATH = C:\cygwin64\home\username\
PATH = /home/username/
OBS_DIR = C18backup

[1m]
FOV = 0.2158  ; [deg^2] PI field of view
AIRMASS_MIN = 1
AIRMASS_MAX = 3
HOURANGLE_MIN = -12
HOURANGLE_MAX = 12
MIN_LUNAR_DIST = 30 ; [deg] minimal lunar distance
FILTER = Clear
EXPTIME = 300
BINNING = 1
HOST = 1m_computer_name ; leave blank to skip plan upload to remote host
USER = username
CYGWIN_PATH = C:\cygwin64\home\username\
PATH = /home/username/
OBS_DIR = 

[IERS]
URL = ftp://cddis.gsfc.nasa.gov/pub/products/iers/finals2000A.all ; IERS table URL (default is: http://maia.usno.navy.mil/ser7/finals2000A.all)

[TREASUREMAP]
BASE = http://treasuremap.space/api/v0/
TARGET = pointings
APITOKEN = 
```

NOTE: To find the `mysql` socket, run:
```
$ netstat -ln | grep mysql
```

The IERS URL option is to solve timeout problems of `wisegcn.observing_tools` functions using `astropy.utils.iers` tables (https://docs.astropy.org/en/stable/utils/iers.html), when the default `http://maia.usno.navy.mil/ser7/finals2000A.all` URL is down.

## Using `wisegcn`

To listen and process public events run (while the `gw` `conda` environment is activated):


```
$ wisegcn-listen
```

This will listen for VOEvents until killed with ctrl+C.

Alternativey, from inside `python` run:

```
import gcn
from wisegcn.handler import process_gcn

print("Listening to GCN notices (press Ctrl+C to kill)...")
gcn.listen(handler=process_gcn)
```

General usage of `wisegcn-listen`:
```
usage: wisegcn-listen [-h] [-c config_file] [-l log_file]

Listen to GCN/TAN VOEvents, respond to GW alerts, and prepare them for
followup observations at the Wise Observatory.

optional arguments:
  -h, --help            show this help message and exit
  -c config_file, --config config_file
                        path to config.ini file (default: config.ini). NOTE:
                        the config file will be copied to the current
                        directory as "config.ini
  -l log_file, --log log_file
                        path to the log file (default: pygcn.log)

```

### Running `wisegcn` offline on a past alert

To run `wisegcn` offline on, e.g., S190814bv-5-Update, run:

```
$ wisegcn-ingest S190814bv-5-Update
```

General usage of `wisegcn-ingest`:
```
usage: wisegcn-ingest [-h] [-c config_file] event_name

Run WiseGCN offline on a specific GW alert, and prepare it for followup
observations at the Wise Observatory.

positional arguments:
  event_name            either a local path to event xml file; or the event
                        name (e.g. S190814bv-5-Update) to download from
                        GraceDB

optional arguments:
  -h, --help            show this help message and exit
  -c config_file, --config config_file
                        path to config.ini file (default: config.ini). NOTE:
                        the config file will be copied to the current
                        directory as 'config.ini'
```

Alternativey, download the event file manually, e.g.:
```
$ curl -O https://gracedb.ligo.org/apiweb/superevents/S190814bv/files/S190814bv-5-Update.xml
```
Then, from inside `python` run (while the `gw` `conda` environment is activated):

```
from wisegcn.handler import process_gcn
import lxml.etree

print("Assuming S190814bv-5-Update.xml is in the working directory")
filename = 'S190814bv-5-Update.xml'

payload = open(filename, 'rb').read()
root = lxml.etree.fromstring(payload)
process_gcn(payload, root)
```

## Additional utilities

You can use `wisegcn` to check the healpix probability of a specific location (based on RA, Dec only, not taking the distance into account), and the localization sky area. These utilities are also Python 2.7 compatible.

### Get healpix probability based on location

```
from wisegcn.utils import get_coo_healpix_probability

ra = 123.45  # [deg]
dec = 12.345  # [deg]
skymap = "/path/to/bayestar.fits.gz"
p = get_coo_healpix_probability(ra, dec, skymap)
```

### Get healpix probability based on GladeID

This part assumes you have configured the `[CATALOG]` part in the `config.ini` file, and the Glade catalog `.npy` file.

```
from wisegcn.utils import get_galaxy_healpix_probability

glade_id = 12345
skymap = "/path/to/bayestar.fits.gz"
p = get_galaxy_healpix_probability(glade_id, skymap)
```

### Get localization sky area based on healpix probability
```
from wisegcn.utils import get_sky_area

credzone = [0.5, 0.9]  # localization probability to consider credible (could be a scalar or a list)
skymap = "/path/to/bayestar.fits.gz"
area = get_sky_area(skymap, credzone)  # [deg^2]
```
## Gravitational Wave Treasure Map
The [Gravitational Wave Treasure Map](http://treasuremap.space/) is designed to help coordinate electromagnetic followup of gravitational-wave events.

### Submit telescope pointings to Treasure Map

After observing, you can upload to the Treasure Map the C28 telescope pointings observed for, e.g., the LIGO event S191216ap on 2019 December 17 by running:

```
from wisegcn.treasuremap import submit_nightly_pointings

submit_nightly_pointings("20191217", "S191216ap", telescope="C28")
```

NOTE: This should be run from a computer that has access to the FITS images, located at `WISE/OBS_PATH` (as defined in the `config.ini` file).
Also, make sure to fill-in the `TREASUREMAP/APITOKEN` keyword in the `config.ini` file with your Treasure Map API token (found under your `Profile` page on the [Treasure Map website](http://treasuremap.space/manage_user)).

## Acknowledgments
Leo P. Singer, Scott Barthelmy, David Guevel, Michael Zalzman, Sergiy Vasylyev.

`wisegcn` is based on [svasyly/pygcn](https://github.com/svasyly/pygcn), which is based on [lpsinger/pygcn](https://github.com/lpsinger/pygcn).
