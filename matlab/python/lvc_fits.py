# ----------------------------------------------------------------------------
# Project
# Module:
# File:
# Title:   LVC Listener and processor
# Author:  Chen Tishler, 03/2024
#
# ----------------------------------------------------------------------------
# -f c:/soc/incoming_alerts/lvc/listener/lvc_2023_07_01_05_14_49_573910.json
# ----------------------------------------------------------------------------

import argparse
import os, json, sys
from astropy.table import Table
import astropy_healpix
from base64 import b64decode
from io import BytesIO
import numpy as np

# ===========================================================================

# ===========================================================================

class LvcExtractFits:
    """

    """

    def __init__(self):
        return

    # -----------------------------------------------------------------------
    def save_skymap_to_fits_file(self, alert, fits_filename, csv_filename):
        """
        Decode the SkyMap from message, use AstroPy HealPix to extract data such as ra/dec.

        :param alert:
        :param filename:
        :return:
        """

        if 'skymap' in alert['event'] and alert['event']['skymap'] is not None and alert['event']['skymap'] != '(stripped)':
            try:
                skymap_bytes = b64decode(alert['event']['skymap'])

                # Convert SkyMap to FITS
                skymap = Table.read(BytesIO(skymap_bytes), format='fits')

                # Save the entire skymap as a FITS file
                if fits_filename:
                    skymap.write(fits_filename, format='fits', overwrite=True)

                # Determine the HEALPix NSIDE from the level of the highest PROBDENSITY pixel
                highest_prob_index = np.argmax(skymap['PROBDENSITY'])
                level, ipix = astropy_healpix.uniq_to_level_ipix(skymap[highest_prob_index]['UNIQ'])
                nside = astropy_healpix.level_to_nside(level)

                # Convert HEALPix indices to RA, DEC
                ra, dec = astropy_healpix.healpix_to_lonlat(skymap['UNIQ'], nside, order='nested')

                # Add RA and DEC to the skymap table
                skymap['RA'] = ra.deg
                skymap['DEC'] = dec.deg

                # Select the relevant columns
                data_to_save = skymap['UNIQ', 'PROBDENSITY', 'RA', 'DEC']

                # Convert to a pandas DataFrame (for easier CSV writing)
                df = data_to_save.to_pandas()

                # Save to CSV
                if csv_filename:
                    df.to_csv(csv_filename, index=False)

                return True

            except Exception as ex:
                self.log('Failed to parse skymap', ex=ex)

        return False

    # =======================================================================

    def process(self, alert_filename):
        fits_filename = alert_filename.rsplit('.', 1)[0] + '.fits'
        csv_filename = alert_filename.rsplit('.', 1)[0] + '.csv'
        self.log(f'Saving skymap to fits file: {alert_filename} - {fits_filename}')

        try:
            with open(alert_filename, 'r') as f:
                notice = f.read()
            alert = json.loads(notice)
            self.save_skymap_to_fits_file(alert, fits_filename, csv_filename)
        except json.JSONDecodeError as ex:
            self.log(f'save_skymap_to_fits_file: ', ex=ex)
            return False


    def main(self):
        parser = argparse.ArgumentParser(
            prog=os.path.basename(sys.argv[0]),
            description='Processes incoming Kafka GCN alerts',
        )

        parser.add_argument('-m', dest='skymap', action='store', default=None, help='')
        args = parser.parse_args()

        # Save skymap to fits file
        if args.skymap:
            alert_filename =  args.skymap
            fits_filename = alert_filename + '.fits'
            self.log(f'Saving skymap to fits file: {args.skymap} - {fits_filename}')

            try:
                with open(alert_filename, 'r') as f:
                    notice = f.read()
                alert = json.loads(notice)
                self.save_skymap_to_fits_file(alert, fits_filename)
            except json.JSONDecodeError as ex:
                self.log(f'save_skymap_to_fits_file: ', ex=ex)
                return False


    def log(self, msg, ex=None):
        if ex:
            msg += ' - ' + str(ex)

        print(msg)

# ===========================================================================

# ===========================================================================

def main():
    proc = LvcExtractFits()
    proc.process('c://temp//alert1.json')
    #proc.main()


if __name__ == '__main__':
    main()
