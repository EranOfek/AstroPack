#
# Currently unused
#

import math
from datetime import datetime, timedelta, timezone, date as dt_date, time as dt_time

from bitfields_utils import encode_bit_fields, decode_bit_fields, get_total_bits, julian_to_datetime, datetime_to_julian

def log(s):
    print(s)

# ===========================================================================

# ImageID - Int64 for ClickHouse tables - see make_image_id()
image_id_bitfields_def = {
    'timestamp': 30,        # Custom timestamp, see BASE_DATETIME
    'cam_id': 10,           # Camera ID - depends on project implementation
    'crop_id': 13,          # Crop ID from raw image
    'ver_id': 8,            # Version ID (pipeline, reprocessing) - see table version
    'img_ver': 2            # Image version in case that we have multiple versions of the same image
}

# Verify that we do not exceed Int64
TOTAL_BITS = get_total_bits(image_id_bitfields_def)
if TOTAL_BITS > 64:
    raise ValueError(f"Total bits in image_id_bitfields_def exceed 64, check or consider to use Int128 for IDs: {TOTAL_BITS}")


# Base year constant
BASE_YEAR = 2023
BASE_DATETIME = datetime(BASE_YEAR, 1, 1)

# ---------------------------------------------------------------------------

# Function to create image ID
def make_image_id(dt, cam_id=0, crop_id=0, ver_id=0):

    timestamp = int((dt - BASE_DATETIME).total_seconds())
    image_id = encode_bit_fields({'timestamp': timestamp, 'cam_id': cam_id, 'crop_id': crop_id, 'ver_id': ver_id}, image_id_bitfields_def, msb=True)
    return image_id


def make_image_id_jd(jd, cam_id=0, crop_id=0, ver_id=0):

    dt = julian_to_datetime(jd)
    timestamp = int((dt - BASE_DATETIME).total_seconds())
    image_id = encode_bit_fields({'timestamp': timestamp, 'cam_id': cam_id, 'crop_id': crop_id, 'ver_id': ver_id}, image_id_bitfields_def, msb=True)
    return image_id


# ===========================================================================

# ===========================================================================

def main():
    for year in range(2023, 2045):
        dt = datetime(year, 1, 1)
        id = make_image_id(dt=dt, cam_id=1, crop_id=1, ver_id=1)
        print(f"year: {id} = {id:X}")


    return

if __name__ == '__main__':
    main()
