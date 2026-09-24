#
# Currently unused
#

import math
from datetime import datetime, timedelta, timezone, date as dt_date, time as dt_time

def log(s):
    print(s)

# ===========================================================================

def max_value(bits):
    if bits <= 0:
        return 0
    else:
        return (1 << bits) - 1


def calc_required_timestamp_bits(years, time_resolution=1):
    """
    Calculate the required number of bits to store a given number of years at a specified time resolution.

    :param years: Number of years
    :param time_resolution: Time resolution in seconds (1 for 1 second, 0.1 for 0.1 seconds, etc.)
    :return: Required number of bits
    """
    # Constants
    seconds_per_minute = 60
    minutes_per_hour = 60
    hours_per_day = 24
    days_per_year = 365.25
    total_time_units = years * days_per_year * hours_per_day * minutes_per_hour * seconds_per_minute / time_resolution
    required_bits = math.ceil(math.log2(total_time_units + 1))
    return required_bits


def calculate_years_storable(timestamp_bits, resolution_sec):
    """
    Calculate the number of years that can be stored given a timestamp's bit size and resolution in seconds.

    Parameters:
    - timestamp_bits: The number of bits allocated for the timestamp.
    - resolution_sec: The resolution of the timestamp in seconds.

    Returns:
    - The number of years that can be stored.
    """

    # Maximum value that can be stored in the given number of bits
    max_value = 2 ** timestamp_bits - 1

    # Total seconds representable by the timestamp_bits
    total_seconds = max_value * resolution_sec

    # Convert total seconds to total years, considering leap years on average
    # 1 year = 365.25 days on average, including leap years
    seconds_per_year = 365.25 * 24 * 60 * 60
    total_years = total_seconds / seconds_per_year

    return total_years


# Function to convert datetime to custom timestamp
def datetime_to_custom_timestamp(dt, base_year, timestamp_bits):
    base_datetime = datetime(base_year, 1, 1)
    delta = dt - base_datetime
    total_milliseconds = int(delta.total_seconds() * 1000)
    adjusted_timestamp = total_milliseconds % (2 ** timestamp_bits)
    return adjusted_timestamp


# Function to reconstruct datetime from custom timestamp
def custom_timestamp_to_datetime(timestamp, base_year):
    base_datetime = datetime(base_year, 1, 1)
    return base_datetime + timedelta(milliseconds=timestamp)


JD_UNIX_EPOCH = 2440587.5  # Julian date at Unix epoch (1970-01-01)
DATETIME_UNIX_EPOCH = datetime(1970, 1, 1)

def julian_to_datetime(jd):
    """
    Convert a Julian Date to a datetime object.

    Parameters:
    - jd (float): Julian Date.

    Returns:
    - datetime.datetime: The corresponding datetime object.
    """

    days_since_epoch = jd - JD_UNIX_EPOCH
    dt = DATETIME_UNIX_EPOCH + timedelta(days=days_since_epoch)
    return dt


def datetime_to_julian(dt):
    """
    Convert a datetime object to a Julian Date.

    Parameters:
    - dt (datetime.datetime): The datetime object to convert.

    Returns:
    - float: The corresponding Julian Date.
    """
    delta = dt - DATETIME_UNIX_EPOCH
    jd = JD_UNIX_EPOCH + delta.total_seconds() / 86400.0
    return jd


def test_julian():
    # Convert Julian Date to datetime
    jd = 2459580.5  # Example Julian Date
    dt = julian_to_datetime(jd)
    print(f"Julian Date {jd} to datetime: {dt}")

    # Convert datetime to Julian Date
    dt = datetime.now()
    jd = datetime_to_julian(dt)
    print(f"datetime {dt} to Julian Date: {jd}")



def encode_bit_fields(values, definition, msb=True):
    """
    Encodes a dictionary of field values into a single integer.

    Parameters:
    - values (dict): Dictionary of field values.
    - definition (dict): Dictionary specifying the number of bits per field.

    Returns:
    - int: The encoded integer.

    Raises:
    - ValueError: If a value does not fit in the specified number of bits.
    """

    encoded = 0
    current_bit = 0
    items = reversed(list(definition.items())) if msb else definition.items()
    for field, bits in items:
        value = values.get(field, 0)
        if value < 0 or value >= 2 ** bits:
            raise ValueError(f"Value {value} for field '{field}' does not fit in {bits} bits")

        encoded |= value << current_bit
        current_bit += bits

    return encoded


def decode_bit_fields(encoded, definition, msb=True):
    """
    Decodes an encoded integer back into a dictionary of field values based on the field definition.

    Parameters:
    - encoded (int): The encoded integer.
    - definition (dict): Dictionary specifying the number of bits per field.

    Returns:
    - dict: A dictionary with field names as keys and decoded values as values.
    """
    decoded_values = {}
    current_bit = 0
    items = reversed(list(definition.items())) if msb else definition.items()
    for field, bits in items:
        mask = (1 << bits) - 1
        value = (encoded >> current_bit) & mask
        decoded_values[field] = value
        current_bit += bits

    return decoded_values


def get_total_bits(definition):
    total_bits = 0
    for _, bits in definition.items():
        total_bits += bits

    return total_bits


def test_encode_int():
    definition = {
        'field1': 4,  # 4 bits for field1
        'field2': 8,  # 8 bits for field2
        'field3': 4,  # 4 bits for field3
        'field4': 8,  # 8 bits for field4
    }

    values = {
        'field1': 3,
        'field2': 6,
        'field3': 14,
        'field4': 100,
    }

    # Encoding
    encoded = encode_bit_fields(values, definition)
    print(f"Encoded value: {encoded} = {encoded:X}")

    # Decoding
    decoded = decode_bit_fields(encoded, definition)
    print(f"Decoded values: {decoded}")

    # Test with a different field order
    definition_reversed = {
        'field4': 8,
        'field3': 4,
        'field2': 8,
        'field1': 4,
    }

    # Encoding with reversed definition
    encoded_reversed = encode_bit_fields(values, definition_reversed)
    print(f"Encoded value (reversed order): {encoded_reversed} = {encoded_reversed:X}")

    # Ensure different results for different orders
    assert encoded != encoded_reversed, "Encoding should differ for different field orders"

# ===========================================================================

# ===========================================================================

def main():
    test_julian()
    test_encode_int()
    return


if __name__ == '__main__':
    main()
