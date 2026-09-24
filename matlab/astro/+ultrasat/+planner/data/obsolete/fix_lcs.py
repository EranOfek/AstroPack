#!/usr/bin/env python3

"""
Convert LCS_grid.txt to simplified format:
Input:
Field,RA,Dec,...

Output:
Name,RA,Dec
LCS_<Field>,RA,Dec
"""

import csv
import os


def convert_file(input_file, output_file):
    """
    :param input_file: Path to input CSV file
    :param output_file: Path to output CSV file
    """

    if not os.path.exists(input_file):
        print(f"Error: file not found: {input_file}")
        return

    with open(input_file, 'r', newline='') as fin, \
         open(output_file, 'w', newline='') as fout:

        reader = csv.DictReader(fin)
        writer = csv.writer(fout)

        # Write new header
        writer.writerow(['Name', 'RA', 'Dec'])

        for row in reader:
            field = row['Field']
            ra = row['RA']
            dec = row['Dec']

            name = f"LCS_{field}"

            writer.writerow([name, ra, dec])

    print(f"Done. Output written to: {output_file}")


if __name__ == "__main__":
    input_file = "LCS_grid.txt"
    output_file = "LCS_fixed.txt"

    convert_file(input_file, output_file)