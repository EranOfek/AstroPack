# ===========================================================================
# gcsmain.py - Main file for GCS Interface and Simulator
#
# Usage:
#
#   python gcsmain.py
#
#
# pip freeze > requirements.txt
#
# ===========================================================================

import os, glob, time, argparse, shutil, csv, json, yaml, openpyxl
from datetime import datetime
from sys import platform
from gcsbase import Component

from gcsifc import GcsInterface
from gcssim import GcsSimulator

#------------------------------- Main -------------------------------

def main():

    # Read command line options
    parser = argparse.ArgumentParser()
    parser.add_argument('-sim', dest='simulator', action='store_true',     default=False,   help='Simulator')
    args = parser.parse_args()

    if args.simulator:
        simulator = GcsSimulator(args)
        simulator.run()
    else:
        interface = GcsInterface(args)
        interface.run()


if __name__ == '__main__':
    main()
