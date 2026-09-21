#!/bin/bash
# Regenerate every DESY product on the corrected TIFF split (counter columns excluded)
set -u
cd /home/sasha/matlab
log=/home/sasha/claude/desy_regenerate_all.log
echo "[$(date)] start" > $log
matlab -batch "run('/home/sasha/claude/desy_ptc_report_run.m')"  > /home/sasha/claude/desy_ptc_report_run.log 2>&1;   echo "[$(date)] 31/32 driver rc=$?" >> $log
matlab -batch "run('/home/sasha/claude/desy_ptc_report_zero.m')" > /home/sasha/claude/desy_ptc_report_zero.log 2>&1;  echo "[$(date)] zero stats rc=$?" >> $log
matlab -batch "run('/home/sasha/claude/desy_txscan_run.m')"      > /home/sasha/claude/desy_txscan_run.log 2>&1;       echo "[$(date)] TX driver rc=$?" >> $log
rm -rf /Data1/DESY_FITS
matlab -batch "run('/home/sasha/claude/desy_fits_export.m')"     > /home/sasha/claude/desy_fits_export.log 2>&1;      echo "[$(date)] FITS export rc=$?" >> $log
echo "[$(date)] ALL DONE" >> $log
