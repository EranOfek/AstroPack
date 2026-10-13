# Running insertDB scripts on euclid: missing AstroPack data files

Running `pipeline.last.insertDB.insertArchiveCatalogs2DB*` on **euclid** (user `euclid`)
failed because some AstroPack data files were missing. On WRX80 these files live under
`~/matlab/data`, which is a symlink to `/Data1/AstroPackData`. On euclid they go under
`/home/euclid/matlab/data/`.

These files are found **by bare filename on the MATLAB path**, not by an absolute path.
After copying, the folders must be on the path (restart MATLAB, or `addpath(genpath(...))`).

## Files that were missing

| # | Error on euclid | Missing file(s) | Source on WRX80 |
|---|---|---|---|
| 1 | `Unable to find file or directory 'inpop21a_TDB_m100_p100_mat_pos_Ear.mat'` | INPOP ephemeris tables, 100-yr `.mat` set (`pos_*`, `vel_*`, `TT`), about 44 MB |
`/Data1/AstroPackData/SolarSystem/INPOP/inpop21a_TDB_m100_p100_mat_*.mat` |
| 2 | `readtable: Unable to find or open 'EOP_C01_IAU2000_1846-now.txt'` | Earth orientation parameters / time tables, about 7 MB | `/Data1/AstroPackData/SolarSystem/Time/` |
| 3 | `Dot indexing is not supported...` in `celestial.time.barycentricJD` at `IP.Constant.AU` | INPOP constants header. `celestial.INPOP.readConstants` only *warns* when it's missing, then leaves
`Constant = []` | `/Data1/AstroPackData/SolarSystem/INPOP/inpop21a_TDB_m100_p100_asc_header.asc` |

## Copy commands (run on WRX80)

```bash
# target dirs
ssh euclid@euclid 'mkdir -p ~/matlab/data/SolarSystem/INPOP ~/matlab/data/SolarSystem/Time'

# 1 + 3: INPOP 100-yr .mat tables and the constants header
rsync -av /Data1/AstroPackData/SolarSystem/INPOP/inpop21a_TDB_m100_p100_mat_*.mat \
		/Data1/AstroPackData/SolarSystem/INPOP/inpop21a_TDB_m100_p100_asc_header.asc \
	euclid@euclid:~/matlab/data/SolarSystem/INPOP/

# 2: time / EOP tables
rsync -av /Data1/AstroPackData/SolarSystem/Time/ euclid@euclid:~/matlab/data/SolarSystem/Time/
```

Optional: copy the other small SolarSystem folders (ELP, VSOPE87, MinorPlanets) in one go,
leaving out the large ones:

```bash
rsync -av --exclude='INPOP/' --exclude='MinorPlanetsCT/' \
	/Data1/AstroPackData/SolarSystem/ euclid@euclid:~/matlab/data/SolarSystem/
```

## Verify on euclid (MATLAB)

```matlab
addpath(genpath('~/matlab/data/SolarSystem'))   % or restart MATLAB
which inpop21a_TDB_m100_p100_mat_pos_Ear.mat
which inpop21a_TDB_m100_p100_asc_header.asc
which EOP_C01_IAU2000_1846-now.txt
IP = celestial.INPOP; IP.Constant.AU            % should print ~1.4960e+08
```

## Other files the script needs (already present on euclid)

`insertArchiveCatalogs2DB` also reads these by default:

- `Args.Template` = `~/matlab/data/db/Design-Database-Pipeline-ClickHouse.xlsx`
(WRX80 source: `/Data1/AstroPackData/db/`)
- `Args.AstroDBPassFile` = `~/.astropack/Passwords.yml` (DB credentials; keep it `chmod 600`)

