pause

set ASTROPACK_PATH=C:\Ultrasat\AstroPack.git
set ASTROPACK_DATA_PATH=C:\AstroPack\Data
set ASTROPACK_CONFIG_PATH=C:\AstroPack\Config
set ULTRASAT_PACK=C:\Ultrasat\Ultrasat.git


mkdir %ASTROPACK_PATH%
mkdir %ASTROPACK_DATA_PATH%
mkdir %ASTROPACK_CONFIG_PATH%
mkdir %ULTRASAT_PACK%

soc_snr_matlab.exe

