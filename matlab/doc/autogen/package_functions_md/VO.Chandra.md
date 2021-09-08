# Package: VO.Chandra


### VO.Chandra.acis_psf

Read and interpolate the Chandra ACIS-S/ACIS-I PSF. Package: VO.Chandra Description: Read and interpolate the Chandra ACIS-S/ACIS-I PSF from the Chandra CalDB files.


    
    Read and interpolate the Chandra ACIS-S/ACIS-I PSF.  
    Package: VO.Chandra  
    Description: Read and interpolate the Chandra ACIS-S/ACIS-I PSF from  
    the Chandra CalDB files.  
    Instellation: 1. Install the Chandra CalDB directory including the 2d  
    PSFs.  
    2. Modify the DefV.CalDBdir to direct to the Chandra CalDB  
    directory.  
    Input  : - Energy [keV]  
    - Detector X position (DetX) [pix].  
    The center of the detector is at 4096.5  
    - Detector Y position (DetY) [pix].  
    The center of the detector is at 4096.5  
    * Arbitrary number of pairs of ...,key,val,... arguments.  
    The following keywords are available:  
    'Camera'  - {'ACIS-S','ACIS-I'}. Default is 'ACIS-S'.  
    'FileName'- PSF file name in the CalDB directory.  
    Default is 1 for 'aciss1998-11-052dpsf1N0002.fits'.  
    1 - in the range of -10:1:+10 arcmin  
    pix scale 6 micron, 256x256 PSF image, 580 Mb  
    2 - in the range of -25:5:+10 arcmin  
    pix scale 12 micron, 512x512 PSF image, 204 Mb  
    3 - in the range of -6:1:+6 arcmin  
    pix scale 2 micron, 512x512 PSF image, 890 Mb  
    4 - in the range of -1:1:+1 arcmin  
    pix scale 1 micron, 512x512 PSF image, 46 Mb  
    'SaveFITS'- A string of a FITS file name in which to save  
    the PSF. If empty matrix then do not save file.  
    Default is empty matrix.  
    Output : - 2D matrix containing the Chandra PSF at the requested energy  
    and position.  
    - Vector of X coordinates corresponding to the PSF image in  
    units of pixels (pixel scale is 0.492"/pix).  
    - Vector of Y coordinates corresponding to the PSF image in  
    units of pixels (pixel scale is 0.492"/pix).  
    Tested : Matlab 2011b  
    By : Eran O. Ofek                    Jan 2013  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: [PSF,X,Y]=VO.Chandra.acis_psf(1,4090,4090,'FileName',1);  
    Reliable: 2  
      
      
### VO.Chandra.build_obsid_cat

Construct a catalog of all Chandra observations Package: VO.Chandra Description: Construct a catalog of all Chandra observations by going over the entire Chandra image archive.


    
    Construct a catalog of all Chandra observations  
    Package: VO.Chandra  
    Description: Construct a catalog of all Chandra observations by going  
    over the entire Chandra image archive.  
    Input  : * Arbitrary number of pairs of arguments: ...,keyword,value,...  
    where keyword are one of the followings:  
    'AO'      - AO to download. Default is 'ao01'.  
    'GetInfo' - Get information [JD, RA, Dec] for each ObsID  
    in addition to the OBSID and its location  
    {true|false}. Default is true.  
    'Verbose'  - {true|false}. Default is true.  
    'OutType'  - Output type:  
    'struct' - structure array.  
    'AstCat' - AstCat object (default).  
    'SaveDir'  - Directory in which to save the catalog.  
    If empty, then do not save.  
    Default is '~/matlab/data/+cats/+X/'.  
    'SaveName' - File name in which to save the catalog.  
    Default is 'ChandraObs_s.mat', where s is the AO.  
    Output : - Structure array or AstCat object of all the Chandra observations.  
    Tested : Matlab R2014a  
    By : Eran O. Ofek                    Jan 2015  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: Cat=VO.Chandra.build_obsid_cat('ao','ao10');  
    after you have the catalogs for all AO, you can collect them  
    into a single catalog  
    Cat=VO.Chandra.build_obsid_cat('collect',true);  
    Reliable: 2  
      
      
### VO.Chandra.ciao_extractspec

Prepare the ARF and MRF Chandra files required for X-ray spectroscopy. Package: VO.Chandra Description: Use Chandra/CIAO tasks to prepare the ARF and MRF files required for X-ray spectroscopy of a source.


    
    Prepare the ARF and MRF Chandra files required for X-ray spectroscopy.  
    Package: VO.Chandra  
    Description: Use Chandra/CIAO tasks to prepare the ARF and MRF files  
    required for X-ray spectroscopy of a source.  
    Instellation: ciao is required.  
    Input  : - The Chandra level 2 event file.  
    - J2000.0 RA of the source for which the spectrum should be  
    extracted.  
    - J2000.0 Dec of the source for which the spectrum should be  
    extracted.  
    * Arbitrary pairs of ...,key,val,... arguments.  
    The following keywords are available:  
    'Aper'    - Object extraction radius [pixels]. Default is 2.5.  
    'Annulus' - sky annulus in for which to calculate the  
    ARF/MRF files. Default is [40 100] pixels.  
    'CooSys'  - Coordinate system of source {'eq','xy'}.  
    Default is 'eq'.  
    'OutRoot' - Output directory. Default is 'spec'.  
    Output : - Number of counts within aperture.  
    Tested : Matlab 2011b  
    By : Eran O. Ofek                    Mar 2013  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: VO.Chandra.ciao_extractspec('acisf11122N002_evt2.fits',X,Y,'CooSys','XY')  
    Reliable: 2 (Modification were not tested)  
      
      
      
      
    start FTOOLS/HEASOFT/CIAO  
### VO.Chandra.run_ciao_command

RUN CIAO command on single or multiple, or all Chandra directories Package: +VO.Chandra


    
    RUN CIAO command on single or multiple, or all Chandra directories  
    Package: +VO.Chandra  
    Input  : * Arbitrary number of pairs of arguments: ...,keyword,value,...  
    where keyword are one of the followings:  
    'Dir'   - If empty then use all directories in cats.X.ChnadraObs  
    If an AstCat object, then assume this is has the same  
    format as cats.X.ChnadraObs.  
    Default is [].  
    'BasePath' - Base path in which the Chandra data is located.  
    Default is '/euler/eran/work/Chandra'.  
    'CIAO' - CIAO program name.  
    Default is 'ciao412'.  
    'Command' - Command to run on each data directory.  
    Default is 'chandra_repro indir=./ outdir=./'.  
    Output : - A cell array of full path on which the commad was executed.  
    By : Eran Ofek                       Aug 2020  
    Example: FullPath=VO.Chandra.run_ciao_command  
      
      
      
### VO.Chandra.wget_all

wget all Chandra observations in cats.X.ChandraObs Package: VO Description:


    
    wget all Chandra observations in cats.X.ChandraObs  
    Package: VO  
    Description:  
    Input  : -  
    * Arbitrary number of pairs of arguments: ...,keyword,value,...  
    where keyword are one of the followings:  
    Output : -  
    License: GNU general public license version 3  
    By : Eran O. Ofek                    Feb 2020  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: VO.Chandra.wget_all  
    Reliable:  
      
      
      
    DefV. =  
    InPar = InArg.populate_keyval(DefV,varargin,mfilename);  
      
      
### VO.Chandra.wget_obsid

Get all the files associated with a Chandra ObsID Package: VO.Chandra Description: Get all the files associated with a Chandra ObsID The Chandra observations catalog is stored in


    
    Get all the files associated with a Chandra ObsID  
    Package: VO.Chandra  
    Description: Get all the files associated with a Chandra ObsID  
    The Chandra observations catalog is stored in  
    cats.X.ChandraObs.  
    Use VO.Chandra.build_obsid_cat to build catalog.  
    Input  : - ObsID  
    * Arbitrary number of pairs of arguments: ...,keyword,value,...  
    where keyword are one of the followings:  
    'Download' - Indicating what to download:  
    'all' - all data (default).  
    'primary' - only data in primary directory.  
    'secondary' - only data in secondary directory.  
    'Output'   - Output data in a 'flat' structure' or  
    'dir' (directory) structure.  
    'none' - do not copy.  
    Default is 'dir'.  
    'Ungzip'   - ungzip data {true|false}. Default is true.  
    'CopyTo'   - Directory in which to cd before copying data.  
    'ReGet'    - Get files again if 'primary' dir exist  
    in directory {true|false}. Default is false.  
    'Extra'    - Extra parameters for wget.  
    'MaxGet'   - Maximum number of files for parallel get  
    (see www.pwget.m). Default is 10.  
    Output : null  
    Tested : Matlab R2014a  
    By : Eran O. Ofek                    Jan 2015  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: VO.Chandra.wget_obsid(366)  
    Reliable: 2  
      
      
    DefV.ChandraCat    = [];  
### VO.Chandra.wget_obsid_old

Get all the files associated with a Chandra ObsID Package: VO.Chandra Description: Get all the files associated with a Chandra ObsID The Chandra observations catalog is stored in


    
    Get all the files associated with a Chandra ObsID  
    Package: VO.Chandra  
    Description: Get all the files associated with a Chandra ObsID  
    The Chandra observations catalog is stored in  
    cats.X.ChandraObs.  
    Use VO.Chandra.build_obsid_cat to build catalog.  
    Input  : - ObsID  
    * Arbitrary number of pairs of arguments: ...,keyword,value,...  
    where keyword are one of the followings:  
    'Download' - Indicating what to download:  
    'all' - all data (default).  
    'primary' - only data in primary directory.  
    'secondary' - only data in secondary directory.  
    'Output'   - Output data in a 'flat' structure' or  
    'dir' (directory) structure.  
    'none' - do not copy.  
    Default is 'dir'.  
    'Ungzip'   - ungzip data {true|false}. Default is true.  
    'CopyTo'   - Directory in which to cd before copying data.  
    'ReGet'    - Get files again if 'primary' dir exist  
    in directory {true|false}. Default is false.  
    'Extra'    - Extra parameters for wget.  
    'MaxGet'   - Maximum number of files for parallel get  
    (see www.pwget.m). Default is 10.  
    Output : null  
    Tested : Matlab R2014a  
    By : Eran O. Ofek                    Jan 2015  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: VO.Chandra.wget_obsid(366)  
    Reliable: 2  
      
