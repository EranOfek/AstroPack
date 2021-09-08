# Package: VO.prep


### VO.prep.build_PS1_htm_cat

build PS1 HDF5/HTM catalog


    
    build PS1 HDF5/HTM catalog  
      
### VO.prep.build_htm_catalog

Build an HTM catalog in HDF5 format for fast queries Package: @VO.prep Description: Given a catalog, prepare the catalog in HDF5 format sorted into HTM cells. This program is used for the construction of


    
    Build an HTM catalog in HDF5 format for fast queries  
    Package: @VO.prep  
    Description: Given a catalog, prepare the catalog in HDF5 format sorted  
    into HTM cells. This program is used for the construction of  
    fast access large catalogs. You can search such catalogs  
    using the VO.search.htmcat_search.  
    Input  : - Catalog, in array or AstCat format, with Long/Lat in radians.  
    * Arbitrary number of pairs of ...,key,val,... parameters.  
    The following keywords are available:  
    'Nsrc' - a matrix of [IndHTM Nsrc], where IndHTM is the HTM  
    index, and Nsrc is the number of sources in HTM.  
    'SaveCol' - Index of columns to save. If empty, then save all  
    columns. Default is empty.  
    'DecRange' - Dec range in between to save HTMs (radians).  
    Default is [-pi./2 pi./2]  
    'ColCell'  - Cell array of column names. Default is {}.  
    'ColUnits' - Cell array of column units. Default is {}.  
    'HTM_Level'- HTM level. Default is 7.  
    'ColRA'    - RA column.  
    'ColDec'   - Dec column.  
    'CatName'  - Catalog base name.  
    'NfilesInHDFf' - Number of HTM catalogs in each HDF file.  
    Default is 100.  
    'IndStep'  - Step for index file. Default is 30.  
    'SaveInd'  - Save index HDF file. Default is true.  
    Output : null  
    Example: [HTM,LevList] = celestial.htm.htm_build(5);  
    F=io.files.load2('FIRST.mat')  
    VO.prep.build_htm_catalog(F,'CatName','FIRST','HTM_Level',6);  
    Reliable: 2  
      
### VO.prep.download_galex




    
      
### VO.prep.get_transmission_curve

Read astronomical filters from WWW into an AstFilter object Package: VO.prep Description: Read astronomical filters from the filter profile service into an AstFilter object.


    
    Read astronomical filters from WWW into an AstFilter object  
    Package: VO.prep  
    Description: Read astronomical filters from the filter profile service  
    into an AstFilter object.  
    Input  : - Telescope name (see http://svo2.cab.inta-csic.es/svo/theory/fps3/index.php?mode=browse&gname=Swift  
    for options).  
    Output : - An AstFilter object.  
    License: GNU general public license version 3  
    By : Eran O. Ofek                    Nov 2017  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: AstF=VO.prep.get_transmission_curve  
    Reliable: 2  
      
      
      
    DefV. =  
    InPar = InArg.populate_keyval(DefV,varargin,mfilename);  
      
    Telescope = 'HST'  
      
      
### VO.prep.install_cats

Install the data/+cats catalog directory Package: VO.prep Description: Install the data/+cats catalog directory. Download


    
    Install the data/+cats catalog directory  
    Package: VO.prep  
    Description: Install the data/+cats catalog directory. Download  
    Input  : * Arbitrary number of pairs of arguments: ...,keyword,value,...  
    where keyword are one of the followings:  
    'Location' - Location in which to install the +cats directory.  
    Default is '~/matlab/data/'.  
    'Dir'      - The cats directory name. Default is '+cats'.  
    'URL'      - URL from which to get the cats files.  
    Default is  
    'https://webhome.weizmann.ac.il/home/eofek/matlab/data_cats.tar.gz'.  
    Output : null  
    License: GNU general public license version 3  
    By : Eran O. Ofek                    Dec 2018  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example:  
    Reliable:  
      
      
      
      
### VO.prep.prep_2mass_htm

prepare 2MASS catalog in HDF5/HTM format


    
    prepare 2MASS catalog in HDF5/HTM format  
      
      
    http://irsa.ipac.caltech.edu/2MASS/download/allsky/  
    http://irsa.ipac.caltech.edu/2MASS/download/allsky/format_psc.html  
      
### VO.prep.prep_DECaLS_htm

SHORT DESCRIPTION HERE Package: VO.prep Description:


    
    SHORT DESCRIPTION HERE  
    Package: VO.prep  
    Description:  
    Input  : -  
    * Arbitrary number of pairs of arguments: ...,keyword,value,...  
    where keyword are one of the followings:  
    Output : -  
    License: GNU general public license version 3  
    By : Eran O. Ofek                    Jan 2018  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: VO.prep.prep_DECaLS_htm  
    Reliable:  
      
      
### VO.prep.prep_NOAO_master

SHORT DESCRIPTION HERE Package: VO.prep Description:


    
    SHORT DESCRIPTION HERE  
    Package: VO.prep  
    Description:  
    Input  : -  
    * Arbitrary number of pairs of arguments: ...,keyword,value,...  
    where keyword are one of the followings:  
    Output : -  
    License: GNU general public license version 3  
    By : Eran O. Ofek                    Mar 2019  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example:  
    Reliable:  
      
      
### VO.prep.prep_atlas_htm

SHORT DESCRIPTION HERE Package: VO.prep Description:


    
    SHORT DESCRIPTION HERE  
    Package: VO.prep  
    Description:  
    Input  : -  
    * Arbitrary number of pairs of arguments: ...,keyword,value,...  
    where keyword are one of the followings:  
    Output : -  
    License: GNU general public license version 3  
    By : Eran O. Ofek                    Feb 2018  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: VO.prep.prep_atlas_htm  
    Reliable:  
      
      
### VO.prep.prep_binary_asteroid

Create a table of bknown binary asteroids Package: VO.prep Description: Create a table of bknown binary asteroids


    
    Create a table of bknown binary asteroids  
    Package: VO.prep  
    Description: Create a table of bknown binary asteroids  
    Input  : -  
    * Arbitrary number of pairs of arguments: ...,keyword,value,...  
    where keyword are one of the followings:  
    Output : -  
    License: GNU general public license version 3  
    By : Eran O. Ofek                    Jun 2018  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example:  
    Reliable:  
      
      
      
      
    DefV. =  
    InPar = InArg.populate_keyval(DefV,varargin,mfilename);  
      
### VO.prep.prep_data_dir

Prepare interface functions for the catalogs in the data directory Package: VO.search Description: Prepare interface functions for the catalogs in the data directory


    
    Prepare interface functions for the catalogs in the data directory  
    Package: VO.search  
    Description: Prepare interface functions for the catalogs in the data directory  
    Input  : * Arbitrary number of pairs if ...,keyword,value,...  
    The possible keywords are possible:  
    'Dir' - Directory in which the data catalogs resides.  
    The directory tree should be stored like a package.  
    each directory start with "+".  
    Default is '~/matlab/data/+datacats/';  
    'Exten' - Cell array of file extensions to map.  
    Default is {'mat'}.  
    Output : null  
    License: GNU general public license version 3  
    By : Eran O. Ofek                    Feb 2017  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: VO.search.prep_data_dir  
    Reliable: 2  
      
      
### VO.prep.prep_gaiadr2_htm

SHORT DESCRIPTION HERE Package: VO Description:


    
    SHORT DESCRIPTION HERE  
    Package: VO  
    Description:  
    Input  : -  
    * Arbitrary number of pairs of arguments: ...,keyword,value,...  
    where keyword are one of the followings:  
    Output : -  
    License: GNU general public license version 3  
    By : Eran O. Ofek                    Apr 2018  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: VO.prep.prep_gaia_htm  
    Reliable:  
      
      
      
      
### VO.prep.prep_gaiadre3_htm

SHORT DESCRIPTION HERE Package: VO Description:


    
    SHORT DESCRIPTION HERE  
    Package: VO  
    Description:  
    Input  : -  
    * Arbitrary number of pairs of arguments: ...,keyword,value,...  
    where keyword are one of the followings:  
    Output : -  
    License: GNU general public license version 3  
    By : Eran O. Ofek                    Apr 2018  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: VO.prep.prep_gaia_htm  
    Reliable:  
      
      
      
      
### VO.prep.prep_generic_htm

Prepare generic catsHTM catalog from declination zone catalogs Package: VO.prep Description: Prepare generic catsHTM catalog from declination zone catalogs.


    
    Prepare generic catsHTM catalog from declination zone catalogs  
    Package: VO.prep  
    Description: Prepare generic catsHTM catalog from declination zone  
    catalogs.  
    Input  : * Arbitrary number of pairs of arguments: ...,keyword,value,...  
    where keyword are one of the followings:  
    '  
    Output : null  
    License: GNU general public license version 3  
    By : Eran O. Ofek                    Feb 2018  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: VO.prep.prep_generic_htm  
    Reliable: 2  
      
      
### VO.prep.prep_hst_images_catalog

SHORT DESCRIPTION HERE Package: VO Description:


    
    SHORT DESCRIPTION HERE  
    Package: VO  
    Description:  
    Input  : -  
    * Arbitrary number of pairs of arguments: ...,keyword,value,...  
    where keyword are one of the followings:  
    Output : -  
    License: GNU general public license version 3  
    By : Eran O. Ofek                    Apr 2018  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: C=VO.prep.prep_hst_images_catalog  
    Reliable:  
      
      
### VO.prep.prep_sdss_offset_htm




    
      
      
    ColCell=VO.prep.prep_sdss_offset_htm  
      
      
    SELECT ra, dec, type, flags,  
    modelMag_u, modelMag_g, modelMag_r, modelMag_i, modelMag_z,  
    modelMagErr_u, modelMagErr_g, modelMagErr_r, modelMagErr_i, modelMagErr_z,  
    offsetRa_u, offsetRa_g, offsetRa_r, offsetRa_i, offsetRa_z,  
    offsetDec_u, offsetDec_g, offsetDec_r, offsetDec_i, offsetDec_z,  
    psffwhm_u, psffwhm_g, psffwhm_r, psffwhm_i, psffwhm_z,  
    TAI_r into mydb.MyTable_1 from PhotoPrimary  
    WHERE modelMag_z<20.0  
      
### VO.prep.prep_ukidss_htm




    
      
    ColCell=VO.prep.prep_ukidss_htm  
      
    DR info: http://wsa.roe.ac.uk/theSurveys.html  
    SQL: http://wsa.roe.ac.uk:8080/wsa/SQL_form.jsp  
    SELECT ra, dec, sigRA, sigDec, Epoch, pStar, pGalaxy, pNoise,  
    yPsfMag, yPsfMagErr, ySerMag2D, ySerMag2DErr, yEll, yPA,  
    j_1PsfMag, j_1PsfMagErr, j_1SerMag2D, j_1SerMag2DErr, j_1Ell, j_1PA,  
    j_2PsfMag, j_2PsfMagErr, j_2SerMag2D, j_2SerMag2DErr, j_2Ell, j_2PA,  
    hPsfMag, hPsfMagErr, hSerMag2D, hSerMag2DErr, hEll, hPA,  
    kPsfMag, kPsfMagErr, kSerMag2D, kSerMag2DErr, kEll, kPA  
    FROM lasSource  
    WHERE  dec>-20.0 and dec<=-10.0  
      
### VO.prep.prep_unWISE_htm

SHORT DESCRIPTION HERE Package: VO Description:


    
    SHORT DESCRIPTION HERE  
    Package: VO  
    Description:  
    Input  : -  
    * Arbitrary number of pairs of arguments: ...,keyword,value,...  
    where keyword are one of the followings:  
    Output : -  
    License: GNU general public license version 3  
    By : Eran O. Ofek                    Feb 2019  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: VO.prep.prep_unWISE_htm  
    Reliable:  
      
      
### VO.prep.prep_wise_htm_cat

reformat the IRSA/WISE catalog files into an HDF5/HTM catalog


    
    reformat the IRSA/WISE catalog files into an HDF5/HTM catalog  
      
      
### VO.prep.read_lensedQSO_db

Read garvitationaly lensed quasars database Package: VO.prep Description: Read garvitationaly lensed quasars database from: https://www.ast.cam.ac.uk/ioa/research/lensedquasars/index.html


    
    Read garvitationaly lensed quasars database  
    Package: VO.prep  
    Description: Read garvitationaly lensed quasars database from:  
    https://www.ast.cam.ac.uk/ioa/research/lensedquasars/index.html  
    Input  : * Arbitrary number of pairs of arguments: ...,keyword,value,...  
    where keyword are one of the followings:  
    'URL' - default is 'https://www.ast.cam.ac.uk/ioa/research/lensedquasars/index.html'  
    Output : - AstCat object containing the list of lensed quasars.  
    License: GNU general public license version 3  
    By : Eran O. Ofek                    Sep 2019  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: T=VO.prep.read_lensedQSO_db;  
    Reliable:  
      
      
### VO.prep.wget_all_hsc

SHORT DESCRIPTION HERE Package: VO Description:


    
    SHORT DESCRIPTION HERE  
    Package: VO  
    Description:  
    Input  : -  
    * Arbitrary number of pairs of arguments: ...,keyword,value,...  
    where keyword are one of the followings:  
    Output : -  
    License: GNU general public license version 3  
    By : Eran O. Ofek                    Apr 2018  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: VO.prep.wget_all_hsc('DecRange',DecRange,'Problem',Problem,'UnDetector',UnDetector);  
    Reliable:  
      
      
### VO.prep.wget_all_skymapper

SHORT DESCRIPTION HERE Package: VO Description:


    
    SHORT DESCRIPTION HERE  
    Package: VO  
    Description:  
    Input  : -  
    * Arbitrary number of pairs of arguments: ...,keyword,value,...  
    where keyword are one of the followings:  
    Output : -  
    License: GNU general public license version 3  
    By : Eran O. Ofek                    Apr 2018  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: VO.prep.wget_all_skymapper  
    Reliable:  
      
      
### VO.prep.wget_all_usnob1

Retrieve USNO-B1 catalog from VizieR and format into HDF5/HTM (catsHTM) Package: VO.prep Description:


    
    Retrieve USNO-B1 catalog from VizieR and format into HDF5/HTM (catsHTM)  
    Package: VO.prep  
    Description:  
    Input  : -  
    * Arbitrary number of pairs of arguments: ...,keyword,value,...  
    where keyword are one of the followings:  
    Output : -  
    License: GNU general public license version 3  
    By : Eran O. Ofek                    Feb 2018  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: VO.prep.wget_all_usnob1  
    Reliable:  
      
      
### VO.prep.wget_pulsar_cat

Read ATNF pulsar catalog from the web into an AstCat object. Package: VO.prep Description: Read ATNF pulsar catalog from the web into an AstCat object.


    
    Read ATNF pulsar catalog from the web into an AstCat object.  
    Package: VO.prep  
    Description: Read ATNF pulsar catalog from the web into an AstCat object.  
    Input  : null  
    * Arbitrary number of pairs of arguments: ...,keyword,value,...  
    where keyword are one of the followings:  
    Output : - AstCat object containing the ATNF pulsar catalog.  
    License: GNU general public license version 3  
    By : Eran O. Ofek                    Apr 2018  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: Pulsar = VO.prep.wget_pulsar_cat;  
    Reliable: 2  
      
      
