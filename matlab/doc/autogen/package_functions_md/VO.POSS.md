# Package: VO.POSS


### VO.POSS.get_dss

Get link to and the FITS image of the DSS Package: VO.POSS Description: Get link to and the FITS image of a digital sky survey image (POSS-I/II, UKST survey). Read the FITS file into


    
    Get link to and the FITS image of the DSS  
    Package: VO.POSS  
    Description: Get link to and the FITS image of a digital sky survey  
    image (POSS-I/II, UKST survey). Read the FITS file into  
    matlab.  
    Input  : - J2000 RA in [radians] or [H M S] or sexagesimal string.  
    - J2000 Dec in [radians] or [H M S] or sexagesimal string.  
    - Field of view [arcmin arcmin], default is [12 12].  
    - Save FITS images to disk {'gzip' | 'y' | 'n'},  
    default is 'gzip'.  
    - Filter/epoch to retrieve,  
    {'2'|'2R'|'2B'|'2I'|'DSS1'|'1R'|'1B'|'QV'|'HST-P2'},  
    Output : - Cell array of links to each gif image.  
    - Cell array of fits images name.  
    (e.g., "DSS_2R_######.###+######.##.fits").  
    The fits images are retrieved only if the user ask for this  
    argument.  
    - Cell array of images matrix.  
    If cell is NaN - image can't retrieved.  
    - Cell array of images header.  
    Note that keywords are stored in:  
    Header{:,:}.PrimaryData.Keywords (see example).  
    Tested : Matlab 7.0  
    By : Eran O. Ofek                    Jul 2005  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: [Link]=VO.POSS.get_dss(1,1,'n','2R');  
    Reliable: 1  
    -  
