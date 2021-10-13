# Package: VO.WISE


### VO.WISE.coo2coaddid

Find all WISE coadd_id for a given coordinate. Package: VO.WISE Description: Given celestial equatorial coordinates, find all WISE coadd_id string ID that covers the coordinates.


    
    Find all WISE coadd_id for a given coordinate.  
    Package: VO.WISE  
    Description: Given celestial equatorial coordinates, find all WISE  
    coadd_id string ID that covers the coordinates.  
    Input  : - J2000.0 RA in [rad] or [H M S] or sexagesimal string.  
    - J2000.0 Dec in [rad] or [Sign D M S] or sexagesimal string.  
    - WISE data relaese, default is 'allsky'.  
    If empty (i.e., []), then use default.  
    Output : - A cell array of lists of coadd_id  
    that covers the given coordinates. Each cell for each  
    coordinate.  
    - An array of JD of frame for [band1] frame.  
    Tested : Matlab R2014a  
    By : Ilia Labzovsky                  Apr 2012  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: [ID,JD,Table]=VO.WISE.coo2coaddid(1.0124,-1.4469);  
    Reliable: 2  
      
### VO.WISE.wget_corrim

Get WISE coadded Atlas image from coadd_id Package: VO.WISE Description: Given a WISE field ID coadd_id for Atlas Images, get the link to, and the WISE corrected image in FITS format. Furthermore, read the corrected image into matlab matrix.


    
    Get WISE coadded Atlas image from coadd_id  
    Package: VO.WISE  
    Description: Given a WISE field ID coadd_id for Atlas Images, get the  
    link to, and the WISE corrected image in FITS format.  
    Furthermore, read the corrected image into matlab matrix.  
    The program first check if the FITS image is exist in  
    current directory and if so, it reads the image from the disk.  
    Note that if nargout>1 then the fits file is retrieved.  
    Input  : - Matrix of images ID coadd_id for Atlas Images  
    - Save FITS images to disk {'gzip' | 'y' | 'n'}, default is 'y'.  
    - Filters to retrieve, default is '1234' (all Filters).  
    - Data release {'prelim_postcryo','prelim','cryo_3band','allsky'}, default is 'allsky'.  
    If empty use default.  
    Output : - Cell array of links to each fits image.  
    Rows for each field, columns for each band.  
    - Cell array of fits images name.  
    - Cell array of images matrix.  
    Rows for each field, columns for each band.  
    If cell is NaN - image can't retrieved.  
    - Cell array of images header.  
    Rows for each field, columns for each band.  
    Note that keywords are stored in: Header{:,:}.PrimaryData.Keywords  
    Tested : Matlab 7.0  
    By : Ilia Labzovsky                  Oct 2012  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: [Link,FN,Image,Header]=VO.WISE.wget_corrim({'0000m016_ab41'},'y','4','allsky');  
    -  
