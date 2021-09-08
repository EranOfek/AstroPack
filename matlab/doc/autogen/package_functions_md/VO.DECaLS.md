# Package: VO.DECaLS


### VO.DECaLS.decals_viewer_link

Get link to DECaLS image viewer Package: VO Description: Get link to DECaLS image viewer


    
    Get link to DECaLS image viewer  
    Package: VO  
    Description: Get link to DECaLS image viewer  
    Input  : - J2000.0 R.A. [rad, sexagesimal, or [H M S]],  
    or object name to be resolved using NameServerFun.  
    - J2000.0 Dec. [rad, sexagesimal, or [sign D M S]].  
    * Arbitrary number of pairs of arguments: ...,keyword,value,...  
    where keyword are one of the followings:  
    'IsDeg' - If true then assume input coordinates is in degrees.  
    Default is false.  
    'Layer' - Layer to plot. Default is 'decals-dr5'.  
    'Zoom'  - Zoom factor. Default is 13.  
    'NameServerFun' - Name server function handle.  
    Default is @VO.name.server_ned.  
    'BaseURL' - Viewer base URL.  
    Default is 'http://legacysurvey.org/viewer?'  
    Output : - Cell array lof links to DECaLS image viewer.  
    License: GNU general public license version 3  
    By : Eran O. Ofek                    Jan 2018  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: Link=VO.DECaLS.decals_viewer_link(220./RAD,-0.1./RAD)  
    Reliable: 2  
      
### VO.DECaLS.prep_decals_htmcat

SHORT DESCRIPTION HERE Package: VO Description:


    
    SHORT DESCRIPTION HERE  
    Package: VO  
    Description:  
    Input  : -  
    * Arbitrary number of pairs of arguments: ...,keyword,value,...  
    where keyword are one of the followings:  
    Output : -  
    License: GNU general public license version 3  
    By : Eran O. Ofek                    Jan 2018  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example:  
    Reliable:  
      
      
### VO.DECaLS.read_sweep_brick_table

SHORT DESCRIPTION HERE Package: VO Description:


    
    SHORT DESCRIPTION HERE  
    Package: VO  
    Description:  
    Input  : -  
    * Arbitrary number of pairs of arguments: ...,keyword,value,...  
    where keyword are one of the followings:  
    Output : -  
    License: GNU general public license version 3  
    By : Eran O. Ofek                    Jan 2018  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example:  
    Reliable:  
      
      
      
