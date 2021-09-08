# Package: VO.PS1


### VO.PS1.add_meta_data2ps1

SHORT DESCRIPTION HERE Package: VO.PS1 Description:


    
    SHORT DESCRIPTION HERE  
    Package: VO.PS1  
    Description:  
    Input  : -  
    * Arbitrary number of pairs of arguments: ...,keyword,value,...  
    where keyword are one of the followings:  
    Output : -  
    License: GNU general public license version 3  
    By : Eran O. Ofek                    Dec 2017  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example:  
    Reliable:  
      
      
      
      
### VO.PS1.get_stack

Get links to PS1 corrected stack FITS images. Package: telescope.PS1 Description: Get link to PS1 FITS images.


    
    Get links to PS1 corrected stack FITS images.  
    Package: telescope.PS1  
    Description: Get link to PS1 FITS images.  
    Input  : - J2000.0 R.A. [rad, [H M S], sexagesimal string]  
    - J2000.0 Dec. [rad, [Sign D M S], sexagesimal string]  
    - Cutout size [arcsec]. Default is 240.  
    Output : - A 5 column cell array of links to the cutout images.  
    Each column for specific filter (g,r,i,z,y) and each line  
    for specific requested coordinate.  
    - A 5 column cell array of links to the full images.  
    Each column for specific filter (g,r,i,z,y) and each line  
    for specific requested coordinate.  
    - Cell array of links to the navigator pages.  
    By : Eran O. Ofek                    Dec 2016  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: [LinkCut,LinkFull,Link]=VO.PS1.get_stack(pi,0.4)  
    Reliable: 2  
      
### VO.PS1.navigator_link

Given J2000 equatorial coordinates get link to PS1 navigator image. Package: telescope.PS1 Description: Get link to PS1 navigator image


    
    Given J2000 equatorial coordinates get link to PS1 navigator image.  
    Package: telescope.PS1  
    Description: Get link to PS1 navigator image  
    Input  : - J2000.0 R.A. [rad, [H M S], sexagesimal string]  
    - J2000.0 Dec. [rad, [Sign D M S], sexagesimal string]  
    - Cutout size [arcsec]. Default is 240.  
    Output : - Cell array of links.  
    By : Eran O. Ofek                    Dec 2016  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: Link=telescope.PS1.navigator_link(pi,0.4)  
    Reliable: 2  
      
### VO.PS1.ps1_2_sdss_mag

PS1 magnitudes to PS1 magnitude using the Finkbeiner+2015 relations Package: VO Description: Convert PS1 magnitudes to PS1 magnitude using the Finkbeiner+2015 relations.


    
    PS1 magnitudes to PS1 magnitude using the Finkbeiner+2015 relations  
    Package: VO  
    Description: Convert PS1 magnitudes to PS1 magnitude using the  
    Finkbeiner+2015 relations.  
    Input  : - PS1 Band name, 'g','r','i','z','y'  
    - PS1 magnitude  
    - PS1 g-band magnitude.  
    - PS1 i-band magnitude.  
    Output : - SDSS magnitude in band.  
    License: GNU general public license version 3  
    By : Eran O. Ofek                    May 2019  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: MagSDSS=VO.PS1.ps1_2_sdss_mag('g',20,20,21)  
    Reliable: 2  
      
      
    a0        a1        a2       a3  
