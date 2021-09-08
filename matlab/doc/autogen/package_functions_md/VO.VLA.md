# Package: VO.VLA


### VO.VLA.read_sad

Read AIPS SAD files. Package: VO.VLA Description: Read AIPS SAD files.


    
    Read AIPS SAD files.  
    Package: VO.VLA  
    Description: Read AIPS SAD files.  
    Input  : - SAD file name  
    Output : - Structure array with the SAD information per source.  
    .H - Flag indicating high point in the residual.  
    .L - Flag indicating low point in the residual.  
    .S - Flag indicating large RMS.  
    - Info.RA, Info.Dec : reference position  
    Info.RMS.  
    All fluxes are given in mJy  
    Tested : Matlab 7.11  
    By : Eran O. Ofek                    Jul 2011  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Reliable: 2  
      
      
### VO.VLA.vla_pbcorr

Calculate primary beam corrections for the VLA antena Package: VO.VLA Description: Calculate primary beam corrections for the VLA antena.


    
    Calculate primary beam corrections for the VLA antena  
    Package: VO.VLA  
    Description: Calculate primary beam corrections for the VLA antena.  
    Input  : - Frequency of observations [GHz].  
    - Angular distance from primary beam [arcmin].  
    - Interpolation method for frequency  
    {'linear'|'nearest'}, default is 'nearest'.  
    Output : Primary beam correction factor (to multiply the flux by).  
    Tested : Matlab 7.11  
    By : Eran O. Ofek                    Jun 2011  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Reliable: 2  
      
      
