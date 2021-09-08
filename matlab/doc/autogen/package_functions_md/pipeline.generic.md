# Package: pipeline.generic


### pipeline.generic.mergeCatalogs




    
      
      
### pipeline.generic.singleRaw2proc

Basic processing of a single raw image into a processed image Including: Reading the image Generate a mask image and mask saturated pixels


    
    Basic processing of a single raw image into a processed image  
    Including:  
    Reading the image  
    Generate a mask image and mask saturated pixels  
    Subtract bias/dark image  
    Divide by flat image  
    Remove Fringe image  
    Break image to sub images  
    Estimate background  
    Basic source findinging  
    Astrometry  
    Update astrometry in catalog  
    Photometric ZP  
    Update photometric ZP in catalog  
    Save products  
    Input  : -  
    Output : -  
    Author : Eran Ofek (Aug 2021)  
    Example: pipeline.generic.singleRaw2proc  
      
