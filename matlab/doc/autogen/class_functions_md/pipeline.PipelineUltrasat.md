# Class: pipeline.PipelineUltrasat



    
      
      
      
      
### PipelineUltrasat




    
### proc_Astrometry

Astrometry


    
    Astrometry  
### proc_CalcBack

Calc Back


    
    Calc Back  
### proc_CalcVar

Calc Var


    
    Calc Var  
### proc_CatHeader

Cat, header


    
    Cat, header  
### proc_Catalogs

Catalogs


    
    Catalogs  
### proc_CreateMask

Create mask


    
    Create mask  
### proc_CrossTalk

Cross-talk (?)


    
    Cross-talk (?)  
### proc_ForcedPhotometry

Forced photometry


    
    Forced photometry  
### proc_Gain1

Gain->1


    
    Gain->1  
### proc_Linearization

Linearization


    
    Linearization  
### proc_Match




    
### proc_PSF_Estimation

PSF estimation


    
    PSF estimation  
### proc_Partitioning

Partitioning w/ overlap (CCDSEC..)


    
    Partitioning w/ overlap (CCDSEC..)  
### proc_PhotCalib

Phot calib.


    
    Phot calib.  
### proc_PreProces




    
### proc_PrepareInputData

Prep raw image as fits


    
    Prep raw image as fits  
### proc_RobustCoadd

Robust Coadd x3,...


    
    Robust Coadd x3,...  
### proc_SearchTransients

Search transients


    
    Search transients  
### proc_SelectClassTransients

Select/class transients


    
    Select/class transients  
### proc_SetSaturatedPixels

Set saturated pixels


    
    Set saturated pixels  
### proc_SourceExtraction

Source extraction + DaoPhot (?)


    
    Source extraction + DaoPhot (?)  
### proc_SourcesSNR

Sources SNR > 20


    
    Sources SNR > 20  
### proc_StarFlat

Star flat


    
    Star flat  
### proc_Subtraction

Subtraction


    
    Subtraction  
### proc_SuperFlat

Super flat


    
    Super flat  
### proc_TranslateToRefGrid

Translate to ref grid (?)


    
    Translate to ref grid (?)  
### proc_UpdateHeader

Update header


    
    Update header  
### proc_deBias

de Bias


    
    de Bias  
### proc_deDark

de Dark


    
    de Dark  
### proc_deFlat

de Flat


    
    de Flat  
### proc_deFringing

de Fringing (?)


    
    de Fringing (?)  
### proc_end




    
### proc_start

ULTRASAT Pipeline Processing, from Pipeline_Data_flow_chart.pdf


    
    ULTRASAT Pipeline Processing, from Pipeline_Data_flow_chart.pdf  
      
    Inputs: Raw image, Header, Telemetry  
      
    Prep raw image as fits  
    Update header  
      
    Partitioning w/ overlap (CCDSEC..)  
      
    Create mask  
    Set saturated pixels  
      
    Linearization  
    Gain->1  
      
    de Bias  
    de Dark  
      
    Cross-talk (?)  
      
    de Fringing (?)  
      
    de Flat  
      
    Super flat  
      
    Star flat  
      
    Calc Bck, VAR  
      
    Sources SNR > 20  
      
    PSF estimation  
      
    Source extraction + DaoPhot (?)  
      
    Catalogs  
      
    Astrometry  
      
    Phot calib.  
    Cat, header  
      
    Match image to Ref (?):  
    Phot, astrometry,  
    rotation (?)  
      
    Robust Coadd x3,...  
      
    Translate to ref grid (?)  
      
    Subtraction  
      
    CR interpolated image (?) from scratch, but  
    add columns/rows  
      
    Cat of transients, CR and streaks  
      
    Forced photometry  
      
    Coadditon on subs x3,x..  
      
    Search transients  
      
    Select/class transients  
      
### process

Run pipeline function by its name


    
    Run pipeline function by its name  
      
    Prepare data  
### processCat




    
### processCoaddOnSubs

Coadditon on subs x3,x..


    
    Coadditon on subs x3,x..  
### processInputImage

Pipeline entry point


    
    Pipeline entry point  
### setProc

Set current process name and details


    
    Set current process name and details  
### unitTest




    
