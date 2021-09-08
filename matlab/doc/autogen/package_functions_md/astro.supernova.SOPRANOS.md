# Package: astro.supernova.SOPRANOS


### astro.supernova.SOPRANOS....




    
### astro.supernova.SOPRANOS.SOPRANOS_GUI

SOPRANOS_GUI MATLAB code for SOPRANOS_GUI.fig SOPRANOS_GUI, by itself, creates a new SOPRANOS_GUI or raises the existing singleton*.


    
    SOPRANOS_GUI MATLAB code for SOPRANOS_GUI.fig  
    SOPRANOS_GUI, by itself, creates a new SOPRANOS_GUI or raises the existing  
    singleton*.  
      
    H = SOPRANOS_GUI returns the handle to a new SOPRANOS_GUI or the handle to  
    the existing singleton*.  
      
    SOPRANOS_GUI('CALLBACK',hObject,eventData,handles,...) calls the local  
    function named CALLBACK in SOPRANOS_GUI.M with the given input arguments.  
      
    SOPRANOS_GUI('Property','Value',...) creates a new SOPRANOS_GUI or raises the  
    existing singleton*.  Starting from the left, property value pairs are  
    applied to the GUI before SOPRANOS_GUI_OpeningFcn gets called.  An  
    unrecognized property name or invalid value makes property application  
    stop.  All inputs are passed to SOPRANOS_GUI_OpeningFcn via varargin.  
      
    *See GUI Options on GUIDE's Tools menu.  Choose "GUI allows only one  
    instance to run (singleton)".  
      
    See also: GUIDE, GUIDATA, GUIHANDLES  
      
    Edit the above text to modify the response to help SOPRANOS_GUI  
      
    Last Modified by GUIDE v2.5 21-Oct-2019 14:38:24  
      
    Begin initialization code - DO NOT EDIT  
### astro.supernova.SOPRANOS.[jd, mjd, cps, cpserr, flux, fluxerr, prevRef, percRef ,RA, Dec, DiscMJD, z ,...




    
### astro.supernova.SOPRANOS.calcGrid

Calculate chi2/dof grid Package: AstroUtil.supernove.SOPRANOS Description: Calculate chi2/dof grid for a SN light curve against a Sapir & Waxman 2017 shock cooling model.


    
    Calculate chi2/dof grid  
    Package: AstroUtil.supernove.SOPRANOS  
    Description: Calculate chi2/dof grid for a SN light curve against a  
    Sapir & Waxman 2017 shock cooling model.  
    Input  : - The name of the supernova (The observations are expected to be  
    in a file named <sn_name>_data.mat and model light  
    curves for interpolation in a file named  
    <sn_name>_<model>_LCs.mat)  
    - Vector of reference times.  
    - Vector of Extinction values (E_{B-V}) - An obselete parameter,  
    VecEbv is now a part of the LCs file.  
    - Shock cooling model to use.  
    - The model accuracy - default is 0.  
    - Ordinal number of the bands for which to calculate the BG out  
    of the data  
    Output : - A file named <sn_name>_<model>_grid.mat  
    the output file includes the vector which span the grid  
    chi2 and number of valid data points grids  
    cell array with chi2 and number of valid data points grids  
    for each band.  
    cell array with chi2 and number of outliers data points grids  
    for each band, in case outlier points were icnluded.  
      
    See also: AstroUtil.supernova.SOPRANOS.prepare_LCs  
    Tested : Matlab 9.5  
    By : Noam Ganot                      Oct 2019  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: AstroUtil.supernova.SOPRANOS.calcGrid('PTF12gnt',  
    Reliable: 2  
      
### astro.supernova.SOPRANOS.cubicZeros

this functions solves the cubic equation of the form: ax^3+bx^2+cx+d=0 a,b,c,d should by either scalar or matrices of the same form.


    
    this functions solves the cubic equation of the form:  
    ax^3+bx^2+cx+d=0  
    a,b,c,d should by either scalar or matrices of the same form.  
      
### astro.supernova.SOPRANOS.findMaximum

Find numerically the maximum likelihood Package: AstroUtil.supernove.SOPRANOS Description: Find numerically the maximum likelihood


    
    Find numerically the maximum likelihood  
    Package: AstroUtil.supernove.SOPRANOS  
    Description: Find numerically the maximum likelihood  
    Input  : - grid file name  
    - Vector of initial conditions of Shock velocity parameter [cm s^-1]  
    - Vector of initial conditions or Progenitor radius [R_\sun]  
    (Vs_in and Rs_in must be of the same length)  
    - background value for each band  
    - minimal transient points constraint for each band  
    Output : - Table with file contents  
      
    See also: AstroUtil.supernova.SOPRANOS.calcGrid  
    Tested : Matlab 9.5  
    By : Noam Ganot                      Oct 2019  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example:  
    AstroUtil.supernova.SOPRANOS.readZTFtxt('ZTF18abokyfk_unbinned.txt');  
    Reliable: 2  
      
      
### astro.supernova.SOPRANOS.plotGrid

Plot the likelihood grid calculated by calcGrid Package: AstroUtil.supernove.SOPRANOS Description: Plot the likelihood grid calculated by calcGrid.


    
    Plot the likelihood grid calculated by calcGrid  
    Package: AstroUtil.supernove.SOPRANOS  
    Description: Plot the likelihood grid calculated by calcGrid.  
    Input  : - grid file name  
    - minimal transient points constaint for each band (0 means no  
    constraint).  
    - peakNumber for plotting the marginal distribution in case of  
    multiple peak solution (default is 1)  
    - External filter to plot subgrid or to mainpulate the valid  
    region. The filter is a string contains matlab code which is  
    executed using eval to change the Vectors which span the grid  
    or the valid variable which defines the valid grid points.  
    - suffix for the fMax results file which describes the filter  
    Output : - Figures and text plotted to the command line.  
      
    Tested : Matlab 9.5  
    By : Noam Ganot                      Oct 2019  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: AstroUtil.supernova.SOPRANOS.plotGrid('PTF12ffs_msw_grid.mat', [0 1], 1, 'VecEbv=VecEbv(VecEbv<=0.09);','Ebv.lte.0.09');  
    Reliable: 2  
      
      
### astro.supernova.SOPRANOS.prepareSNdata

Prepare Observation data and ancillary data for a SN Package: AstroUtil.supernove.SOPRANOS Description: Prepare Observation data and ancillary data for a SN


    
    Prepare Observation data and ancillary data for a SN  
    Package: AstroUtil.supernove.SOPRANOS  
    Description: Prepare Observation data and ancillary data for a SN  
    Input  : - The name of the supernova (The observations are expected to be  
    in a CSV file, txt file, or out_PTF48R file with the SN name).  
    - the SN redshift  
    - the SN RA in sexagesimal hours  
    - the SN dec in sexagesimal degrees  
    Output : - A file named <sn_name>_data.mat  
      
    See also: AstroUtil.supernova.SOPRANOS.calcGrid  
    AstroUtil.supernova.SOPRANOS.prepare_LCs  
    Tested : Matlab 9.5  
    By : Noam Ganot                      Oct 2019  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: AstroUtil.supernova.SOPRANOS.prepareSNdata('PTF12gnt')  
    Reliable: 2  
      
      
### astro.supernova.SOPRANOS.prepare_LCs

Prepare Model light curves for interpolation Package: AstroUtil.supernove.SOPRANOS Description: Calculate model light curves grid for interpolation  by calcGrid using Sapir & Waxman 2017 shock cooling model.


    
    Prepare Model light curves for interpolation  
    Package: AstroUtil.supernove.SOPRANOS  
    Description: Calculate model light curves grid for interpolation  by  
    calcGrid using Sapir & Waxman 2017 shock cooling model.  
    for each band in its input it calculates 1000x1001 grid  
    spanned by bolometric luminosity and photosphere  
    temperature of the observed flux in the band, take into  
    consdireation the distance, redshift, extinction and filter  
    transmission curve.  
    Input  : - The name of the supernova (The observations, the SN redshift  
    and coordinats  are expected to be in a file named <sn_name>_data.mat)  
    - Shock cooling model to use.  
    - Vector of Shock velocity parameter values [cm s^-1]  
    - Vector of Progenitor radius values [R_\sun]  
    - Vector of Ejecta Mass values [M_\sun]  
    - Vector of progenitor envelope parameter values  
    - Vector of Extinction values (E_{B-V}).  
    - Progenitor type ['RSG' or 'BSG']  
    Output : - A file named <sn_name>_<model>_LCs.mat  
      
    See also: AstroUtil.supernova.SOPRANOS.calcGrid  
    Tested : Matlab 9.5  
    By : Noam Ganot                      Oct 2019  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: AstroUtil.supernova.SOPRANOS.calcGrid('PTF12gnt',  
    Reliable: 2  
      
      
### astro.supernova.SOPRANOS.readPTFout

read PTF out file Package: AstroUtil.supernove.SOPRANOS Description: read PTF out file and read the matching GALEX data of the GALEX/PTF experiment.


    
    read PTF out file  
    Package: AstroUtil.supernove.SOPRANOS  
    Description: read PTF out file and read the matching GALEX data of the  
    GALEX/PTF experiment.  
    Input  : - the supernova name (for example 'PTF12gnt')  
    Output : - Table with file contents  
      
    See also: AstroUtil.supernova.SOPRANOS.calcGrid  
    Tested : Matlab 9.5  
    By : Noam Ganot                      Oct 2019  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example:  
    AstroUtil.supernova.SOPRANOS.readPTFout('PTF12gnt');  
    Reliable: 2  
      
      
    Auto-generated by MATLAB on 2019/06/29 10:28:55  
      
### astro.supernova.SOPRANOS.readUVOTascii

readUVOTascii Import UVOT subtracted data from an ascii file. tab = readUVOTascii(FILENAME) Reads data from text file FILENAME for the default selection.


    
    readUVOTascii Import UVOT subtracted data from an ascii file.  
    tab = readUVOTascii(FILENAME) Reads data from text file FILENAME  
    for the default selection.  
      
    tab = readUVOTascii(FILENAME, STARTROW, ENDROW) Reads data from  
    rows STARTROW through ENDROW of text file FILENAME.  
      
    Example:  
    tab = readUVOTascii('ZTF18abeajml.ascii', 2, 86);  
      
    See also TEXTSCAN.  
      
    Auto-generated by MATLAB on 2019/12/14 11:25:02  
      
    Initialize variables.  
### astro.supernova.SOPRANOS.readZTF

read ZTF data file as exported from ZTF marshal Package: AstroUtil.supernove.SOPRANOS Description: read ZTF data file


    
    read ZTF data file as exported from ZTF marshal  
    Package: AstroUtil.supernove.SOPRANOS  
    Description: read ZTF data file  
    Input  : - file name  
    Output : - Table with file contents  
      
    Tested : Matlab 9.5  
    By : Noam Ganot                      Oct 2019  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example:  
    AstroUtil.supernova.SOPRANOS.readZTFtxt('ZTF18abokyfk_unbinned.txt');  
    Reliable: 2  
      
      
    Auto-generated by MATLAB on 2019/08/10 09:10:04  
      
    Initialize variables.  
### astro.supernova.SOPRANOS.readZTFtxt

read ZTF text file Package: AstroUtil.supernove.SOPRANOS Description: read ZTF text file


    
    read ZTF text file  
    Package: AstroUtil.supernove.SOPRANOS  
    Description: read ZTF text file  
    Input  : - file name  
    Output : - Table with file contents  
      
    See also: AstroUtil.supernova.SOPRANOS.calcGrid  
    Tested : Matlab 9.5  
    By : Noam Ganot                      Oct 2019  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example:  
    AstroUtil.supernova.SOPRANOS.readZTFtxt('ZTF18abokyfk_unbinned.txt');  
    Reliable: 2  
      
      
    Auto-generated by MATLAB on 2019/02/16 10:21:34  
      
    Initialize variables.  
### astro.supernova.SOPRANOS.ztfBands

Split the table into different bands and read the filter Obj Package: AstroUtil.supernove.SOPRANOS Description: Split the table into different bands and read the filter Obj


    
    Split the table into different bands and read the filter Obj  
    Package: AstroUtil.supernove.SOPRANOS  
    Description: Split the table into different bands and read the filter Obj  
    Input  : - The table read by readZTF  
    Output : - A file named <sn_name>_<model>_LCs.mat  
      
    See also: AstroUtil.supernova.SOPRANOS.readZTF  
    Tested : Matlab 9.5  
    By : Noam Ganot                      Oct 2019  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: AstroUtil.supernova.SOPRANOS.ztfBands(Tab)  
    Reliable: 2  
      
      
### astro.supernova.SOPRANOS.ztfBandsTxt

Split the table into different bands and read the filter Obj Package: AstroUtil.supernove.SOPRANOS Description: Split the table into different bands and read the filter Obj


    
    Split the table into different bands and read the filter Obj  
    Package: AstroUtil.supernove.SOPRANOS  
    Description: Split the table into different bands and read the filter Obj  
    Input  : - The table read by readZTFtxt  
    Output : - A file named <sn_name>_<model>_LCs.mat  
      
    See also: AstroUtil.supernova.SOPRANOS.readZTFtxt  
    Tested : Matlab 9.5  
    By : Noam Ganot                      Oct 2019  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: AstroUtil.supernova.SOPRANOS.ztfBands.Txt(Tab)  
    Reliable: 2  
      
      
