# Package: VO.prep.GAIA


### VO.prep.GAIA.gaia_dr1_build_cat

Build the GAIA-DR1 fast access catalog Example: Counter=VO.prep.GAIA.gaia_dr1_build_cat


    
    Build the GAIA-DR1 fast access catalog  
    Example: Counter=VO.prep.GAIA.gaia_dr1_build_cat  
      
### VO.prep.GAIA.gaia_dr1_cat_columns

Get the GAIA-DR1 secondary catalog column names Description: Get the GAIA-DR1 secondary catalog column names


    
    Get the GAIA-DR1 secondary catalog column names  
    Description: Get the GAIA-DR1 secondary catalog column names  
    Input  : null  
    Output : - Cell array of column names.  
    - Structure of column names.  
    Example: [ColCell,Col]=VO.prep.GAIA.gaia_dr1_cat_columns  
    Reliable: 2  
      
### VO.prep.GAIA.gaia_dr1_read_file

Read GAIA-DR1 file for reformatting purposses Description: Given a GAIA-DR1 CSV file name, read the file into a cell array and also return a matrix of selected columns of good entries.


    
    Read GAIA-DR1 file for reformatting purposses  
    Description: Given a GAIA-DR1 CSV file name, read the file into a cell  
    array and also return a matrix of selected columns of  
    good entries.  
    Input  : - File name.  
    Output : - Matrix of selected columns of all entries  
    - Cell array of column names:  
    [RA, ErrRA, Dec, ErrDec, MagG, ErrMagG].  
    - Cell array of all columns.  
    - Flag of good entries  
    Example: cd /raid/eran/Catalogue/GAIA-DR1/Orig  
    Data=VO.prep..GAIA.gaia_dr1_read_file('GaiaSource_000-012-071.csv');  
      
### VO.prep.GAIA.gaia_dr1_readall2hdf5

Create an HDF5 version of the GAIA-DR1 files with a subset of columns. Example: Count=VO.prep.GAIA.gaia_dr1_readall2hdf5


    
    Create an HDF5 version of the GAIA-DR1 files with a subset of columns.  
    Example: Count=VO.prep.GAIA.gaia_dr1_readall2hdf5  
      
### VO.prep.GAIA.gaia_dr1_readall_select

Select GAIA sources in Dec zone for constructing GAIA catalog Pacakge: VO.prep.GAIA


    
    Select GAIA sources in Dec zone for constructing GAIA catalog  
    Pacakge: VO.prep.GAIA  
      
      
### VO.prep.GAIA.get_files_gaia_dr1

Get GAIA DR1 files from GAIA archive Package: VO.prep.GAIA Description: Get GAIA DR1 files from GAIA archive


    
    Get GAIA DR1 files from GAIA archive  
    Package: VO.prep.GAIA  
    Description: Get GAIA DR1 files from GAIA archive  
    Input  : null (see internal parameters)  
    Output : null  
    Example: VO.prep.GAIA.get_files_gaia_dr1  
    Reliable: 2  
      
### VO.prep.GAIA.get_tgas_dr1

Retrieve and format the GAIA-DR1 TGAS catalog Package: VO.prep.GAIA Description: Retrieve the GAIA DR1 TGAS catalog files and reformat it


    
    Retrieve and format the GAIA-DR1 TGAS catalog  
    Package: VO.prep.GAIA  
    Description: Retrieve the GAIA DR1 TGAS catalog files and reformat it  
      
      
