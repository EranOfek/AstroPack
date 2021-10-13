# Package: VO.VizieR


### VO.VizieR.catalog_mapping

Mapping of VizieR catalogs columns Package: VO.VizieR Description: Get the VizieR catalogs mapping indicating the format (column names, units and location) for each catalog.


    
    Mapping of VizieR catalogs columns  
    Package: VO.VizieR  
    Description: Get the VizieR catalogs mapping indicating the format  
    (column names, units and location) for each catalog.  
    Input  : - Catalog program name. If not provided than return all  
    catalogs.  
    - Map name. If more than one mapping exist per catalog than this  
    can be used to identify the specific map. Default is empty.  
    Output : - A structure array containing the column mapping information  
    for the all catalogs.  
    By : Eran O. Ofek                    Feb 2017  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: Map=VO.VizieR.catalog_mapping  
    Reliable: 2  
      
      
      
### VO.VizieR.cds_astcat_search

Query a VizieR catalog using the cdsclient tools Package: VO.VizieR Description: Query a VizieR catalog using the cdsclient command line tools. Allow to query a specific catalog by coordinates, or object name and return the output in various formats.


    
    Query a VizieR catalog using the cdsclient tools  
    Package: VO.VizieR  
    Description: Query a VizieR catalog using the cdsclient command line  
    tools. Allow to query a specific catalog by coordinates, or  
    object name and return the output in various formats.  
    Catalog.VizieR.cdsclient_prog_names can be used to identify  
    existing catalog excess programs.  
    In order to execuate a query you have to verify that the  
    catalog mapping exist in Catalog.VizieR.catalog.mapping.  
    Input  : - Catalog name. E.g, 'finducac4'. See  
    Catalog.VizieR.cdsclient_prog_names for available catalog names.  
    - J2000.0 R.A. or object name.  
    If the third argument is empty then this is an object name.  
    RA and Dec Units are defined by 'CooUnits' argument.  
    - J2000.0 Dec.  
    * Arbitary number of pairs of ...,keyword,value,... arguments.  
    Possible keywords are:  
    'OutType' - Options are:  
    'AstCat' - An AstCat object.  
    'AstCatTable' - An AstCat object with a table  
    catalog.  
    'cell' - Cell array.  
    'table' - A Table.  
    'mat' - A matrix.  
    Default is 'AstCat'.  
    'MapName' - Mapping name. Default is empty.  
    'RemoveStringCol' - Remove all columns with string format from  
    output catalog. This can be useful to present the  
    output in an Matrix or AstCat formats.  
    Default is true.  
    'Replace999' - Replace 999 with NaN. Default is true.  
    'CooUnits'- Coordinate units. Default is 'deg'.  
    'Radius'  - Search radius. Default is 10.  
    For box search this can be [X,Y] box size.  
    'RadiusUnits' - Search radius units. Default is 'arcmin'.  
    See convert.angular for options.  
    Default is 'arcmin'.  
    'ObjName' - Object name. If provided, then will search by  
    object name. Default is empty.  
    'RegionType' - Search region type: 'circ'|'box'.  
    Default is 'circ'.  
    'MaxRecord' - Max. number of records. Default is 100000.  
    'PathPar'   - Additional arguments to pass to  
    Catalog.VizieR.cdsclient_path.  
    Default is {}.  
    'ExpandTabs'- Exapnd tabs to spaces. Default is true.  
    Output : - Output catalog  
    - Cell array of column names.  
    - Cell array of column units.  
    - The complete result returned from CDS in a string format.  
    By : Eran O. Ofek                    Feb 2017  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: Cat=VO.VizieR.cds_astcat_search('finducac4',1,1,'OutType','table')  
    Cat=VO.VizieR.cds_astcat_search('finducac4',1,1,'OutType','astcat','CooUnits','rad')  
    Reliable: 2  
      
### VO.VizieR.cdsclient_path

Return the path of the local cdsclient directory Package: VO.VizieR Description: Return the path of the local cdsclient directory. Edit in order to change the parameters.


    
    Return the path of the local cdsclient directory  
    Package: VO.VizieR  
    Description: Return the path of the local cdsclient directory.  
    Edit in order to change the parameters.  
    Input  : * Optional keyword 'Path' and its value.  
    Default is '~/matlab/bin/cdsclient-3.80/'.  
    Output : - Local cdsclient directory path.  
    By : Eran O. Ofek                    Feb 2017  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: Path=VO.VizieR.cdsclient_path('Path','~/matlab/bin/cdsclient-3.80/');  
    Reliable: 2  
      
### VO.VizieR.cdsclient_prog_names

Return the list of programs in the cdsclient directory Package: VO.VizieR


    
    Return the list of programs in the cdsclient directory  
    Package: VO.VizieR  
    Input  : null  
    Output : - Strcture array of cdsclient directory content.  
    By : Eran O. Ofek                    Feb 2017  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: DirCon=VO.VizieR.cdsclient_prog_names; {DirCon.name}  
    Reliable: 2  
      
### VO.VizieR.construct_vizquery

Constrct a query string for the Vizier cdsclient command line Package: VO.VizieR Description: Constrct a query string for the Vizier cdsclient command line


    
    Constrct a query string for the Vizier cdsclient command line  
    Package: VO.VizieR  
    Description: Constrct a query string for the Vizier cdsclient command line  
    Input  : * Arbitrary number of pairs of ...,keyword,value,... arguments.  
    The following keywords are supported:  
    'CatName' - The cdclient program for the specific catalog.  
    See Catalog.VizieR.cdsclient_prog_names.  
    Default is 'finducac4'.  
    'RA'      - J2000.0 RA [sexagesimal, radians or deg].  
    'Dec'     - J2000.0 Dec [sexagesimal, radians or deg].  
    'CooUnits'- Coordinate units. Default is 'deg'.  
    'Radius'  - Search radius. Default is 10.  
    For box search this can be [X,Y] box size.  
    'RadiusUnits' - Search radius units. Default is 'arcmin'.  
    See convert.angular for options.  
    'ObjName' - Object name. If provided, then will search by  
    object name. Default is empty.  
    'RegionType' - Search region type: 'circ'|'box'.  
    Default is 'circ'.  
    'MaxRecord' - Max. number of records. Default is 100000.  
    'PathPar'   - Additional arguments to pass to  
    Catalog.VizieR.cdsclient_path.  
    Default is {}.  
    'IncludePath' - Include path in command string.  
    Default is true.  
    Output : - Command string  
    By : Eran O. Ofek                    Feb 2017  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: Path=VO.VizieR.construct_vizquery  
    Path=VO.VizieR.construct_vizquery('RA',90.0,'Dec',-15.2)  
    Reliable: 2  
      
      
