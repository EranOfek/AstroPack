function Result = addImages2DB(Args)
    % Add images from a directory to a database
    % Description: Add images from a directory to a database
    % Input:   - 
    %          * ...,key,val,...
    %          'DataDir'       : the root directory of a tree to search images within
    %          'InputImages'   : the mask of the input image filenames
    %          'DBname'        : DB name
    %          'DBtable'       : DB table
    %          'Hash'          : whether to calculate a hashsum of the file and add it to the table
    % Output : - scalar success flag (0 -- images successfully added to the DB)
    % Tested : Matlab R2020b
    % Author : A. Krassilchtchikov et al. (May 2023)
    % Examples: db.addImages2DB('DataDir','/home/sasha/Raw2/','DBname','LAST','DBtable','RAW');
    %           db.addImages2DB('DataDir','/home/sasha/Obs2/','InputImages','LAST*sci*proc_Image*.fits','DBtable','PROC')
    %           db.addImages2DB('DataDir','/home/sasha/Obs2/','InputImages','LAST*sci*coadd_Image*.fits','DBtable','COADD')
    
    
    arguments
        
        Args.DataDir        =    '/home/sasha/Raw/';                % The directory containing the input images
        Args.InputImages    =    'LAST*sci*raw_Image*.fits';         % The mask of the input image filenames
        Args.DBname         =    'LAST';
        Args.DBtable        =    'RAW';
        Args.Hash  logical  =    true;
        
    end
    
    % get a list of input files according to the input mask 
    
    ImageFiles  = dir ( fullfile(Args.DataDir,'**',Args.InputImages) );

    Imfiles = repmat({''}, numel(ImageFiles),1);

    Images  = Imfiles;
    Headers = Imfiles;

    for Img = 1:1: numel(ImageFiles)

        Imfiles{Img} = fullfile(ImageFiles(Img).folder, ImageFiles(Img).name);
        Images{Img} = AstroImage(Imfiles(Img));
        Headers{Img} = AstroHeader;
        Headers{Img}.Data = Images{Img}.Header;

    end
    
    % call the sub to populate the database
                        
    db.populateDB ( Imfiles, 'DBname', Args.DBname, 'DBtable', Args.DBtable, 'Hash', Args.Hash );
%     db.populateDB ( Images, 'DBname', Args.DBname, 'DBtable', Args.DBtable, 'Hash', Args.Hash );
%     db.populateDB ( Headers, 'DBname', Args.DBname, 'DBtable', Args.DBtable, 'Hash', Args.Hash );
    

    % assign the Result value
    
    Result = 0;   % success
    
                % Queries and deleting records (tuples):
%     
%           Q = db.DbQuery('lastdb:raw_images');
%           Q.select('*', 'TableName','raw_images','OutType','Table')
%           Q.select('*', 'TableName','raw_images','Where', 'filename like ''%143%''','OutType','Table')
%           Q.deleteRecord('TableName', 'raw_images', 'Where', 'filename like ''%143%''')          

end


