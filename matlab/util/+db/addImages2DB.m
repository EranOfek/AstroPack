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
    % Output : scalar success flag (0 -- images successfully added to the DB)
    % Tested : Matlab R2020b
    % Author : A. Krassilchtchikov et al. (May 2023)
    % Example: db.addImages('DataDir','/home/sasha/Raw2/','DBname','LAST','DBtable','RAW');
    
    
    arguments
        
        Args.DataDir        =    '/home/sasha/Raw2/';                % The directory containing the input images
        Args.InputImages    =    'LAST*sci*raw_Image*.fits';         % The mask of the input image filenames
        Args.DBname         =    'LAST';
        Args.DBtable        =    'RAW';
        Args.Hash  logical  =    true;
        
    end
    
    % get a list of input files according to the input mask 
    
    Images  = dir ( fullfile(Args.DataDir,'**',Args.InputImages) );
    Imfiles = string(zeros(numel(Images),1));
    Str = repmat({''}, numel(Images),1);
    for Img = 1:1: numel(Images)
        Imfiles(Img) = fullfile(Images(Img).folder, Images(Img).name);
    end
    
    % call the sub to populate the database
                        
    db.populateDB ( Imfiles, 'DBname', Args.DBname, 'DBtable', Args.DBtable, 'Hash', Args.Hash );
    
    % assign the Result value
    
    Result = 0;   % success
    
                % Queries and deleting records (tuples):
%     
%           Q = db.DbQuery('lastdb:raw_images');
%           Q.select('*', 'TableName','raw_images','OutType','Table')
%           Q.select('*', 'TableName','raw_images','Where', 'filename like ''%143%''','OutType','Table')
%           Q.deleteRecord('TableName', 'raw_images', 'Where', 'filename like ''%143%''')          

end


