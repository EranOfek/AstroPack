function Result = populateDB( Data, Args )
    % Populate a database with metadata (header data) from a list of input images
    % Description: Populate a database with metadata (header data) from a list of input images
    % Input:   - Data : a cell array containing either 
    %               a) file names of FITS images or
    %               b) AstroImages or
    %               c) AstroHeaders
    %          * ...,key,val,...
    %          'DBname'        : DB name
    %          'DBtable'       : DB table
    %          'Hash'          : whether to calculate a hashsum of the file and add it to the table
    %          'FileNames'     : an optinal cell array of file names (if
    %          only AstroImages or AstroHeaders are provided)
    % Output : scalar success flag (0 -- images successfully added to the DB)         
    % Tested : Matlab R2020b
    % Author : A. Krassilchtchikov et al. (May 2023)
    % Example: db.populateDB ( Imfiles, 'DBtype', 'LAST', 'DBtable', 'RAW', 'Hash', Args.Hash );

    arguments
        
        Data                           % input images (file names or AstroImages) or AstroHeaders
        Args.DBname       = 'LAST';    % DB name
        Args.DBtable      = 'RAW';     % DB table
        Args.Hash logical = true;      % whether to calculate a hashsum and add it to the table
        Args.FileNames    = {};        % an optional cell array of file names (for the case the first argument is not a file list)
        
    end
    
    % determine the number of input images:
    
    NImg = numel(Data);
    
    % check whether it is possible to get files for the hash sum

    if numel(Args.FileNames) ~= NImg && ( isa(Data{1}, 'AstroImage') ||  isa(Data{1}, 'AstroHeader') )
        Args.Hash = false;
    end
    
    % populate the database
    
    switch lower(Args.DBname)
        
        case 'last'
            
            % Create a LastDb object with default connection parameters
            LDB = db.LastDb();
                    
            for Img = 1:1:NImg
                       
                if isa( Data{Img}, 'AstroImage' )
                    AH = Data{Img}.Header;
                elseif isa( Data{Img}, 'AstroHeader' )
                    AH = Data{Img};
                else
                    AH = AstroHeader( Data(Img), 1 ); 
                end
                        
                if Args.Hash
                    Sum_h64 = tools.checksum.xxhash('FileName', char( Data(Img) ) ); 
                else
                    Sum_h64 = '';
                end
            
                % populate the DB
                
                switch lower(Args.DBtable)          
                
                    case 'raw'
            
                        LDB.addRawImage(AH.File, AH, 'xxhash', Sum_h64);

                    case 'proc'
                   
                        LDB.addProcImage(AH.File, AH, 'xxhash', Sum_h64);

                    case 'coadd'
            
                        LDB.addCoaddImage(AH.File, AH, 'xxhash', Sum_h64);

                    otherwise
                    
                        error('The requested table does not exist yet, exiting..');
                        
                end
                
            end
                    
            % Queries and deleting records (tuples):
%     
%           Q = db.DbQuery('lastdb:raw_images');
%           Q.select('*', 'TableName','raw_images','OutType','Table')
%           Q.select('*', 'TableName','raw_images','Where', 'filename like ''%143%''','OutType','Table')
%           Q.deleteRecord('TableName', 'raw_images', 'Where', 'filename like ''%143%''')          
                                
        otherwise
            
            error('The requested DB does not exist, exiting..');
                    
    end
    
    %
    
    cprintf('hyper','The requested DB successfully populated with image metadata.\n');
    Result = 0;   % successfully populated

end
