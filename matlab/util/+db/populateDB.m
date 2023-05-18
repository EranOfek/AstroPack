function Result = populateDB( ImageFiles, Args )
    % Populate a database with a list of input images
    % Description: Populate a database with a list of input images
    % Input:   - ImageFiles         : a vector of input file names
    %          - Args.DBname        : DB name
    %          - Args.DBtable       : DB table
    %          - Args.Hash          : whether to calculate a hashsum of the file and add it to the table
    %          
    % Tested : Matlab R2020b
    %     By : A. Krassilchtchikov et al.    May 2023
    % Example: db.populateDB ( Imfiles, 'DBtype', 'LAST', 'DBtable', 'RAW', 'Hash', Args.Hash );

    arguments
        
        ImageFiles             % input images
        Args.DBname  = 'LAST'; % DB name
        Args.DBtable = 'RAW';  % DB table
        Args.Hash    = 1;      % whether to calculate a hashsum and add it to the table
        
    end
    
    % determine the number of input images:
    
    NImg = numel(ImageFiles);
    
    % populate the database
    
    switch lower(Args.DBname)
        
        case 'last'
            
            % Create a LastDb object with default connection parameters
            LDB = db.LastDb();
                    
            for Img = 1:1:NImg
                       
                AH    = AstroHeader(ImageFiles(Img), 1); 
                        
                if Args.Hash
                    Sum_h64 = tools.checksum.xxhash('FileName', char( ImageFiles(Img) ) ); 
                else
                    Sum_h64 = '';
                end
            
                % populate the DB
                
                switch lower(Args.DBtable)          
                
                    case 'raw'
            
                        LDB.addRawImage(AH.File, AH, 'xxhash', Sum_h64);

                    case 'proc'
                   
%                         LDB.addProcImage(AH.File, AH, 'xxhash', Sum_h64);
                        cprintf('err','The requested table does not exist yet, exiting..');
                        return
                        
                    otherwise
                    
                        cprintf('err','The requested table does not exist, exiting..');
                        return
                        
                end
                
            end
                    
            % Queries and deleting records (tuples):
%     
%           Q = db.DbQuery('lastdb:raw_images');
%           Q.select('*', 'TableName','raw_images','OutType','Table')
%           Q.select('*', 'TableName','raw_images','Where', 'filename like ''%143%''','OutType','Table')
%           Q.deleteRecord('TableName', 'raw_images', 'Where', 'filename like ''%143%''')          
                                
        otherwise
            
            cprintf('err','The requested DB does not exist, exiting..');
            return
                    
    end
    
    %
    
    cprintf('hyper','The requested DB successfully populated with image metadata.\n');
    Result = 0;   % successfully populated

end
