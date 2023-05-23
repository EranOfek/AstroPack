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




% 
% 
% function Result = fillRaw(Args)
%     % Populate a database from a catalog of RAW images
%     % Package: db.LastDb
%     % Description: Make a mosaic sky image from a set of input AstroImages
%     %          - Args.DataDir       : The top directory containing the input images
%     %          - Args.InputImages   : The mask of the input image filenames
%     %          
%     % Tested : Matlab R2020b
%     %     By : A. Krassilchtchikov et al.    May 2023
%     % Example: TBD
% 
%     arguments
%         
%         Args.DataDir        =    '/home/sasha/Raw2/';            % The directory containing the input images
%         Args.InputImages    =    'LAST*raw_Image*.fits';        % The mask of the input image filenames
%         
%     end
% 
% %     % Create tables (if needed)
% %     LDB.createTables();
%     
%     % Create a LastDb object with default connection parameters
%     LDB = db.LastDb();
%     
%     % Find the image files according to the input mask 
%     ImFiles = dir ( fullfile(Args.DataDir,'**',Args.InputImages) );
%     NImage  = size( ImFiles, 1 );
%         
%     % Load AstroHeader object from image FITS file
%     
% %     cd(Args.DataDir);
% %     FN=FileNames.generateFromFileName( Args.InputImages );  
% 
% %    FileList = genFull(FN);   
% %     AH = AstroHeader(Args.InputImages, 1); 
% %     
% %     NImage = size(FileList,1);
% %    
% % %     LDB.addRawImage(FileName, AH);
% %     for Img = 1:1:NImage
% %         FileName = FileList{Img};
% %         AHead = AH(Img);
% %         LDB.addRawImage(FileName, AHead);
% % %         LDB.addRawImage(FileList{Img}, AH(Img));
% %     end
% 
%     % add the rows to the database
%     for Img = 1:1:NImage
%         
%         Image = strcat(ImFiles(Img).folder,'/',ImFiles(Img).name);
%         AH    = AstroHeader(Image, 1); 
%         Image_h64 = tools.checksum.xxhash('FileName', Image); 
%         LDB.addRawImage(AH.File, AH, 'xxhash', Image_h64);
%         
%     end
%     
%     % Queries and deleting records (tuples):
%     
%     Q = db.DbQuery('lastdb:raw_images');
%     Q.select('*', 'TableName','raw_images','OutType','Table')
%     Q.select('*', 'TableName','raw_images','Where', 'filename like ''%143%''','OutType','Table')
% %     Q.deleteRecord('TableName', 'raw_images', 'Where', 'filename like ''%143%''')
% 
% end