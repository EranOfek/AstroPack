function [T,Error,FileName] = insertImages(Obj, Args)
    % Insert images (headers) to DB
    %       Including the following steps:
    %       1. Convert AstroImages headers to table with the requested
    %       columns.
    %       2. Add Image ID based on time stamp and other info.
    %       3. Add healpix indices
    %       4. write table to csv file.
    %       5. Insert to DB.
    % Input  : - AstroImage array of images with headers.
    %          * ...,key,val,... 
    %            'Db' - Db class handle. If empty, then will create one.
    %                   If empty, then the db will be closed after
    %                   insertion, otherwise it will not be closed.
    %                   Default is [].
    %            'DbName' - DB name. Default is [].
    %            'DbTable' - DB table. If empty, then will not write table
    %                   to DB. Default is [].
    %            'ColNameDic' - Either a cell array of header keywords to
    %                   extract from headers and insert to output table, or
    %                   a structure array with element per column to
    %                   extract and the following fields:
    %                   .ColName - Column name to extract.
    %                   .ColFun - A function to apply to the extracted
    %                           value. If empty, do not apply function.
    %                           Default is empty.
    %                   .ColNameOut - The column name in the output table.
    %                           If empty, use input column name.
    %                           Default is empty.
    %                           If the ColFun returns more then one input,
    %                           then this should be a cell array of output
    %                           column names, per each one of the outputs.
    %                  This argument must be provided.
    %                  You can use: db.util.read_xls2tableFormat to
    %                  generate it.
    %            'ID_Origin' - Behaviore regardind adding image ID to
    %                   table. The ID is writen into column name defined in
    %                   'ColNameID'.
    %                   If [], then do not add image ID to table.
    %                   If vector then add it to table as image ID.
    %                   If NaN, then generate image ID using:
    %                           imProc.db.generateImageID
    %                   Default is NaN.
    %            'ColNameID' - Column name in which to write the image ID.
    %                   Default is 'id_proc'.
    %            'FormatStID' - A structure array containing information on how
    %                   to construct the ID. The following fields should be
    %                   provided:
    %                   .Key - Header keyword name from which to retrieve
    %                       the ID that will be stored in a sub array of
    %                       bits corresponding to this key.
    %                   .BitNum - The number of bits in the sub array.
    %                   .Fun - A function handle to apply to the keyword
    %                       value in order to get the value to enode in the
    %                       sub array of bits.
    %                   Default: see code.
    %             'ColRA' - Table column name containing J2000 RA.
    %                   Used for BJD and Healpix. Default is 'RA'.
    %             'ColDec' - Table column name containing J2000 Dec.
    %                   Used for BJD and Healpix. Default is 'DEC'.
    %             'CooUnits' - RA/Dec coo units. Default is 'deg'.
    %             'HealpixType' - Healpix type. If empty, then do not add
    %                   healpix index.
    %                   Default is 'nested'.
    %             'HealpixLevel' - Healpix level.
    %                   Default is 2.^[3, 8, 16]
    %             'ColHealpix' - Column names of healpix indices.
    %                   Default is ["UPIX_PARTITION", "UPIX_LOW", "UPIX_HIGH"]
    %             'UniqueID' - A logical indicating if healpix index is
    %                   unique ID. Default is true.
    %
    %             'CreateCsv' - A logical indicating if to create A CSV file.
    %                   Default is true.
    %             'FileName' - The csv file name that will be created.
    %                   Default is tempname.
    %             'DeleteFile' - A logical indicating if to delete
    %                   the csv file after insertion.
    %                   Default is false.
    %             'table2csvArgs' - A cell array of additional
    %                   arguments to pass to db.Db.table2csv.
    %                   Default is {}.
    %
    % Output : - A table object with the additional columns.
    %          - The CSV file name.
    %            If you want to see this file you have to set:
    %            'DeleteFile',false, 'CreateCsv',true
    % Author : Eran Ofek (2024 Oct) 
    % Example: A=AstroImage('LAST*coadd_Image_1.fits');
    %          ColNameDic = ["MIDJD", "RA", "DEC", "NODENUMB", "MOUNTNUM","CAMNUM", "CROPID", "ORIGSEC", "ORIGUSEC", "UNIQSEC"]
    %          T=imProc.db.insertImages(A,'ColNameDic',ColNameDic, 'ColJD','MIDJD')
    %
    %          [St(1:numel(ColNameDic)).ColName]=deal(ColNameDic{:});
    %          [St(8:10).ColFun] = deal(@(x) int16(imUtil.ccdsec.ccdsecStr2num(x)));
    %          St(8).ColNameOut = ["origsec_xmin", "origsec_xmax", "origsec_ymin", "origsec_ymax"];
    %          St(9).ColNameOut = ["origusec_xmin", "origusec_xmax", "origusec_ymin", "origusec_ymax"];
    %          St(10).ColNameOut = ["uniqsec_xmin", "uniqsec_xmax", "uniqsec_ymin", "uniqsec_ymax"];
    %          T=imProc.db.insertImages(A,'ColNameDic',St, 'ColJD','MIDJD')
    %
    %          R=db.util.read_xls2tableFormat;
    %          T=imProc.db.insertImages(A,'ColNameDic',R);

    arguments
        Obj

        Args.Db           = [];
        Args.DbName       = [];
        Args.DbTable      = [];   % if empty then do not insert to Db

        Args.ColNameDic

        %
        Args.ID_Origin     = NaN; %[];   % [] - no ID; NaN - generate; number
        Args.ColNameID     = 'id_proc';
        
        Args.FormatStID    = [];

        % Healpix indexing
        Args.ColRA         = 'RA';
        Args.ColDec        = 'DEC';
        Args.CooUnits      = 'deg';
        Args.HealpixType   = 'nested';
        Args.HealpixLevel  = 2.^[3, 8, 16];   % diameter ~ 13 deg, 0.4 deg, 5.7"
        Args.ColHealpix    = ["UPIX_PARTITION", "UPIX_LOW", "UPIX_HIGH"];
        Args.UniqueID logical = true;

        % Write table
        Args.CreateCsv logical    = true;
        Args.FileName             = tempname; % If empty, then skip this step (see writetable for more options)
        Args.DeleteFile logical   = false;  % delete file after Db insertion
        Args.table2csvArgs        = {};
        
    end

    % convert headers to table
    T  = imProc.header.headers2table(Obj, 'OutType','table', 'ColNameDic',Args.ColNameDic);        
    T  = tools.table.table_cell2string(T);
    Nt = size(T,1);

    % add Time unique ID
    %if ~isempty(Args.ColIntTimeDB)
    %    T = db.util.insertIntegerTime2table(T, 'ColJD',Args.ColJD, 'ColIntTime', Args.ColIntTimeDB, 'IntTimeFun',Args.IntTimeFun);
    %end
        
    % add insert time unique ID
    %if ~isempty(Args.ColInsertIntTimeDB)
    %    InsertJD = celestial.time.julday;   % JD now
    %    T = db.util.insertIntegerTime2table(T, 'ColJD',InsertJD, 'ColIntTime', Args.ColInsertIntTimeDB, 'IntTimeFun',Args.IntTimeFun);
    %end

    if isempty(Args.ID_Origin)
        % skip - do not add ID
    else
        if isnan(Args.ID_Origin)
            % Generate ID using: imProc.db.generateImageID
            [~,ID] = imProc.db.generateImageID(Obj, 'KeyID',[], 'FormatSt',Args.FormatStID);
        else
            % User provided ID
            ID = Args.ID_Origin;
        end
        % insert ID to table
        T.(Args.ColNameID) = ID;
    end

    % clean bad lines were RA or Dec = NaN:
    Ind = find(isnan(T.RA) | isnan(T.DEC));
    T(Ind,:) = [];

    % add healpix ID
    if ~isempty(Args.HealpixType)
        T = db.util.insertHealpixIndex2table(T, 'ColRA',Args.ColRA, 'ColDec',Args.ColDec, 'CooUnits',Args.CooUnits,...
                                            'HealpixType',Args.HealpixType, 'HealpixLevel',Args.HealpixLevel,...
                                            'ColHealpix',Args.ColHealpix, 'UniqueID',Args.UniqueID);
    end

    if Args.CreateCsv
        FileName = erase(Args.FileName,' ');
        db.Db.table2csv(T, 'FileName',FileName, Args.table2csvArgs{:});        
    else
        FileName = [];
    end
    
    % insert csv to db
    if ~isempty(Args.DbTable)
        if isempty(Args.Db)
            Db = db.Db;
        else
            Db = Args.Db;
        end
    
        DbTableStr = db.Db.concatDbTable(Args.DbName, Args.DbTable);

        [Error, FileName]=Db.insertCsv(DbTableStr, FileName, 'FileName',FileName, 'DeleteFile',Args.DeleteFile, 'table2csvArgs',Args.table2csvArgs);
        if isempty(Args.Db)
            Db.disconnectCH_Java % disconnect Java
        end
    end

    if Args.DeleteFile
        delete(Args.FileName);
    end

end
