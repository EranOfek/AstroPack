function [T,Error,FileName] = insertCatalog(Obj, Args)
    % Insert catalogs in AstroImage to DB
    %       Including the following steps:
    %       1. Convert AstroImages catalog to table with the requested
    %       columns.
    %       2. Add Image ID based on time stamp and other info.
    %       3. Add healpix indices
    %       4. write table to csv file.
    %       5. Insert to DB.
    % Input  : - self.
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
    %            'GeoPos' - Geodetic position. If [], then assume geocentric position
    %                   and return zeros. Otherwise should be [Long, Lat, Height]
    %                   in [rad, rad, m]. Default is [].
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
    %          - Error message
    %          - The CSV file name.
    %            If you want to see this file you have to set:
    %            'DeleteFile',false, 'CreateCsv',true
    % Author : Eran Ofek (2024 Oct) 
    % Example: 

    arguments
        Obj
        Args.Header       = [];     % if the object is an AstroCatalog, we need to provide AstroHeader as well 
        
        Args.Db           = [];
        Args.DbName       = [];
        Args.DbTable      = [];
        Args.ColNameDic      
        Args.GeoPos       = [];
        Args.InTimeScale  = 'UTC';
        Args.ColJD        = 'JD';
        Args.ColRA        = 'RA';   % J2000
        Args.ColDec       = 'DEC';  % J2000
        Args.CooUnits     = 'deg';
        Args.VelOutUnits  = 'cm/s';
        Args.INPOP        = [];

        Args.InsertID logical  = true;
        Args.KeyID        = 'ID_PROC_IM';
        Args.generateImageID_Args = {};

        Args.AddSrcID logical = true;
        Args.ColSrcID         = 'ID_PROC_SRC';

        % Healpix indexing
        %Args.ColRA         = 'RA';
        %Args.ColDec        = 'DEC';
        %Args.CooUnits      = 'deg';
        Args.HealpixType   = 'nested';
        Args.HealpixLevel  = 2.^[3, 8, 16];   % diamater ~ 13 deg, 0.4 deg, 5.7"
        Args.ColHealpix    = ["UPIX_PARTITION", "UPIX_LOW", "UPIX_HIGH"];
        Args.UniqueID logical = true;
        %Args.insertHealpixArgs    = {};

        Args.ColBJD       = 'BJD'; % if [] - do not add
        Args.ColBaryVel   = 'BARYVEL'; % if [] - do not add

        % Write table
        Args.CreateCsv logical    = true;
        Args.FileName   = tempname; % If empty, then skip this step (see writetable for more options)
        Args.table2csvArgs = {};
        Args.DeleteFile logical          = false;  % delete file after Db insertion

    end

    Nobj = numel(Obj);
    % read each catalog, selct columns, and convert their names
    for Iobj=1:1:Nobj
        if isa(Obj, 'AstroImage')
            Tmp = Obj(Iobj).CatData.Table;

            
            if Args.InsertID
                % get Image ID from header
                ID = Obj(Iobj).HeaderData.getKeyVal(Args.KeyID, 'Val2Num',false);
                if ischar(ID)
                    ID = tools.string.mex.str2uint64(ID);
                elseif isnan(ID)
                    [~,ID]=imProc.db.generateImageID(Obj(Iobj), Args.generateImageID_Args{:});
                end
            else
                ID = [];
            end
        elseif isa(Obj, 'AstroCatalog') || isstruct(Obj) % struct -- the table of Asteroids 
            Tmp = Obj(Iobj).Table;
            
            % add some columns from the header:           
            Nrow = size(Tmp,1);
            Tmp.('NODENUMB') = repmat(Args.Header(Iobj).getVal('NODENUMB'), Nrow,1);
            Tmp.('MOUNTNUM') = repmat(Args.Header(Iobj).getVal('MOUNTNUM'), Nrow,1);
            Tmp.('CAMNUM')   = repmat(Args.Header(Iobj).getVal('CAMNUM'),   Nrow,1);
            Tmp.('INGESTION_TIME_JD') = repmat(Args.Header(Iobj).getVal('INGESTION_TIME_JD'), Nrow,1);
            
            if Args.InsertID
                % get Image ID from header
                ID = Args.Header(Iobj).getVal(Args.KeyID, 'Val2Num',false);
                if ischar(ID)
                    ID = tools.string.mex.str2uint64(ID);
                elseif isnan(ID)
                    [~,ID]=imProc.db.generateImageID(Args.Header(Iobj), Args.generateImageID_Args{:});
                end
            else
                ID = [];
            end
        else
            ID  = [];
            Tmp = Obj(Iobj).Table;
        end

        % select tables
%         Tmp = Tmp.({Args.ColNameDic.ColName});  %% this line does not work, the following 2 lines do the job:
        Tmp.Properties.VariableNames = upper(Tmp.Properties.VariableNames);
        Tmp = Tmp(:, ismember(Tmp.Properties.VariableNames, {Args.ColNameDic.ColName}));
        
        % run functions
        %IndFun = find(~tools.cell.isempty_cell({Args.ColNameDic.ColFun}));
        %for If=1:1:numel(IndFun)

        % change column names
%         Tmp.Properties.VariableNames = Args.ColNameDic.ColNameOut; %% this line does not work, the following 4 lines do the job:        
        VarNames = Tmp.Properties.VariableNames;
        [IsMatch, idx] = ismember(VarNames, {Args.ColNameDic.ColName});
        VarNames(IsMatch) = cellstr([Args.ColNameDic(idx(IsMatch)).ColNameOut]);
        Tmp.Properties.VariableNames = VarNames;     

        % insert additional columns - cat by cat
        Nrow = size(Tmp,1);
        % insert ID:
        if ~isempty(ID)
            Tmp.(Args.KeyID) = repmat(ID, Nrow,1);
        end

        if Args.AddSrcID
            Nsrc = size(Tmp,1);
            Tmp.(Args.ColSrcID) = (1:1:Nsrc).';
        end

        % concat all tables
        if Iobj==1
            T = Tmp;
        else
            T = [T;Tmp];
        end

    end
    
    % clean the tables from the lines containing JD, RA or Dec = NaN:
    T(isnan(T.(Args.ColJD)) | isnan(T.(Args.ColRA)) | isnan(T.(Args.ColDec)), :) = [];

    % insert additional global columns

    % insert BJD
    if ~isempty(Args.ColBJD)
        if ~isempty(Args.ColBaryVel)
            [BJD, BVel] = celestial.time.barycentricJD(T.(Args.ColJD), T.(Args.ColRA), T.(Args.ColDec), 'INPOP',Args.INPOP,...
                                            'GeoPos',Args.GeoPos,...
                                            'InTimeScale',Args.InTimeScale,...
                                            'CooUnits',Args.CooUnits,...
                                            'VelOutUnits',Args.VelOutUnits);
            T.(Args.ColBaryVel) = BVel;
        else
            [BJD] = celestial.time.barycentricJD(T.(Args.ColJD), T.(Args.ColRA), T.(Args.ColDec), 'INPOP',Args.INPOP,...
                                            'GeoPos',Args.GeoPos,...
                                            'InTimeScale',Args.InTimeScale,...
                                            'CooUnits',Args.CooUnits,...
                                            'VelOutUnits',Args.VelOutUnits);
        end
        T.(Args.ColBJD) = BJD;
    end

    % insert healpix index
    T=db.util.insertHealpixIndex2table(T, 'ColRA',Args.ColRA, 'ColDec',Args.ColDec, 'CooUnits',Args.CooUnits,...
                                          'HealpixType',Args.HealpixType, 'HealpixLevel',Args.HealpixLevel,...
                                          'ColHealpix',Args.ColHealpix, 'UniqueID',Args.UniqueID);


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

        [Error, FileName]=Db.insertCsv(DbTableStr, FileName, 'FileName',FileName, 'DeleteFile',Args.DeleteFile, ...,
                             'ColumnNames',lower(T.Properties.VariableNames),'table2csvArgs',Args.table2csvArgs);
        if isempty(Args.Db)
            Db.disconnectCH_Java % disconnect Java
        end
    end

    if Args.DeleteFile
        delete(Args.FileName);
    end


end
