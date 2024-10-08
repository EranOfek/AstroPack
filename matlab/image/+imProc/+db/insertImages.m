function [T] = insertImages(Obj, Args)
    % One line description
    %     Optional detailed description
    % Input  : - 
    %          - 
    %          * ...,key,val,... 
    %            'Db' - Db class handle. If empty, then will create one.
    %                   If empty, then the db will be closed after
    %                   insertion, otherwise it will not be closed.
    %                   Default is [].
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
    % Output : - 
    % Author : Eran Ofek (2024 Oct) 
    % Example: A=AstroImage('LAST*coadd_Image_1.fits');
    %          ColNameDic = ["MIDJD", "RA", "DEC", "NODENUMB", "MOUNTNUM", "CAMNUM", "CROPID"]
    %          T=imProc.db.insertImages(A,'ColNameDic',ColNameDic)

    arguments
        Obj

        Args.Db           = [];
        Args.DbName       = [];
        Args.DbTable      = [];   % if empty then do not insert to Db

        Args.ColNameDic

        % Unique time ID indexing
        % bit encoded JD-J2000 in ms
        Args.TimeColDB     = 'ID_Time';    % column name in DB (skip if empty)
        Args.ColJD         = 'MIDJD';      % JD column name in input table
        Args.TimeIndexFun  = @(jd) uint64((jd-2451545.5).*86400.*1000);  % number of ms since J2000

        % Unique insert time ID indexing
        % bit encoded JD-J2000 in ms
        Args.InsertTimeColDB     = 'ID_Time';    % column name in DB (skip if empty)

        % Unique instrument ID indexing
        % bit encoded instrument information
        Args.ID_ColDB      = 'ID_Inst';     % column name in DB (skip if empty)
        Args.ColID         = ["NODENUMB", "MOUNTNUM", "CAMNUM", "CROPID"];  % Columns in input table from which to compose the ID
        Args.BitDigits     = [16 16 16 16];  % number of bits per ColID

        % Healpix indexing
        Args.ColRA         = 'RA';
        Args.ColDec        = 'DEC';
        Args.CooUnits      = 'deg';
        Args.HealpixType   = 'nested';
        Args.HealpixLevel  = 2.^[3, 8, 16];   % diamater ~ 13 deg, 0.4 deg, 5.7"
        Args.HealpixColDB  = ["NSIDE_PARTITION", "NSIDE_LOW", "NSIDE_HIGH"];

        % Write table
        Args.FileName   = 'output.csv';  % tempname; % If empty, then skip this step (see writetable for more options)
        Args.FileType   = 'text';        % see writetable for optoins
        Args.WriteVarNames = {};
        Args.Delimiter     = ',';
        Args.LineEnding    = '\r\n';
        Args.WriteVariableNames logical  = false;
        Args.QuoteStrings                = 'minimal';
        Args.WriteMode                   = 'overwrite';
        Args.writetableArgs              = {};
        Args.DeleteFile logical          = false;  % delete file after Db insertion
    end

    % convert headers to table
    T  = imProc.header.headers2table(Obj, 'OutType','table', 'ColNameDic',Args.ColNameDic);
    Nt = size(T,1);

    % add Time unique ID
    if ~isempty(Args.TimeColDB)
        % populate the TimeColDB
        T.(Args.TimeColDB) = Args.TimeIndexFun(T.(Args.ColJD));
    end

    % add insert time unique ID
    if ~isempty(Args.InsertTimeColDB)
        InsertJD = celestial.time.julday;   % JD now
        T.(Args.InsertTimeColDB) = Args.TimeIndexFun(repmat(InsertJD, Nt, 1));
    end


    % add instrument uniqye ID
    if ~isempty(Args.ID_ColDB)
        Nc = numel(Args.ColID);
        InstID = zeros(Nt, Nc);
        for Ic=1:1:Nc
            InstID(:,Ic) = T.(Args.ColID{Ic});
        end
        T.(Args.ID_ColDB) = tools.bit.bitEncode(Args.BitDigits, InstID);
    end           


    % add healpix ID
    Nlevel = numel(Args.HealpixLevel);
    for Ilevel=1:1:Nlevel
        T.(Args.HealpixColDB{Ilevel}) = celestial.healpix.ang2pix(Args.HealpixLevel(Ilevel), T.(Args.ColRA), T.(Args.ColDec), 'Type',Args.HealpixType, 'CooUnits',Args.CooUnits);
    end

    % write table to csv file
    writetable(T, Args.FileName, 'FileType',Args.FileType,...
                                 'Delimiter',Args.Delimiter,...
                                 'LineEnding',Args.LineEnding,...
                                 'WriteVariableNames',Args.WriteVariableNames,...
                                 'QuoteStrings',Args.QuoteStrings,...
                                 'WriteMode',Args.WriteMode,...
                                 Args.writetableArgs{:});


    % insert csv to db
    if ~isempty(Args.DbTable)
        if isempty(Args.Db)
            Db = db.Db;
        else
            Db = Args.Db;
        end
    
        DbTableStr = db.Db.concatDbTable(Args.DbName, Args.DbTable);

        Db.insert(DbTableStr, Args.FileName);
        if isempty(Args.Db)
            Db.disconnectCH_Java % disconnect Java
        end
    end

    if Args.DeleteFile
        delete(Args.FileName);
    end

end
