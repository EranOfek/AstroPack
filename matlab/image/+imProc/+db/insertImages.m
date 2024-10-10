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
    %          ColNameDic = ["MIDJD", "RA", "DEC", "NODENUMB", "MOUNTNUM","CAMNUM", "CROPID", "ORIGSEC", "ORIGUSEC", "UNIQSEC"]
    %          T=imProc.db.insertImages(A,'ColNameDic',ColNameDic, 'ColJD','MIDJD')
    %
    %          [St(1:numel(ColNameDic)).ColName]=deal(ColNameDic{:});
    %          [St(8:10).ColFun] = deal(@(x) int16(imUtil.ccdsec.ccdsecStr2num(x)));
    %          St(8).ColNameOut = ["origsec_xmin", "origsec_xmax", "origsec_ymin", "origsec_ymax"];
    %          St(9).ColNameOut = ["origusec_xmin", "origusec_xmax", "origusec_ymin", "origusec_ymax"];
    %          St(10).ColNameOut = ["uniqsec_xmin", "uniqsec_xmax", "uniqsec_ymin", "uniqsec_ymax"];
    %          T=imProc.db.insertImages(A,'ColNameDic',St, 'ColJD','MIDJD')

    arguments
        Obj

        Args.Db           = [];
        Args.DbName       = [];
        Args.DbTable      = [];   % if empty then do not insert to Db

        Args.ColNameDic

        % Unique time ID indexing
        % bit encoded JD-J2000 in ms
        Args.ColIntTimeDB      = 'ID_TIME'; % column name in DB (skip if empty)
        Args.ColJD             = 'JD';  % column name or numeric (if numeric, then use as is)
        Args.IntTimeFun        = @(jd) uint64((jd-2451545.5).*86400.*1000);  % number of ms since J2000
        
        % Unique insert time ID indexing
        % bit encoded JD-J2000 in ms
        Args.ColInsertIntTimeDB     = 'ID_Time';    % column name in DB (skip if empty)

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
        Args.ColHealpix    = ["NSIDE_PARTITION", "NSIDE_LOW", "NSIDE_HIGH"];
        Args.UniqueID logical = true;

        % Write table
        Args.FileName   = '/home/eran/output.csv';  % tempname; % If empty, then skip this step (see writetable for more options)
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
    T  = tools.table.table_cell2string(T);
    Nt = size(T,1);

    % add Time unique ID
    if ~isempty(Args.ColIntTimeDB)
        T = db.util.insertIntegerTime2table(T, 'ColJD',Args.ColJD, 'ColIntTime', Args.ColIntTimeDB, 'IntTimeFun',Args.IntTimeFun);
    end
        
    % add insert time unique ID
    if ~isempty(Args.ColInsertIntTimeDB)
        InsertJD = celestial.time.julday;   % JD now
        T = db.util.insertIntegerTime2table(T, 'ColJD',InsertJD, 'ColIntTime', Args.ColInsertIntTimeDB, 'IntTimeFun',Args.IntTimeFun);
    end

    % % add instrument uniqye ID
    % if ~isempty(Args.ID_ColDB)
    %     Nc = numel(Args.ColID);
    %     InstID = zeros(Nt, Nc);
    %     for Ic=1:1:Nc
    %         InstID(:,Ic) = T.(Args.ColID{Ic});
    %     end
    %     T.(Args.ID_ColDB) = tools.bit.bitEncode(Args.BitDigits, InstID);
    % end           

    % add healpix ID
    T = db.util.insertHealpixIndex2table(T, 'ColRA',Args.ColRA, 'ColDec',Args.ColDec, 'CooUnits',Args.CooUnits,...
                                            'HealpixType',Args.HealpixType, 'HealpixLevel',Args.HealpixLevel,...
                                            'ColHealpix',Args.ColHealpix, 'UniqueID',Args.UniqueID);

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