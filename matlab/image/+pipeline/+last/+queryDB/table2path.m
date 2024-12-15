function [AFN, AllPaths, AllFiles] = table2path(T, Args)
    % Given table with entries from last_visits DB table, prep full path + file nemaes to images.
    % Input  : - A matlab table which is the output of a query on the
    %            last.last_visits DB table.
    %          * ...,key,val,... 
    %            'BasePath' - BasePath for arcive data.
    %                   Default is '/marvin'.
    %            'DefCounter' - Counter value. Default is 0.
    %            'DefType' - Type. Default is 'sci'.
    %            'DefLevel' - Level. Default is 'coadd'.
    %            'DefProduct' - Product. Default is 'Image'.
    %
    %            See code for additional arguments.
    %
    % Output : - AstroFileName object with the requested entries.
    %          - A string arry of file path for each entry in table.
    %          - A string arry of file name for each entry in table.
    %          
    % Author : Eran Ofek (2024 Dec) 
    % Example: [A, P,F]=pipeline.last.queryDB.table2path(T);

    arguments
        T
        Args.BasePath          = '/marvin';

        Args.DefCounter        = 0;
        Args.DefType           = 'sci';
        Args.DefLevel          = 'coadd';
        Args.DefProduct        = 'Image';

        Args.ProjName          = 'LAST';
        Args.ColNode           = 'nodenumb';
        Args.ColMount          = 'mountnum';
        Args.ColCam            = 'camnum';
        Args.ColJD             = 'jd_start';
        Args.ColFilter         = 'filter';
        Args.ColFieldID        = 'fieldid';
        Args.ColCCDID          = 'ccdid';
        Args.ColCropID         = 'cropid';
        Args.ColSubDir         = 'subdir';
        
    end

    AFN = AstroFileName;
    AFN.ProjName = {Args.ProjName, T.(Args.ColNode), T.(Args.ColMount), T.(Args.ColCam)};

    AFN.JD       = T.(Args.ColJD);
    AFN.julday2time;
    AFN.Filter   = T.(Args.ColFilter);
    AFN.Counter  = Args.DefCounter;
    AFN.FieldID  = T.(Args.ColFieldID);
    AFN.CCDID    = T.(Args.ColCCDID);
    AFN.CropID   = T.(Args.ColCropID);
    AFN.SubDir   = T.(Args.ColSubDir);

    AFN.Type     = Args.DefType;
    AFN.Level    = Args.DefLevel;
    AFN.Product  = Args.DefProduct;

    if nargin>1
        AllPaths      = AFN.genPath([],'AddSubDir',true);
        AllFiles      = AFN.genFile([]);
    end

end
