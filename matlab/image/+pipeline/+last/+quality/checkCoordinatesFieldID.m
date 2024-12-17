function [GoodFlag] = checkCoordinatesFieldID(T, Args)
    % Given a table of images queried from DB, check that the coordinates and fieldid are consistent
    % with the master fieldid table.
    % Input  : - Table which is the output of a DB query on last
    %            last_visist table.
    %          * ...,key,val,... 
    %            'FieldsList' - Option:
    %                    [] - generate using telescope.Scheduler.
    %                    [FieldID, RA, Dec] - matrix (all coo in deg).
    %                    string/char - table name containing the fields.
    %                   Default is [].
    %            'DB' - If 'FieldsList is char/string then DB object on
    %                   which to query the table must pbe provided.
    %                   Default is [].
    %            'MaxDist' - Max allowed distance [deg] between image
    %                   coordinates listed in table and the master
    %                   FieldsList. Default is 0.3.
    %            'ColFieldID' - Default is 'fieldid'.
    %            'ColRA' - Default is 'ra'.
    %            'ColDec' - Default is 'dec'.
    %            'ColTRA' - Default is 'm_ra'.
    %            'ColTDec' - Default is 'm_dec'.
    % Output : - A vector of logicals indicating if the coordinates are
    %            consistent (true) or not (false).
    % Author : Eran Ofek (2024 Dec) 
    % Example: GF = pipeline.last.quality.checkCoordinatesFieldID(T)

    arguments
        T
        Args.FieldsList        = []; % generate; 'string' - table name to query; matrix of [FieldID, RA, Dec]
        Args.DB                = [];
        Args.MaxDist           = 0.3;
        Args.ColFieldID        = 'fieldid';
        Args.ColRA             = 'ra';
        Args.ColDec            = 'dec';
        Args.ColTRA            = 'm_ra';
        Args.ColTDec           = 'm_dec';
        
    end
    RAD = 180./pi;

    if isempty(Args.FieldsList)
        % generate with default parameters
        S = telescope.Scheduler;
        S.generateRegularGrid;
        Args.FieldsList = [str2double(S.List.Table.FieldName), S.List.Table.RA, S.List.Table.Dec];
    else
        if isnumeric(Args.FieldsList)
            % do nothing
        elseif ischar(Args.FieldsList) || isstring(Args.FieldsList)
            % query
            if isempty(Args.DB)
                error('When FieldsList is char/string DB object must be provided');
            else
                Query = sprintf('SELECT %s, %s, %s FROM %s', Args.ColFieldID, Args,ColRA, Args.ColDec, Args.FieldsList);
                TFL   = Args.DB.query(Query);
                Args.FieldsList = [TFL.(Args.ColFieldID), TFL.(Args.ColRA), TFL.(Args.ColDec)];
            end
        else
            error('Unknown FieldsList type');
        end
    end

    Nt = size(T,1);
    GoodFlag = true(Nt,1);
    for It=1:1:Nt
        Tmp = split(T.(Args.ColFieldID),'.');
        BaseFieldID = str2double(Tmp{1});
        Ifid = find(BaseFieldID == Args.FieldsList(:,1));
        if numel(Ifid)==1
            Dist = celestial.coo.sphere_dist(T.(Args.ColTRA)(It)./RAD, T.(Args.ColTDec)(It)./RAD, Args.FieldsList(:,2)./RAD, Args.FieldsList(:,3)./RAD).*RAD;
            if Dist>Args.MaxDist
                GoodFlag(It) = false;
            end
        else
            error('FieldID found %d times in FieldsList',numel(Ifid));
        end
    end

end
