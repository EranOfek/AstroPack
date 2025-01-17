function Result = queryCooBoxConstraints(RA, Dec, Args)
    % Generate approximate constraints for a coordinates box search
    % taking into account poles and RA zero crossings.
    % In the case of near poles a conservative search region is
    % used.
    % Input  : - RA [deg].
    %          - Dec [deg].
    %          * ...,key,val,...
    %            'HalfWidth' - Box half width [deg].
    %                   Default is [0.55 0.55].
    %            'ColRA' - RA column name. Default is 'ra'.
    %            'ColDec' - dec column name. Default is 'dec'.
    % Output : - A cell array of constraints on RA and Dec.
    %            Use genWhereClause and genQuery in order to use
    %            this constraints to generate a select/where
    %            clause.
    % Author : Eran Ofek (Dec 2024)
    % Example: R=db.search.queryCooBoxConstraints(0.1,20)

    arguments
        RA
        Dec
        Args.HalfWidth = [0.55 0.55];

        Args.ColRA     = 'ra';
        Args.ColDec    = 'dec';
    end

    if numel(Args.HalfWidth)==1
        Args.HalfWidth = [Args.HalfWidth, Args.HalfWidth];
    end

    RA2 = RA + Args.HalfWidth(1)./cosd(Dec);
    RA1 = RA - Args.HalfWidth(1)./cosd(Dec);
    Dec1 = Dec - Args.HalfWidth(2);
    Dec2 = Dec + Args.HalfWidth(2);

    if Dec2>90
        Dec2 = 90;
        RA1  = 0;
        RA2  = 360;
    end
    if Dec1<-90
        Dec1 = -90;
        RA1  = 0;
        RA2  = 360;
    end
    if RA1<0
        RA1 = [RA1 0];
        RA2 = [360 RA2];
    end
    if RA2>360
        RA2 = [360 RA2];
        RA1 = [RA1 0];
    end
    Const = {Args.ColRA, [RA1(1), RA2(1)];
             Args.ColDec, [Dec1, Dec2]};

    if numel(RA1)>1
        Const1 = {Args.ColRA, [RA1(2), RA2(2)]};
    else
        Const1 = {};
    end
    Result = [Const; Const1];


end

