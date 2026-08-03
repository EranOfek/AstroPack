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
    %            If the box crosses RA=0, then the first constraint is
    %            applied to an SQL expression of the RA offset from the
    %            search position, rather than to the RA column itself.
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

    HalfWidthRA = Args.HalfWidth(1)./cosd(Dec);
    RA1  = RA - HalfWidthRA;
    RA2  = RA + HalfWidthRA;
    Dec1 = Dec - Args.HalfWidth(2);
    Dec2 = Dec + Args.HalfWidth(2);

    % does the box cross RA=0?
    CrossZero = RA1<0 || RA2>360;

    if Dec2>90
        Dec2      = 90;
        RA1       = 0;
        RA2       = 360;
        CrossZero = false;
    end
    if Dec1<-90
        Dec1      = -90;
        RA1       = 0;
        RA2       = 360;
        CrossZero = false;
    end

    if CrossZero
        % Constrain the RA offset from the search position rather than RA itself.
        % A single range constraint then covers both sides of the RA=0 crossing
        % (two RA ranges would be combined by genWhereClause with AND) (issue #579).
        ColRA = sprintf('(modulo(%s-%.15g+540,360)-180)', Args.ColRA, RA);
        Result = {ColRA,       [-HalfWidthRA, HalfWidthRA];
                  Args.ColDec, [Dec1, Dec2]};
    else
        Result = {Args.ColRA,  [RA1, RA2];
                  Args.ColDec, [Dec1, Dec2]};
    end

end

