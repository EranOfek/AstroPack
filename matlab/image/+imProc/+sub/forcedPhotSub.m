function [AC] = forcedPhotSub(AD, Coo, Args)
    % Forced photometry for subtraction images and its meta data.
    %   This function uses: imProc.sources.forcedPhot
    %   In addition for forced photometry on the D image, this function
    %   also perform forced phot on the new and ref images as well as S,
    %   Scorr and Z2 images.
    % Input  : - An AstroZOGY object.
    %          - A two column matrix of [RA, Dec] or [X,Y] coordinates.
    %          * ...,key,val,... 
    %            'CooUnits' - Input coordinate units. Options are:
    %                   'pix' - pixel coordinates.
    %                   'deg'|'rad' - units of ra/dec.
    %                   Default is 'deg'.
    %            'MaxIter' - Maximum iterations in forced phot.
    %                   Default is 0 (i.e., not position refinment).
    %            'PrefixNew' - Prefix to be added to the columns
    %                   originating from the new image.
    %                   Default is 'New_'.
    %            'PrefixRef' - Prefix to be added to the columns
    %                   originating from the ref image.
    %                   Default is 'Ref_'.
    %            'ColJD' - JD of new column name. If empty, then do not add
    %                   JD. Default is 'JD'.
    %            'ColRefJD' - JD of ref column name. If empty, then do not add
    %                   JD. Default is 'RefJD'.
    %            'ColS' - Like JD, but for the S value at the position.
    %                   Default is 'S'.
    %            'ColScorr' - Like JD, but for the Scorr value at the position.
    %                   Default is 'Scorr'.
    %            'ColZ2' - Like JD, but for the Z2 value at the position.
    %                   Default is 'Z2'.
    %            'NewHeaderKeys' - List of columns to be retrievd from the
    %                   New image header and added to table.
    %                   Default is {'LIMMAG', 'FIELDID', 'MOUNTNUM', 'CAMNUM', 'CROPID'}
    %            'RefHeaderKeys' - List of columns to be retrievd from the
    %                   Ref image header and added to table.
    %                   Default is {'LIMMAG'}
    %            'HeaderKeys' - List of columns to be retrievd from the
    %                   diff image header and added to table.
    %                   Default is {'LIMMAG'}
    %            'ConcatData' - Either a tatble or struct array of columns
    %                   to add to the table.
    %            'KeyJD' - Optional JD header keyword (for expediting
    %                   code).
    %                   Default is [].
    %
    % Output : - An AstroCatalog object with element per subtraction
    %            object. The catalg contains a table with all the extracted
    %            information.
    % Author : Eran Ofek (2025 Apr) 
    % Example: AC=imProc.sub.forcedPhotSub(AD, [RA, Dec])

    arguments
        AD
        Coo
        Args.CooUnits          = 'deg';
        Args.MaxIter           = 0;
        Args.PrefixNew         = "New_";
        Args.PrefixRef         = "Ref_";
        
        Args.ColJD             = 'JD';
        Args.ColRefJD          = 'RefJD';
        Args.ColS              = 'S';
        Args.ColScorr          = 'Scorr';
        Args.ColZ2             = 'Z2';

        Args.NewHeaderKeys     = {'LIMMAG', 'FIELDID', 'MOUNTNUM', 'CAMNUM', 'CROPID'};
        Args.RefHeaderKeys     = {'LIMMAG'};
        Args.HeaderKeys        = {'LIMMAG'};
        
        Args.ConcatData        = [];  % table or struct

        Args.KeyJD             = [];
    end

    Ncoo = size(Coo,1);

    if isempty(Args.PrefixNew)
        AddNew = false;
    else
        AddNew = true;
    end
    if isempty(Args.PrefixRef)
        AddRef = false;
    else
        AddRef = true;
    end


    ColVals  = [string(Args.ColJD), string(Args.ColRefJD), string(Args.ColS), string(Args.ColScorr), string(Args.ColZ2)];

    if isstruct(Args.ConcatData)
        Tconcat = struct2table(Args.ConcatData);
    else
        Tconcat = Args.ConcatData;
    end
    Ncon = size(Tconcat,1);

    N  = numel(AD);
    AC = AstroCatalog([N,1]);  % output AstroCatalog
    for I=1:1:N

        [ResultD] = imProc.sources.forcedPhot(AD(I), 'Coo',Coo,     'CooUnits', Args.CooUnits, 'AddRefStarsDist', 0, 'OutType','table', 'MaxIter',Args.MaxIter);
        if AddRef && ~isempty(AD(I).Ref)
            [ResultR] = imProc.sources.forcedPhot(AD(I).Ref, 'Coo',Coo, 'CooUnits', Args.CooUnits, 'AddRefStarsDist', 0, 'OutType','table', 'MaxIter',Args.MaxIter);
            % add prefix to Ref and New table columns
            ResultR.Properties.VariableNames = Args.PrefixRef + ResultR.Properties.VariableNames;
        else
            ResultR = [];
        end
        if AddNew && ~isempty(AD(I).New)
            [ResultN] = imProc.sources.forcedPhot(AD(I).New, 'Coo',Coo, 'CooUnits', Args.CooUnits, 'AddRefStarsDist', 0, 'OutType','table', 'MaxIter',Args.MaxIter);
            % add prefix to Ref and New table columns
            ResultN.Properties.VariableNames = Args.PrefixNew + ResultN.Properties.VariableNames;
        else
            ResultN = [];
        end

        if isempty(Args.ColJD)
            JD = [];
        else
            JD    = AD(I).julday('KeyJD',Args.KeyJD);
        end
        if isempty(Args.ColRefJD) || isempty(AD(I).Ref)
            RefJD   = [];
            ColVals = setdiff(ColVals, "RefJD");
        else
            RefJD = AD(I).Ref.julday('KeyJD',Args.KeyJD);
        end
        
           
        if isempty(Args.ColS)
            S_val = [];
        else
            % read S value at position
            S_val     = double(imUtil.image.getValPos(AD(I).S, ResultD.X, ResultD.Y));
        end

        if isempty(Args.ColScorr)
            Scorr_val = [];
        else
            % read Scorr value at position
            Scorr_val     = double(imUtil.image.getValPos(AD(I).Scorr, ResultD.X, ResultD.Y));
        end

        if isempty(Args.ColZ2)
            Z2_val = [];
        else
            % read Z^2 value at position
            Z2_val     = double(imUtil.image.getValPos(AD(I).Z2, ResultD.X, ResultD.Y));
        end

        Vals     = [repmat(JD,Ncoo,1), repmat(RefJD,Ncoo,1), S_val, Scorr_val, Z2_val];
        Tmeta    = array2table(Vals, 'VariableNames',ColVals);

        
        Tdiff    = struct2table(AD(I).getStructKey(Args.HeaderKeys));
        if ~isempty(AD(I).Ref)
            Tref     = struct2table(AD(I).Ref.getStructKey(Args.RefHeaderKeys));
            Tref.Properties.VariableNames = Args.PrefixRef + Tref.Properties.VariableNames;
        else
            Tref = [];
        end
        if ~isempty(AD(I).New)
            Tnew     = struct2table(AD(I).New.getStructKey(Args.NewHeaderKeys));
            Tnew.Properties.VariableNames = Args.PrefixNew + Tnew.Properties.VariableNames;
        else
            Tnew = [];
        end
        Thead    = repmat([Tdiff, Tref, Tnew], Ncoo,1);

        Icon = min(Ncon, I);
        if Icon==0
            Tcon = table;
        else
            Tcon = Tconcat(Icon,:);
        end
        
        AC(I).Catalog = [ResultD, ResultN, ResultR, Tmeta, Thead, Tcon];
    end

end
