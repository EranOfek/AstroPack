function Result = addPrimary(Obj, CCDSEC, Args)
    % Add a 'primary' ownership column to catalogs of overlapping sub images
    %   The sub images (crops) of a full image overlap, so a source near a
    %   partition line appears in the catalogs of several crops. Under the
    %   policy of issue #1180 the Overlap mask bit marks the full overlap
    %   region in all the crops covering it, so it can not be used to select
    %   a single copy of such a source. This function adds a column (default
    %   name 'primary') that records the ownership instead: it is 1 if the
    %   exact X,Y position of the source falls inside the given section
    %   (normally the unique section of the crop, the UNIQSEC header
    %   keyword), and 0 otherwise.
    %   The unique sections tile the full image with no gaps or overlaps,
    %   and the crop offsets are integer, so each sky source gets primary=1
    %   in exactly one crop: the section test uses the half open pixel
    %   interval [Xmin-0.5, Xmax+0.5), consistently in all the crops.
    %   Concatenating the crop catalogs and keeping primary==1 therefore
    %   yields a full frame catalog with no duplicates and no losses.
    %   Sources with a NaN position get 0.
    % Input  : - An AstroImage object (with a populated CatData), or an
    %            AstroCatalog object.
    %          - CCDSEC [Xmin Xmax Ymin Ymax] of the section owning the
    %            sources, given in the image own frame. Either a 1x4 vector
    %            applied to all the elements, or an Nobj-by-4 matrix, line
    %            per element. If empty, then read it from the header
    %            keyword given by 'KeySEC' (AstroImage input only).
    %            Default is [].
    %          * ...,key,val,...
    %            'ColX' - The column name of the X coordinate.
    %                   Default is 'X'.
    %            'ColY' - The column name of the Y coordinate.
    %                   Default is 'Y'.
    %            'ColName' - The name of the new column.
    %                   Default is 'primary'.
    %            'ColPos' - The position at which to insert the new column.
    %                   Default is Inf (i.e., last column).
    %            'KeySEC' - The header keyword from which to read the
    %                   section when the second input is empty.
    %                   Default is 'UNIQSEC'.
    %            'CreateNewObj' - A logical indicating if to copy the input
    %                   object before inserting the new column. If false,
    %                   the input object is modified.
    %                   Default is false.
    % Output : - The input object with the added (or replaced) column.
    % Author : Dana Kovaleva (Aug 2026)
    % Example: AI = imProc.cat.addPrimary(AI, [80 1637 70 1647]);
    %          AI = imProc.cat.addPrimary(AI);   % UNIQSEC from the header

    arguments
        Obj                             % AstroImage or AstroCatalog
        CCDSEC                    = [];
        Args.ColX     (1,:) char  = 'X';
        Args.ColY     (1,:) char  = 'Y';
        Args.ColName  (1,:) char  = 'primary';
        Args.ColPos               = Inf;
        Args.KeySEC   (1,:) char  = 'UNIQSEC';
        Args.CreateNewObj logical = false;
    end

    if Args.CreateNewObj
        Result = Obj.copy;
    else
        Result = Obj;
    end

    Nsec = size(CCDSEC,1);
    if ~isempty(CCDSEC) && size(CCDSEC,2)~=4
        error('CCDSEC must be a 4 column matrix of [Xmin Xmax Ymin Ymax]');
    end

    Nobj = numel(Result);
    if ~isempty(CCDSEC) && Nsec~=1 && Nsec~=Nobj
        error('CCDSEC must contain either a single line or a line per object element');
    end

    for Iobj=1:1:Nobj
        if isa(Result, 'AstroImage')
            Cat = Result(Iobj).CatData;
        else
            Cat = Result(Iobj);
        end

        % resolve the owning section of this element
        if isempty(CCDSEC)
            if ~isa(Result, 'AstroImage')
                error('When CCDSEC is empty the input must be an AstroImage, so that the section can be read from the header');
            end
            Sec = Result(Iobj).HeaderData.getVal(Args.KeySEC, 'UseDict',false);
            if ischar(Sec) || isstring(Sec)
                Sec = imUtil.ccdsec.ccdsecStr2num(Sec);
            end
            if numel(Sec)~=4 || any(isnan(Sec))
                error('Header keyword %s of element %d does not contain a valid section', Args.KeySEC, Iobj);
            end
            Sec = Sec(:).';
        else
            Isec = min(Iobj, Nsec);
            Sec  = double(CCDSEC(Isec,:));
        end

        if Cat.sizeCatalog>0
            XY = Cat.getColMulti({Args.ColX, Args.ColY});

            % half open pixel bounds: a source exactly on a partition line is
            % owned by the crop on its right/top, consistently in all crops
            Primary = double(XY(:,1) >= Sec(1)-0.5 & XY(:,1) < Sec(2)+0.5 & ...
                             XY(:,2) >= Sec(3)-0.5 & XY(:,2) < Sec(4)+0.5);

            if Cat.isColumn(Args.ColName)
                Cat.replaceCol(Primary, Args.ColName);
            else
                Cat.insertCol(Primary, Args.ColPos, {Args.ColName}, {''});
            end
        end
    end

end
