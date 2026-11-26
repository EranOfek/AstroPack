function [Result, Duplicate, Group] = removeDuplicates(Cat, Args)
    % Remove duplicate transient candidates found in more than one sub image or reference.
    %   Candidates within 'Radius' of each other with different values of
    %   the 'GroupCol' column (crop or reference) are linked, and each
    %   connected group is one set of duplicates. In each group, the
    %   candidate with the lowest score is the survivor, regardless of
    %   FLAGS_TRANSIENT (issue #1374), and all group members with the
    %   survivor's 'GroupCol' value are kept (near candidates in the same
    %   sub image are distinct detections). Ties are broken by the lowest
    %   'GroupCol' value.
    %   The default score is the distance from the center of the sub image,
    %   as in pipeline.last.pipes.pipelineII, from which this is moved.
    % Input  : - An AstroCatalog of candidates, e.g., the merged catalog of
    %            all sub images of a visit. Need not be sorted; the rows
    %            keep their order.
    %          * ...,key,val,...
    %            'Radius' - Match radius [arcsec]. Default is 1.5.
    %            'GroupCol' - Name of the column holding the numeric
    %                   identifier of the image the candidate was found in.
    %                   Candidates with the same value are never linked.
    %                   Default is 'CROPID'.
    %            'Score' - A vector of scores, one per candidate. Lower is
    %                   better. If empty, use the distance of [ColX, ColY]
    %                   from the image center, ImageSize./2.
    %                   Default is [].
    %            'ImageSize' - [Ny, Nx] of the sub images. Required if
    %                   'Score' is empty. Default is [].
    %            'ColX' - X column for the center distance. Default is 'XPEAK'.
    %            'ColY' - Y column for the center distance. Default is 'YPEAK'.
    % Output : - The AstroCatalog without the duplicates.
    %          - A logical vector, true for the removed candidates.
    %          - The duplicate group index of each candidate (single
    %            candidates are groups of one). Empty if fewer than 2
    %            candidates.
    % Author : Ruslan Konno (May 2026)
    % Example: TCL1 = pipeline.last.transients.removeDuplicates(TCL1, 'ImageSize', size(AD(1).Image));

    arguments
        Cat
        Args.Radius    = 1.5;
        Args.GroupCol  = 'CROPID';
        Args.Score     = [];
        Args.ImageSize = [];
        Args.ColX      = 'XPEAK';
        Args.ColY      = 'YPEAK';
    end

    Arcsec2Rad = pi./(180.*3600);

    [MRA, MDec] = Cat.getLonLat('rad');
    NCand = numel(MRA);
    Duplicate = false(NCand,1);
    Group     = [];
    if NCand > 1
        if isempty(Args.Score)
            if isempty(Args.ImageSize)
                error('pipeline:removeDuplicates:noImageSize', 'ImageSize is required when Score is empty');
            end
            HalfSize = Args.ImageSize(:).'./2;
            [DupX, DupY] = Cat.getXY('ColX',Args.ColX, 'ColY',Args.ColY);
            Score = sqrt((DupX-HalfSize(2)).^2+(DupY-HalfSize(1)).^2);
        else
            Score = Args.Score(:);
        end

        % The search needs the catalog sorted by Dec; map the indices back
        [~, Ord] = sort(MDec);
        SelfMatches = VO.search.search_sortedlat_multi( ...
                [MRA(Ord), MDec(Ord)], MRA, MDec, Args.Radius.*Arcsec2Rad);
        IMatch = repelem((1:NCand)', vertcat(SelfMatches.Nmatch));
        JMatch = Ord(vertcat(SelfMatches.Ind));
        % Link only matches in different sub images
        GroupID = Cat.getCol(Args.GroupCol);
        Link = IMatch ~= JMatch & GroupID(IMatch) ~= GroupID(JMatch);
        LinkMat = sparse(IMatch(Link), JMatch(Link), 1, NCand, NCand);
        % Duplicate groups (single candidates are groups of one)
        Group = conncomp(graph(LinkMat | LinkMat'))';

        % The candidate with the lowest score is the survivor of each
        % group; keep the group members with its GroupCol value.
        MinScore = accumarray(Group, Score, [], @min);
        Survivor = Score == MinScore(Group);
        SurvivorID = accumarray(Group(Survivor), GroupID(Survivor), ...
                [max(Group) 1], @min);
        Duplicate = GroupID ~= SurvivorID(Group);
    end

    Result = Cat.selectRows(~Duplicate);
end
