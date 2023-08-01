function tracks=findTracks(AI,Args)
% Looks for satellite or airplane tracks among the sources in an astroimage
%  catalog. Tracks are defined as linear chains of identified sources,
%  which are closer than MaxDist (pixels) apart, and approximately collinear
%
% Input:
%   a scalar AstroImage with a populated source catalog
%    - Key-value arguments
%      'MaxDist' maximal distance of one sources from others, to be
%                considered part of a track [default 20, pixels]
%      'MinDist' sources closer than this are clustered together
%                disregarding the collinearity of their joining segments
%                [default 3 pixels]
%      'MinTrackLength' minimal number of closeby sources for a track to be
%                       classified as such [default 3 sources]
%      'SlopeTol' angular tolerance in track orientation [default 5 degrees]
%      'RemoveFromCatalog' if true, remove the tracks detected from the
%                          catalog in AstroImage [default true]
% Output:
%   a cell of arrays of integer indices, each element denoting groups of
%   sources identified as part of the same track
%
% Working dummily on clustering slopes and distances of pairs of sources.
%  For N sources, this involves O(N^2) operations
%
% Author: Enrico Segre, July 2023

    arguments
        AI AstroImage
        Args.MaxDist = 20;
        Args.MinDist =  3;
        Args.SlopeTol = 5;
        Args.MinTrackLength = 3;
        Args.RemoveFromCatalog = true; 
    end

    if Args.MinTrackLength < 3
        Args.MinTrackLength = 3;
    end

    cosMinSlope=cosd(Args.SlopeTol);

    x=AI.Table.X;
    y=AI.Table.Y;
    np=numel(x);

    % dist and slope matrices are symmetrical, but it is way faster to
    %  compute them at once than to complute only half of the matrix with a
    %  loop
    dx=repmat(x,1,np)-repmat(x',np,1);
    dy=repmat(y,1,np)-repmat(y',np,1);
    dist=sqrt(dx.^2+dy.^2);
    dist(1:np+1:end)=nan; % better than 0 to exclude point from its neighbors

    % traverse the list of points and build clusters
    clusterlabel=zeros(1,np);
    nclusters=0;
    for i=1:np
        neighbors=dist(:,i)<Args.MaxDist;
        if clusterlabel(i)==0
            % begin labelling a new cluster
            nclusters=nclusters+1;
            clusterlabel(i)=nclusters;
            clusterlabel(neighbors)=nclusters;
        else
            % add the new neighbors to this cluster
            clusterlabel(neighbors)=clusterlabel(i);
        end
    end

    uniqueclusters=unique(clusterlabel(clusterlabel>0));

    % the condition for points to belong to a track is:
    % -tracks have at least three members
    % -none of the slopes to the other members is different than the mean
    %  slope of more than SlopeTol
    % First prune clusters composed only of one or two members
    realcluster=false(size(uniqueclusters));
    for k=1:numel(uniqueclusters)
        nmembers=find(clusterlabel==uniqueclusters(k));
        if numel(nmembers)>=Args.MinTrackLength
            realcluster(k)=true;
        end
    end
    uniqueclusters=uniqueclusters(realcluster);
    % fprintf('%d unique clusters found\n',numel(uniqueclusters))

    % now prune sources which are close enough to the track but not in line
    %  with them
    for k=1:numel(uniqueclusters)
        members=find(clusterlabel==uniqueclusters(k));
        for j=members
            % find the closest two neighbors
            [~,jj]=sort(dist(j,:));
            j1=jj(1); j2=jj(2);
            % cos of the minimal angle > cosMinSlope
            %  do this for two of the angles to overcome false negative
            %  when two track points are very close, and the outlier
            %  sees them under a small angle
            q1= (abs((x(j)-x(j1))*(x(j)-x(j2)))+...
                abs((y(j)-y(j1))*(y(j)-y(j2)))) / ...
                (dist(j,j1)*dist(j,j2)) < cosMinSlope;
            q2= (abs((x(j2)-x(j1))*(x(j)-x(j2)))+...
                abs((y(j2)-y(j1))*(y(j)-y(j2)))) / ...
                (dist(j2,j1)*dist(j,j2)) < cosMinSlope;
            if (q1 || q2) && ...
               (dist(j,j1)>Args.MinDist && dist(j,j2)>Args.MinDist)
                clusterlabel(j)=0;
            end
        end
    end

    % count again clusters with at least MinTrackLength members
    realcluster=false(size(uniqueclusters));
    for k=1:numel(uniqueclusters)
        nmembers=find(clusterlabel==uniqueclusters(k));
        if numel(nmembers)>=Args.MinTrackLength
            realcluster(k)=true;
        end
    end
    uniquetracks=uniqueclusters(realcluster);

    % output tracks in a cell array
    tracks=cell(1,numel(uniquetracks));
    for k=1:numel(uniquetracks)
        tracks{k}=find(clusterlabel==uniquetracks(k));
    end

    % remove tracks from catalog if so requested
    if Args.RemoveFromCatalog
        AI.CatData.Catalog(horzcat(tracks{:}),:)=[];
        AI.Table=[]; % like that, to update. It is designed so.
    end
