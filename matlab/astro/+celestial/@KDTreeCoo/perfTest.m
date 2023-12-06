function perfTest
    % performence tests for celestial.KDTreeCoo
   
    
    RAD = 180./pi;
    RadiusAS  = 1000;
    RadiusRAD = RadiusAS./(RAD.*3600);
    VLA=cats.radio.VLASS1;
    
    % search using sphere_dist_fast
    'sphere_dist'
    tic;
    Dist=celestial.coo.sphere_dist_fast(1,1,VLA.Catalog(:,1),VLA.Catalog(:,2));
    ii=find(Dist<RadiusRAD);
    toc
    %Elapsed time is 0.082938 seconds.

    % search using binary search
    'binsearch'
    tic;
    Ind=VO.search.search_sortedlat(VLA.Catalog(:,1:2), 1, 1, RadiusRAD, 'UseMex',true);
    toc
    % Elapsed time is 0.016182 seconds.
    
    % search using predefined KDTree
    K=celestial.KDTreeCoo;          
    K=K.populate(VLA.Catalog(:,1:2));
    'KDTree'
    tic;
    [ID,D] = K.coneSearch(1,1,RadiusAS,'Type','K');
    toc
    %Elapsed time is 0.001863 seconds.

    'KDTree matlab'
    tic;
    [ID,D] = K.coneSearch(1,1,RadiusAS,'Type','M');
    toc
    
    % multiple cone searches
    
    % for Ncoo>~1000 MATLAB become faster...
    
    Ncoo = 1000;
    RA  = rand(Ncoo,1);
    Dec = rand(Ncoo,1);
    % search using binary search
    'binsearch multi'
    tic;
    Ind=VO.search.search_sortedlat_multi(VLA.Catalog(:,1:2), RA, Dec, RadiusRAD, 'UseMex',true);
    toc
    
    'KDTree'
    tic;
    [ID,D] = K.coneSearch(RA,Dec,RadiusAS, 'Type','K');
    toc
    
    'KDTree matlab'
    tic;
    [ID,D] = K.coneSearch(RA,Dec,RadiusAS, 'Type','M');
    toc
    
    
end