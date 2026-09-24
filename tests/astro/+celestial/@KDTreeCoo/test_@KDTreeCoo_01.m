function Result=unitTest()
    % celestial.KDTreeCoo.unitTest
   
    K=celestial.KDTreeCoo;  
    VLA=cats.radio.VLASS1;
    K=K.populate(VLA.Catalog(:,1:2))
    [ID,D] = K.coneSearch(1,1,1000);
    
    Result = true;
end