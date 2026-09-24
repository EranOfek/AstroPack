function Result = unitTest()
    % Component.unitTest
    
    C=astro.Cosmology('wmap3');
    
    C=astro.Cosmology.parameters;
    
    C.e_z(rand(3,3));
    
    Result = true;
end
