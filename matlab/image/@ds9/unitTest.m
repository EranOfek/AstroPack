function Result=unitTest
    % unitTest for the ds9 static class
    % Example: ds9.unitTest
   
    ds9.open;
    ds9(rand(100,100),1);
    ds9.zoom(3)
    ds9.plot(10,10)
    
    Result = true;
end