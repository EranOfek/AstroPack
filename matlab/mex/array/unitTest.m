function unitTest()

    % ------------------------------------------- OR
    % 2D - Compare MATLAB and MEX
    for Iter=1:1000
        rows = int32(rand*100);
        cols = int32(rand*100);
        Array = int32(double(0xFFFFFFFF) * rand(rows, cols));
        for dim=1:2
            Output = tools.array.bitor_array(Array, dim, false);
            MexOutput = tools.array.bitor_array(Array, dim, true);
            assert(isequal(Output, MexOutput));   
        end
    end
       
    % 3D - Compare MATLAB and MEX
    for Iter=1:1000
        rows = int32(rand*100);
        cols = int32(rand*100);
        deps = 2 + int32(rand*100);        
        Array = uint32(double(0xFFFFFFFF) * rand(rows, cols, deps));
        for dim=1:3
            Output = tools.array.bitor_array(Array, dim, false);
            MexOutput = tools.array.bitor_array(Array, dim, true);
            assert(isequal(Output, MexOutput));   
        end
    end    


    % ------------------------------------------- AND
    % 2D - Compare MATLAB and MEX
    for Iter=1:1000
        rows = int32(rand*100);
        cols = int32(rand*100);
        Array = int32(double(0xFFFFFFFF) * rand(rows, cols));
        for dim=1:2
            Output = tools.array.bitand_array(Array, dim, false);
            MexOutput = tools.array.bitand_array(Array, dim, true);
            assert(isequal(Output, MexOutput));   
        end
    end
       
    % 3D - Compare MATLAB and MEX
    for Iter=1:1000
        rows = int32(rand*100);
        cols = int32(rand*100);
        deps = 2 + int32(rand*100);        
        Array = uint32(double(0xFFFFFFFF) * rand(rows, cols, deps));
        for dim=1:3
            Output = tools.array.bitand_array(Array, dim, false);
            MexOutput = tools.array.bitand_array(Array, dim, true);
            assert(isequal(Output, MexOutput));   
        end
    end    
    
end

