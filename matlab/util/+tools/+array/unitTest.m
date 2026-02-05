function [Result] = unitTest()
    % unitTest for: tools.array

    %% tools.array.mex.countNaN

    A=rand(100,100);
    A(11:20)=NaN;
    
    if sum(isnan(A))~=tools.array.mex.countNaN(A)
        error('Problem with: tools.array.mex.countNaN');
    end

    %% tools.array.bitor_array.m
    Array = uint32(randi(2^16,1600,1600,20));          
    Val1 = tools.array.bitor_array(Array,3,false);
    Val2 = tools.array.bitor_array(Array,3,true);
    if any(Val1~=Val2)
        error('Problem with: tools.array.bitor_array');
    end

    %% tools.array.bitand_array.m
    Array = uint32(randi(2^16,1600,1600,20));          
    Val1 = tools.array.bitand_array(Array,3,false);
    Val2 = tools.array.bitand_array(Array,3,true);
    if any(Val1~=Val2)
        error('Problem with: tools.array.bitor_array');
    end


    %%
    
    Result = true;
end
