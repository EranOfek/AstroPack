function [Result] = unitTest()
    % unitTest for: tools.array

    %% tools.array.mex.countNaN

    A=rand(100,100);
    A(11:20)=NaN;
    
    if sum(isnan(A))~=tools.array.mex.countNaN(A)
        error('Problem with: tools.array.mex.countNaN');
    end

    %% tools.array.bitor_array.m
    Array = randi(2^16,1600,1600,20);   
    I = rand(size(Array))>0.05;
    Array(I)=0;
    Array = uint32(Array);
    Val1 = tools.array.bitor_array(Array,3,[false false]);
    Val2 = tools.array.bitor_array(Array,3,false);
    Val3 = tools.array.bitor_array(Array,3,true);
    if any(Val1~=Val2)
        error('Problem with: tools.array.bitor_array');
    end
    if any(Val1~=Val3)
        error('Problem with: tools.array.mex.bitor_dim');
    end

    %% tools.array.bitand_array.m
    Array = randi(2^16,1600,1600,20);   
    I = rand(size(Array))>0.05;
    Array(I)=0;
    Array = uint32(Array);
    Val1 = tools.array.bitand_array(Array,3,[false false]);
    Val2 = tools.array.bitand_array(Array,3,false);
    Val3 = tools.array.bitand_array(Array,3,true);

    if any(Val1~=Val2)
        error('Problem with: tools.array.bitor_array');
    end
    if any(Val1~=Val3)
        error('Problem with: tools.array.mex.bitand_dim');
    end

    %% bitsetFlag

    Array = uint32(zeros(1716,1716));
    Flag  = rand(1716,1716)>0.95;
       
    Res1 = tools.array.bitsetFlag(Array, Flag, 13, true, [false false]);
    Res2 = tools.array.bitsetFlag(Array, Flag, 13, true, false);
    Res3 = tools.array.bitsetFlag(Array, Flag, 13, true, true);

    if sum(Res1~=Res2)
        error('Problem with tools.array.bitsetFlag - old mex');
    end
    if sum(Res1~=Res3)
        error('Problem with tools.array.mex.bitsetFlag - new mex');
    end

    %% tools.array.unique_count
    Vec=randi(100,10000,1);
    [UnVal1,Count1]=tools.array.unique_count(Vec,@strcmpi,'search');
    [UnVal2,Count2]=tools.array.unique_count(Vec,@strcmpi,'sort');  
    [UnVal3,Count3]=tools.array.unique_count(Vec,@strcmpi,'scan'); 
    if sum(UnVal1~=UnVal2)>0 || sum(UnVal1~=UnVal3) || sum(Count1~=Count2)>0 || sum(Count1~=Count3)>0
        error('Problem with tools.array.unique_count');
    end



    %%
    
    Result = true;
end
