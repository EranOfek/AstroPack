function [Result] = unitTest()
    % unitTest for: tools.array


    %% tools.array.mex.allocateUninit
    A = tools.array.mex.allocateUninit([10 10 3],'single');

    
    %% tools.array.mex.countNaN

    A=rand(100,100);
    A(11:20)=NaN;
    
    if sum(isnan(A))~=tools.array.mex.countNaN(A)
        error('Problem with: tools.array.mex.countNaN');
    end

    %% tools.array.mex.selectIndNaN

    Matrix=rand(1000,100);
    Ind=randi(1000,500,1);
    Ind(2)=NaN;           
    NewMatrix=tools.array.mex.selectIndNaN(Matrix,Ind);
    Ind0 = double(Ind(:)); Good = ~isnan(Ind0);NewMatrix1 = NaN(numel(Ind0), size(Matrix,2));NewMatrix1(Good,:) = double(Matrix(Ind0(Good),:));

    if max(abs(NewMatrix1-NewMatrix),[],'all')>0
        error('problem with tools.array.mex.selectIndNaN');
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

    % more bitsetFlag tests

    test_bitsetFlag();

    %% tools.array.mex.bitsetFlagMulti
    test_bitsetFlagMulti()


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


function test_bitsetFlag()

    rng(1);

    Classes = {'uint8','uint16','uint32','uint64'};
    NumTrialsPerClass = 50;

    fprintf('Testing bitsetFlag...\n');

    for Ic = 1:numel(Classes)
        ClassName = Classes{Ic};

        switch ClassName
            case 'uint8'
                MaxBit = 8;
            case 'uint16'
                MaxBit = 16;
            case 'uint32'
                MaxBit = 32;
            case 'uint64'
                MaxBit = 64;
            otherwise
                error('Unexpected class');
        end

        for It = 1:NumTrialsPerClass

            % Random size
            Size1 = randi([1,50]);
            Size2 = randi([1,40]);
            Sz = [Size1, Size2];

            % Random array of requested integer class
            A = randomIntegerArray(Sz, ClassName);

            % Random logical flag mask
            F = rand(Sz) > rand();

            % Random bit number
            BitNumber = randi(MaxBit);

            % Random SetVal
            SetVal = rand() > 0.5;

            % ---- MATLAB reference ----
            Ref = referenceBitsetFlag(A, F, BitNumber, SetVal);

            % ---- MEX without prescan (default) ----
            Out0 = tools.array.mex.bitsetFlag(A, F, BitNumber, SetVal);

            if ~isequal(Out0, Ref)
                error('Mismatch without prescan. Class=%s Trial=%d Bit=%d SetVal=%d', ...
                    ClassName, It, BitNumber, SetVal);
            end

            % ---- MEX with explicit prescan=false ----
            Out1 = tools.array.mex.bitsetFlag(A, F, BitNumber, SetVal, false);

            if ~isequal(Out1, Ref)
                error('Mismatch with prescan=false. Class=%s Trial=%d Bit=%d SetVal=%d', ...
                    ClassName, It, BitNumber, SetVal);
            end

            % ---- MEX with prescan=true ----
            Out2 = tools.array.mex.bitsetFlag(A, F, BitNumber, SetVal, true);

            if ~isequal(Out2, Ref)
                error('Mismatch with prescan=true. Class=%s Trial=%d Bit=%d SetVal=%d', ...
                    ClassName, It, BitNumber, SetVal);
            end
        end

        fprintf('  %s passed\n', ClassName);
    end

    % Extra edge/uniform-mask tests
    testSpecialCases();

    fprintf('All tests passed successfully.\n');
end


function Ref = referenceBitsetFlag(A, F, BitNumber, SetVal)
    % MATLAB reference implementation

    Ref = A;

    if SetVal ~= 0
        % Set the selected bit only where F is true
        Ref(F) = bitset(Ref(F), BitNumber, 1);
    else
        % Clear the selected bit only where F is true
        Ref(F) = bitset(Ref(F), BitNumber, 0);
    end
end


function A = randomIntegerArray(Sz, ClassName)

    switch ClassName
        case 'uint8'
            A = uint8(randi([0, intmax('uint8')], Sz));
        case 'uint16'
            A = uint16(randi([0, intmax('uint16')], Sz));
        case 'uint32'
            % Build uint32 from two uint16 chunks to avoid randi limitations
            Hi = uint32(randi([0, 65535], Sz));
            Lo = uint32(randi([0, 65535], Sz));
            A = bitor(bitshift(Hi, 16), Lo);
        case 'uint64'
            % Build uint64 from four uint16 chunks
            P1 = uint64(randi([0, 65535], Sz));
            P2 = uint64(randi([0, 65535], Sz));
            P3 = uint64(randi([0, 65535], Sz));
            P4 = uint64(randi([0, 65535], Sz));
            A = bitor( ...
                    bitor(bitshift(P1, 48), bitshift(P2, 32)), ...
                    bitor(bitshift(P3, 16), P4) );
        otherwise
            error('Unsupported class');
    end
end


function testSpecialCases()

    fprintf('  Running special-case tests...\n');

    Cases = {
        'uint8',  8
        'uint16', 16
        'uint32', 32
        'uint64', 64
        };

    for I = 1:size(Cases,1)
        ClassName = Cases{I,1};
        MaxBit    = Cases{I,2};

        A = randomIntegerArray([20,30], ClassName);

        for BitNumber = [1, MaxBit]
            for SetVal = [0, 1]

                % All false mask
                F = false(size(A));
                Ref = referenceBitsetFlag(A, F, BitNumber, SetVal);
                Out = tools.array.mex.bitsetFlag(A, F, BitNumber, SetVal);
                assert(isequal(Out, Ref), 'Special case failed: all false');

                Out = tools.array.mex.bitsetFlag(A, F, BitNumber, SetVal, true);
                assert(isequal(Out, Ref), 'Special case failed: all false + prescan');

                % All true mask
                F = true(size(A));
                Ref = referenceBitsetFlag(A, F, BitNumber, SetVal);
                Out = tools.array.mex.bitsetFlag(A, F, BitNumber, SetVal);
                assert(isequal(Out, Ref), 'Special case failed: all true');

                Out = tools.array.mex.bitsetFlag(A, F, BitNumber, SetVal, true);
                assert(isequal(Out, Ref), 'Special case failed: all true + prescan');

                % Single true pixel
                F = false(size(A));
                F(randi(numel(F))) = true;
                Ref = referenceBitsetFlag(A, F, BitNumber, SetVal);
                Out = tools.array.mex.bitsetFlag(A, F, BitNumber, SetVal);
                assert(isequal(Out, Ref), 'Special case failed: single true');

                % Single false pixel
                F = true(size(A));
                F(randi(numel(F))) = false;
                Ref = referenceBitsetFlag(A, F, BitNumber, SetVal);
                Out = tools.array.mex.bitsetFlag(A, F, BitNumber, SetVal, true);
                assert(isequal(Out, Ref), 'Special case failed: single false');
            end
        end
    end
end

function test_bitsetFlagMulti()

    rng(1);

    Classes = {'uint8','uint16','uint32','uint64'};
    NtrialPerClass = 40;

    fprintf('Testing bitsetFlagMulti...\n');

    for Ic = 1:numel(Classes)
        ClassName = Classes{Ic};

        switch ClassName
            case 'uint8'
                MaxBit = 8;
            case 'uint16'
                MaxBit = 16;
            case 'uint32'
                MaxBit = 32;
            case 'uint64'
                MaxBit = 64;
            otherwise
                error('Unexpected class');
        end

        for It = 1:NtrialPerClass
            Sz = [randi([1,40]), randi([1,30])];
            Mask = randomIntegerArray(Sz, ClassName);

            Nops = randi([1,8]);

            Args = cell(1, 1 + 3*Nops);
            Args{1} = Mask;

            Ops = repmat(struct('F',[],'Bit',[],'SetVal',[]), Nops, 1);

            for Iop = 1:Nops
                Ops(Iop).F      = rand(Sz) > rand();
                Ops(Iop).Bit    = randi(MaxBit);
                Ops(Iop).SetVal = rand() > 0.5;

                Args{1 + 3*(Iop-1) + 1} = Ops(Iop).F;
                Args{1 + 3*(Iop-1) + 2} = Ops(Iop).Bit;
                Args{1 + 3*(Iop-1) + 3} = Ops(Iop).SetVal;
            end

            Ref = reference_bitsetFlagMulti(Mask, Ops);
            Out = tools.array.mex.bitsetFlagMulti(Args{:});

            if ~isequal(Out, Ref)
                error('Mismatch in random test. Class=%s Trial=%d', ClassName, It);
            end
        end

        fprintf('  %s passed random tests\n', ClassName);
    end

    fprintf('  Running special-case tests...\n');

    for Ic = 1:numel(Classes)
        ClassName = Classes{Ic};

        switch ClassName
            case 'uint8'
                MaxBit = 8;
            case 'uint16'
                MaxBit = 16;
            case 'uint32'
                MaxBit = 32;
            case 'uint64'
                MaxBit = 64;
        end

        Mask = randomIntegerArray([25,35], ClassName);

        Ops = struct('F', false(size(Mask)), 'Bit', randi(MaxBit), 'SetVal', randi([0,1]));
        Ref = reference_bitsetFlagMulti(Mask, Ops);
        Out = tools.array.mex.bitsetFlagMulti(Mask, Ops.F, Ops.Bit, Ops.SetVal);
        assert(isequal(Out, Ref), 'All-false flags case failed');

        Ops = struct('F', true(size(Mask)), 'Bit', randi(MaxBit), 'SetVal', randi([0,1]));
        Ref = reference_bitsetFlagMulti(Mask, Ops);
        Out = tools.array.mex.bitsetFlagMulti(Mask, Ops.F, Ops.Bit, Ops.SetVal);
        assert(isequal(Out, Ref), 'All-true flags case failed');

        Bit = randi(MaxBit);
        Ops(1) = struct('F', rand(size(Mask)) > 0.7, 'Bit', Bit, 'SetVal', 1);
        Ops(2) = struct('F', rand(size(Mask)) > 0.7, 'Bit', Bit, 'SetVal', 0);
        Ops(3) = struct('F', rand(size(Mask)) > 0.7, 'Bit', Bit, 'SetVal', 1);

        Ref = reference_bitsetFlagMulti(Mask, Ops);
        Out = tools.array.mex.bitsetFlagMulti(Mask, ...
            Ops(1).F, Ops(1).Bit, Ops(1).SetVal, ...
            Ops(2).F, Ops(2).Bit, Ops(2).SetVal, ...
            Ops(3).F, Ops(3).Bit, Ops(3).SetVal);

        assert(isequal(Out, Ref), 'Repeated same-bit updates failed');
    end

    fprintf('All bitsetFlagMulti tests passed successfully.\n');
end


function Out = reference_bitsetFlagMulti(Mask, Ops)

    Out = Mask;

    for Iop = 1:numel(Ops)
        if Ops(Iop).SetVal ~= 0
            Out(Ops(Iop).F) = bitset(Out(Ops(Iop).F), Ops(Iop).Bit, 1);
        else
            Out(Ops(Iop).F) = bitset(Out(Ops(Iop).F), Ops(Iop).Bit, 0);
        end
    end
end