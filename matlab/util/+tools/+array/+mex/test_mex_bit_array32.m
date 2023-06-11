
%function test_mex_bit_array()

    Array1     = uint32([ 0x0001; 0x0002; 0x0004; 0x0008 ]);
        
	% Input - prepare 2D matrix
    Array2a    = uint32([ 0x0001, 0x0002, 0x0004, 0x0008;...
                          0x0011, 0x0022, 0x0014, 0x0018 ]);

    Array2b    = uint32([ 0x0101, 0x0102, 0x0104, 0x0108;...
                          0x0111, 0x0122, 0x0114, 0x0118 ]);
                     
    Array2c    = uint32([ 0x1001, 0x1002, 0x1004, 0x1008;...
                          0x1111, 0x1122, 0x1114, 0x1118 ]);                     
                     
    % Add 3rd dimension
    Array3d = Array2a;
    Array3d(:,:,2) = Array2b;
    Array3d(:,:,3) = Array2c;    
    
    disp(Array3d);
    tools.array.mex.mex_bitor_array_int32(Array3d, 3);    
    
    % Expected results
    
    % 1d
    ExpectedOr1   = uint32([0x000F]);
    
    % 2d
    ExpectedOr2a  = uint32([ 0x0011, 0x0022, 0x0014, 0x0018 ]);
    ExpectedOr2b  = uint32([ 0x0111, 0x0122, 0x0114, 0x0118 ]);    
    ExpectedOr2c  = uint32([ 0x1111, 0x1122, 0x1114, 0x1118 ]);    
    
    
    ExpectedOr2dim2 = uint32([ 0x000F; 0x003F ]);

    % 3d, dim=1
    ExpectedOr3d  = ExpectedOr2a;
    ExpectedOr3d(:,:,2) = ExpectedOr2b;        

    % 3d, dim = 2
    ExpectedOr3dDim2a = uint32([ 0x000F; 0x003F ]);
    ExpectedOr3dDim2b = uint32([ 0x010F; 0x013F ]);
    ExpectedOr3dDim2c = uint32([ 0x100F; 0x113F ]);
    
    % 3d, dim = 3
    ExpectedOr3dDim3  = uint32([ 0x1101, 0x1102, 0x1104, 0x1108;...
                                 0x1111, 0x1122, 0x1114, 0x1118 ]);

    %--------------------------------------------------- 1D
    % Or - Input: 1d -> Output: scalar, ndims=2
    Or = tools.array.bitor_array(Array1, 1, false);
    disp(size(Or));
    disp(Or);
    assert(strcmp(class(Or), 'uint32'));
    assert(ndims(Or) == 2);
    assert(isequal(Or, ExpectedOr1));
    %--------------------------------------------------- 2D
    % Or - Input: 2d, dim=1 -> Output: 2d, ndims=2
    Or = tools.array.bitor_array(Array2a, 1, false);
    disp(size(Or));
    disp(Or);
    assert(strcmp(class(Or), 'uint32'));
    assert(ndims(Or) == 2);
    assert(isequal(Or, ExpectedOr2a));
    
    % Or - Input: 2d, dim=2 -> Output: 2d, ndims=2
    Or = tools.array.bitor_array(Array2a, 2, false);
    disp(size(Or));
    disp(Or);
    assert(strcmp(class(Or), 'uint32'));
    assert(ndims(Or) == 2);
    assert(isequal(Or, ExpectedOr2dim2));    
    
    %--------------------------------------------------- 3D
    % Or - Input: 3d, dim=1 -> Output: 3d, ndims=3
    Or3d = tools.array.bitor_array(Array3d, 1, false);
    disp(size(Or3d));
    disp(Or3d);
    assert(strcmp(class(Or3d), 'uint32'));
    assert(ndims(Or3d) == 3);
    assert(isequal(Or3d(:,:,1), ExpectedOr2a));
    assert(isequal(Or3d(:,:,2), ExpectedOr2b));
    assert(isequal(Or3d(:,:,3), ExpectedOr2c));    
    
    % Or - Input: 3d,dim=2 -> Output: 3d, ndims=3
    Or3d = tools.array.bitor_array(Array3d, 2, false);
    disp(size(Or3d));
    disp(Or3d);
    assert(strcmp(class(Or3d), 'uint32'));
    assert(ndims(Or3d) == 3);
    assert(isequal(Or3d(:,:,1), ExpectedOr3dDim2a));
    assert(isequal(Or3d(:,:,2), ExpectedOr3dDim2b));
    assert(isequal(Or3d(:,:,3), ExpectedOr3dDim2c));
    
    % Or - Input: 3d, dim=3 -> Output: 2d, ndims=2
    Or3d = tools.array.bitor_array(Array3d, 3, false);
    disp(size(Or3d));
    disp(Or3d);
    assert(strcmp(class(Or3d), 'uint32'));
    assert(ndims(Or3d) == 2);
    assert(isequal(Or3d, ExpectedOr3dDim3));   
    
    %---------------------------------------------------------- MEX
    
    %--------------------------------------------------- 1D    
    
    % Or - Input: 2d -> Output: scalar, ndims=2
    Or = tools.array.mex.mex_bitor_array_int32(Array1, 1);
    disp(Or);
    assert(strcmp(class(Or), 'uint32'));
    assert(ndims(Or) == 2);
    assert(isequal(Or, ExpectedOr1));
    
    %--------------------------------------------------- 2D
    
    % Or - Input: 2d, dim=1 -> Output: 2d, ndims=2
    Or = tools.array.mex.mex_bitor_array_int32(Array2a, 1);
    disp(Or);
    assert(strcmp(class(Or), 'uint32'));
    assert(ndims(Or) == 2);
    assert(isequal(Or, ExpectedOr2a));
    
    % Or - Input: 2d, dim=2 -> Output: 2d, ndims=2
    Or = tools.array.mex.mex_bitor_array_int32(Array2a, 2);
    disp(Or);
    assert(strcmp(class(Or), 'uint32'));
    assert(ndims(Or) == 2);
    assert(isequal(Or, ExpectedOr2dim2));    
    
    %--------------------------------------------------- 3D    
    % Or - Input: 3d, dim=1 -> Output: 3d, ndims=3
    Or3dm = tools.array.bitor_array(Array3d, 1);
    Or3d = tools.array.mex.mex_bitor_array_int32(Array3d, 1);
    disp(Or3d);
    assert(strcmp(class(Or3d), 'uint32'));
    assert(ndims(Or3d) == 3);
    assert(isequal(Or3d(:,:,1), ExpectedOr2a));
    assert(isequal(Or3d(:,:,2), ExpectedOr2b));
    assert(isequal(Or3d(:,:,3), ExpectedOr2c));
    assert(isequal(Or3d, Or3dm));
    
    
    % Or - Input: 3d,dim=2 -> Output: 3d, ndims=3
    Or3d = tools.array.mex.mex_bitor_array_int32(Array3d, 2);
    disp(Or3d);
    assert(strcmp(class(Or3d), 'uint32'));
    assert(ndims(Or3d) == 3);
    assert(isequal(Or3d(:,:,1), ExpectedOr3dDim2a));
    assert(isequal(Or3d(:,:,2), ExpectedOr3dDim2b));
    assert(isequal(Or3d(:,:,3), ExpectedOr3dDim2c));
        
    % Or - Input: 3d, dim=3 -> Output: 2d, ndims=2
    Or3d = tools.array.mex.mex_bitor_array_int32(Array3d, 3);
    disp(Or3d);
    assert(strcmp(class(Or3d), 'uint32'));
    assert(ndims(Or3d) == 2);
    assert(isequal(Or3d, ExpectedOr3dDim3));   
        
    %-------------------------------------------------------- Random test
    
    % 2D - Compare MATLAB and MEX
    for Iter=1:1000
        rows = 1+int32(rand*100);
        cols = 1+int32(rand*100);
        Array = uint32(double(0xFFFFFFFF) * rand(rows, cols));
        for dim=1:2
            Output = tools.array.bitor_array(Array, dim, false);
            MexOutput = tools.array.mex.mex_bitor_array_int32(Array, dim, true);
            assert(isequal(Output, MexOutput));   
            MexOutput = tools.array.bitor_array(Array, dim, true);
            assert(isequal(Output, MexOutput),'failed 2D');   
        end
    end
    
    
    % 3D - Compare MATLAB and MEX
    for Iter=1:1000
        rows = 1+int32(rand*100);
        cols = 1+int32(rand*100);
        deps = 2 + int32(rand*100);        
        Array = uint32(double(0xFFFFFFFF) * rand(rows, cols, deps));
        for dim=1:3
            Output = tools.array.bitor_array(Array, dim, false);
            MexOutput = tools.array.mex.mex_bitor_array_int32(Array, dim, true);
            assert(isequal(Output, MexOutput));               
            MexOutput = tools.array.bitor_array(Array, dim, true);
            assert(isequal(Output, MexOutput),"failed 3D");   
        end
    end    

    %%
    %-------------------------------------------------------- Convert Performance    
    % Test conversion to uint32
    for Iter=1:10
        BigArray = uint32(double(0xFFFFFFFF) * rand(8000, 8000));
        BigArray32 = [];
        t = tic;
        BigArray32 = uint32(BigArray);
        MatlabTime = toc(t);
        fprintf('Convert: %.6f\n', MatlabTime);        
    end
            
    %-------------------------------------------------------- Performance
    
    % Or - 3d - dim=3, returns 2d    
    for Iter=1:5
        BigArray = uint32(double(0xFFFFFFFF) * rand(4000, 4000));

        for dim=1:2
            t = tic;
            Or3d = tools.array.bitor_array(BigArray, 1);
            MatlabTime = toc(t);

            t = tic;
            MexOr3d = tools.array.mex.mex_bitor_array_int32(BigArray, 1);
            MexTime = toc(t);

            fprintf('2D, dim=%d: Matlab: %.6f, Mex: %.6f\n', dim, MatlabTime, MexTime);                    
            assert(isequal(Or3d, MexOr3d));       
        end
    end
    
    
    % Or - 3d
    for Iter=1:3
        BigArray = uint32(double(0xFFFFFFFF) * rand(400, 400, 100));

        for dim=1:3
            t = tic;
            Or3d = tools.array.bitor_array(BigArray, 1);
            MatlabTime = toc(t);

            t = tic;
            MexOr3d = tools.array.mex.mex_bitor_array_int32(BigArray, 1);
            MexTime = toc(t);

            fprintf('3D, dim=%d: Matlab: %.6f, Mex: %.6f\n', dim, MatlabTime, MexTime);                    
            assert(isequal(Or3d, MexOr3d));       
        end    
    end
    
    
%end

