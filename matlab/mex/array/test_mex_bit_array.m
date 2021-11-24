%

%function test_mex_bit_array()

    Array1     = uint32([ 0x0001; 0x0002; 0x0004; 0x0008 ]);
        
	% Input - prepare 2D matrix
    Array2a    = uint32([ 0x0001, 0x0002, 0x0004, 0x000A;...
                         0x0011, 0x0022, 0x0014, 0x0018 ]);

    Array2b    = uint32([ 0x0101, 0x0102, 0x0104, 0x010A;...
                         0x0111, 0x0122, 0x0114, 0x0118 ]);
                     
    % Add 3rd dimension
    Array3d = Array2a;
    Array3d(:,:,2) = Array2b;
    
    % Expected results
    
    % 1d
    ExpectedOr1   = 0x000F;
    
    % 2d
    ExpectedOr2a  = uint32([ 0x0011, 0x0022, 0x0014, 0x001A ]);
    ExpectedOr2b  = uint32([ 0x0111, 0x0122, 0x0114, 0x011A ]);    
    
    % 3d, dim=1
    ExpectedOr3d  = ExpectedOr2a;
    ExpectedOr3d(:,:,2) = ExpectedOr2b;        

    %3d, dim = 2
    ExpectedOr3dDim2a = uint32([ 0x000F; 0x003F ]);
    ExpectedOr3dDim2b = uint32([ 0x010F; 0x013F ]);
    
    % 3d, dim = 3
    ExpectedOr3dDim3  = uint32([ 0x0101, 0x0102, 0x0104, 0x010A;...
                                 0x0111, 0x0122, 0x0114, 0x0118 ]);
                     

    
    % Or - Input: 1d -> Output: scalar
    Or = tools.array.bitor_array(Array1);
    disp(Or);
    assert(strcmp(class(Or), 'uint32'));
    assert(ndims(Or) == 2);
    assert(isequal(Or, ExpectedOr1));
    
    % Or - Input: 2d -> Output: 2d
    Or = tools.array.bitor_array(Array2a);
    disp(Or);
    assert(strcmp(class(Or), 'uint32'));
    assert(ndims(Or) == 2);
    assert(isequal(Or, ExpectedOr2a));
    
    % Or - Input: 3d, dim=1 -> Output: 3d
    Or3d = tools.array.bitor_array(Array3d, 1);
    disp(Or3d);
    assert(strcmp(class(Or3d), 'uint32'));
    assert(ndims(Or3d) == 3);
    assert(isequal(Or3d(:,:,1), ExpectedOr2a));
    assert(isequal(Or3d(:,:,2), ExpectedOr2b));
    
    % Or - Input: 3d,dim=2 -> Output: 3d
    Or3d = tools.array.bitor_array(Array3d,2);
    disp(Or3d);
    assert(strcmp(class(Or3d), 'uint32'));
    assert(ndims(Or3d) == 3);
    assert(isequal(Or3d(:,:,1), ExpectedOr3dDim2a));
    assert(isequal(Or3d(:,:,2), ExpectedOr3dDim2b));
        
    % Or - Input: 3d, dim=3 -> Output: 2d
    Or3d = tools.array.bitor_array(Array3d, 3);
    disp(Or3d);
    assert(strcmp(class(Or3d), 'uint32'));
    assert(ndims(Or3d) == 2);
    assert(isequal(Or3d, ExpectedOr3dDim3));   
    
    %---------------------------------------------------------- MEX
    % MEX - 2d
    Or = mex_bitor_array(Array, 1);
    disp(size(Out));    
    assert(ndims(Or) == 2);
    %assert(isequal(Or, ExpectedOr));

    %3d MEX - 3d, dim=1
    Or = mex_bitor_array(Array, 1);
    disp(Or3d);
    assert(strcmp(class(Or3d), 'uint32'));
    assert(ndims(Or3d) == 3);
    %assert(isequal(Or3d(:,:,1), ExpectedOr));
    %assert(isequal(Or3d(:,:,2), ExpectedOr2));
    
    
    % MEX - 3d, dim=3
    Or3d = mex_bitor_array(Array, 3);
    disp(Or3d);
    assert(strcmp(class(Or3d), 'uint32'));
    assert(ndims(Or3d) == 2);
    %assert(isequal(Or3d, ExpectedOr3dDim3));   
    
    %-------------------------------------------------------- Performance
    
    % Or - 3d - dim=3, returns 2d
    BigArray = uint32(rand(100, 100, 100));
    for Iter=1:5
        t = tic;
        Or3d = tools.array.bitor_array(BigArray, 3);
        MatlabTime = toc - t;

        t = tic;
        MexOr3d = mex_bitor_array(BigArray, 3);
        MexTime = toc - t;

        fprintf('Matlab: %.6f, Mex: %.6f\n', MatlabTime, MexTime);                    
        assert(isequal(Or3d, MexOr3d));       
    end
    
    
%end
