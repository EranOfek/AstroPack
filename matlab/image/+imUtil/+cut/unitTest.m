function Result = unitTest()
    % Unit-Test for imUtil.cut package
	%io.msgStyle(LogLevel.Test, '@start', 'test started');
    
    mex_bitwise_cutouts_unitTest();
    
    %% imUtil.cut.mex.imageCutouts

    for I=1:1:100
        R=rand(1716,1716);
        X=rand(3000,1).*1716;
        Y=rand(3000,1).*1716;
        [a1]=imUtil.cut.mex.imageCutouts(R,X,Y,25);
        [a]=imUtil.cut.mex.mex_cutout(R,[X Y],25,0,0,0,1);
        if max(abs(squeeze(a)-a1),[],'all')>0
            error('Problem with imUtil.cut.mex.imageCutouts');
        end
    end

    X = -100;
    Y = 2000;
    [a1]=imUtil.cut.mex.imageCutouts(R,X,Y,25);
    if ~all(a1==0)
        error('Problem with imUtil.cut.mex.imageCutouts / out of bound test');
    end


    %% imUtil.cut.mex.image2cube  /  imUtil.cut.mex.cube2image
    VX=(1:1:1716); VY=VX.';
    Im=VX.*1.1+VY.*1.2;
    [Sub_CCDSEC, NSub, NoOverlapCCDSEC, NewNoOverlapCCDSEC, CentersXY] = imUtil.cut.gridSubImage([1716 1716], [256 256]);
    
    Sub=imUtil.cut.partition_subimage(Im,Sub_CCDSEC);
    Cube=imUtil.cut.mex.image2cube(Im,Sub_CCDSEC);             
    if max(abs(Sub{1}-Cube(:,:,1)),[],'all')>0
        error('Problem with imUtil.cut.mex.image2cube');
    end
    
    FullImage = imUtil.cut.mex.cube2image(Cube, Sub_CCDSEC, NoOverlapCCDSEC, NewNoOverlapCCDSEC);
    if max(abs(Im-FullImage),[],'all')>0
        error('Problem with imUtil.cut.mex.cube2image');
    end

    %% imUtil.cut.subimage_grid (issue #1278)
    % for every grid: the sub images lie inside the image and contain their
    % non-overlapping region, the non-overlapping regions tile the image
    % exactly, and the non-overlapping region in the sub image frame is
    % consistent with the one in the full image frame (it used to be stale
    % for the edge sub images, and MakeSquare used to push the sub images
    % of a 1x3 split outside the image)
    Grids = {[240 200], [1 3], [12 12], true;...
             [240 200], [1 3], [12 12], false;...
             [240 200], [3 1], [12 12], true;...
             [240 200], [2 2], [10 10], true;...
             [240 200], [3 3], [10 10], true;...
             [9600 6388], [6 4], [64 64], true;...
             [1726 1726], [13 13], [16 16], true};
    for Ig=1:1:size(Grids,1)
        SizeXY = Grids{Ig,1};
        [CCDSEC, UnCCDSEC, ~, ~, NewNoOverlap] = imUtil.cut.subimage_grid(SizeXY, 'SubSizeXY',SizeXY, 'Nxy',Grids{Ig,2}, 'OverlapXY',Grids{Ig,3}, 'MakeSquare',Grids{Ig,4});
        if ~all(CCDSEC(:,[1 3])>=1, 'all') || ~all(CCDSEC(:,2)<=SizeXY(1)) || ~all(CCDSEC(:,4)<=SizeXY(2))
            error('Problem with imUtil.cut.subimage_grid: sub image outside the image');
        end
        if ~all(CCDSEC(:,[1 3])<=UnCCDSEC(:,[1 3]) & CCDSEC(:,[2 4])>=UnCCDSEC(:,[2 4]), 'all')
            error('Problem with imUtil.cut.subimage_grid: sub image does not contain its non-overlapping region');
        end
        Cover = zeros(SizeXY(2), SizeXY(1));
        for Isub=1:1:size(UnCCDSEC,1)
            Cover(UnCCDSEC(Isub,3):UnCCDSEC(Isub,4), UnCCDSEC(Isub,1):UnCCDSEC(Isub,2)) = Cover(UnCCDSEC(Isub,3):UnCCDSEC(Isub,4), UnCCDSEC(Isub,1):UnCCDSEC(Isub,2)) + 1;
        end
        if ~all(Cover==1, 'all')
            error('Problem with imUtil.cut.subimage_grid: non-overlapping regions do not tile the image');
        end
        if ~isequal(NewNoOverlap, UnCCDSEC - CCDSEC(:,[1 1 3 3]) + 1)
            error('Problem with imUtil.cut.subimage_grid: NewNoOverlap is inconsistent with CCDSEC');
        end
    end
    % the 1x3 split must be a partition, not three copies of the image
    [CCDSEC] = imUtil.cut.subimage_grid([240 200], 'SubSizeXY',[240 200], 'Nxy',[1 3], 'OverlapXY',[12 12]);
    if size(unique(CCDSEC,'rows'),1)~=3 || any(CCDSEC(:,4)-CCDSEC(:,3)+1 > 100)
        error('Problem with imUtil.cut.subimage_grid: 1x3 split is not a partition of the image');
    end




    %%
    D=rand(100,100,3);
    C1=imUtil.cut.trim(D,[1 2],false,[],true);
    C2=imUtil.cut.trim(D,[1 2],false,[],false);
    if max(abs(C1-C2),[],'all')>0
        error('Problem with imUtil.cut.mex.trimImage');
    end
    C1=imUtil.cut.trim(D,[1 2 1 3],false,[],true);
    C2=imUtil.cut.trim(D,[1 2 1 3],false,[],false);
    if max(abs(C1-C2),[],'all')>0
        error('Problem with imUtil.cut.mex.trimImage');
    end
    C1=imUtil.cut.trim(D,[1 2 1 5],'center',[],true);
    C2=imUtil.cut.trim(D,[1 2 1 5],'center',[],false);
    if max(abs(C1-C2),[],'all')>0
        error('Problem with imUtil.cut.mex.trimImage');
    end
    C1=imUtil.cut.trim(D,[1 2 1 3],true,[],true);
    C2=imUtil.cut.trim(D,[1 2 1 3],true,[],false);
    if max(abs(C1-C2),[],'all')>0
        error('Problem with imUtil.cut.mex.trimImage');
    end


    D=rand(100,100);
    C1=imUtil.cut.trim(D,[1 2],false,[],true);
    C2=imUtil.cut.trim(D,[1 2],false,[],false);
    if max(abs(C1-C2),[],'all')>0
        error('Problem with imUtil.cut.mex.trimImage');
    end
    C1=imUtil.cut.trim(D,[1 2 1 3],false,[],true);
    C2=imUtil.cut.trim(D,[1 2 1 3],false,[],false);
    if max(abs(C1-C2),[],'all')>0
        error('Problem with imUtil.cut.mex.trimImage');
    end
    C1=imUtil.cut.trim(D,[1 2 1 5],'center',[],true);
    C2=imUtil.cut.trim(D,[1 2 1 5],'center',[],false);
    if max(abs(C1-C2),[],'all')>0
        error('Problem with imUtil.cut.mex.trimImage');
    end
    C1=imUtil.cut.trim(D,[1 2 1 3],true,[],true);
    C2=imUtil.cut.trim(D,[1 2 1 3],true,[],false);
    if max(abs(C1-C2),[],'all')>0
        error('Problem with imUtil.cut.mex.trimImage');
    end

    % mex.trimImage
    Image=rand(9600,6400,'single');
    Nsim = 1000;
    tic;
    for i=1:Nsim
        R2=Image(3001:4000,1001:2000);
    end
    T=toc;
    fprintf('Trimimage using matlab: %f\n',T);

    tic;
    for i=1:Nsim
        R1=imUtil.cut.mex.trimImage(Image,[3001 4000 1001 2000]);
    end
    T=toc;
    fprintf('Trimimage using imUtil.cut.mex.trimImage: %f\n',T);

    if max(abs(R1-R2))>0
        error('Problem with imUtil.cut.mex.trimImage');
    end


	%io.msgStyle(LogLevel.Test, '@passed', 'test passed');
	Result = true;
end

%--------------------------------------------------------------------------

function Result = mex_bitwise_cutouts_unitTest()

    %io.msgStyle(LogLevel.Test, '@start', 'mex_bitwise_cutouts test started');

    iterations = 1;

    positions = 1000;
    rows = 1700;
    cols = 1700;    
    stamp_size = 5;
    
    bitwise_or = [true,false];
    bitwise_or_old = ["or","and"];
    old_cutouts_total_time_16 = 0;
    old_cutouts_total_time_32 = 0;
    bitwise_cutouts_total_time_16 = 0;
    bitwise_cutouts_total_time_32 = 0;

    for mode=1:length(bitwise_or)
        for i=1:iterations          
            % generate a random integer matrix
            randomMatrix_16 = uint16(randi(2^16 - 1, rows, cols));
            randomMatrix_32 = uint32(randi(2^32 - 1, rows, cols));
    
            % generate random positions
            x_pos = round(rand(1, positions) * (cols - 1)) + 1;
            y_pos = round(rand(1, positions) * (rows - 1)) + 1;
            pos = [x_pos; y_pos]';
    
            % perform 16 bit cutout and bitwise using mex function
            t=tic();
            bitwise_res_16 = imUtil.cut.mex.mex_bitwise_cutouts_int16(randomMatrix_16,x_pos,y_pos,stamp_size,bitwise_or(mode));
            bitwise_cutouts_total_time_16 = bitwise_cutouts_total_time_16 + toc(t);
    
            % perform 16 bit cutout and bitwise using old function
            t = tic();
            bitwise_res_32 = imUtil.cut.mex.mex_bitwise_cutouts_int32(randomMatrix_32,x_pos,y_pos,stamp_size,bitwise_or(mode));
            bitwise_cutouts_total_time_32 = bitwise_cutouts_total_time_32 + toc(t);
    
            % perform 32 bit cutout and bitwise using old function
            t = tic();
            IC = MaskImage({randomMatrix_16});
            old_res_16 = IC.bitwise_cutouts(pos,bitwise_or_old(mode),'HalfSize',stamp_size);
            old_cutouts_total_time_16 = old_cutouts_total_time_16 + toc(t);

            % compare to older version of cutout operation (currently only
            % accepts uint16 and performs the cutout without bitwise
            t = tic();
            IC = MaskImage({randomMatrix_32});
            old_res_32 = IC.bitwise_cutouts(pos,bitwise_or_old(mode),'HalfSize',stamp_size);
            old_cutouts_total_time_32 = old_cutouts_total_time_32 + toc(t);

            assert(isequal(old_res_16, bitwise_res_16'), '16bit cutout calculations are not identical.');
            assert(isequal(old_res_32, bitwise_res_32'), '32bit cutout calculations are not identical.');
            
        end

        bitwise_cutouts_avg_time_16 = bitwise_cutouts_total_time_16 / iterations;
        bitwise_cutouts_avg_time_32 = bitwise_cutouts_total_time_32 / iterations;        
        old_cutouts_avg_time_16 = old_cutouts_total_time_16 / iterations;
        old_cutouts_avg_time_32 = old_cutouts_total_time_32 / iterations;
        ratio_16 = bitwise_cutouts_avg_time_16 / old_cutouts_avg_time_16;
        ratio_per_16 = ratio_16*100;
        ratio_32 = bitwise_cutouts_avg_time_32 / old_cutouts_avg_time_32;
        ratio_per_32 = ratio_32*100;        
    
        fprintf('Bitwise cutouts (bitwise_or: %s) uint16 avg time: %d, old cutouts avg time: %d, ratio: %.2f%% \n',num2str(bitwise_or(mode)),bitwise_cutouts_avg_time_16,old_cutouts_avg_time_16,ratio_per_16);
        fprintf('Bitwise cutouts (bitwise_or: %s) uint32 avg time: %d, old cutouts avg time: %d, ratio: %.2f%% \n',num2str(bitwise_or(mode)),bitwise_cutouts_avg_time_32,old_cutouts_avg_time_32,ratio_per_32);

    end


    

    Result = true;

    %io.msgStyle(LogLevel.Test, '@passed', 'passed');    
end



%--------------------------------------------------------------------------

