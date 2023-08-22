function Result = mex_bitwise_cutouts_unitTest()

    positions = 1000;
    rows = 1700;
    cols = 1700;    
    stamp_size = 5;

    bitDepth = 16; % Number of bits in each value
    bitwise_or = true;
    
    iterations = 1000;
    old_cutouts_total_time = 0;
    bitwise_cutouts_total_time = 0;

    for i=1:iterations
    
        
        % Generate a random integer matrix
        if (bitDepth == 16)
            randomMatrix = uint16(randi(2^bitDepth - 1, rows, cols));
        elseif (bitDepth == 32)
            randomMatrix = uint32(randi(2^bitDepth - 1, rows, cols));
        end

        x_pos = rand(1,positions)*cols;
        y_pos = rand(1,positions)*rows;
        pos = [x_pos; y_pos]';

        if (bitDepth == 16)
            t=tic();
            bitwise_res = imUtil.cut.mex.mex_bitwise_cutouts_int16(randomMatrix,x_pos,y_pos,stamp_size,true);
            bitwise_cutouts_total_time = bitwise_cutouts_total_time + toc(t);
        elseif (bitDepth == 32)
            t=tic();
            bitwise_res = imUtil.cut.mex.mex_bitwise_cutouts_int32(randomMatrix,x_pos,y_pos,stamp_size,true);
            bitwise_cutouts_total_time = bitwise_cutouts_total_time + toc(t);

        end        



        t=tic();
        [Cube, im_sub] = imUtil.cut.mex.mex_cutout(randomMatrix, pos, stamp_size, 0, 0, 0, 0);
        % old_res = tools.array.bitor_array(Cube,3,true);
        old_cutouts_total_time = bitwise_cutouts_total_time + toc(t);
        
    end

    bitwise_cutouts_avg_time = bitwise_cutouts_total_time / iterations;
    old_cutouts_avg_time = old_cutouts_total_time / iterations;
    ratio = bitwise_cutouts_avg_time / old_cutouts_avg_time;

    fprintf('Bitwise cutouts avg time: %d, old cutouts avg time: %d, ratio: %d',bitwise_cutouts_avg_time,old_cutouts_avg_time,ratio);

    Result = true;

end