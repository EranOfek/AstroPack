% Package Unit-Test
%
% ### Requirements:
%
%
%


function Result = unitTest()
    % Package Unit-Test   
	io.msgStyle(LogLevel.Test, '@start', 'test started');
    
    mex_bitwise_cutouts_unitTest();
    
	io.msgStyle(LogLevel.Test, '@passed', 'test passed');
	Result = true;
end

%--------------------------------------------------------------------------

function Result = mex_bitwise_cutouts_unitTest()

    io.msgStyle(LogLevel.Test, '@start', 'mex_bitwise_cutouts test started');

    iterations = 1000;

    positions = 1000;
    rows = 1700;
    cols = 1700;    
    stamp_size = 5;
    
    bitwise_or = [true,false];
    old_cutouts_total_time = 0;
    bitwise_cutouts_total_time_16 = 0;
    bitwise_cutouts_total_time_32 = 0;

    for mode=1:length(bitwise_or)
        for i=1:iterations          
            % generate a random integer matrix
            randomMatrix_16 = uint16(randi(2^16 - 1, rows, cols));
            randomMatrix_32 = uint32(randi(2^32 - 1, rows, cols));
    
            % generate random positions
            x_pos = rand(1,positions)*cols;
            y_pos = rand(1,positions)*rows;
            pos = [x_pos; y_pos]';
    
            % perform 16 bit cutout and bitwise using mex function
            t=tic();
            bitwise_res_16 = imUtil.cut.mex.mex_bitwise_cutouts_int16(randomMatrix_16,x_pos,y_pos,stamp_size,bitwise_or(mode));
            bitwise_cutouts_total_time_16 = bitwise_cutouts_total_time_16 + toc(t);
    
            % perform 32 bit cutout and bitwise using mex function
            t=tic();
            bitwise_res_32 = imUtil.cut.mex.mex_bitwise_cutouts_int32(randomMatrix_32,x_pos,y_pos,stamp_size,bitwise_or(mode));
            bitwise_cutouts_total_time_32 = bitwise_cutouts_total_time_32 + toc(t);
    
            % compare to older version of cutout operation (currently only
            % accepts uint16 and performs the cutout without bitwise
            t=tic();
            [Cube, im_sub] = imUtil.cut.mex.mex_cutout(randomMatrix_16, pos, stamp_size, 0, 0, 0, 0);
            % if bitwise_or
                % old_res = tools.array.bitor_array(Cube,3,true);
            % else
                % old_res = tools.array.bitand_array(Cube,3,true);
            old_cutouts_total_time = old_cutouts_total_time + toc(t);
            
        end

        bitwise_cutouts_avg_time_32 = bitwise_cutouts_total_time_32 / iterations;
        bitwise_cutouts_avg_time_16 = bitwise_cutouts_total_time_16 / iterations;
        old_cutouts_avg_time = old_cutouts_total_time / iterations;
        ratio = bitwise_cutouts_avg_time_16 / old_cutouts_avg_time;
        ratio_per = ratio*100;
    
        fprintf('Bitwise cutouts (bitwise_or: %s) uint16 avg time: %d, old cutouts avg time: %d, ratio: %.2f%% \n',num2str(bitwise_or(mode)),bitwise_cutouts_avg_time_16,old_cutouts_avg_time,ratio_per);
        fprintf('Bitwise cutouts (bitwise_or: %s) uint32 avg time: %d \n',num2str(bitwise_or(mode)),bitwise_cutouts_avg_time_32);

    end

    Result = true;

    io.msgStyle(LogLevel.Test, '@passed', 'passed');    
end



%--------------------------------------------------------------------------

