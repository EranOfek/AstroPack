function Result = mex_cutouts_unitTest()

    % Iterate all combinations
    for rows=[6,7,8,9,10]
        for cols=[6,7,8,9,10]
            a = reshape(1:rows*cols, [rows,cols]);
            for cs=[1,3,5]
                cut_size = cs;
                if cut_size > rows || cut_size > cols
                    continue;
                end
                
                n = 1;
                pos = [];
                for x=1:cols
                    for y=1:rows
                        pos(end+1,:) = double([x, y]);
                        [Cube] = imUtil.cut.mex.mex_cutout(a, pos, cut_size);
                        n = n+2;
                    end
                end        
            end
        end
    end
    
    
    % That used to crash before Chen's fix
    load III.mat;  
    MaxRadius = 12;
    [Cube] = imUtil.cut.mex.mex_cutout(Image,double([X, Y]),MaxRadius.*2+1);

    Result = true;
end
