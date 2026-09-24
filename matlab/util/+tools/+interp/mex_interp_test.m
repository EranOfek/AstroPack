iter_num = 100;

for j = 1:2

    num_elements = 10^j;

    X = 1:num_elements;
    Y = 1:num_elements;
    [XI, YI] = meshgrid(1:0.1:num_elements, 1:0.1:num_elements);
    
    linear_time = 0;
    mat_linear_time = 0;
    nearest_time = 0;
    mat_nearest_time = 0;
    
    for i = 1:iter_num
        V = rand(num_elements)*1000;
        
        tic;
        ZI_linear = tools.interp.mex.mex_interp2_double(X, Y, V, XI, YI, 'linear');
        linear_time = toc + linear_time;
        tic;
        ZI_mat_linear = interp2(X,Y,V,XI,YI,'linear');
        mat_linear_time = toc + mat_linear_time;
        diff_linear = ZI_linear-ZI_mat_linear;
        sum_linear = sum(diff_linear,'all');
        avg_linear = sum_linear / (size(diff_linear,1) * size(diff_linear,2));
        assert(avg_linear<1e-12);
        
        tic;
        ZI_nearest = tools.interp.mex.mex_interp2_double(X, Y, V, XI, YI, 'nearest');
        nearest_time = toc + nearest_time;
        tic;
        ZI_mat_nearest = interp2(X,Y,V,XI,YI,'nearest');
        mat_nearest_time = toc + mat_nearest_time;
        diff_nearest = ZI_nearest-ZI_mat_nearest;
        sum_nearest = sum(diff_nearest,'all');
        avg_nearest = sum_nearest / (size(sum_nearest,1) * size(sum_nearest,2));
        assert(avg_nearest<1e-12);
    end
    
    avg_linear_time = linear_time / iter_num;
    avg_mat_linear_time = mat_linear_time / iter_num;
    avg_nearest_time = nearest_time / iter_num;
    avg_mat_nearest_time = mat_nearest_time / iter_num;
    
    fprintf("For %d by %d elements:\n",num_elements,num_elements);
    fprintf("%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%\n")
    
    fprintf("matlab linear time: %.6f s\n",avg_mat_linear_time);
    fprintf("mex linear time: %.6f s\n",avg_linear_time);
    fprintf("matlab nearest time: %.6f s\n",avg_mat_nearest_time);
    fprintf("mex nearest time: %.6f s\n",avg_nearest_time);
    fprintf("%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%\n\n")
end