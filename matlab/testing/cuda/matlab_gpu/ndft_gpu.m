NN = 10 .^ (1:4);
iters = 10;

for iii = 1 : length(NN)

    time_gpu_no_sort = 0;

    N = NN(iii);

    M = readmatrix(strcat("../inputs/ndft_input_",int2str(N),".csv"));
    t=M(1,:);
    m=M(2,:);
    f=M(3,:);

    for iter = 1:iters
    
        %% matlab gpu no sort
        ttic = tic;
        
        % Transfer data to GPU
        t_gpu = gpuArray(t);
        m_gpu = gpuArray(m);
        f_gpu = gpuArray(f);
        
        % Create a matrix on the GPU
        exp_matrix_gpu = m_gpu .* exp(-2 * pi * 1i * f_gpu.' * t_gpu);
        
        % Compute Fourier coefficients on the GPU
        p_gpu = sum( exp_matrix_gpu, 2) / N;
        
        % Transfer the result back to the CPU if needed
        p_matlab_gpu = gather(p_gpu);
        
        time_gpu_no_sort = time_gpu_no_sort + toc(ttic);
        
        
        %% Sort time vector and measurements
        % ttic = tic;
        % 
        % [t_sorted, sort_idx] = sort(t);
        % m_sorted = m(sort_idx);
        % 
        % time_sort = time_sort + toc(ttic);
        % 
        % 
        % %% matlab gpu after sort
        % ttic = tic;
        % 
        % % Transfer data to GPU
        % t_gpu_s = gpuArray(t_sorted);
        % m_gpu_s = gpuArray(m_sorted);
        % f_gpu_s = gpuArray(f);
        % 
        % % Create a matrix on the GPU
        % exp_matrix_gpu_s = m_gpu_s .* exp(-2 * pi * 1i * f_gpu_s.' * t_gpu_s);
        % 
        % % Compute Fourier coefficients on the GPU
        % p_gpu_s = sum( exp_matrix_gpu_s, 2) / N;
        % 
        % % Transfer the result back to the CPU if needed
        % p_matlab_gpu_s = gather(p_gpu_s);
        % 
        % time_gpu_sort = time_gpu_sort + toc(ttic);
    end


    time_gpu_no_sort_avg    = time_gpu_no_sort/iters;
    % time_sort_avg           = time_sort/iters;
    % time_gpu_sort_avg       = time_gpu_sort/iters;
    fprintf("For N=%d, gpu time=%e \n",N,time_gpu_no_sort_avg);

    writematrix(p_matlab_gpu,strcat("ndft_matlab_gpu_output_",int2str(N),".csv"));
    clear t m f t_gpu m_gpu f_gpu exp_matrix_gpu p_gpu p_matlab_gpu;

end