NN = 10 .^ (1:4);
iters = 10;

for iii = 1 : length(NN)

    time_matlab = 0;

    N = NN(iii);

    M = readmatrix(strcat("../inputs/ndft_input_",int2str(N),".csv"));
    t=M(1,:);
    m=M(2,:);
    f=M(3,:);

    for iter = 1:iters
        
        % t = rand(1,N)*1000;
        % m = rand(1,N);
        % f = logspace(log10(1), log10(1000), N);
        % writematrix([t;m;f],strcat("ndft_input_",int2str(N),".csv"));

        %% matlab
        ttic = tic;
        
        exp_matrix = m .* exp(-2 * pi * 1i * f' .* t);
        p_matlab = sum(exp_matrix, 2) / N;
        
        time_matlab = time_matlab + toc(ttic);
        
    end

    time_matlab_avg         = time_matlab/iters; 
    fprintf("For N=%d, regular time=%e \n",N,time_matlab_avg);

    writematrix(p_matlab,strcat("ndft_matlab_cpu_output_",int2str(N),".csv"));
    clear t m f exp_matrix p_matlab;

end