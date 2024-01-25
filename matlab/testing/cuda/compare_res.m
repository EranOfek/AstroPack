tolerance = 1e-6;

% Loop through each subdirectory
for i = 1:4
    N = power(10,i);

    cc_file = strcat("cpp_cpu/ndft_cpp_cpu_output_",num2str(N),".csv");
    cg_file = strcat("cpp_gpu/ndft_cpp_gpu_output_",num2str(N),".csv");
    mc_file = strcat("matlab_cpu/ndft_matlab_cpu_output_",num2str(N),".csv");
    mg_file = strcat("matlab_gpu/ndft_matlab_gpu_output_",num2str(N),".csv");

    cc_data = readtable(cc_file);
    cg_data = readtable(cg_file);
    mc_data = readtable(mc_file);
    mg_data = readtable(mg_file);

    for j = 1:height(cc_data)
        cc_value = cc_data{j, "Var1"};
        cg_value = cg_data{j, "Var1"};
        mc_value = mc_data{j, "Var1"};
        mg_value = mg_data{j, "Var1"};
    
        % Check if the real and imaginary parts are within tolerance
        assert(abs(real(cc_value) - real(cg_value)) < tolerance && ...
               abs(imag(cc_value) - imag(cg_value)) < tolerance, 'Data mismatch between cc and cg');
           
        assert(abs(real(cc_value) - real(mc_value)) < tolerance && ...
               abs(imag(cc_value) - imag(mc_value)) < tolerance, 'Data mismatch between cc and mc');
           
        assert(abs(real(cc_value) - real(mg_value)) < tolerance && ...
               abs(imag(cc_value) - imag(mg_value)) < tolerance, 'Data mismatch between cc and mg');        
    end

end

disp('All CSV files are the same across directories.');
