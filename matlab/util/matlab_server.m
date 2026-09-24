function matlab_server()
    input_fifo = '/tmp/matlab_input';
    output_fifo = '/tmp/matlab_output';

    % Ensure FIFOs exist
    if ~exist(input_fifo, 'file'), system(['mkfifo ', input_fifo]); end
    if ~exist(output_fifo, 'file'), system(['mkfifo ', output_fifo]); end

    % Open FIFOs
    fid_input = fopen(input_fifo, 'r');
    fid_output = fopen(output_fifo, 'w');

    fprintf('MATLAB server started. Listening on %s\n', input_fifo);

    while true
        cmd = fgetl(fid_input);
        if ischar(cmd)
            try
                result = evalc(cmd);
                fprintf(fid_output, '%s\n', strtrim(result));  % Send output to FIFO
                fflush(fid_output);  % Ensure it gets written
            catch ME
                fprintf(fid_output, 'ERROR: %s\n', ME.message);
                fflush(fid_output);
            end
        end
    end
end
