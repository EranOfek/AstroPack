% https://www.mathworks.com/help/matlab/import_export/share-memory-between-applications.html

function answer
% Respond to SEND using memmapfile class.

disp('ANSWER server is awaiting message');

filename = fullfile(tempdir, 'talk_answer.dat');

% Create the communications file if it is not already there.
if ~exist(filename, 'file')
    [f, msg] = fopen(filename, 'wb');
    if f ~= -1
        fwrite(f, zeros(1,256), 'uint8');
        fclose(f);
    else
        error('MATLAB:demo:answer:cannotOpenFile', ...
              'Cannot open file "%s": %s.', filename, msg);
    end
end

% Memory map the file.
m = memmapfile(filename, 'Writable', true, 'Format', 'uint8');

while true
    % Wait until the first byte is not zero.
    while m.Data(1) == 0
        pause(.25);
    end
    
    % The first byte now contains the length of the message.
    % Get it from m.
    msg = char(m.Data(2:1+double(m.Data(1))))';

    % Display the message.
    disp('Received message from SEND:')
    disp(msg)
    
    % Transform the message to all uppercase.
    m.Data(2:1+double(m.Data(1))) = upper(msg);
   
    % Signal to SEND that the response is ready.
    m.Data(1) = 0;
end
