% https://www.mathworks.com/help/matlab/import_export/share-memory-between-applications.html

% Learn how to implement Mutex for ipc

% Background threads (Java or other solution?)



function send
% Interactively send a message to ANSWER using memmapfile class.
 
filename = fullfile(tempdir, 'talk_answer.dat');
 
% Create the communications file if it is not already there.
if ~exist(filename, 'file')
    [f, msg] = fopen(filename, 'wb');
    if f ~= -1
        fwrite(f, zeros(1,256), 'uint8');
        fclose(f);
    else
        error('MATLAB:demo:send:cannotOpenFile', ...
              'Cannot open file "%s": %s.', filename, msg);
    end
end
 
% Memory map the file.
m = memmapfile(filename, 'Writable', true, 'Format', 'uint8');
 
while true
    % Set first byte to zero, indicating a message is not
    % yet ready.
    m.Data(1) = 0;
 
    str = input('Enter text (or RETURN to end): ', 's');
 
    len = length(str);
    if (len == 0)
        disp('Terminating SEND function.')
        break;
    end
    
    % Warn if the message is longer than 255 characters.
    if len > 255
        warning('ml:ml','SEND input will be truncated to 255 characters.');
    end
    str = str(1:min(len,255));  % Limit message to 255 characters.
    len = length(str); % Update len if str has been truncated. 
    
    % Update the file via the memory map.
    m.Data(2:len+1) = str;
    m.Data(1)=len;
   
 
    % Wait until the first byte is set back to zero, 
    % indicating that a response is available.
    while (m.Data(1) ~= 0)
        pause(.25);
    end
    
    % Display the response.
    disp('response from ANSWER is:')
    disp(char(m.Data(2:len+1))')
   
end




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



