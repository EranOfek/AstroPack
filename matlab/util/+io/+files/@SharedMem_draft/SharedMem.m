% https://www.mathworks.com/help/matlab/import_export/share-memory-between-applications.html
% Learn how to implement Mutex for ipc
% Background threads (Java or other solution?)

% #functions (autogen)
% SharedMem -
% answer - Respond to SEND using memmapfile class.
% close - Close file
% delete -
% doGetBuf - Get next buffer from shared-memory, return bytes object or None if queue is empty Return buf, put_counter, width, height, flags
% getBuf - Return buf, put_counter, w, h with m = Locker(...)
% open -
% read - Read buffer from shared-memory
% send - Interactively send a message to ANSWER using memmapfile class.
% write - Write buffer to shared-memory
% #/functions (autogen)
%

%
% // Shared memory header, at offset 0
% struct MemHeader {
%     uint32_t    signature;      // Unique signature
%     uint32_t    memsize;        // Size of entire shared memory block
%     uint32_t    flags;          // Flags
%     uint32_t    item_size;      // Image buffer size
%     uint32_t    item_alloc;     // Number of buffers currently in queue
%     uint32_t    head;           // Head index
%     uint32_t    tail;           // Tail index
%     uint32_t    count;          // Number of items in buffer
% };
%
% struct BufHeader {
%     uint32_t    put_counter;    // Running counter by put_buf()
%     uint32_t    buf_len;        // Data length
%     uint32_t    width;          // Image width
%     uint32_t    height;         // Image height
%     uint32_t    flags;          // Flags, BUFH_...
% };


classdef SharedMem < Component
    
    properties (Hidden, SetAccess = public)
        FileName
        FileSize
        MMap
        MemSize
        MemSignature = 0x11031973
        MemHeaderSize = 8 * 4       % sizeof(struct MemHeader)
        BufHeaderSize = 5 * 4       % sizeof(struct BufHeader)
        
        % Flags
        BUFH_UYVY = 0x00000001
        BUFH_MAT_RGB = 0x00000002
    end
    

    methods % Constructor
        
        function Obj = SharedMem
            Obj.setName('SharedMem')
            

        end
        
        
        % Destructor
        function delete(Obj)
            Obj.msgLog(LogLevel.Debug, 'deleted: %s', Obj.Uuid);
            Obj.close();
        end
        
    end
    
    
    methods
               
        function Result = open(Obj, FName, Args)
            arguments
                Obj
                FName
                Args.Create = false
                Args.FileSize = 0
            end

            % Open memory mapped file
            Obj.FileName = FName;
            print('SharedMemory.open: ' + Obj.FileName)

            % Open/create file and set size
            if Args.Create
                Data = uint8(1:Obj.MemSize);
                fileID = fopen(Obj.FileName, 'w');
                fwrite(fileID, Data, 'uint8');
                fclose(fileID);
            end
            
			Stat = dir(Obj.FileName);
			Obj.FileSize = Stat.bytes;
			
            print('file size: ', os.path.getsize(Obj.filename), 'mmap size: ', Obj.memsize)

            % Open mmap
            Obj.MMap = memmapfile(filename, 'Writable', true, 'Format', 'uint8');
        end


        function Result = close(Obj)
            % Close file
            print('SharedMemory.close: ' + Obj.FileName)

            % Close mmap
            if ~isempty(Obj.MMap)
                self.mmap.close()
                Obj.MMap = [];
            end

            print('SharedMemory.close: done: ' + Obj.FileName)
        end

        
        function Result = write(Obj, pos, buf)
            % Write buffer to shared-memory
            self.mmap.seek(pos)
            self.mmap.write(buf)
        end


        function Result = read(Obj, pos, nbytes):
            % Read buffer from shared-memory
            self.mmap.seek(pos)
            buf = self.mmap.read(nbytes)
            return buf
        end


        function Result = getBuf(Obj)
            % Return buf, put_counter, w, h
            %with m = Locker(...)
            buf, put_counter, width, height, flags = self.do_get_buf()
            return buf, put_counter, width, height, flags
        end


        function Result = doGetBuf(Obj)
            % Get next buffer from shared-memory, return bytes object or None if queue is empty
            % Return buf, put_counter, width, height, flags

            % Read header to array of unsigned integers
            header = array.array('I')
            header_bytes = self.read(0, self.memheader_size)
            if len(header_bytes) > 0
                header.frombytes(header_bytes)

                % Check signature to validate that we have a correct header
                signature = header[0]
                if signature == self.memsignature

                    % Check 'count' field for number of items in queue
                    count = header[7]
                    if count > 0

                        % Get tail and copy buffer from memory
                        memsize = header[1]
                        flags = header[2]
                        item_size = header[3]
                        item_alloc = header[4]
                        tail = header[6]
                        hdr_offset = self.memheader_size + (tail * (item_size + self.bufheader_size))

                        % Get buffer length
                        hdr_buf = self.read(hdr_offset, self.bufheader_size)
                        hdr = array.array('I')
                        hdr.frombytes(hdr_buf)

                        % Get data from header (BufHeader)
                        put_counter = hdr[0]
                        buflen = hdr[1]
                        width = hdr[2]
                        height = hdr[3]
                        flags = hdr[4]

                        % Get data buffer
                        buf = self.read(hdr_offset + self.bufheader_size, buflen)

                        % Update header: count, tail
                        count = count - 1
                        header[7] = count
                        tail = tail + 1
                        if tail == item_alloc:
                            tail = 0
                        end
                        header[6] = tail

                        % Update header, tail and count are at [5], [6]
                        header_update = array.array('I')
                        header_update.append(tail)
                        header_update.append(count)
                        update_bytes = header_update.tobytes()
                        tail_offset_in_header = 6 * 4
                        self.write(tail_offset_in_header, update_bytes)

                        return buf, put_counter, width, height, flags
                    end
                end
            end

            return None, 0, 0, 0, 0
        end
    end
                    
                    
    % Unit test
    methods(Static)
        Result = unitTestServerSide()
            % Simple test (without header) - write current time string to shared memory


        Result = unitTestClientSide()
            % Simple test - read string from shared memory
        
        
        Result = unitTestClientSideBuf()
            % Read next buffer from shared-memory queue

        Result = unitTest()

    end
             
    
end




function send
% Interactively send a message to ANSWER using memmapfile class.
 
filename = fullfile(tempdir, 'talk_answer.dat');
 
% Create the communications file if it is not already there.
%if ~exist(filename, 'file')
if ~isfile(filename)
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
%if ~exist(filename, 'file')
if ~isfile(filename)
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



