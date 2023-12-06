function files_arrived(Files, WaitTime)
% Check if all files in a list arrived to disk (i.e., size not increasing).
% Package: io.files
% Description: Check if all files in a list arrived to disk. This is done
%              by checking that the file size does not increase with time.
% Input  : - Cell array of files to check. 
%            If empty, then check all files in directory.
%            Default is empty.
%          - Time to wait between tests. Default is 1 s.
% Output : null. Return when file sizes converged.
% License: GNU general public license version 3
%     By : Eran O. Ofek                    May 2017
%    URL : http://weizmann.ac.il/home/eofek/matlab/
% Example: io.files.files_arrived({'files_arrived'});
% Reliable: 2
%--------------------------------------------------------------------------

arguments
    Files    = [];
    WaitTime = 1;
end

if isempty(Files)
    D = dir('*');
    Files = {D(~[D.isdir]).name};
end

Arrived = false;
Dir1 = io.files.dir_cell(Files);
while ~Arrived    
    % check if all files arrived
    pause(WaitTime);
    Dir2 = io.files.dir_cell(Files);
    %[Dir1.bytes] - [Dir2.bytes]
    try
        if (all(([Dir1.bytes] - [Dir2.bytes])==0))
            Arrived = true;
        end
    catch
        fprintf('May be a problem Dir1 vs. Dir2');
        pause(3);
    end

    Dir1 = Dir2;
end
pause(0.2);

    