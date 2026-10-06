% @QA - Write

% Package Unit-Test
%
% ### Requirements:
%
%
%


function Result = unitTest()
    % Package Unit-Test   
	io.msgStyle(LogLevel.Test, '@start', 'test started');
    
    func_unitTest();
    verifyTextFile_unitTest();
    
	io.msgStyle(LogLevel.Test, '@passed', 'test passed');
	Result = true;
end

%--------------------------------------------------------------------------


function Result = func_unitTest()
	% Function Unit-Test
	io.msgStyle(LogLevel.Test, '@start', 'test started');
   
	io.msgStyle(LogLevel.Test, '@passed', 'passed');
	Result = true;
end


%--------------------------------------------------------------------------


%--------------------------------------------------------------------------


function Result = verifyTextFile_unitTest()
    % Unit-Test for io.files.verifyTextFile
    % The two corrupt fixtures are the real ones found on last09e and last06w
    % (AstroPack #1376): a FITS header card, and raw image bytes.
    io.msgStyle(LogLevel.Test, '@start', 'verifyTextFile test started');

    ISO  = '^\d{4}-\d{2}-\d{2}T\d{2}:\d{2}:\d{2}';
    Line = '2026-10-05T14:00:00 ready-for-transfer';
    Dir  = tempname;
    mkdir(Dir);
    File = fullfile(Dir, '.status');

    % every status-line form seen in production must pass
    Good = {'2025-05-25T19:09:12 ready-for-transfer'
            '2025-05-26T10:23:47+00:00 transfered #  hours from ready for transfer'
            '2026-09-19T13:29:17+00:00 transfered # 25 hours from ready for transfer'
            '2026-06-05T11:41:53 injected into the visit image DB'
            '2026-06-21T02:54:20 Injected into the visit image table'
            '2026-07-09T16:22:41 injected into the proc catalog DB'};
    FID = fopen(File,'w');
    fprintf(FID,'%s\n',Good{:});
    fprintf(FID,'%s\n',Line);
    fclose(FID);
    [OK,Reason] = io.files.verifyTextFile(File,'MustContain',Line,'LinePattern',ISO);
    assert(OK, 'real status file rejected: %s', Reason);

    % a second writer appending after us must not fail the check
    FID = fopen(File,'a');
    fprintf(FID,'%s\n','2026-10-05T14:00:01 transfered # 1 hours');
    fclose(FID);
    [OK,~] = io.files.verifyTextFile(File,'MustContain',Line,'LinePattern',ISO);
    assert(OK, 'append by another writer must still verify');

    % FITS header card, printable: only the line grammar catches it
    FID = fopen(File,'w');
    fprintf(FID,'%s',"  label for field  30   TFORM30 = '1D      '  / data format of field: 8-byte");
    fclose(FID);
    FID = fopen(File,'a');
    fprintf(FID,'\n%s\n',Line);
    fclose(FID);
    [OK,~] = io.files.verifyTextFile(File,'MustContain',Line);
    assert(OK, 'without LinePattern a printable clobber is expected to pass');
    [OK,Reason] = io.files.verifyTextFile(File,'MustContain',Line,'LinePattern',ISO);
    assert(~OK && contains(Reason,'LinePattern'), 'header card not caught: %s', Reason);

    % raw image bytes
    FID = fopen(File,'w');
    fwrite(FID, uint8([129 225 129 207 129 214 130 7]), 'uint8');
    fclose(FID);
    [OK,Reason] = io.files.verifyTextFile(File,'MustContain',Line,'LinePattern',ISO);
    assert(~OK && contains(Reason,'not text'), 'binary not caught: %s', Reason);

    % truncated to zero
    FID = fopen(File,'w');
    fclose(FID);
    [OK,Reason] = io.files.verifyTextFile(File,'MustContain',Line);
    assert(~OK && strcmp(Reason,'empty'), 'empty not caught: %s', Reason);
    [OK,~] = io.files.verifyTextFile(File,'AllowEmpty',true);
    assert(OK, 'AllowEmpty must accept a zero-length file');

    % our own line lost
    FID = fopen(File,'w');
    fprintf(FID,'%s\n',Good{1:2});
    fclose(FID);
    [OK,Reason] = io.files.verifyTextFile(File,'MustContain',Line,'LinePattern',ISO);
    assert(~OK && contains(Reason,'absent'), 'missing line not caught: %s', Reason);

    % absent file, and the size guard
    delete(File);
    [OK,Reason] = io.files.verifyTextFile(File,'MustContain',Line);
    assert(~OK && strcmp(Reason,'does not exist'), 'absent file not caught: %s', Reason);
    FID = fopen(File,'w');
    fprintf(FID,'%s\n',Line);
    fclose(FID);
    [OK,Reason] = io.files.verifyTextFile(File,'MaxBytes',4);
    assert(~OK && contains(Reason,'MaxBytes'), 'MaxBytes not honoured: %s', Reason);

    rmdir(Dir,'s');
    io.msgStyle(LogLevel.Test, '@passed', 'verifyTextFile passed');
    Result = true;
end
