function Result=unitTest()
    % unitTest for tools.checksum
    %test_xxhash();
    
    Result = true;
end


function test_xxhash()
    % NOT GOOD - windos only
	% Test hashing a string
    data = 'a';
    xx = tools.checksum.xxhash('Data', data);
    fprintf('text: %s\n', xx);

	filename = 'README.md';
    xx = tools.checksum.xxhash('FileName', filename);
    fprintf('file: %s - %s\n', filename, xx);    
    
	filename = 'c:\\temp\\v08.mp4';
    xx = tools.checksum.xxhash('FileName', filename);
    fprintf('file: %s - %s\n', filename, xx);    
end

