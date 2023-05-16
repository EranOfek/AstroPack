function test_xxhash()

	% Test hashing a string
    data = 'a';
    xx = xxhash('Data', data);
    fprintf('text: %u\n', xx);

	filename = 'README.md';
    xx = xxhash('FileName', filename);
    fprintf('file: %s - %u\n', filename, xx);    
    
	filename = 'c:\\temp\\v08.mp4';
    xx = xxhash('FileName', filename);
    fprintf('file: %s - %u\n', filename, xx);    
end

