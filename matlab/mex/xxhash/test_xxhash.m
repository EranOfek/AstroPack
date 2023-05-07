function test()

	testHashString();
	testHashFile();
end


function testHashString()
	% Test hashing a string
	data = 'hello world';
	expected = 'b5f4a5cc25822d41';
	actual = dec2hex(xxhash_mex(data, length(data)));
	testCase.verifyEqual(actual, expected);
end


function testHashFile()
	% Test hashing a file
	filename = 'testfile.bin';
	data = uint8([1 2 3 4 5]);
	fid = fopen(filename, 'wb');
	fwrite(fid, data, 'uint8');
	fclose(fid);
	expected = 'd9cf9a56b1a31b10';
	actual = dec2hex(xxhash_mex(filename, 0));
	delete(filename);
	testCase.verifyEqual(actual, expected);
end

