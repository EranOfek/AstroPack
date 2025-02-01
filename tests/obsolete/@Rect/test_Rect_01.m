function Result = unitTest()

	io.msgLog(LogLevel.Test, 'Rect test started');
	
	A = Rect(0, 0, 10, 20);
	B = Rect(0, 0, 10, 20);
	C = Rect([0, 9, 0, 19]);
	
	assert(A.Right == A.X + A.Width-1);
	assert(A.Right == A.X + A.Width-1);          
	
	assert(A.Bottom == A.Y + A.Height-1);            
	assert(A.Right == B.Right);
	assert(A.Right == B.Right);
	
	assert(C.X == C.Left);
	assert(C.Y == C.Top);                        
	assert(C.Right ~= 0);
	assert(C.Bottom ~= 0);
	
	assert(A.Right == C.Right);
	assert(A.Bottom == C.Bottom);
			   
	assert(all(A.CCDSEC == B.CCDSEC));
	assert(all(A.CCDSEC == C.CCDSEC));

	% Copy rect, since it is a non-handle class, modifing the data
	% should result in different instances
	D = C;
	D.Width = D.Width + 1;
	D.Height = D.Height + 1;
	assert(D.Width ~= C.Width);
	assert(D.Height ~= C.Height);
	assert(D.Right ~= C.Right);
	assert(D.Bottom ~= C.Bottom);
	
	io.msgLog(LogLevel.Test, 'Rect test passed');            
	Result = true;
	
end
