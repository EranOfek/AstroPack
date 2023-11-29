% Package Unit-Test
%
% ### Requirements:
%
%
%


function Result = unitTest()
    % Package Unit-Test   
	%io.msgStyle(LogLevel.Test, '@start', 'test started');
    
    func_unitTest();
    
    % sub2ind_fast
    Ind=imUtil.image.sub2ind_fast([3 3],2,2);
    if Ind~=5
        error('sub2ind_fast failed');
    end
 
    % moment2
    Image = rand(1024,1024); X=rand(1e4,1).*1023+1; Y=rand(1e4,1).*1023+1;
    [M1,M2,Aper]=imUtil.image.moment2(Image,X,Y);
    
    Matrix = imUtil.kernel2.gauss(2, [31 31]);
    [M1,M2,Aper]=imUtil.image.moment2(Matrix,16,16)
    if ~(abs(M1.X-16)<0.01 & abs(M1.Y-16)<0.01)
        error('moment2 failed');
    end
    
	%io.msgStyle(LogLevel.Test, '@passed', 'test passed');
	Result = true;
end

%--------------------------------------------------------------------------


function Result = func_unitTest()
	% Function Unit-Test
	%io.msgStyle(LogLevel.Test, '@start', 'test started');
   
	%io.msgStyle(LogLevel.Test, '@passed', 'passed');
	Result = true;
end


%--------------------------------------------------------------------------

