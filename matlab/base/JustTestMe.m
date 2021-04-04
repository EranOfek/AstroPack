
function Result = JustTestMe()

    msgLog("JustTestMe called");
    return true;

end


function testJustTestMe()
    msgLog("testJustTestMe called, now calling the function");
    
    Result = JustTestMe();
    disp(Result);    
end


function msgLog(varargin)
    %fprintf('Configuration: ');
    fprintf(varargin{:});
    fprintf('\n');
end

