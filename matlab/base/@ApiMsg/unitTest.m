function Result = unitTest()
    % ApiMsg.unitTest
    
    io.msgLog(LogLevel.Test, 'ApiMsg test started');

    % Create instances
    A = ApiMsg;    
    A.api_src = 'gui.planner';
    A.api_dst = 'scheduler';
    A.api_msg_type = 'approve';
    A.api_msg = '{"trg_pk": 1}'; 
    A.post();
    
    S = A.pollResult();
    fprintf(S);
  
    io.msgStyle(LogLevel.Test, '@passed', 'ApiMsg test passed');                          
    Result = true;
end
