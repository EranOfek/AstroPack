%
% https://www.mathworks.com/help/matlab/matlab_prog/use-a-matlab-timer-object.html
% https://www.mathworks.com/help/matlab/matlab_prog/timer-callback-functions.html
%

t = timer('TimerFcn', 'stat=false; disp(''Timer!'')',... 
                 'StartDelay',10);
start(t)

stat=true;
while(stat==true)
  disp('.')
  pause(1)
end

