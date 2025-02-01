N_TESTS = 1001;        % number of time the test is repeated
N_SIZE = 565789;       % size of the input matrix X
N_FOUND = 123500;      % number of element of X that should be counted
xx = zeros(N_TESTS,1); % variable for polyfit: get the cumulative computation time from linear fit.
xx(:,1) = 1:N_TESTS;
RANGE = 10;            % range of number for the input X  
COND = '==';           % condition to test

VAL = 2*RANGE*rand() - RANGE;
% VAL = nan; % override the randomly generated test value with a particular value e.g. 0, nan, etc.
X =  2*RANGE*rand(N_SIZE,1) - RANGE;  % generate randomly the input X

%% modify X to make sure the number of hit count is exactly N_FOUND
IDX_HIT = fix(1:N_SIZE/N_FOUND:N_SIZE);
switch(COND)
    case '=='
        X(X==VAL) = X(X==VAL) + 1;
        X(IDX_HIT) = VAL;
    case '~='
        X(X==VAL) = X(X==VAL) + 1;
        X(~IDX_HIT) = VAL;        
    case '>'
        X(X>VAL) = X(X>VAL) - max(X(X>VAL));
        X(IDX_HIT) = VAL + 1;
    case '>='
        X(X>=VAL) = X(X>=VAL) - max(X(X>=VAL)) - 1;
        X(IDX_HIT) = VAL + 1;
    case '<'
        X(X<VAL) = X(X<VAL) + min(X(X<VAL)) + 1;
        X(IDX_HIT) = VAL - 1;
    case '<='
        X(X<=VAL) = X(X<=VAL) + min(X(X<=VAL)) + 1;
        X(IDX_HIT) = VAL - 1;
end

a = zeros(N_TESTS,1);
r = zeros(N_TESTS,1);

%% sum(X==VAL)
switch(COND)
    case '=='
        tic,
        for ix=1:N_TESTS
            r(ix) = sum(X==VAL);
            a(ix) = toc;
        end
    case '~='
        tic,
        for ix=1:N_TESTS
            r(ix) = sum(X~=VAL);
            a(ix) = toc;
        end
    case '>'
        tic,
        for ix=1:N_TESTS
            r(ix) = sum(X>VAL);
            a(ix) = toc;
        end
    case '>='
        tic,
        for ix=1:N_TESTS
            r(ix) = sum(X>=VAL);
            a(ix) = toc;
        end
    case '<'
        tic,
        for ix=1:N_TESTS
            r(ix) = sum(X<VAL);
            a(ix) = toc;
        end
    case '<='
        tic,
        for ix=1:N_TESTS
            r(ix) = sum(X<=VAL);
            a(ix) = toc;
        end
end
p=polyfit(xx,a,1);
if length(unique(r))== 1 && r(1) == N_FOUND
    N_HIT = 'PASS';
else
    N_HIT = 'FAIL';
end

MSG = sprintf('%-22s%-15s%-10s\n--------------------------------------------\n',' function','time (s)','# hit ?');
MSG = [MSG, sprintf('%-22s%-17.3f%-12s\n',sprintf(' sum(X%sVAL)',COND),N_TESTS*p(1),N_HIT)];

%% length(find(q==VAL))
switch(COND)
    case '=='
        tic,
        for ix=1:N_TESTS
            r(ix) = length(find(X==VAL));
            a(ix) = toc;
        end
    case '~='
        tic,
        for ix=1:N_TESTS
            r(ix) = length(find(X~=VAL));
            a(ix) = toc;
        end
    case '>'
        tic,
        for ix=1:N_TESTS
            r(ix) = length(find(X>VAL));
            a(ix) = toc;
        end
    case '>='
        tic,
        for ix=1:N_TESTS
            r(ix) = length(find(X>=VAL));
            a(ix) = toc;
        end
    case '<'
        tic,
        for ix=1:N_TESTS
            r(ix) = length(find(X<VAL));
            a(ix) = toc;
        end
    case '<='
        tic,
        for ix=1:N_TESTS
            r(ix) = length(find(X<=VAL));
            a(ix) = toc;
        end
end
p=polyfit(xx,a,1);
if length(unique(r))== 1 && r(1) == N_FOUND
    N_HIT = 'PASS';
else
    N_HIT = 'FAIL';
end

MSG = [MSG, sprintf('%-22s%-17.3f%-12s\n',sprintf(' length(find(X%sVAL))',COND),N_TESTS*p(1),N_HIT)];

%% mcount(X,VAL,COND)
tic,
for ix=1:N_TESTS
    r(ix) = mcount(X,VAL,COND);
    a(ix) = toc;
end
p = polyfit(xx,a,1);
if length(unique(r))== 1 && r(1) == N_FOUND
    N_HIT = 'PASS';
else
    N_HIT = 'FAIL';
end
MSG = [MSG, sprintf('%-22s%-17.3f%-12s\n',sprintf(' mcount(X,VAL,''%s'')',COND),N_TESTS*p(1),N_HIT)];
MSG = [sprintf('\n\n      Size of X : [%d %d]\n            VAL : %g\n    # iteration : %d\n # expected hit : %d\n\n',size(X),VAL, N_TESTS, N_FOUND),MSG];
disp(MSG)