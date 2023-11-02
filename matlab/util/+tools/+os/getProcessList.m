function procList = getProcessList(taskname)
%Get the process list (pid and name).
%
%   procList = getProcess() get the full process list.
%   procList = getProcess('taskname') search (case insensitive)
%   for a specific process name.
%
%   Copyright 2015 Gabriele Piantadosi

    procList={};
    
    if(ispc)
        if(nargin==0)
            [response, tasks_s] = system('tasklist');
        else
            [response, tasks_s] = system(sprintf('tasklist | find "%s" /I', taskname));
        end
        if(response==0)
            tasks = regexp(tasks_s, '\n*', 'split');
            p=0;
            for i=1:numel(tasks)
                splits = regexp(strtrim(tasks{i}), ' *', 'split');
                if(numel(splits)>2 && isfinite(str2double(splits{2})))
                    p=p+1;
                    procList{p,1} = str2double(splits{2});
                    procList{p,2} = splits{1};
                end
            end
        end
    elseif(ismac || isunix)
        if(nargin==0)
            [response, tasks_s] = system('ps -Ac');
        else
            [response, tasks_s] = system(sprintf('ps -Ac | grep "%s" -i', taskname));
        end
        if(response==0)
            tasks = regexp(tasks_s, '\n*', 'split');
            p=0;
            for i=1:numel(tasks)
                splits = regexp(strtrim(tasks{i}), ' *', 'split');
                if(numel(splits)>2 && isfinite(str2double(splits{1})))
                    p=p+1;
                    procList{p,1} = str2double(splits{1});
                    procList{p,2} = splits{4};
                end
            end
        end
    else
        
    end
end

