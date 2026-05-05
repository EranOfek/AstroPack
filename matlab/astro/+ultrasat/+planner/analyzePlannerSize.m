function analyzePlannerSize(planner, Args)
    % =========================================================================
    % Analyze memory usage of uplanner object and print large fields only
    %
    % Usage:
    %   ultrasat.utils.analyzePlannerSize(planner)
    %
    % Focus:
    %   - large top-level fields
    %   - UniqTarg columns
    %   - Vis, FieldObjects, etc.
    % =========================================================================
    
    arguments
        planner
        Args.MinMB (1,1) double = 1.0   % threshold to print
    end
    
    fprintf('\n=== Planner Size Analysis ===\n');
    
    % -------- Helper --------
        function mb = getSizeMB(x)
            try
                mb = numel(getByteStreamFromArray(x)) / 1e6;
            catch
                mb = NaN;
            end
        end
    
        function printLine(name, mb, info)
            if mb >= Args.MinMB
                fprintf('%-30s %8.2f MB   %s\n', name, mb, info);
            end
        end
    
    % -------- Top-level properties --------
    
    fprintf('\n--- Top-level fields ---\n');
    
    props = properties(planner);
    
    for i = 1:numel(props)
        name = props{i};
    
        try
            val = planner.(name);
            mb = getSizeMB(val);
    
            if isnumeric(val) || islogical(val)
                info = sprintf('[%s]', mat2str(size(val)));
            elseif istable(val)
                info = sprintf('[%d x %d table]', height(val), width(val));
            elseif isstruct(val)
                info = sprintf('[struct]');
            elseif iscell(val)
                info = sprintf('[%s cell]', mat2str(size(val)));
            else
                info = class(val);
            end
    
            printLine(name, mb, info);
    
        catch
            % ignore inaccessible fields
        end
    end
    
    % -------- UniqTarg breakdown --------
    
    if ~isempty(planner.UniqTarg)
        fprintf('\n--- UniqTarg columns ---\n');
    
        T = planner.UniqTarg;
        vars = T.Properties.VariableNames;
    
        for i = 1:numel(vars)
            name = vars{i};
    
            try
                v = T.(name);
                mb = getSizeMB(v);
    
                if isnumeric(v) || islogical(v)
                    info = sprintf('[%s]', mat2str(size(v)));
                elseif iscell(v)
                    info = sprintf('[%s cell]', mat2str(size(v)));
                elseif isstring(v) || iscategorical(v)
                    info = sprintf('[%d elements]', numel(v));
                else
                    info = class(v);
                end
    
                printLine(['UniqTarg.' name], mb, info);
    
            catch
            end
        end
    end
    
    % -------- Deep dive for known heavy fields --------
    
    % --- FieldObj ---
    if ismember('FieldObj', planner.UniqTarg.Properties.VariableNames)
        try
            v = planner.UniqTarg.FieldObj{1};
            fprintf('\n--- FieldObj internal (sample row) ---\n');
    
            if isstruct(v)
                fn = fieldnames(v);
                for i = 1:numel(fn)
                    val = v.(fn{i});
                    mb = getSizeMB(val);
    
                    if isnumeric(val) || islogical(val)
                        info = sprintf('[%s]', mat2str(size(val)));
                    elseif iscell(val)
                        info = sprintf('[%s cell]', mat2str(size(val)));
                    else
                        info = class(val);
                    end
    
                    printLine(['FieldObj.' fn{i}], mb, info);
                end
            end
        catch
        end
    end
    
    % --- Vis ---
    if ~isempty(planner.Vis)
        fprintf('\n--- Vis breakdown ---\n');
    
        try
            v = planner.Vis;
            fn = fieldnames(v);
    
            for i = 1:numel(fn)
                val = v.(fn{i});
                mb = getSizeMB(val);
    
                if isnumeric(val) || islogical(val)
                    info = sprintf('[%s]', mat2str(size(val)));
                else
                    info = class(val);
                end
    
                printLine(['Vis.' fn{i}], mb, info);
            end
        catch
        end
    end
    
    fprintf('\n=== End Analysis ===\n\n');

end
