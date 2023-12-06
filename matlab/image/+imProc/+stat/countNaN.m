function Result = countNaN(Obj, DataProp, Args)
    % Count the number of NaNs (or not NaNs) in each image
    % Input  : - An AstroImage object.
    %          - Data property in which to count. Default is 'Image'.
    %          * ...,key,val,...
    %            'NotNaN' - A logical indicating if to count NaN (false),
    %                   or not NaN (true). Default is false (count NaNs).
    % Output : - An array of the number of NaNs (or not NaNs) for each
    %            image.
    % Author : Eran Ofek (Dec 2022)
    % Example: imProc.stat.countNaN(AI)
   
    arguments
        Obj
        DataProp             = 'Image';
        Args.NotNaN logical  = false;
    end
    
    Nobj   = numel(Obj);
    Result = nan(size(Obj));
    for Iobj=1:1:Nobj
        if Args.NotNaN
            % count not NaNs
            Result(Iobj) = sum(~isnan(Obj(Iobj).(DataProp)(:)));
        else
            % count NaNs
            Result(Iobj) = sum(isnan(Obj(Iobj).(DataProp)(:)));
        end
    end
    
end
