function Result = isempty_property(Obj, PropertyName)
    % Check if a certain object property is empty in a matrix of objects
    % Input:  - a matrix of objects
    %         - the name of the property of interest
    % Output: - a vector of logical whose length is equal to the length of
    %           the input object
    % Author: A.M. Krassilchtchikov (Jan 2024)
    % Example: AI(1:2) = AstroImage; 
    %          tools.class.isempty_property(AI,'Image')
    Result = false(length(Obj),1);    
    for Iobj = 1:length(Obj)
        if isempty(Obj(Iobj).(PropertyName))
            Result(Iobj) = true;            
        end
    end
end