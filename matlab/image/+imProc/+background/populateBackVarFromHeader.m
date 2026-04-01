function [Result] = populateBackVarFromHeader(Obj, NewObj, Args)
    % Populate (or replace) the back/var in AstroImage from its header. 
    % Input  : - (Obj) An AstroImage/AstroDiff/AstroZOGY object.
    %          - (NewObj) A new AstroImage/AstroDiff/AstroZOGY object (same size as
    %            the first input argument).
    %            If empty, then the back/var from Obj header will be added
    %            to the Back/Var properties of Obj.
    %            If given, then the back/var from Obj header will be added
    %            to the Back/Var properties of NewObj.
    %          * ...,key,val,... 
    %            'KeyBack' - Header keyword name from which to get the background.
    %                   Default is 'MEDBCK'.
    %            'KeyVar' - Header keyword name from which to get the variance.
    %                   Default is 'MEDVAR'.
    %            'CreateNewObj' - A logical indicating if the result is a
    %                   new copy of Obj or NewObj. Default is false.
    % Output : - The input object with the Back/Var info from the header
    %            populated in the Back/Var properties.
    % Author : Eran Ofek (2026 Mar) 
    % Example: [Result] = imProc.background.populateBackVarFromHeader(Obj, NewObj);

    arguments
        Obj
        NewObj                 = [];
        Args.KeyBack                          = 'MEDBCK'; % 
        Args.KeyVar                           = 'MEDVAR'; %
      
        Args.CreateNewObj      = false;
    end


    if isempty(NewObj)
        if Args.CreateNewObj
            Result = Obj.copy;
        else
            Result = Obj;
        end
    else
        if Args.CreateNewObj
            Result = NewObj.copy;
        else
            Result = NewObj;
        end
    end

    % Get Back/Var from header:
    StBackVar = Obj.getStructKey({Args.KeyBack, Args.KeyVar});
    BackVec   = [StBackVar.(Args.KeyBack)].';
    VarVec    = [StBackVar.(Args.KeyVar)].';
    
    Nobj = numel(Obj);
    for Iobj=1:1:Nobj
        Result(Iobj).BackData.Data = BackVec(Iobj);
        Result(Iobj).VarData.Data  = VarVec(Iobj);
    end


end
