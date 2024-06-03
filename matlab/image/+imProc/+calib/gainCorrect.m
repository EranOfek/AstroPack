function Result = gainCorrect(Obj, Gain, Args)
    % Divide image by gain and update header.
    % Input  : - An AstroImage object.
    %          - A char array of Gain header keyword name, or a numeric
    %            array of gain values (scalar, or one per image).
    %            Units are electrons/ADU.
    %            Default is 'GAIN';
    %          * ...,key,val,...
    %            'CreateNewObj' - Default is false.
    %            'getValArgs' - A cell array of additional arguments to
    %                   pass to AstroHeader/getVal. Default is {}.
    %            'DataProp' - AstroImage data properties which to divide by
    %                   gain. Default is {'Image','Var','Back'}.
    %            'replaceValArgs' - A cell array of additional arguments to
    %                   pass to AstroHeader/replaceVal. Default is {}.
    %            'OrigGainKey' - An header keyword name in which to write the
    %                   original gain. Default is 'ORIGGAIN'
    %            'DefaultGain' - The default gain in case can't find gain
    %                   in header. Default is 1.
    % Output : - An AstroImage object with gain=1 and updated header.
    %            NOTE: The catalog is not modified.
    %            The DataType field of the AstroImage is modified to
    %            Electrons
    % Author : Eran Ofek (Jul 2021)
    % Example: AI = AstroImage({rand(10,10)});
    %          imProc.calib.gainCorrect(AI)
    
    arguments
        Obj
        Gain                      = 'GAIN';  % keyword, scalar, vector [e/ADU]
        Args.CreateNewObj logical = false;
        Args.getValArgs cell      = {};
        Args.DataProp             = {'Image','Var','Back'};
        Args.replaceValArgs cell  = {};
        Args.OrigGainKey          = 'ORIGGAIN';
        Args.DefaultGain          = 1;
    end
    DefGainKey = 'GAIN';
    
    %[Result, Args.CreateNewObj] = createNewObj(Obj, Args.CreateNewObj, nargout);
    if Args.CreateNewObj
        Result = Obj.copy;
    else
        Result = Obj;
    end
    
    Ngain = numel(Gain);
    Nprop = numel(Args.DataProp);
    
    Nobj = numel(Obj);
    for Iobj=1:1:Nobj
        % get GAIN
        if isnumeric(Gain)
            Igain   = min(Iobj, Ngain);
            GainVal = Gain(Igain);
        else
            GainVal = getVal(Obj(Iobj).HeaderData, Gain, Args.getValArgs{:});
        end
        if isnan(GainVal)
            GainVal = Args.DefaultGain; 
        else
            GainVal = GainVal;
        end
        
        % divide image by gain
        for Iprop=1:1:Nprop
            Result(Iobj).(Args.DataProp{Iprop}) = Result(Iobj).(Args.DataProp{Iprop}).*GainVal;
        end
        
        % update header keywords
        if ~isnumeric(Gain)
            Result(Iobj).HeaderData = replaceVal(Result(Iobj).HeaderData, Gain, 1, Args.replaceValArgs{:});
        else
            Result(Iobj).HeaderData = replaceVal(Result(Iobj).HeaderData, DefGainKey, 1, Args.replaceValArgs{:});
        end
        
        % write old GAIN value
        Result(Iobj).HeaderData = replaceVal(Result(Iobj).HeaderData, Args.OrigGainKey, GainVal, Args.replaceValArgs{:});
               
        % Set the DataType of the AstroImage to electrons
        Result(Iobj).DataType = AstroDataType.Electrons;
    end
    
end