function Result = replaceVal(Obj, InVal, OutVal, Args)
    % Replace values in images stored in AstroImage with new values.
    %   Using: imUtil.image.replaceVal
    % Input  : - An AstroImage object.
    %          - Value to replace, or a [Min Max] range.
    %            If scalar, than search for this value and replace these
    %            pixels with the value in the OutVal argument.
    %            If [Min Max] than a range. In this case will search for
    %            values in the range (if 'UseOutRange'= false), or out of
    %            range (if 'UseOutRange'= true).
    %            Default is NaN.
    %          - Value by which to replace all the selected pixels.
    %            Default is 0.
    %          * ...,key,val,...
    %            'UseOutRange' - A logical. If false then will search for
    %                   values in range. If true, then will search for
    %                   values out of range.
    %                   Default is true (i.e., in range).
    %            'Prop' - A cell array of properties in the AstroImage on
    %                   which to operate the replaceVal operation.
    %                   Default is {'Image'}.
    %            'CreateNewObj' - A logical indicating if to create a new
    %                   AstroImage object (true), or to modify the input
    %                   object (false). Default is false.
    % Output : - An updated AstroImage object.
    % Author : Eran Ofek (May 2023)
    % Example: Image = AstroImage({rand(100,100).*10});
    %          Result = imProc.image.replaceVal(Image)
    %          Result = ImProc.image.replaceVal(Image,[1.5 2.5])
    %          Result = ImProc.image.replaceVal(Image,[1.5 2.5],0,'UseOutRange',true)

    arguments
        Obj AstroImage
        InVal      = NaN;
        OutVal     = 0;
        Args.UseOutRange logical     = false;
        Args.Prop                    = {'Image'};
        Args.CreateNewObj logical    = false;
    end

    if Args.CreateNewObj
        Result = Obj.copy;
    else
        Result = Obj;
    end

    if ischar(Args.Prop)
        Args.Prop = {Args.Prop};
    end
    Nprop = numel(Args.Prop);
    Nobj  = numel(Obj);

    for Iobj=1:1:Nobj
        for Iprop=1:1:Nprop
            Result(Iobj).(Args.Prop{Iprop}) = imUtil.image.replaceVal(Obj(Iobj).(Args.Prop{Iprop}), InVal, OutVal, 'UseOutRange',Args.UseOutRange);
        end
    end
    
end
