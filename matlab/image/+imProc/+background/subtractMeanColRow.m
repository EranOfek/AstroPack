function Result = subtractMeanColRow(Obj, Args)
    % Subtract the collapsed median of rows and columns from an AstroImage.
    %       This function may be useful in order to remove lines correlated
    %       noise from images.
    %       Using: imUtil.background.subtractMeanColRow
    % Input  : - An AstroImage object.
    %          * ...,key,val,...
    %            'SubMedRow' - Subtract median of rows. Default is true.
    %            'SubMedCol' - Subtract median of columns. Default is true.
    %            'MeanFun' - Function handle for mean. Second argument of
    %                   function must be the dimension index.
    %                   Default is @median.
    %            'MeanFunArgs' - A cell array of additional arguments to
    %                   pass to the mean function, after the dimension
    %                   argument. Default is {'omitnan'}.
    %            'DataProp' - Data property on which to operate.
    %                   Default is 'Image'.
    %            'CreateNewObj' - Copy the input object. Default is false.
    % Output : - An AstroImage from which the row/lines of the image itself
    %            was subtracted.
    %            The background and variance data properties are not
    %            updated or used.
    % Author : Eran Ofek (Dec 2021)
    % Example: AI = AstroImage({rand(1600,1600)});
    %          AI = imProc.background.subtractMeanColRow(AI)
    
    
    arguments
        Obj
        Args.SubMedRow logical        = true;
        Args.SubMedCol logical        = true;
        Args.MeanFun function_handle  = @median;
        Args.MeanFunArgs cell         = {'omitnan'};
        Args.DataProp                 = 'Image';
        Args.CreateNewObj logical     = false;
    end
    
    if Args.CreateNewObj
        Result = Obj.copy;
    else
        Result = Obj;
    end
    
    Nobj = numel(Obj);
    for Iobj=1:1:Nobj
        Result(Iobj).(Args.DataProp) = imUtil.background.subtractMeanColRow(Obj(Iobj).(Args.DataProp), 'SubMedRow',Args.SubMedRow,...
                                                                                             'SubMedCol',Args.SubMedCol,...
                                                                                             'MeanFun',Args.MeanFun,...
                                                                                             'MeanFunArgs',Args.MeanFunArgs);
    end
end