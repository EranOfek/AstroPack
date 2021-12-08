function Result = subtractMeanColRow(Obj, Args)
    % Subtract the collapsed median of rows and columns from an AstroImage.
    %       This function may be useful in order to remove lines correlated
    %       noise from images.
    %       Using: imUtil.background.subtractMeanColRow
    % Input  : - An AstroImage object.
    %          * ...,key,val,...
    %            'SubOnlyRowColComp' - A logical indicating if to subtract
    %                   only the columns/rows correlated noise component
    %                   and to keep the image background (true), or to
    %                   remove the col/rows completely (false).
    %                   Default is true.
    %                   remove the background image, then subtract the
    %                   row/columns collapse median, and then return the
    %                   background image (if RetBack=true).
    %                   I.e., remove only the correlated
    %                   noise, but keep the background as it.
    %                   Default is [].
    %            'RetBack' - Return the background to the image.
    %                   Default is true.
    %            'SubMedRow' - Subtract median of rows. Default is true.
    %            'SubMedCol' - Subtract median of columns. Default is true.
    %            'MeanFun' - Function handle for mean. Second argument of
    %                   function must be the dimension index.
    %                   Default is @imUtil.background.rmeanCol.
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
        Args.SubOnlyRowColComp logical = true;
        Args.RetBack logical           = true;
        Args.SubMedRow logical         = true;
        Args.SubMedCol logical         = true;
        Args.MeanFun function_handle   = @median; %@imUtil.background.rmeanCol; %@median;
        Args.MeanFunArgs cell          = {};  %{'omitnan'};
        Args.DataProp                  = 'Image';
        Args.CreateNewObj logical      = false;
    end
    
    if Args.CreateNewObj
        Result = Obj.copy;
    else
        Result = Obj;
    end
    
    Nobj = numel(Obj);
    for Iobj=1:1:Nobj
        if Args.SubOnlyRowColComp
            Back = Result(Iobj).Back;
        else
            Back = [];
        end
        Result(Iobj).(Args.DataProp) = imUtil.background.subtractMeanColRow(Obj(Iobj).(Args.DataProp),...
                                                                                             'Back',Back,...
                                                                                             'RetBack',Args.RetBack,...
                                                                                             'SubMedRow',Args.SubMedRow,...
                                                                                             'SubMedCol',Args.SubMedCol,...
                                                                                             'MeanFun',Args.MeanFun,...
                                                                                             'MeanFunArgs',Args.MeanFunArgs);
    end
end