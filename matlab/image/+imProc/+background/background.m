function Result = background(Obj, Args)
    % Calculate background and variance of an AstroImage object.
    % Input  : - An AstroImage object multi elements supported).
    %          * ...,key,val,...
    %            'BackFun' - A function handle for the background (and
    %                   optionally variance) estimation.
    %                   The function is of the form:
    %                   [Back,[Var]]=Fun(Matrix,additional parameters,...),
    %                   where the output Variance is optional.
    %                   The additional parameters are provided by the
    %                   'BackFunPar' keyword (see next keyword).
    %                   Default is @median [other example:
    %                   @imUtil.background.mode]
    %            'BackFunPar' - A cell array of additional parameters to pass
    %                   to the BackFun function.
    %                   Default is {[1 2]} [other example: {true,true,0.1}]
    %            'VarFun' - A function handle for the background estimation.
    %                   The function is of the form:
    %                   [Var]=Fun(Matrix,additional parameters,...).
    %                   The additional parameters are provided by the
    %                   'VarFunPar' keyword (see next keyword).
    %                   If NaN, then will not calculate the variance.
    %                   If empty, then will assume the variance is returned as
    %                   the second output argument of 'BackFun'.
    %                   If a string then will copy Back value into the Var.
    %                   Default is empty (i.e., @imUtil.background.rvar returns
    %                   the robust variance as the second output argument).
    %            'VarFunPar' - A cell array of additional parameters to pass
    %                   to the VarFun function.
    %                   Default is {}.
    %            'SubSizeXY' - The [X,Y] size of the partitioned sub images.
    %                   If 'full' or empty, use full image.
    %                   Default is [128 128].
    %            'Overlap' - The [X,Y] additional overlaping buffer between
    %                   sub images to add to each sub image.
    %                   Default is 16.
    %            'DiluteStep' - Dilution for background calculation. This is
    %                   the step size in which the data in each sub image is selected.
    %                   Default is 1 (no dilution).
    %            'ExtendFull' - A logical indicating if to extend the
    %                   background map into a full-size image. Default is true.
    %            'EstimateRowColNoise' - A logical indicating if to
    %                   estimate the rows/columns correlated noise using 
    %                   imUtil.background.backgroundMeanColRow and to
    %                   update the background image accordingly.
    %                   Default is false.
    %            'backgroundMeanColRowArgs' - A cell array of addidtional
    %                   arguments to pass to imUtil.background.backgroundMeanColRow
    %                   Default is {}.
    %
    %            'SubBack' - A logical indicating if to subtract the
    %                   background from the image. Default is false.
    %            'KeepScaled' - A logical indicating if to rescale the
    %                   background and variance images to the full size.
    %                   I.e., this corresponds to how the images are stored
    %                   in the ImageComponent class.
    %                   Default is false.
    %            'ReCalcBack' - A logical indicating if to recalculate the
    %                   background even if exist. Default is true.
    %            'CreateNewObj' - Indicating if the output
    %                   is a new copy of the input (true), or an
    %                   handle of the input (false).
    %                   Default is false.
    %            'AddHeaderInfo' - A logical indicating if to add header
    %                   keywords with background and variance statistics.
    %                   Including the following keys: {'MEANBCK','MEDBCK','STDBCK','MEANVAR','MEDVAR'};
    %                   Default is true.
    %            There are some additional hidden arguments.
    % Example: AI = AstroImage({rand(1024,1024)});
    %          Result = imProc.background.background(AI);
   
    arguments
        Obj AstroImage
        
        Args.BackFun                     = @imUtil.background.mode; %@median;
        Args.BackFunPar cell             = {true}; %{[1 2]};
        Args.VarFun                      = []; %@imUtil.background.rvar; % [];
        Args.VarFunPar cell              = {};
        Args.SubSizeXY                   = [128 128];
        Args.Overlap                     = 16;
        Args.DiluteStep                  = 1;
        Args.ExtendFull(1,1) logical     = true; %false;
        Args.EstimateRowColNoise logical = false;
        Args.backgroundMeanColRowArgs cell = {};
        
        Args.SubBack(1,1) logical        = false;
        
        Args.KeepScaled(1,1) logical     = false;
        Args.ReCalcBack(1,1) logical     = true;
        Args.CreateNewObj logical        = false;
        
        % header
        Args.AddHeaderInfo(1,1) logical  = true;
        Args.UseFastMedian logical       = true;
        
        % hidden parameters
        Args.ImageProp char              = 'ImageData';
        Args.ImagePropIn char            = 'Image';
        Args.BackProp char               = 'BackData';
        Args.BackPropIn char             = 'Data';
        Args.VarProp char                = 'VarData';
        Args.VarPropIn char              = 'Data';
    end
    
    if Args.CreateNewObj
        Result = Obj.copy();
    else
        Result = Obj;
    end
    
    % pre defined header key names to add
    Keys = {'MEANBCK','MEDBCK','STDBCK','MEANVAR','MEDVAR'};
    
    Nobj = numel(Obj);
    for Iobj=1:1:Nobj
        if isempty(Obj(Iobj).BackData.Data) 
            % no background yet
            ReCalcBack = true;
        else
            ReCalcBack = Args.ReCalcBack;
        end
            
        if ReCalcBack
            [Result(Iobj).(Args.BackProp).(Args.BackPropIn), Result(Iobj).(Args.VarProp).(Args.VarPropIn)] = ...
                            imUtil.background.background(Obj(Iobj).(Args.ImageProp).(Args.ImagePropIn),...
                                                            'BackFun',Args.BackFun,...
                                                            'BackFunPar',Args.BackFunPar,...
                                                            'VarFun',Args.VarFun,...
                                                            'VarFunPar',Args.VarFunPar,...
                                                            'SubSizeXY',Args.SubSizeXY,...
                                                            'Overlap',Args.Overlap,...
                                                            'DiluteStep',Args.DiluteStep,...
                                                            'ExtendFull',Args.ExtendFull);
                        
            % remove lines/row correlated noise
            if Args.EstimateRowColNoise
                Result(Iobj).(Args.BackProp).(Args.BackPropIn) = imUtil.background.backgroundMeanColRow(Obj(Iobj).(Args.ImageProp).(Args.ImagePropIn),...
                                                                             Result(Iobj).(Args.BackProp).(Args.BackPropIn),...
                                                                             Args.backgroundMeanColRowArgs{:});
            end
            
            % set the scale parameter
            SizeImage = size(Result(Iobj).(Args.ImageProp).(Args.ImagePropIn));
            SizeBack  = size(Result(Iobj).(Args.BackProp).(Args.BackPropIn));
            %SizeVar   = size(Result(Iobj).(Args.VarProp).(Args.VarPropIn));
            Scale = SizeImage./SizeBack;
            if Scale(1)==Scale(2)
                Result(Iobj).(Args.BackProp).Scale = Scale(1);
                Result(Iobj).(Args.VarProp).Scale  = Scale(1);
            else
                error('Scale must be the same in both axes');
            end
            
            if Args.KeepScaled
                Result(Iobj).BackData.imresize('UpdateObj',true);
                Result(Iobj).VarData.imresize('UpdateObj',true);
            end
            
            if Args.SubBack
                % subtract background
                subtractBack(Result(Iobj).(Args.ImageProp), Result(Iobj).(Args.BackProp).Image );
                
            end
            
            % Add info to header
            if Args.AddHeaderInfo
                %
                MeanBack = mean( Result(Iobj).(Args.BackProp).(Args.BackPropIn), 'all');
                
                StdBack  = std( Result(Iobj).(Args.BackProp).(Args.BackPropIn), [],'all');
                MeanVar = mean( Result(Iobj).(Args.VarProp).(Args.VarPropIn), 'all');
                
                if Args.UseFastMedian
                    MedBack  = fast_median( Result(Iobj).(Args.BackProp).(Args.BackPropIn)(:));
                    MedVar  = fast_median( Result(Iobj).(Args.VarProp).(Args.VarPropIn)(:));
                else
                    MedBack  = median( Result(Iobj).(Args.BackProp).(Args.BackPropIn), 'all');
                    MedVar  = median( Result(Iobj).(Args.VarProp).(Args.VarPropIn), 'all');
                end
                
                %Keys = {'MEANBCK','MEDBCK','STDBCK','MEANVAR','MEDVAR'};
                Vals  = {MeanBack, MedBack, StdBack, MeanVar, MedVar};
                Result(Iobj).HeaderData.replaceVal(Keys,Vals);
                
            end
        end
    end
    
    
end