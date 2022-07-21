function Result = nonlinearityCorrection(Obj, Correction, Args)
    % Apply a flux non-linearity correction to an AstroImage object.
    %   See also imUtil.calib.nonlinearityCorrection
    % Input  : - An AstroImage object with images.
    %          - A correction table [Flux, CorrectionFactor]
    %            or a structure with .Flux and .Corr fields.
    %            The correction factor is either multiplicative or by
    %            division (see 'Operator' argument).
    %          * ...,key,val,...
    %            'InterpMethod' - Default is 'linear'.
    %            'Operator' - function handle operator to apply on the correction factor.
    %                   Default is @rdivide (i.e., divide image by factor).
    %            'PropData' - AstroImage object data property on which to
    %                   apply the correction. Default is 'Image'.
    %            'CreateNewObj' - A logical indicating if to create a new
    %                   copy of the object. Default is false.
    % Output : - An AstroImage object with corrected images.
    % Author : Eran Ofek (Jul 2022)
    % Example: Corr = [0 1; 1000 1.1;20000 0.9; 70000 0.8]; 
    %          Res = imUtil.calib.nonlinearityCorrection(AI, Corr);
   
    arguments
        Obj AstroImage
        Correction   % [ADU, Corr]
        Args.InterpMethod           = 'linear';
        Args.Operator               = @rdivide;
        Args.PropData               = 'Image';
        Args.CreateNewObj logical   = false;
    end
    
    if Args.CreateNewObj
        Result = Obj.copy;
    else
        Result = Obj;
    end
    
    Nobj = numel(Obj);
    for Iobj=1:1:Nobj
        Result(Iobj).(Args.PropData) = imUtil.calib.nonlinearityCorrection(Result(Iobj).(Args.PropData), Correction, 'InterpMethod',Args.InterpMethod,...
                                                                                                                     'Operator',Args.Operator);
   
    end
        
end
