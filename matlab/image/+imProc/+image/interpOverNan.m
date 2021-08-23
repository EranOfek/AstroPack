function Result = interpOverNan(Obj, Args)
    % interpolate AstroImage over NaN values
    % Input  : - An AstroImage object.
    %          * ...,key,val,...
    %            'Method' - Interpolation method.
    %                   Default is 'inpaint_nans'.
    %            'MethodInpaint' - inpaint_nans method. Default is 0.
    %                   See inpaint_nans for options.
    %            'DataProp' - A cell array of data properties on which to operate the
    %                   interpolation. Default is {'Image'}.
    %            'CreateNewObj' - Indicating if the output
    %                   is a new copy of the input (true), or an
    %                   handle of the input (false).
    %                   If empty (default), then this argument will
    %                   be set by the number of output args.
    %                   If 0, then false, otherwise true.
    %                   This means that IC.fun, will modify IC,
    %                   while IB=IC.fun will generate a new copy in
    %                   IB.
    % Outout : - An AstroImage object with interpolation over NaNs
    % Author : Eran Ofek (Jul 2021)
    % Example: AI = AstroImage({ones(100,100)});
    %          AI.Image(50,50:51)=NaN;
    %          AI.Image(70,70) = NaN;
    %          imProc.image.interpOverNan(AI);
    
    arguments
        Obj
        Args.Method         = 'inpaint_nans';
        Args.MethodInpaint  = 0;
        Args.DataProp cell  = {'Image'};
        Args.CreateNewObj   = [];
    end
    
    if isempty(Args.CreateNewObj)
        if nargout==0
            Args.CreateNewObj = false;
        else
            % create new obj
            Args.CreateNewObj = true;
        end
    end
    
    if Args.CreateNewObj
        Result = Obj.copyObject;
    else
        Result = Obj;
    end
    
    Nprop = numel(Args.DataProp); 
    Nobj  = numel(Obj);
    for Iobj=1:1:Nobj
        for Iprop=1:1:Nprop
            switch lower(Args.Method)
                case 'inpaint_nans'
                    % perform for each dimension beyond 2 (i.e., images in
                    % a cube)
                    Ndim    = ndims(Obj(Iobj).(Args.DataProp{Iprop}));
                    if Ndim>2
                        % N-D image
                        SizeIm = size(Obj(Iobj).(Args.DataProp{Iprop}));
                        
                        Nimages = prod(SizeIm(3:end)); % number of images in the dim>2 indices
                        for Iimages=1:1:Nimages
                            Result(Iobj).(Args.DataProp{Iprop})(:,:,Iimages) = inpaint_nans(Obj(Iobj).(Args.DataProp{Iprop})(:,:,Iimages), Args.MethodInpaint);
                        end
                    else
                        % 2D image
                        Result(Iobj).(Args.DataProp{Iprop}) = inpaint_nans(Obj(Iobj).(Args.DataProp{Iprop}), Args.MethodInpaint);
                    end
                otherwise
                    error('Unknown Method option');
            end
        end
    end
        
end
    
