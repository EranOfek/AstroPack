function Result = interpOverNan(Obj, Args)
    % interpolate AstroImage over NaN values
    % Input  : - An AstroImage object.
    %          * ...,key,val,...
    %            'Method' - Interpolation method: 'inpaint_nans' | 'rowcol'
    %                   | 'convpix'
    %                   Default is 'convpix'.
    %            'MethodInpaint' - inpaint_nans method. Default is 0.
    %                   See inpaint_nans for options.
    %            'DataProp' - A cell array of data properties on which to operate the
    %                   interpolation. Default is {'Image'}.
    %            'MaskInterpolated' - A logical indicating if to mark
    %                   interpolated data in the Mask image.
    %                   Default is true.
    %            'Kernel' - Kernel for 'convpix' method.
    %                   Either a matrix or function handle that generates
    %                   the kernel. Default is @imUtil.kernel2.gauss.
    %            'KernelArgs' - A cell array of arguments to pass to the
    %                   Kernel function. Default is {}.
    %            'BitNameInterpolated' - BitName to mark interpolated data.
    %                   Default is 'Interpolated'.
    %            'CreateNewObj' - A logical indicating if to copy the input
    %                   object. Default is false.
    % Outout : - An AstroImage object with interpolation over NaNs
    % Author : Eran Ofek (Jul 2021)
    % Example: AI = AstroImage({ones(100,100)});
    %          AI.Image(50,50:51)=NaN;
    %          AI.Image(70,70) = NaN;
    %          imProc.image.interpOverNan(AI);
    %
    % Example with SINGLE for testing:
    %   addpath('D:\Ultrasat\AstroPack.git\matlab\external\Inpaint_nans')
    %   AI = AstroImage({rand(100,100)});  AI.cast('single');  AI.Image(10,10) = NaN;  BI = imProc.image.interpOverNan(AI)
    %
    
    arguments
        Obj
        Args.Method               = 'convpix'; %'inpaint_nans';  % 'inpaint_nans' | 'rowcol' | 'convpix'
        Args.MethodInpaint        = 0;
        Args.DataProp cell        = {'Image'};
        Args.MaskInterpolated logical = true;
        Args.Kernel               = @imUtil.kernel2.gauss;
        Args.KernelArgs cell      = {};
        Args.BitNameInterpolated  = 'Interpolated';
        Args.CreateNewObj logical = false;
    end
        
    if Args.CreateNewObj
        Result = Obj.copy();
    else
        Result = Obj;
    end
    
    % if isnumeric(Args.Kernel)
    %     KernelConv = Args.Kernel;
    % else
    %     KernelConv = Args.Kernel(Args.KernelArgs{:});
    % end
    % KernelConv = single(KernelConv);
    
    Nprop = numel(Args.DataProp); 
    Nobj  = numel(Obj);
    for Iobj=1:1:Nobj
        for Iprop=1:1:Nprop
            
            % Update the Mask image 'Interpolated' bit
            if Args.MaskInterpolated
                Flag = isnan(Obj(Iobj).(Args.DataProp{Iprop}));
                Result(Iobj).MaskData = maskSet(Obj(Iobj).MaskData, Flag, Args.BitNameInterpolated, 1);
            end
            
            switch lower(Args.Method)
                case 'convpix'
                    % use interpImageConvPix
                    Ndim    = ndims(Obj(Iobj).(Args.DataProp{Iprop}));
                    if Ndim>2
                        % N-D image
                        SizeIm = size(Obj(Iobj).(Args.DataProp{Iprop}));
                        
                        Nimages = prod(SizeIm(3:end)); % number of images in the dim>2 indices
                        for Iimages=1:1:Nimages
                            Result(Iobj).(Args.DataProp{Iprop})(:,:,Iimages) = imUtil.interp.interpImageConvPix(Obj(Iobj).(Args.DataProp{Iprop})(:,:,Iimages), [], [], 'Kernel',Args.Kernel, 'KernelArgs',Args.KernelArgs);
                        end
                    else
                        % 2D image
                        Result(Iobj).(Args.DataProp{Iprop}) = imUtil.interp.interpImageConvPix(Obj(Iobj).(Args.DataProp{Iprop}), [], [], 'Kernel',Args.Kernel, 'KernelArgs',Args.KernelArgs);
                        
                    end
                                        
                case 'rowcol'
                    % use the row/col linear method
                    Ndim    = ndims(Obj(Iobj).(Args.DataProp{Iprop}));
                    if Ndim>2
                        % N-D image
                        SizeIm = size(Obj(Iobj).(Args.DataProp{Iprop}));
                        
                        Nimages = prod(SizeIm(3:end)); % number of images in the dim>2 indices
                        for Iimages=1:1:Nimages
                            Result(Iobj).(Args.DataProp{Iprop})(:,:,Iimages) = imUtil.interp.interpImageRowCol(Obj(Iobj).(Args.DataProp{Iprop})(:,:,Iimages));
                        end
                    else
                        % 2D image
                        Result(Iobj).(Args.DataProp{Iprop}) = imUtil.interp.interpImageRowCol(Obj(Iobj).(Args.DataProp{Iprop}));
                        
                    end
                    
                    
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
                        if isa(Obj(Iobj).(Args.DataProp{Iprop}), 'single')
                            Result(Iobj).(Args.DataProp{Iprop}) = inpaint_nans_single(Obj(Iobj).(Args.DataProp{Iprop}), Args.MethodInpaint);
                        else
                            Result(Iobj).(Args.DataProp{Iprop}) = inpaint_nans(Obj(Iobj).(Args.DataProp{Iprop}), Args.MethodInpaint);
                        end
                    end
                otherwise
                    error('Unknown Method option');
            end
        end
    end
        
end
    
