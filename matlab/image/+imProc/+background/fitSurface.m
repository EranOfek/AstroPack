function [Result, ResPar] = fitSurface(Obj, Args)
    % Fit a 2-D surafce to an image in an AstroImage object, and store it
    % in the Back property.
    %   Linearly fit a 2-D surface of some polynomial shape to a matrix.
    % Input  : - A 2-D matrix.
    %          * ...,key,val,...
    %            'Fun' - A cell array of functions that will be used to
    %                   define the design matrix, or a char array
    %                   indicating some predefined functionals.
    %                   Default is 'poly1' which will generate:
    %                   {@(x,y)ones(size(x)), @(x,y)x, @(x,y)y, @(x,y)x.*y};
    %            'StepXY' - Step size in X and Y in which the 2D image will
    %                   be sampled, and only the sampled pixels will be fitted.
    %                   This is mainly used for speeding up the function.
    %                   Default is 5.
    %            'Norm' - A logical indicating if to normalize the X and Y
    %                   values prior to the fit.
    %                   Default is true.
    %            'Niter' - Number of sigma clipping iterations.
    %                   Default is 3;
    %            'SigmaClip' - Sigma clipping, in units of the rms.
    %                   Default is 5.
    %            'CreateNewObj' - A logical indicating if to generate a new
    %                   copy of the input object.
    %                   Default is false.
    %            'DataProp' - AstroImage data property containing the data to fit.
    %                   Default is 'Image'.
    %            'BackProp' - AstroImage background property in which to
    %                   store the fitted surface.
    %                   Default is 'Back'.
    % Author : Eran Ofek (Jun 2023)
    % Example: [MatX, MatY] = meshgrid( (1:1:1700), (1:1:1700) );
    %          II = [2 + 0.001.*MatX - 0.002.*MatY + 1e-4.*MatX.*MatY];
    %          Z = AstroImage({II});
    %          [Z,P]=imProc.background.fitSurface(Z);

    arguments
        Obj                                 % AstroImage or ImageComponent
        Args.DataProp       = 'Image';
        Args.BackProp       = 'Back';
        Args.Fun            = 'poly1';
        Args.StepXY         = 5;
        Args.Norm logical   = true;
        Args.Niter          = 3;
        Args.SigmaClip      = 5;
        Args.CreateNewObj logical  = false;
        
    end
   
    if Args.CreateNewObj
        Result = Obj.copy;
    else
        Result = Obj;
    end
    
    Nobj = numel(Obj);
  
    
    for Iobj=1:1:Nobj
        % for each image
        
        [ResPar(Iobj), Result(Iobj).(Args.BackProp)] = imUtil.background.fitSurface(Result(Iobj).(Args.DataProp), 'Fun',Args.Fun,...
                                                                                          'StepXY',Args.StepXY,...
                                                                                          'Norm',Args.Norm,...
                                                                                          'Niter',Args.Niter,...
                                                                                          'SigmaClip',Args.SigmaClip);
        
    end
end