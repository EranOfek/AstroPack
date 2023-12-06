function Result = functionalResponse(ImObj, Args)
    % Fit the pixel response to light as a function of intensity in a cube of images
    % Description: Given an AstroImage of flat field or dark images which have different mean
    %              intensity, fit some linear models to each model intensity
    %              vs. a user specified expected intensity or the mean value of
    %              the image.
    %              This function can be used to fit the flat/dark image in each
    %              pixel, to fit non-linaeity and to look for pixels with
    %              anomalous response.
    %              The function by default fit a null hypothesis model of the
    %              form intensity=alpha*MeanIntensity (where alpha is a fitted
    %              free parameter).
    %              In addition the function fit additional user specified
    %              models (e.g., offset + linear term, or quadratic model).
    %              The Delta Chi^2 between these models and the null hypothesis
    %              is calculated.
    % Input  : - An AStroImage object.
    %          * Pairs of ...,key,val,... arguments. Options are:
    %            'CCDSEC' - CCDSEC on which to operate:
    %                   [Xmin, Xmax, Ymin, Ymax].
    %                   Use [] for the entire image.
    %                   If not [], then DataPropIn/Out will be
    %                   modified to 'Image'.
    %            'DataPropIn' - The data property that contains the
    %                   the data in the ImageComponent.
    %                   Default is 'Data'.
    %            'DataProp' - Data propery to fit. Default is
    %                   'ImageData'.
    %            'Gain' - Gain. The cube will be multiplied by these factor.
    %                   This is needed beacuse the function assume the images
    %                   noise is Poisson. Default is 1.
    %            'ReadNoise' - Readnoise in electrons, used for the \chi^2
    %                   calculation. Default is 5.
    %            'MeanFun' - If 'Intensity' is not porovided, this is a function
    %                   handle that will operate on each image in the cube in
    %                   order to calculate its mean value.
    %                   Default is @median.
    %            'MeanFunPar' - A cella array of additional parameters to pass
    %                   to the 'MeanFun' function handle.
    %                   Default is {[1, 2]'omitnan'}.
    %            'Intensity' - A vector if intensities for each image in the
    %                   cube. This can be the mean intensity of each image
    %                   (after gain correction), or exposure time (if flat is
    %                   based on constant illumination surface).
    %                   If empty, then will estimate the intensity using the
    %                   'MeanFun' option.
    %                   Default is empty.
    %            'Model' - A cell array of additional models to fit.
    %                   Each element in the cell array is a string that
    %                   corresponds to one of the following models:
    %                   'c+x' : - bias + alpha*Intensity model
    %                   'c+x+X^2' - bias + alpha*I + beta.*I.^2 model
    %                   'x+x^2' - alpha*I + beta.*I.^2 model
    %                   Default is {'c+x','c+x+x^2','x+x^2'}.
    % Output : - A structure of the fit results. The following fields are
    %            available:
    %            .H0 (data for the null hypothesis fit.
    %               A structure with the following fields:
    %               .Model - Model name - i.e., 'x'
    %               .Par.Par - A matrix of the fitted parameters (response
    %                       image  - i.e., flat field image).
    %               .Chi2 - A matrix of \chi^2 per pixel.
    %               .Npar - The number of free parameters.
    %               .Ndof - The number of degrees of freedom (Nobs-Npar).
    %               .ProbChi2 - 1 - The cumulative probability of the chi2,dof.
    %            .H1 (data for the alternative hypothsis fits).
    %               This is a structure array with element per alternative
    %               model:
    %               .Model - Model name - e.g., 'c+x'
    %               .Par(i).Par - A matrix of the fitted parameters (response
    %                       image  - i.e., flat field image).
    %                       where i is the free parameter index.
    %                       For example, in the 'c+x' model i=1 is for the bias
    %                       level and i=2 is for the slope (response).
    %               .Chi2 - A matrix of \chi^2 per pixel.
    %               .Npar - The number of free parameters.
    %               .Ndof - The number of degrees of freedom (Nobs-Npar).
    %               .ProbDeltaChi2 - 1 - The cumulative probability of the
    %                       Delta \chi^2 between H1 and H0 where Npar-1 is the
    %                       number of degrees of freedom.
    %                       small or 0 where the model is prefered over H0.
    % Author : Eran Ofek (Apr 2021)
    % Example: AI = AstroImage({ones(3,3), 2.*ones(3,3), 10.*ones(3,3), 11.*ones(3,3), 13.*ones(3,3)});
    %          C  = imProc.image.Stack;
    %          Result = C.functionalResponse(AI);
    %          Result = C.functionalResponse(AI, 'Intensity',[1 2 10 11 13])


    arguments
        ImObj                              = [];
        Args.CCDSEC                        = [];
        Args.DataProp char                 = 'ImageData';
        Args.DataPropIn char               = 'Data';
        Args.Gain                          = 1;   % if char array then this is a header keyword name
        Args.ReadNoise                     = 5;   % if char array then this is a header keyword name
        Args.MeanFun function_handle       = @median
        Args.MeanFunPar cell               = {[1 2],'omitnan'};
        Args.Intensity                     = [];  % if char array then this is a header keyword name (e.g., 'EXPTIME')
        Args.Model cell                    = {'c+x','c+x+x^2','x+x^2'};
    end
    DimIndex = 3;

    % allocate output
    Result = AstroImage;

    Nim = numel(ImObj);

    % create a cube for each dataset
    [Cube] = imProc.image.images2cube(ImObj, 'CCDSEC',Args.CCDSEC, 'DimIndex',DimIndex, 'DataProp',Args.DataProp, 'DataPropIn',Args.DataPropIn);

    % obtain Gain from header
    if ischar(Args.Gain) || iscellstr(Args.Gain)
        Args.Gain = funHeader(ImObj, @getVal, Args.Gain);
    end

    % obtain readnoise from header
    if ischar(Args.ReadNoise) || iscellstr(Args.ReadNoise)
        Args.ReadNoise = funHeader(ImObj, @getVal, Args.ReadNoise);
    end

    % obtain Intensity from header (e.g., EXPTIME)
    if ischar(Args.Intensity) || iscellstr(Args.Intensity)
        Args.Intensity = funHeader(ImObj, @getVal, Args.Intensity);
    end

    % fit response to each pixel
    Result = imUtil.calib.pixel_flat_response(Cube, 'Gain',Args.Gain,...
                                                    'ReadNoise',Args.ReadNoise,...
                                                    'MeanFun',Args.MeanFun,...
                                                    'MeanFunPar',Args.MeanFunPar,...
                                                    'Intensity',Args.Intensity,...
                                                    'Model',Args.Model);



end

