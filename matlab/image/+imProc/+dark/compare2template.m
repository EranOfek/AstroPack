function [FlagBad, FracBadPixels, Z] = compare2template(Obj, Template, Args)
    % Compare AstroImage to a template and variance and flag image
    %   which are different than the template.
    % Input  : - An AstroImage object containing images.
    %            The comparison is done 1 to 1, 1 to many, or many
    %            to 1.
    %          - A template image with the same size
    %            of the input image. This can be either a
    %            matrix or an AstroImage object.
    %            If this is an AstroImage it may include a variance image.
    %          * ...,key,val,...
    %            'TemplateVar' - A variance image. If provided, and
    %                   the template is an AstroImage, this will
    %                   override the content of the variance image
    %                   in the AstroImage.
    %            'Nsigma' - Threshold in units of number of sigmas.
    %                   Defualt is 5.
    %            'MaxFracBadPixels' - If the fraction of pixels
    %                   which value deviates from the template by
    %                   more/less than Nsigma is larger than this
    %                   threshold then the output FlagBad will be
    %                   set to true. Default is 0.0001.
    %            'UseImageVar' - A logical indicating if to add the
    %                   AstroImage variance to the template
    %                   variance. If false, use only the template
    %                   variance. Default is true.
    %            'DataProp' - Data property in the AstroImage.
    %                   Default is 'Image'.
    %            'VarProp' - Variance property in the template
    %                   image. Default is 'Var'.
    % Output : - A column vector of logicals indicating if the
    %            fraction of bad pixels (above or below threshold)
    %            is above MaxFracBadPixels.
    %          - A column vector of the fraction of bad pixels in
    %            each image.
    %          - An ImageComponent containing the Z image.
    %            (i.e., Image-Template)/sqrt(Var).
    % Author : Eran Ofek (Apr 2021)
    % Example: AI = AstroImage({2.*randn(10,10)});
    %          Template = AstroImage({0},'Var',{4});
    %          [FlagBad, FracbadPixels, Z] = imProc.dark.compare2template(AI, Template)

    arguments
        Obj AstroImage
        Template
        Args.TemplateVar                      = [];
        Args.Nsigma                           = 5;
        Args.MaxFracBadPixels(1,1)            = 0.0001;
        Args.UseImageVar                      = true;

        Args.DataProp                         = 'Image';
        Args.VarProp                          = 'Var';
    end

    if isa(Template,'AstroImage')
        % assume that the Template include the variance image
        % check if TemplateVar is given
        if ~isempty(Args.TemplateVar)
            % override the template variance in the Template
            % AstroImage
            Template.(Args.VarProp) = Args.TemplateVar;
        end
    elseif isnumeric(Template)
        Template = AstroImage({Template},'Var',{Args.TemplateVar});
    else
        error('Template must be an AstroImage or matrix');
    end

    Ntemp = numel(Template);
    Nobj  = numel(Obj);

    Nmax    = max(Ntemp, Nobj);
    FlagBad       = false(Nmax,1);
    FracBadPixels = nan(Nmax,1);
    if nargout>2
        Z = ImageComponent([Nmax,1]);
    end
    for Imax=1:1:Nmax
        Iobj  = min(Imax, Nobj);
        Itemp = min(Imax, Ntemp);

        if Args.UseImageVar && ~isempty(Obj(Iobj).(Args.VarProp))
            % comobine the image and template variances
            TotVar = Template(Itemp).(Args.VarProp) + Obj(Iobj).(Args.VarProp);
        else
            TotVar = Template(Itemp).(Args.VarProp);
        end
        Zstat   = (Obj(Iobj).(Args.DataProp) - Template(Itemp).(Args.DataProp))./sqrt(TotVar);
        NbadPix = sum(abs(Zstat)>Args.Nsigma,'all');

        FracBadPixels(Imax) = NbadPix./numel(Zstat);
        FlagBad(Imax)       = FracBadPixels(Imax)>Args.MaxFracBadPixels;

        if nargout>2
            Z(Imax).Image = Zstat;
        end

    end


end
        