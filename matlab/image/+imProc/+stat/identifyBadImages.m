function Result = identifyBadImages(Obj, Args)
    % Identify bad images based on simple statistical properties:
    %       Fraction of pixels above threshold value
    %       Number of pixels in the ACF above some threshold correlation.
    % Input  : - An AstroImage object.
    %          * ...,key,val,...
    %            'ThresholdACF' - Threshold aotocorrelation.
    %                   Default is 0.25.
    %            'ThresholdVal' - Threshold image value.
    %                   Default is 10000.
    %            'MaxFracAboveVal' - Max frac of pixels above ThresholdVal.
    %                   If number is above this value, than the image will
    %                   decalared as bad.
    %                   Default is 0.3.
    %            'MaxPixAboveACF' - Max num of pix above some correlation.
    %                   If number of ACF pixels above ThresholdACF, exceed
    %                   this value, then the image will be declared as bad.
    %                   Default is 20.
    %            'backgroundArgs' - A cell array of arguments to pass to
    %                   the imUtil.background.background.
    %                   This will be used only if the 'Back' field in the
    %                   AstroImage is not populated.
    %                   Default is {}.
    %            'PopulateBack' - A logical indicating if to populate the
    %                   calculated back in the AstroImage.
    %                   Default is false.
    % Output : - A structure array (element per image) with the followng fields
    %            .Npix
    %            .NpixAboveThresholdVal
    %            .NpixAboveThresholdACF
    %            .BadImageFlag - true if image is bad.
    % Author : Eran Ofek (Dec 2021)
    % Example: Result = imProc.stat.identifyBadImages(Obj, Args)
    
    arguments
        Obj AstroImage
        
        Args.ThresholdACF           = 0.25;
        Args.ThresholdVal           = 10000;
        Args.MaxFracAboveVal        = 0.3;
        Args.MaxPixAboveACF         = 20;
        
        Args.backgroundArgs cell    = {};
        Args.PopulateBack logical   = false;
        Args.DataProp               = 'Image';
        Args.BackProp               = 'Back';
    end
    
    
    Nobj   = numel(Obj);
    Result = struct('Npix',cell(Nobj,1),...
                    'NpixAboveThresholdVal',cell(Nobj,1),...
                    'NpixAboveThresholdACF',cell(Nobj,1),...
                    'BadImageFlag'cell(Nobj,1));
                
    for Iobj=1:1:Nobj
        % Number of pixels above threshold
        Result(Iobj).Npix = numel(Obj(Iobj).(Args.DataProp));
        Result(Iobj).NpixAboveThresholdVal =  sum(Obj(Iobj).(Args.DataProp) > Args.ThresholdVal, 'all');
        
        
        % background
        if isempty(Obj(Iobj).(Args.BackProp))
            Back = imUtil.background.background(Obj(Iobj).(Args.DataProp), Args.backgroundArgs{:});
            if Args.PopulateBack
                Obj(Iobj).(Args.BackProp) = Back;
            end
        else
            Back = Obj(Iobj).(Args.BackProp);
        end
        
        BackSubImage = Obj(Iobj).(Args.DataProp) - Back;
        
        % Autocorrelation function
        [ACF] = imUtil.filter.autocor(BackSubImage, 'Norm',true, 'SubBack',false);
        
        Result(Iobj).NpixAboveThresholdACF = sum(ACF > Args.ThresholdACF, 'all');
        
        Result(Iobj).BadImageFlag = (Result(Iobj).NpixAboveThresholdVal./Result(Iobj).Npix) > Args.MaxFracAboveVal && ...
                                    Result(Iobj).NpixAboveThresholdACF > Args.MaxPixAboveACF;
    end
end