function [Result,ACF] = identifyBadImages(Obj, Args)
    % Identify bad images based on simple statistical properties:
    %       Fraction of pixels above threshold value
    %       Number of pixels in the ACF above some threshold correlation.
    %       Distance from ACF center to the furthest ACF value above some
    %       threshold.
    %       NOTE: I guess that staellites constellations may be identified
    %       as bad images.
    % Input  : - An AstroImage object, or a char file name with optional
    %            wild cards, or a cell array of image names.
    %            If input is a char or cell, then the images will be ready
    %            into memory one at a time.
    %          * ...,key,val,...
    %            'CCDSEC' - CCDSEC [Xmin Xmax Ymin Ymax] on which to
    %                   operate the ACF tests. If empty, then use entire image.
    %                   Default is [].
    %            'ThresholdACFnpix' - Threshold aotocorrelation.
    %                   Default is 0.25.
    %            'ThresholdACFdist' - ACF threshold for max dist test. 
    %                   Default is 0.5.
    %            'ThresholdMaxDistACF' - ACF distance threshold.
    %                   If the maximal distance for ACF with values above
    %                   'ThresholdACFdist' is larger than this threshold
    %                   than the image is declared as bad.
    %                   Default is 6 pix.
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
    %                   Default is {'BackFun',@median, 'BackFunPar',{[1 2],'omitnan'}, 'SubSizeXY',[]}
    %            'PopulateBack' - A logical indicating if to populate the
    %                   calculated back in the AstroImage.
    %                   Default is false.
    % Output : - A structure array (element per image) with the followng fields
    %            .Npix
    %            .NpixAboveThresholdVal
    %            .NpixAboveThresholdACF
    %            .BadImageFlag - true if image is bad.
    %          - The ACF of the background subtracted image.
    %            Only, the last analyzed image is returned.
    % Author : Eran Ofek (Dec 2021)
    % Example: Result = imProc.stat.identifyBadImages(Obj, Args)
    
    arguments
        Obj AstroImage
        
        Args.CCDSEC                 = [];
        Args.ThresholdACFnpix       = 0.8;
        Args.ThresholdACFdist       = 0.5;
        Args.ThresholdMaxDistACF    = 6;
        Args.ThresholdVal           = 5000;
        Args.MaxFracAboveVal        = 0.3;
        Args.MaxPixAboveACF         = 20;
        
        Args.backgroundArgs cell    = {'BackFun',@median, 'BackFunPar',{[1 2],'omitnan'}, 'SubSizeXY',[]};
        Args.PopulateBack logical   = false;
        Args.DataProp               = 'Image';
        Args.BackProp               = 'Back';
    end
    
    if isa(Obj, 'AstroImage')
        % do nothing
        List = [];
        Nobj   = numel(Obj);
    elseif ischar(Obj)
        List = io.files.filelist(Obj);
        Nobj = numel(List);
    elseif iscell(Obj)
        List = Obj;
        Nobj = numel(List);
    else
        error('Unknown input type');
    end
    
    
    Result = struct('Npix',cell(Nobj,1),...
                    'NpixAboveThresholdVal',cell(Nobj,1),...
                    'NpixAboveThresholdACF',cell(Nobj,1),...
                    'BadImageFlag',cell(Nobj,1));
                
    for Iobj=1:1:Nobj
        if isempty(List)
            % do nothing
            Iim = Iobj;
        else
            % read image into AstroImage
            Obj = AstroImage(List{Iobj});
            Iim = 1;
        end
        
        if isempty(Args.CCDSEC)
            Image = Obj(Iim).(Args.DataProp);
            Back  = Obj(Iim).(Args.BackProp);
        else
            Image = Obj(Iim).(Args.DataProp)(Args.CCDSEC(3):Args.CCDSEC(4), Args.CCDSEC(1):Args.CCDSEC(2));
            if isempty(Obj(Iim).(Args.BackProp))
                Back = [];
            else
                Back  = Obj(Iim).(Args.BackProp)(Args.CCDSEC(3):Args.CCDSEC(4), Args.CCDSEC(1):Args.CCDSEC(2));
            end
        end
        
        
        % Number of pixels above threshold
        Result(Iobj).Npix = numel(Image);
        Result(Iobj).NpixAboveThresholdVal =  sum(Image > Args.ThresholdVal, 'all');
        
        
        % background
        if isempty(Back)
            Back = imUtil.background.background(Image, Args.backgroundArgs{:});
            if Args.PopulateBack
                Obj(Iim).(Args.BackProp) = Back;
            end
        end
        
        BackSubImage = Image - Back;
        
        
        % FFU: treat saturated pixels!
        BackSubImage(BackSubImage>40000) = 0;
        
        % Autocorrelation function
        [ACF] = imUtil.filter.autocor(BackSubImage, 'Norm',true, 'SubBack',false);
        
        SizeACF = size(ACF);
     
        II = find(ACF>Args.ThresholdACFdist);
        [I,J]=imUtil.image.ind2sub_fast(SizeACF,II);
        Result(Iobj).MaxDistACFabove = max(sqrt(sum(([I,J] - SizeACF.*0.5).^2,2)));
        
        
        
        
        Result(Iobj).NpixAboveThresholdACF = sum(ACF > Args.ThresholdACFnpix, 'all');
        
        Result(Iobj).BadImageFlag = (Result(Iobj).NpixAboveThresholdVal./Result(Iobj).Npix) > Args.MaxFracAboveVal || ...
                                    Result(Iobj).NpixAboveThresholdACF > Args.MaxPixAboveACF || ...
                                    Result(Iobj).MaxDistACFabove > Args.ThresholdMaxDistACF;
    end
end