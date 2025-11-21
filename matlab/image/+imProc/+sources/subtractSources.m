function [Result, SubModel] = subtractSources(Obj, Args)
    % Subtract a list of sources with a PSF from an image in AstroImage
    %     Given an AstroImage object, and PSF(s), and source catalog with
    %     X, Y, Flux of sources, subtract the sources from the image.
    %     This function calls: imUtil.sources.subtractSources
    %     This function can works with multiple PSFs in the same image.
    %     (i.e., if the PSF is an array of PSFs and the catalog contains
    %     the index of the PSF to use for each source).
    % Input  : - AN AstroImage object array.
    %          * ...,key,val,... 
    %            'PSF' - An optional PSF array to use for the sources
    %                   subtraction. If empty, then will attempt to read
    %                   the PSF from the AstroImage object.
    %                   Default is [].
    %            'XYFP' - An optional 3/4 column matrix of
    %                   [X, Y, Flux, [PSF_Index]] of sources to subtract
    %                   from the imafe. If empty, then will attempt to read
    %                   this information from the AstroCatalog in the 
    %                   AstroImage object.
    %                   Default is [].
    %            'ShiftMethod' - PSF shift method.
    %                   Default is 'fft'.
    %            'subtractSourcesArgs' - A cell array of additional
    %                   arguments to pass to imUtil.sources.subtractSources
    %                   Default is {}.
    %
    %            'ColX' - Column name in the AstroCatalog that contains the
    %                   X coordinates of the source.
    %                   Default is 'X'.
    %            'ColY' - Same as 'ColX', but for Y coordinate.
    %                   Default is 'Y'.
    %            'ColFlux' - Same as 'ColX', but for Flux.
    %                   Default is 'FLUX_PSF'.
    %            'ColPSFInd' - Same as 'ColX', but for the PSF index.
    %                   Note that if the column doesn't exist then the
    %                   indices will be set to 1.
    %                   Default is 'PSF_INDEX'.
    %
    %            'CreateNewObj' - A logical indicating if to create a new
    %                   copy of the input AstroImage object.
    %                   Default is true.
    %
    % Output : - An AstroImage object in which the images were replaced by
    %            the original images from which the spources were
    %            subtracted.
    %          - An AstroImage object containing the subtracted model from
    %            each image.
    % Author : Eran Ofek (2025 Nov) 
    % Example: [AI] = imUtil.sources.subtractSources(AI)


    arguments
        Obj
        Args.PSF                 = [];
        Args.XYFP                = [];  % [X, Y, F, PSF_Index]
        Args.ShiftMethod         = 'fft';
        Args.subtractSourcesArgs = {};
        Args.ColX                = 'X';
        Args.ColY                = 'Y';
        Args.ColFlux             = 'FLUX_PSF';
        Args.ColPSFInd           = 'PSF_INDEX';
        Args.CreateNewObj        = true;
    end

    if Args.CreateNewObj
        Result = Obj.copy;
    else
        Result = Obj;
    end

    if ~isempty(Args.XYFP)
        [Nxy,Ncol] = size(Args.XYFP);
        if Ncol<4
            Args.XYFP = [Args.XYFP; ones(Nxy,1)];
        end
    end

    Nobj = numel(Obj);
    if nargout>1
        SubModel = AstroImage(size(Result));
    end
    
    for Iobj=1:1:Nobj
        if isempty(Args.PSF)
            PSF = Result.PSFData;
        else
            PSF = Args.PSF;
        end

        % get X, Y, PSF_Index of sources
        if isempty(Args.XYFP)
            [X, Y, Flux, PSF_Index] = getCol(Obj(Iobj).CatData, {Args.ColX, Args.ColY, Args.ColFlux, Args.ColPSFInd});
            if any(isnan(PSF_Index))
                PSF_Index = ones(size(X));
            end
        end

        Npsf = numel(PSF);
        SubIm = Result(Iobj).ImageData.Image;
        for Ipsf=1:1:Npsf
            IndPSF = find(PSF_Index==Ipsf);
            [SubIm, SrcIm] = imUtil.sources.subtractSources(SubIm, PSF(Ipsf).Data, 'IsShiftedPSF',false,...
                                                                                   'X',X(IndPSF),...
                                                                                   'Y',Y(IndPSF),...
                                                                                   'Flux',Flux(IndPSF),...
                                                                                   'DX',0,...
                                                                                   'DY',0,...
                                                                                   'ShiftMethod',Args.ShiftMethod,...
                                                                                   Args.subtractSourcesArgs{:});
            if nargout>1
                if Ipsf==1
                    SubModel(Iobj).ImageData.Image = SrcIm;
                else
                    SubModel(Iobj).ImageData.Image = SubModel(Iobj).ImageData.Image + SrcIm;
                end
            end

        end

        Result(Iobj).ImageData.Image = SubIm;
    end


end
