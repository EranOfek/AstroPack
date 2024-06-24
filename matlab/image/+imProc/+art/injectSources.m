function [AI, InjectedCat] = injectSources(AI0, Cat, PSF, Flux, Args)
    % Inject/subtract source images at given pixel positions of an AstroImage
    %     AI can be a stack of objects
    % Input  : - a stack of AstroImages or make a new one 
    %          - a 2-column matrix of exact (sub)pixel positions or an AstroCatalog  
    %          - a vector of source fluxes or 1 flux value for all the sources
    %          - a cube or a cell array of PSF stamps or a single PSF stamp
    %          * ...,key,val,... 
    %        'Subtract' - false (def.) - add sources, true - subtract sources
    %        'CreateNewObj' - false (def.) operate on the same AI or make a copy         
    %        'UpdateHeader' - add to the header information on the number of injected sources 
    %        'UpdateCat' - merge the injected catalog with the existing one
    %        'Oversample'- oversampling of the input PSF stamps (2 means the PSF is 2 times finer than the image pixel) 
    %        'RotAngle'  - rotation angle(s) of the input PSF stamps [deg]
    %        'Recenter'  - true (def.) whether to fft-shift the source PSF according to their subpixel positions
    %        'PositivePSF'   - false (def.) whether to improve PSF wings (see imUtil.art.createSourceCube)
    %        'AddBackground' - false (def.) whether to add background to the image 
    %        'SubtractBackground' - false (def.) if true, subtract background instead of adding it 
    %        'Back'     - single value or an background image
    %        'AddNoise' - false (def.) whether to add noise to the resulting source + background image
    %        'NoiseModel' - see imUtil.art.addNoise
    %        'NoisePar' - see imUtil.art.addNoise
    % Output : - the AstroImage stack with added or subtracted source stamps and merged catalogs 
    %          - a catalog of injected sources 
    % Author : A.M. Krassilchtchikov (2024 Jun) 
    % Example: [AI, InjectedCat] = imProc.art.injectSources(AI0, Cat, PSF, Flux, 'PositivePSF', true)
    %
    arguments
        AI0
        Cat
        PSF
        Flux
        Args.Subtract     logical     = false;
        
        Args.CreateNewObj logical     = false;
        
        Args.UpdateHeader logical     = true;   
        Args.UpdateCat    logical     = true;
        
        Args.Oversample               = [];
        Args.RotAngle                 = [];
        Args.Recenter     logical     = true;
        Args.PositivePSF  logical     = false;
        
        Args.AddBackground            = false;
        Args.SubtractBackground       = false;
        Args.Back                     = [];
        
        Args.AddNoise                 = false;
        Args.NoiseModel               = [];
        Args.NoisePar                 = [];
    end  
    % make an empty AstroImage object if it does not exist
    if ~strcmpi(class(AI0),'astroimage')
        warning('The input object is not AstroImage, using an empty one instead \n');
        AI0 = AstroImage();
    end
    % new object
    if Args.CreateNewObj
        AI = AI0.copy;
    else
        AI = AI0;
    end
    % prepare fluxed source stamps
    if ismatrix(Cat)         
        Nsrc = size(Cat,1);
        X1Y1 = Cat;   
        if numel(Flux) < Nsrc
            Flux = repmat(Flux,1,Nsrc);
        end
        InjectedCat = AstroCatalog({[X1Y1(:,1) X1Y1(:,2) Flux]},'ColNames',{'X1','Y1','FLUX_APER_3'});
    elseif strcmpi(class(Cat),'astrocatalog')        
        InjectedCat = Cat;
%         Nsrc = height(Cat.Table);
        X1Y1 = [Cat.Catalog.X Cat.Catalog.Y];
    end
    [CubePSF, XY] = imUtil.art.createSourceCube(PSF, X1Y1, Flux, ...
        'Recenter', Args.Recenter, 'Oversample', Args.Oversample, ...
        'RotAngle', Args.RotAngle, 'PositivePSF', Args.PositivePSF);
    % loop over input AI objects
    for Iobj = 1:numel(AI)
        % do the injection/subtraction and merge catalogs, if requested
        if ~Args.Subtract
            AI(Iobj).Image = imUtil.art.addSources(AI(Iobj).Image,CubePSF,XY);
            if Args.UpdateCat
              AC(1) = AI(Iobj).CatData;
              AC(2) = InjectedCat;
              AI(Iobj).CatData = merge(AC,AC(2).ColNames); 
            end
        else
            AI(Iobj).Image = imUtil.art.subtractSources(AI(Iobj).Image,CubePSF,XY);
        end
        % add/subtract background
        if isempty(AI(Iobj).Back) && Args.AddBackground
            AI(Iobj).Image = imUtil.art.addBackground(AI(Iobj).Image, Args.Back, 'Subtract', Args.SubtractBackground);
            AI(Iobj).Back  = Args.Back;
        end
        if Args.AddNoise
            AI(Iobj).Image = imUtil.art.addNoise(AI(Iobj).Image,Args.NoiseModel,'NoisePar',Args.NoisePar);
        end
        % update the headers
        if Args.UpdateHeader
            % AI(Iobj).Header: which keyword shall we use? HISTORY? 
            % add HISTORY to AstroHeader dictionary? 
        end
    end        
end
