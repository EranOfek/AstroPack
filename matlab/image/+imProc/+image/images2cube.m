function varargout = images2cube(Obj, Args)
    % Convert the images in AstroImage object into a cube.
    %       Each data property (e.g., 'ImageData', 'BackData')
    %       produce a cube.
    % Input  : - An array of AstroImage objects.
    %          * ...,key,val,...
    %            'CCDSEC' - A 4 column matrix of CCDSEC of each
    %                   image in ImageComponent to insert into the
    %                   cube [Xmin, Xmax, Ymin, Ymax].
    %                   If single line, then use the same CCDSEC
    %                   for all images. If empty, use entore image.
    %                   Default is [].
    %            'DataPropIn' - Data property, in ImageComponent,
    %                   from which to take
    %                   the image. Default is 'Image'.
    %            'DimIndex' - Cube dimension of the image index.
    %                   Either 1 or 3. Default is 3.
    %            'DataProp' - The data properties for which the
    %                   cubes will be calculated.
    %                   Default is {'ImageData','BackData',
    %                   'VarData', 'MaskData'}.
    % Output : * A cube for each DataProp, by the order of their
    %            appearnce in DataProp.
    % Author : Eran Ofek (Apr 2021)
    % Notes  : Doing this operation directly (without
    %       astroImage2ImageComponent) will be only a few percents
    %       faster.
    % Example: AI = AstroImage({rand(1000,1000), rand(1000,1000), rand(1000,1000)})
    %          [CubeImage, CubeBack] = imProc.image.images2cube(AI)
    %          [CubeImage] = imProc.image.images2cube(AI,'CCDSEC',[1 2 2 5])

    arguments
        Obj
        Args.CCDSEC                        = [];
        Args.DataPropIn                    = 'Image';
        Args.DimIndex                      = 3;
        Args.DataProp                      = {'ImageData','BackData', 'VarData', 'MaskData'};
    end

    [Out{1:nargout}] = astroImage2ImageComponent(Obj, 'CreateNewObj',false,...
                                        'ReturnImageComponent',false,...
                                        'DataProp',Args.DataProp);

    varargout = cell(1,nargout);
    for Iarg=1:1:nargout          
        varargout{Iarg} = images2cube(Out{Iarg}, 'CCDSEC',Args.CCDSEC,...
                                                 'DataPropIn',Args.DataPropIn,...
                                                 'DimIndex',Args.DimIndex);
    end

end
