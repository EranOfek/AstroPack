function [CutoutCube, ActualXY] = cutouts(Obj, XY, Args)
    % Break a single image to a cube of cutouts around given positions
    %       including optional sub pixel shift.
    %       Uses ImageComponent/cutouts.
    % Input  : - A single element AstroImage object.
    %          - A two column matrix of [X, Y] positions around
    %            which to generate the cutouts.
    %          * ...,key,val,...
    %            'HalfSize' - Cutout half size (actual size will be
    %                   1+2*HalfSize. Default is 8.
    %            'PadVal' - padding value for cutouts near edge or
    %                   without circular shifts.
    %            'CutAlgo' - Algorithm: ['mex'] | 'wmat'.            
    %            'IsCircle' - If true then will pad each cutout
    %                   with NaN outside the HalfSize radius.
    %                   Default is false.
    %            'Shift' - A logical indicating if to shift
    %            'ShiftAlgo' - Shift algorithm ['lanczos3'] |
    %                   'lanczos2' | 'fft'.
    %            'IsCircFilt' - While using lanczos, is circshift
    %                   is circular or not. Default is false.
    %            'DataPropIC' - Data property inside ImageComponent,
    %                   from which to extract
    %                   the cutouts. Default is 'Image'.
    %            'DataProp' - Data property from which to extract
    %                   the cutouts. Default is 'ImageData'.
    % Outout : - A cube of size 1+2*HalfSize X 1+2*HalfSize X
    %               numberOfCutouts. each layer contain a cutout
    %               and 3rd dim is for cutout index.
    %          - A two column matrix of the actual positions
    %            [X,Y], around which the cutouts are extracted.
    %            These may be rounded if 'RoundPos' is set to true.
    % Author : Eran Ofek (Apr 2021)
    % Example: IC = ImageComponent({rand(1000,1000)});
    %
    %          AI=AstroImage({rand(1000,1000)});
    %          XY = rand(10000,2).*900 + 50;
    %          Cube = imProc.image.cutouts(AI, XY);
    %          Cube = imProc.image.cutouts(AI, XY,'Shift',true);
    %          Cube = imProc.image.cutouts(AI, XY,'Shift',true,'IsCircFilt',true);
    
    arguments
        Obj(1,1)
        XY(:,2)                     = zeros(0,2);
        Args.DataProp               = 'ImageData';
        Args.DataPropIC             = 'Image';
        Args.HalfSize               = 8;
        Args.PadVal                 = NaN;
        Args.CutAlgo                = 'mex';  % 'mex' | 'wmat'
        Args.IsCircle               = false;
        Args.Shift(1,1) logical     = false;
        Args.ShiftAlgo              = 'lanczos3';  % 'fft' | 'lanczos2' | 'lanczos3' | ...
        Args.IsCircFilt(1,1) logical = true;
    end

    [CutoutCube, ActualXY] = cutouts(Obj.(Args.DataProp), XY, 'DataProp',Args.DataPropIC,...
                                             'HalfSize',Args.HalfSize,...
                                             'PadVal',Args.PadVal,...
                                             'CutAlgo',Args.CutAlgo,...
                                             'IsCircle',Args.IsCircle,...
                                             'Shift',Args.Shift,...
                                             'ShiftAlgo',Args.ShiftAlgo,...
                                             'IsCircFilt',Args.IsCircFilt);

end
