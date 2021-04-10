function [SubImage,CCDSEC,Center,NooverlapCCDSEC]=partition_subimage(Image,CCDSEC,Args)
% Partition image into sub images
% Package: mUtil.image
% Description: Partition image into sub images defined either by a CCDSEC
%              matrix, or by imUtil.image.subimage_grid function.
% Input  : - Image.
%          - A 4 column matrix of CCDSEC by which to partition the image.
%            Line per sub image. If empty, will use imUtil.image.subimage_grid
%            Default is empty.
%          * Arbitrary number of pairs of input arguments ...,key,val,...
%            The following keywords are available:
%            'Output' - Output type {['cell'] | 'struct'}
%            'FieldName' - Field name in a struct output in which to store
%                       the sub images. Default is 'Im'.
%            'SubSizeXY' - Sub image size [X,Y]. Default is [128 128].
%            'Nxy' - Number of sub images along each dimension [Nx, Ny].
%                    If empty then use SubSizeXY. Default is [].
%            'OverlapXY' - Overlapping extra [X, Y] to add to SubSizeXY
%                    from each side. Default is [32 32].
% Output : - A cell array or a structure array (depends on Output type), of
%            the sub images.
%          - A matrix of the CCDSEC for each sub image. A line per image.
%            [xmin, xmax, ymin, ymax].
%          - Centers of sub images [X,Y].
%          - CCDSEC without overlap. This will be rturned only of input CCDSEC
%            is empty. Otherwise, will return empty matrix.
% Tested : Matlab R2011b
%     By : Eran O. Ofek                    Mar 2020
%    URL : http://weizmann.ac.il/home/eofek/matlab/
% Example: [SubImage,CCDSEC,Center]=imUtil.image.partition_subimage(rand(256,256),[],'SubSizeXY',[64 64],'OverlapXY',[10 10],'Output','struct');
%          [SubImage]=imUtil.image.partition_subimage(rand(256,258),CCDSEC)
%          cellfun(@(x) std(x(:)),SubImage,'UniformOutput',false)  % calculate std for each sub image
%          [SubImage]=imUtil.image.partition_subimage(rand(256,258),CCDSEC,'Output','struct');
% Reliable: 2
%--------------------------------------------------------------------------

arguments
    Image                {mustBeNumeric(Image)}
    CCDSEC                                                = [];
    Args.Output                                           = 'cell';  % 'cell' | 'struct'
    Args.SubSizeXY(1,2)  {mustBeNumeric(Args.SubSizeXY)}  = [128 128];
    Args.Nxy                                              = [];
    Args.OverlapXY       {mustBeNumeric(Args.OverlapXY)}  = [32 32];
    Args.FieldName                                        = 'Im';
end

SizeXY = fliplr(size(Image));

NooverlapCCDSEC = [];
if isempty(CCDSEC)
    % partition image using subimage_grid
    [CCDSEC,NooverlapCCDSEC,Center,Nxy] = imUtil.image.subimage_grid(SizeXY,...
                                                            'SubSizeXY',Args.SubSizeXY,...
                                                            'Nxy',Args.Nxy,...
                                                            'OverlapXY',Args.OverlapXY);
end


Nsub = size(CCDSEC,1);


for Isub=1:1:Nsub
    if (CCDSEC(Isub,1)<1 || CCDSEC(Isub,2)>SizeXY(1) || CCDSEC(Isub,3)<1 || CCDSEC(Isub,4)>SizeXY(2))
        error('CCDSEC is out of image boundries');
    end
    if ~all(CCDSEC==floor(CCDSEC))
        error('CCDSEC must contain integer values');
    end
    
    switch lower(Args.Output)
        case 'cell'
            if (Isub==1)
                SubImage = cell(Nsub,1);
            end
            SubImage{Isub} = Image(CCDSEC(Isub,3):CCDSEC(Isub,4),CCDSEC(Isub,1):CCDSEC(Isub,2));
        case 'struct'
            if (Isub==1)
                SubImage = struct(Args.FieldName,cell(Nsub,1));
            end
            SubImage(Isub).(Args.FieldName) = Image(CCDSEC(Isub,3):CCDSEC(Isub,4),CCDSEC(Isub,1):CCDSEC(Isub,2));
        otherwise
            error('Unknown Output option');
    end
end

Center = [mean(CCDSEC(:,1:2),2), mean(CCDSEC(:,3:4),2)];
