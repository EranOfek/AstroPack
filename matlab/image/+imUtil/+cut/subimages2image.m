function [FullImage]=subimages2image(SubImage,CCDSEC,Args)
% construct the full image from sub-images
% Package: mUtil.image
% Input  : - A cell array of images, or a structure array of images (for
%            that purpose imCl and SIM are struct).
%            Each sub image can be an image which size is equivalent to the
%            size of the image in each CCDSEC line, or a scalar.
%            If this is a scalar, then it will be duplicated all over the
%            sub image.
%          - a matrix of CCDSEC [xmin xmax ymin ymax]. One line per
%            elelement in the sub images array.
%            The CCDSEC positions, refers to the final position of the sub
%            images in the reconstrcted full image.
%          * Arbitrary number of pairs of input arguments ...,key,val,...
%            The following keywords are available:
%            'FieldName' - Field name in a struct output in which to store
%                       the sub images. Default is 'Im'.
%                       This is good also for a imCl or SIM input.
%            'StitchMethod' - Stitching method:
%                       'IgnoreOverlap' - Ignore overlap between images.
%                                   This option is fast.
%                       'MeanOverlap'   - Take the nan mean over overlap
%                                   regions.
%                                   This option is slow and requires a lot
%                                   of memory (size of full image X number
%                                   of sub images).
%                       Default is 'IgnoreOverlap'.
% Output : - Full reconstucted image.
% Tested : Matlab R2011b
%     By : Eran O. Ofek                    Mar 2020
%    URL : http://weizmann.ac.il/home/eofek/matlab/
% Example: [SubImage,CCDSEC,Center,NoCCDSEC]=imUtil.cut.partition_subimage(Image,[],'SubSizeXY',[512 512],'OverlapXY',[16 16],'Output','struct');
%          [FullImage]=imUtil.cut.subimages2image(SubImage,CCDSEC);
% Reliable: 2
%--------------------------------------------------------------------------

arguments
    SubImage
    CCDSEC                                 = [];
    Args.StitchMethod                      = 'IgnoreOverlap';  % 'MeanOverlap' | 'IgnoreOverlap'
    Args.FieldName                         = 'Im';
end


Nsub = numel(SubImage);
FullCCDSEC = [min(CCDSEC(:,1)), max(CCDSEC(:,2)), min(CCDSEC(:,3)), max(CCDSEC(:,4))];

switch lower(Args.StitchMethod)
    case 'meanoverlap'
        FullCube  = nan(FullCCDSEC(4), FullCCDSEC(2), Nsub);
        for Isub=1:1:Nsub
            if iscell(SubImage)
                SubI = SubImage{Isub};
            else
                SubI = SubImage(Isub).(Args.FieldName);
            end
            FullCube( CCDSEC(Isub,3):CCDSEC(Isub,4), CCDSEC(Isub,1):CCDSEC(Isub,2), Isub ) = SubI;

        end

        FullImage = nanmean(FullCube,3);
    case 'ignoreoverlap'
        FullImage = nan(FullCCDSEC(4), FullCCDSEC(2) );
        for Isub=1:1:Nsub
            if iscell(SubImage)
                SubI = SubImage{Isub};
            else
                SubI = SubImage(Isub).(Args.FieldName);
            end
            FullImage( CCDSEC(Isub,3):CCDSEC(Isub,4), CCDSEC(Isub,1):CCDSEC(Isub,2) ) = SubI;

        end
    otherwise
        error('Unknown StitchMethod option');
end

    
