function [Sub,ListEdge,ListCenter]=image_partitioning(Image,BlockSize,Args)
% Partition a 2D image into sub images. OBSOLETE: use
% imUtil.image.partition_subimages instead
% Package: ImUtil
% Description: Partition a 2D image into sub images.
% Input  : - A single 2D image (matrix).
%          - BlockSize [X,Y], or [X] (will be copied as [X, X]).
%            Alternatively, if this is empty then will use ListEdge and
%            ListCenter parameters.
%          * Arbitrary number of pairs of arguments: ...,keyword,value,...
%            where keyword are one of the followings:
%            'ListEdge' - [xmin, xmax, ymin, ymax] as returned by
%                   imUtil.partition.subimage_boundries.
%                   This is used only if BlockSize is empty.
%                   Default is empty.
%            'ListCenter' - [xcenter,ycenter] as returned by
%                   imUtil.partition.subimage_boundries.
%                   This is used only if BlockSize is empty.
%                   Default is empty.
%            'Overlap' - Overlapping buffer. Default is 10 pix.
%            'FieldNameIm' - Image field name in output structure.
%                   Default is 'Im'.
%            'FieldNameX' - X center field name in output structure.
%                   Default is 'CenterX'.
%            'FieldNameY' - Y center field name in output structure.
%                   Default is 'CenterY'.
% Output : - A structure array in which each element contains a
%            sub image (stored in the .Im field).
%          - Four column matrix of list of blocks in image
%            [Xmin, Xmax, Ymin, Ymax].
%          - Four column matrix of list of block centers [Xcen, Ycen].
% License: GNU general public license version 3
%     By : Eran O. Ofek                    Sep 2019
%    URL : http://weizmann.ac.il/home/eofek/matlab/
% Example:
% [Sub,ListEdge,ListCenter]=imUtil.partition.image_partitioning(Image,[256 256]);
% Reliable: 
%--------------------------------------------------------------------------


arguments
    Image
    BlockSize
    Args.ListEdge         = [];
    Args.ListCenter       = [];
    Args.Overlap          = 10;
    Args.FieldNameIm      = 'Im';
    Args.FieldNameX       = 'CenterX';
    Args.FieldNameY       = 'CenterY';
end


ImageSize = fliplr(size(Image));  % [X,Y]


if numel(BlockSize)==1
    BlockSize = [BlockSize, BlockSize];
end

if isempty(BlockSize)
    ListEdge = Args.ListEdge;
    ListCenter = Args.ListCenter;
else
    [ListEdge,ListCenter] = imUtil.partition.subimage_boundries(ImageSize,BlockSize,Args.Overlap,'simple');
end
    


    
Xmin = ListEdge(:,1);
Xmax = ListEdge(:,2);
Ymin = ListEdge(:,3);
Ymax = ListEdge(:,4);

Nblock = size(ListEdge,1);

Sub = tools.struct.struct_def({'Im','CenterX','CenterY'},Nblock,1);

for Iblock=1:1:Nblock
    Sub(Iblock).(Args.FieldNameIm) = Image(Ymin(Iblock):Ymax(Iblock),Xmin(Iblock):Xmax(Iblock));
    Sub(Iblock).(Args.FieldNameX)   = ListCenter(Iblock,1);
    Sub(Iblock).(Args.FieldNameY)   = ListCenter(Iblock,2);
end

Nx = numel(unique(ListCenter(:,1)));
Ny = size(ListCenter,1)./Nx;

Sub = reshape(Sub,Ny,Nx);



  