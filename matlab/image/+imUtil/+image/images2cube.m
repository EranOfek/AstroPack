function [Cube]=images2cube(Images,Args)
% Store a set of images of the same size in a cube.
% Package: imUtil.image
% Description: Given a cell array of images or a structure array that
%              contains a field with image, convert it to a cube of
%              images. By default the image index is 3.
% Input  : - 
%          * Arbitrary number of pairs of input arguments ...,key,val,...
%            The following keywords are available:
%            'FieldName' - If the input is a structure array, then this is
%                          the field name containing the image.
%                          Default is 'Im.
%            'IndexDim'  - The dimension in which the image index is
%                          stored in the output. Default is 3.
%                          This can be either 1 or 3.
% Output : - A cube of images.
% Tested : Matlab R2011b
%     By : Eran O. Ofek                    Mar 2020
%    URL : http://weizmann.ac.il/home/eofek/matlab/
% Example: [Cube]=imUtil.image.images2cube({rand(5,4),ones(5,4)});
%          [Cube]=imUtil.image.images2cube({rand(5,4),ones(5,4)},'IndexDim',1);
% Reliable: 2

arguments
    Images
    Args.FieldName          = 'Im';
    Args.IndexDim           = 3;
end



if isnumeric(Images)
    % assume already in a cube format
    Cube = Images;
else
    if iscell(Images)
        % input: a cell of images
        N = numel(Images);
        Size1 = size(Images{1});
        Cube = zeros(Size1(1),Size1(2),N);
        for I=1:1:N
            if ~all(size(Images{I})==Size1)
                error(sprintf('Image number %d is not the same size as image number 1',I));
            else
                Cube(:,:,I) = Images{I};
            end
        end
    else
        if isstruct(Images)
            % input: a structure array with field 'FieldName'
            N = numel(Images);
            Size1 = size(Images(1).(Args.FieldName));
            Cube = zeros(Size1(1),Size1(2),N);
            for I=1:1:N
                if ~all(size(Images(I).(Args.FieldName))==Size1)
                    error(sprintf('Image number %d is not the same size as image number 1',I));
                else
                    Cube(:,:,I) = Images(I).(Args.FieldName);
                end
            end
        else
            error('Uknown input images type');
        end
    end
end


% reshuffle the image index dimension
if Args.IndexDim==1
    if ndims(Cube)==3
        Cube = permute(Cube,[3 1 2]);
    else
        % do nothing
    end
else
    if Args.IndexDim==3
        % do nothing - image index already in 3rd dimension
    else
        error('Unknown IndexDim option - must be 1 or 3');
    end
end
