% Usage: [cutouts, image_subtracted] = mexCutout(image, positions, cut_size, pad_value=0, replace_value=0, debug_bit=0, use_memcpy=1); 
%
% INPUTS: 
%   -image: provide a full frame image to cut from. Can handle double 
%           (64bit), single (32bit), uint16 and logicals.
%           Can also input a set of images as a 3D matrix. In this case, 
%           will cut the same positions from each image. 
%   -positions: an Nx2 matrix where the first column is the X positions and 
%               the second column is the Y positions of the centers of the 
%               requested cutouts. 
%   -cut_size: number of pixels on each side of the cutout. The cutouts are
%              square (making them rectangles requres code adjustments). 
%   -pad_value: what to put if the cutout falls outside the image (defualt 
%               is zero but also can use NaN, for double/single precision). 
%   -replace_value: what to put instead of cutouts in the image_subtracted. 
%                   the default is zero but can be NaN for double/single. 
%   -debug_bit: level of verbosity (default zero, for quiet execution). 
%   -use_memcpy: speedup execution using memcpy code. 
%
% OUTPUTS: 
%   -cutouts: square cutouts at the positions given. The size of the first 
%             two dimensions is "cut_size". The size of the 3rd dimension 
%             is equal to the number of input images (3rd dim of "image"). 
%             The 4th dimension is the number of cutout positions given, 
%             i.e., the number of rows in "positions". 
%   -image_subtracted: the same image or images as given, but with the area
%                      of each cutout removed and replaced with another 
%                      value (i.e., the "replace_value"). This is useful 
%                      for looking for transients after removing all the 
%                      known stars in the image. 
%                      If no second output is requested, the function does 
%                      not waste time calculating this. 
%                      Since it needs to make a copy of the input, this can
%                      take substantially more time than just making cutous. 
