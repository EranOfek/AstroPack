% Contents of the imProc.dark package
%       A collection of tools for dark/bias/overscan creation and subtraction.
%       Operates on AstroImage class object.
% Content:
%       isBias.m - Identify and validate bias images.
%       isDark.m - Identify and validate dark images.
%       bias.m - Create a master bias image, from a list of images
%               (including optionaly identyfing and validating the images).
%       debias.m - Subtract a master bias from science images (including
%               optionaly identyfing and validating the images, and
%               creating the bias image).
%       overscan.m - Create and subtract an overscan bias from images.
%       compare2template.m - Compare an image to a template.
%       identifyFlaringPixels.m - Identify flaring pixels.
%       identifySimilarImages.m - Identify pairs of images which have
%               identical values in some of their pixels.
%       unitTest.m - unitTest for this package.
% Related packages:
%       imProc.stack - coaddition of images.
%       imProc.flat - flat fielding of astronomical images
