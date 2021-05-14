% Contents of the imProc.flat package
%       A collection of tools for flat-field creation and division,
%       and tools for measuring non-linearity of images.
%       Operates on AstroImage class object.
% Content:
%       isFlat.m - Identify and validate flat images.
%       flat.m - Create a master flat image (including optional
%               identification of the flat images).
%       deflat.m - Divide a science images by a master flat (including
%               optional flat identification and creation).
%       unitTest.m - unitTest for this package.
% Related packages:
%       imProc.stack - coaddition of images.
%       imProc.dark - bias/dark/overscan of astronomical images
