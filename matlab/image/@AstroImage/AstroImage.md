;#autogen:ignore

Class Hierarchy: Base -> Component -> AstroImage

AstroImage is a container for images and images meta data, as well as basic functionality for image manipulation.
SHORT PARAGRAPH with primary capabilities/functionality. 
DETAILED TEXT with full capabilities/functionality. 
For additional help see manuals.main
Properties
ImageData - A SciImage object containing the science image. Most image related methods will operate on this property by default.
BackData - A BackImage object containing the background image, or a background scalar.
VarData - A VarImage object containing the variance image, or a variance scalar.
MaskData - A MaskImage object containing the mask Image. Each pixel in the mask image corresponds to a pixel in the image. Each pixel is an integer, in which each bit is a flag for the exsistence of a problem or property (e.g., this pixel is saturated).
HeaderData - An AstroHeader object of the image headers.
CatData - An AstroCatalog object containing the catalog data.
PSFData - A PSFData object containing the PSF data.
WCS - An AstroWCS object containing the WCS data.
PropagateErr - A logical indicating if error propgation is activated when using operators.
Dependent Properties 
These dependent properties allow accessing the image data directly.
Image - Get the image from ImageData.Image
Back - BackData.Data
Var - VarData.Data
Mask - MaskData.Data
Header - HeaderData.Data
Key - HeaderData.Key
Cat - CatData.Data

DETAILED text about important and non-trivial properties:
non-trivial property (H3)

Additional & Hidden Properties 
Relations - sets the relation between the dependent property and the data property.

Constructor
The AstroImage constructor is used to generate new objects from input images or from a list of files.
Some examples:
% create a single object and supply the science image
AI = AstroImage({ones(10,10)})
% create a 2 by 2 array of empty images
AI = AstroImage([2 2]);
% set the HDU number and provide the background images
AI = AstroImage(FileNames,'HDU',1,'Back',FileNamesBack,'BackHDU',1);
% Provide the variance image and scale
AI = AstroImage({rand(10,10)},'var',{rand(5,5)},'VarScale',2);

Setters and getters
.....
Static methods
imageIO2AstroImage - Convert an ImageIO object into an AstroImage object.
readImages2AstroImage - Create AstroImage object and read images into a specific property.
unitTest - unitTest for AstroImage.
Methods
General methods
isemptyImage - Check if data images in AstroImage object are empty.
sizeImage - Return the size of images in AstroImage object.

maskSet - Set the value of a bit in a bit mask (Maskdata) in AstroImage.
Header Related Methods
isImType - Check if header IMTYPE keyword value equal some type.
julday - Return the Julian day for AstroImage object.
getStructKey - Get multiple keys from headers in multiple AstroImage and store in a structure array.
Convert data types
astroImage2ImageComponent - Convert an AstroImage data into SciImage, BackImage, etc. objects.
astroImage2AstroCatalog - Convert the CataData in AstroImage object into an AstroCatalog object array.
cast - Cast the image/back/var data in AstroImage (transform to a new type).
object2array - Convert an AstroImage object that contains scalars into an array.
Operate functions on AstroImage sub-classes
These methods activate functions that belong to the AstroImage properties.
funCat - Apply function of Cat properties in AstroImage array.
funHeader - Apply function of HeaderData properties in AstroImage array.
funHeaderScalar - Apply function that return a scalar on HeaderData properties in AstroImage array.
funWCS - Apply function of WCS properties in AstroImage array.
funPSF - Apply function of PSF properties in AstroImage array.
Examples
AI = AstroImage({rand(10,10), rand(10,10)});
funHeader(AI,@insertKey,{'GAIN',2,''});
Operators
funUnary - Apply an unary function on AstroImage object.
funUnaryScalar - Apply a unary operator that return scalar on AstroImage and return an numeric array
funBinary - Apply a binary operator to AstroImage
funBinaryProp - Apply binary function on a single property of AstroImage
funBinaryImVar - Apply a binary operator with error propagation to the ImageData and VarData.
crop - crop an AstroImage images and catalogs and update WCS
plus - Apply the plus operator between AstroImage objects.
minus - Apply the minus operator between AstroImage objects.
times - Apply the times operator between AstroImage objects.
rdivide - Apply the rdivide operator between AstroImage objects.
conv - Convolve images with their PSF, or another PSF
filter - Filter images with their PSF, or another PSF
Examples
% add images
AI = AstroImage({ones(3,3)});
AI2 = AstroImage({ones(3,3)});
AI = funBinary(AI,3,@plus); % eq. to AI = AI+3;
Result = funBinary(AI,AI2,@plus); % eq. to Result = AI+AI2;
% subtract only the back image
AI = AstroImage({ones(3,3)},'Back',{2*ones(3,3)});
AI2 = AstroImage({ones(3,3)},'Back',{ones(3,3)});
Result = funBinaryProp(AI,AI2,@minus,'DataProp','BackData');
