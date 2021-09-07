# Package: imProc.dark


### imProc.dark.bias

Generate a super bias image from a s et of bias images.


### imProc.dark.compare2template

Compare AstroImage to a template and variance and flag image which are different than the template.


### imProc.dark.debias

Subtract bias (and construct if needed) from a list of images


### imProc.dark.identifyFlaringPixels

Identify flaring pixels in a cube of images Searched by looking at (Cube-Mean)/Std>Threshold


### imProc.dark.identifySimilarImages

Search for sucessive images with a fraction of identical pixel values This is useful in order to identify problems with the detector firmware (i.e., some regions of the detector in two surcessive images are identical due to a readout


### imProc.dark.isBias

Check and validate that a set of images in an AstroImage object are bias images


### imProc.dark.isDark

Check and validate that a set of images in an AstroImage object are dark images


### imProc.dark.overscan

Create overscan images and optionally subtract from images


