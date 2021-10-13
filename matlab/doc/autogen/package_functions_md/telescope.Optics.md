# Package: telescope.Optics


### telescope.Optics.airy

Calculate the monochromatic Airy function (circular diffraction) Package: telescope.Optics Description: Calculate the theoretical diffraction pattern for a perfect circular aperture.


    
    Calculate the monochromatic Airy function (circular diffraction)  
    Package: telescope.Optics  
    Description: Calculate the theoretical diffraction pattern for a perfect  
    circular aperture.  
    Input  : - Aperture radius [cm].  
    - Wavelength [cm].  
    - (Vector of) angle for optical axis [radians].  
    - Intensity, at optical axis, default is 1.  
    Output : - Intensity at angle theta from the optical axis.  
    Tested : Matlab 7.3  
    By : Eran O. Ofek                    Jan 2008  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Reliable: 2  
    -  
      
### telescope.Optics.noll_index

Return the Zernike index from the Noll index. Package: telescope.Optics Description: Calculate the Zernike indices given the Noll index and visa versa.


    
    Return the Zernike index from the Noll index.  
    Package: telescope.Optics  
    Description: Calculate the Zernike indices given the Noll index and  
    visa versa.  
    Input  : * Either j (the Noll index) or two parameters n and m.  
    Output : * Either n and m or j (the Noll index).  
    Tested : Matlab R2011b  
    By : Eran O. Ofek                    Jan 2014  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: [n,m]=noll_index([1;6;8;100]);  
    [j]=noll_index([0;5;22],[0;-3;20]);  
    Reliable: 2  
      
### telescope.Optics.phase_complex

Return the phase of complex numbers. Package: telescope.Optics Description: Given an N-D array of complex numbers, return the phase of each complex number (i.e., atan2(imag(Matrix),real(Matrix))).


    
    Return the phase of complex numbers.  
    Package: telescope.Optics  
    Description: Given an N-D array of complex numbers, return the phase of  
    each complex number  
    (i.e., atan2(imag(Matrix),real(Matrix))).  
    Input  : - An array of complex numbers..  
    Output : - The phases of the complex numbers.  
    Tested : Matlab R2011b  
    By : Eran O. Ofek                    Jan 2014  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: Phase=phase_complex(Matrix);  
    Reliable: 2  
      
      
### telescope.Optics.refraction_index

Return the refraction index as a function of wavelength Package: telescope.Optics Description: Return the refraction index as a function of wavelength for various materials.


    
    Return the refraction index as a function of wavelength  
    Package: telescope.Optics  
    Description: Return the refraction index as a function of wavelength  
    for various materials.  
    Input  : -  
    * Arbitrary number of pairs of arguments: ...,keyword,value,...  
    where keyword are one of the followings:  
    'T' - Temperature [C]. Default is 20.  
    'Material' - Options are:  
    'SiO2' (fused silica) default.  
    Output : - Wavelength [Ang].  
    - Refraction index  
    - [Wave[Ang], Transmission per 1cm thickness]  
    License: GNU general public license version 3  
    By : Eran O. Ofek                    Oct 2019  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: [L,n,t]=telescope.Optics.refraction_index('Material','SiO2')  
    Reliable:  
      
      
      
      
### telescope.Optics.telescope_support

Construct an image of a telescope entrance pupil (support). Package: telescope.Optics Description: Construct an image of a telescope entrance pupil (support). For example, a clear circular pupil.


    
    Construct an image of a telescope entrance pupil (support).  
    Package: telescope.Optics  
    Description: Construct an image of a telescope entrance pupil (support).  
    For example, a clear circular pupil.  
    Input  : - Size of matrix to construct (e.g., [128 128]).  
    Default is [128 128].  
    - One of the following support types:  
    'circ'    - Circular support (default).  
    'circobs' - Circular support with central obscuration.  
    'rect'    - Rectangular support.  
    - Additional parameters describing the support.  
    For 'circ' - No parameters.  
    For 'circobs' - A parameter describing the radius of the  
    central obscuration in units of the  
    aperture.  
    For 'rect' - The ratio between the axis.  
    Default is 0.15.  
    Output : - A matrix of the support.  
    - Vector of X-axis grid points.  
    - Vector of Y-axis grid points.  
    Tested : Matlab R2011b  
    By : Eran O. Ofek                    Feb 2014  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: [Support,VecX,VecY]=telescope_support([128 128]);  
    Reliable: 2  
      
      
### telescope.Optics.wavefront2image

Construct an image from the wavefront Package: telescope.Optics Description: Construct a wavefront from a matrix representation of the wavefront, and calculate the image plane of a point source with a given support (obstraction).


    
    Construct an image from the wavefront  
    Package: telescope.Optics  
    Description: Construct a wavefront from a matrix representation of the  
    wavefront, and calculate the image plane of a point source  
    with a given support (obstraction).  
    Input  : - Matrix which represent the amplitude of the wavefront in each  
    point in the pupil plane. The pupil plane range is always  
    between -1 and 1 in both axes.  
    For example, this is the SumY matrix returned by  
    zerwavefront2image.m.  
    Default is ones(128,128).  
    - Telescope support. This is a matrix of the telescope  
    support (obscurations). The size of this matrix should be  
    identical to the size of the X-Y grid. Alternatively,  
    this can be one of the following strings:  
    'circ' - generate a circular support. Default.  
    - Over sampling. Default is 2 (i.e., Nyquist sampling).  
    Always provide sampling larger than 2.  
    The final image pixel scale is lambda/d/OverSamp.  
    - Normalize the final image plans (Image and Image_NS).  
    Options are:  
    'no'  - do not normalize.  
    'sum' - Normalize by the final image sum. Default.  
    - Optional supports parameters. See telescope_support.m  
    Default is 0.15.  
    Output : - Image plane before the fft shift.  
    - Image plane after the fft shift (source is in the center).  
    Tested : Matlab R2011b  
    By : Eran O. Ofek                    Feb 2014  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    See also: zerwavefront2image_indiv.m, zerwavefront2image.m  
    Example:  generate Airy function  
    [Image_NS,Image]=wavefront2image([],'circ',4);  
    pcolor(log10(Image)), shading interp; axis square, colorbar  
    Reliable: 2  
      
      
### telescope.Optics.zer_cj_variance

The expectency of std of the atmospheric Zernike coefficients. Package: telescope.Optics Description: Return the expectency of standard deviation of the atmospheric Zernike c_j coefficients.


    
    The expectency of std of the atmospheric Zernike coefficients.  
    Package: telescope.Optics  
    Description: Return the expectency of standard deviation of the  
    atmospheric Zernike c_j coefficients.  
    Input  : - Maximum J. Default is 100.  
    * Arbitrary number of pairs or arguments: ...,keyword,value,...  
    where keyword are one of the followings:  
    'D'  - Telescope diamater [cm]. Default is 100.  
    'r0' - Fried length [cm]. Default is 10.  
    'Model' - The Zernike coefficient model to use:  
    'zpl' - Power-law appropriate for Zenike functions  
    (default).  
    'pl'  - pure power law.  
    'Nrand' - Number of random relaizations of C to generate.  
    Default is 100.  
    Output : - Expectency of standard deviation of the atmospheric  
    Zernike c_j coefficients.  
    - Vector of J-s (i.e., (1:1:MaxJ)).  
    - Random realizations of C_j. Realization per raw.  
    Tested : Matlab R2011b  
    By : Eran O. Ofek                    May 2014  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: [AmpC,J,C]=telescope.Optics.zer_cj_variance(100);  
    Reliable: 2  
      
      
### telescope.Optics.zernike_xy

A driver function for the external zerfun.m function. Package: telescope.Optics Description: A driver function for the external zerfun.m function. Given a list of zernike indices and a grid in the X-Y plan calculate a cube of the zernike functions. The first


    
    A driver function for the external zerfun.m function.  
    Package: telescope.Optics  
    Description: A driver function for the external zerfun.m function.  
    Given a list of zernike indices and a grid in the X-Y plan  
    calculate a cube of the zernike functions. The first  
    and second dimensions of the cube corresponds to the  
    X and Y axes, while the third dimension to the zernike  
    index.  
    Input  : - Row vector of Zernike n indices. Alternatively, if the  
    second input argument is empty, then this is the Noll index  
    (see noll_index.m).  
    Default is (1:1:10).  
    - Row vector of Zernike m indices. Default is empty.  
    - Vector of X in the X-Y grid. Default is (-1:0.01:1).'.  
    Alternatively, this can be a matrix of X values returned  
    by meshgrid.  
    - Vector of Y in the X-Y grid. Default is (-1:0.01:1).'.  
    Alternatively, this can be a matrix of Y values returned  
    by meshgrid.  
    - Normalization method:  
    'no'   - no normalization.  
    'norm' - Normalized Zernike functions.  
    See zernfun.m for details.  
    Alternatively, if this is a number (of active pixels) than  
    each Zernike function is normalized such that  
    \sum_{x,y}{z^2(x,y)} = N, where N is the number  
    of active pixels. Default.  
    - A flag {true|false} indicating if to use NaN's outside  
    the circle or zero. Default is true (i.e., put NaN's).  
    Output : - A cube of the Zernike functions. The first  
    and second dimensions of the cube corresponds to the  
    X and Y axes, while the third dimension to the zernike  
    index.  
    Tested : Matlab R2011b  
    By : Eran O. Ofek                    Jan 2014  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: Zxy=zernike_xy; surface(sum(Zxy,3)); shading interp  
    Reliable: 2  
      
      
### telescope.Optics.zerwavefront2image

Wavefront from Zernike polynomials and calculate the image plane. Package: telescope.Optics Description: Construct a wavefront using Zernike polynomials, and calculate the image plane of a point source with a given support (obstraction). This function sum all the


    
    Wavefront from Zernike polynomials and calculate the image plane.  
    Package: telescope.Optics  
    Description: Construct a wavefront using Zernike polynomials, and  
    calculate the image plane of a point source with a  
    given support (obstraction). This function sum all the  
    zernike function in the pupil plane and generate the  
    corresponding image plane.  
    Input  : - Row vector of Zernike n indices. Alternatively, if the  
    second input argument is empty, then this is the Noll index  
    (see noll_index.m).  
    Default is (1:1:10).  
    - Row vector of Zernike m indices. Default is empty.  
    - Amplitude corresponding to each [n,m] value.  
    Default is ones(size(n)).  
    - Vector of X in the X-Y grid. Default is (-1:1./63.5:1).'.  
    If scalar then this is the number of elements in the vector.  
    - Vector of Y in the X-Y grid. Default is (-1:1./63.5:1).'.  
    If scalar then this is the number of elements in the vector.  
    - Telescope support. This is a matrix of the telescope  
    support (obscurations). The size of this matrix should be  
    identical to the size of the X-Y grid. Alternatively,  
    this can be one of the following strings:  
    'circ' - generate a circular support. Default.  
    - Over sampling. Default is 2 (i.e., Nyquist sampling).  
    Always provide sampling larger than 2.  
    The final image pixel scale is lambda/d/OverSamp.  
    - Normalize the final image plans (Image and Image_NS).  
    Options are:  
    'no'  - do not normalize.  
    'sum' - Normalize by the final image sum. Default.  
    Output : - Image plane before the fft shift.  
    - Image plane after the fft shift.  
    - The sum of the Zernike polynomials weighted by their  
    amplitudes.  
    Tested : Matlab R2011b  
    By : Eran O. Ofek                    Jan 2014  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    See also: zerwavefront2image_indiv.m, wavefront2image.m  
    Example: [Image_NS,Image,SumY]=telescope.Optics.zerwavefront2image;  
    To generate a realistic spackle image generated  
    by the atmosphere:  
    Note that the size of the seeing disk in this example is  
    (D/r0)*OverSamp:  
    J = (1:1:100); D=100; r0=5;  
    [AmpC,J,C]=telescope.Optics.zer_cj_variance(100,'Nrand',1,'D',D,'r0',r0);  
    [Image_NS,Image,SumY]=telescope.Optics.zerwavefront2image(J,[],C);  
    pcolor(log10(Image)), shading interp; axis square, colorbar  
    Reliable: 2  
      
      
### telescope.Optics.zerwavefront2image_indiv

Wavefront from individual Zernike polynomials and calculate the image. Package: telescope.Optics Description: Construct a wavefront using Zernike polynomials, and calculate the image plane of a point source with a given support (obstraction).


    
    Wavefront from individual Zernike polynomials and calculate the image.  
    Package: telescope.Optics  
    Description: Construct a wavefront using Zernike polynomials, and  
    calculate the image plane of a point source with a  
    given support (obstraction).  
    However, unlike wavefront.m this function works on  
    individual Zernike polynomials and return a cube in  
    which each the the third dimension corresponds to  
    the image plane of an individual Zernike function.  
    Input  : - Row vector of Zernike n indices. Alternatively, if the  
    second input argument is empty, then this is the Noll index  
    (see noll_index.m).  
    Default is (1:1:10).  
    - Row vector of Zernike m indices. Default is empty.  
    - Amplitude corresponding to each [n,m] value.  
    Default is ones(size(n)).  
    - Vector of X in the X-Y grid. Default is (-1:1./63.5:1).'.  
    If scalar then this is the number of elements in the vector.  
    - Vector of Y in the X-Y grid. Default is (-1:1./63.5:1).'.  
    If scalar then this is the number of elements in the vector.  
    - Telescope support. This is a matrix of the telescope  
    support (obscurations). The size of this matrix should be  
    identical to the size of the X-Y grid. Alternatively,  
    this can be one of the following strings:  
    'circ' - generate a circular support. Default.  
    - Over sampling. Default is 2 (i.e., Nyquist sampling).  
    Always provide sampling larger than 2.  
    The final image pixel scale is lambda/d/OverSamp.  
    - Normalize the final image plans (Image and Image_NS).  
    Options are:  
    'no'   - do not normalize.  
    'sum'  - Normalize by the final image cube sum.  
    'sum1' - Normalize by the individual images sum. Default.  
    Output : - Image plane before the fft shift.  
    - Image plane after the fft shift.  
    - The sum of the Zernike polynomials weighted by their  
    amplitudes.  
    Tested : Matlab R2011b  
    By : Eran O. Ofek                    Jan 2014  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    See also: zerwavefront2image.m, wavefront2image.m  
    Example: [Image_NS,Image,SumY]=zerwavefront2image_indiv;  
    To generate a realistic speckle image generated  
    by the atmosphere:  
    Note that the size of the seeing disk in this example is  
    (D/r0)*OverSamp:  
    [AmpC,J,C]=zer_cj_variance(100,'Nrand',1);  
    [Image_NS,Image,SumY]=zerwavefront2image_indiv(J,[],ones(1,100));  
    pcolor(log10(Image)), shading interp  
    axis square, colorbar  
    Reliable: 2  
      
      
