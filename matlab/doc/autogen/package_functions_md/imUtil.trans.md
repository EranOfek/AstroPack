# Package: imUtil.trans


### imUtil.trans.extrapolate2d

Example: imUtil.trans.extrapolate2d([1 1 1000 1000],[1 1000 1 1000],[1 1 1000 1000].*2 + 1)


    
      
    Example: imUtil.trans.extrapolate2d([1 1 1000 1000],[1 1000 1 1000],[1 1 1000 1000].*2 + 1)  
      
      
### imUtil.trans.fit_transformation

Package: mUtil.trans Description: In a 2-D image interpolate over NaNs or over pixels in which a bit mask has a specific bit equal true.


    
      
    Package: mUtil.trans  
    Description: In a 2-D image interpolate over NaNs or over pixels in  
    which a bit mask has a specific bit equal true.  
    Input  : -  
    * Arbitrary number of pairs of input arguments ...,key,val,...  
      
    Output : - Interpolated image  
    Tested : Matlab R2011b  
    By : Eran O. Ofek                    Feb 2014  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example:  
      
      
      
### imUtil.trans.imTransform

Image transformation Example: TC=Tran2D; TC.ParY=zeros(1,13);  TC.ParX=zeros(1,13); TC.ParX(1:2) = 1; TC.ParX(5)=0.03; TC.ParX(7)=0.01; TC.ParY(1) = 2; TC.ParY(3)=1.01; TC.ParY(5)=0.01; TC.ParY(8)=0.001;


    
    Image transformation  
      
    Example: TC=Tran2D; TC.ParY=zeros(1,13);  TC.ParX=zeros(1,13);  
    TC.ParX(1:2) = 1; TC.ParX(5)=0.03; TC.ParX(7)=0.01;  
    TC.ParY(1) = 2; TC.ParY(3)=1.01; TC.ParY(5)=0.01; TC.ParY(8)=0.001;  
    Image = rand(1000,1000);  
    OutImage = imUtil.trans.imTransform(Image, TC);  
      
      
### imUtil.trans.iminterp

Interpolate a 2D image over NaNs or mask image Package: mUtil.trans Description: In a 2-D image interpolate over NaNs or over pixels in which a bit mask has a specific bit equal true.


    
    Interpolate a 2D image over NaNs or mask image  
    Package: mUtil.trans  
    Description: In a 2-D image interpolate over NaNs or over pixels in  
    which a bit mask has a specific bit equal true.  
    Input  : - Image.  
    - Mask Image. This is either a bit mask, or an array of  
    logicals (see 'Bit' for details). Default is empty.  
    Will interpolate in any pixels >0.  
    If empty, then will interpolate over NaN pixels.  
    Default is [].  
    Using the Util.external.inpaint_nans function.  
    * Arbitrary number of pairs of input arguments ...,key,val,...  
    The following keywords are available:  
    'Bit' - The index of a bit in the bit mask (second input  
    argument) which to select pixels over which  
    interpolation is required.  
    Default is empty. If empty then assume that the Mask  
    is a 2-D array of logicals set to true in pixels  
    over which interpolation is required.  
    'IntMethod'- inpaint_nans.m interpolation method. Default is 0.  
    Output : - Interpolated image  
    Tested : Matlab R2011b  
    By : Eran O. Ofek                    Feb 2014  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: [MatX,MatY]=meshgrid((1:1:5),(1:1:5)); A=MatX.^2+sqrt(MatY);  
    A(3,3)=NaN; A(2,3)=NaN;  
    InterpImage=imUtil.trans.iminterp(A);  
    [MatX,MatY]=meshgrid((1:1:5),(1:1:5)); A=MatX.^2+sqrt(MatY);  
    Flag=false(5,5); A(3,3)=true;  
    InterpImage=imUtil.trans.iminterp(A,Flag);  
    BitMask=zeros(5,5,'uint16'); BitMask(3,4)=1;  
    InterpImage=imUtil.trans.iminterp(A,BitMask,'Bit',1);  
    Reliable: 2  
      
      
### imUtil.trans.shift_fft

Shift Image using the sub pixel Fourier shift theorem (sinc interp.) Package: imUtil.image Description: Shift an image using the FFT shift thorem. This works well when the image does not contain sharp artifacts. Sharp artifacts will produce ringing.


    
    Shift Image using the sub pixel Fourier shift theorem (sinc interp.)  
    Package: imUtil.image  
    Description: Shift an image using the FFT shift thorem. This works well  
    when the image does not contain sharp artifacts.  
    Sharp artifacts will produce ringing.  
    Note that the shift is defined on the content of the image,  
    rather than the image boundries - e.g., the stars will be  
    shifted in the requested direction.  
    Input  : - An image (2D matrix).  
    - X shift to apply to input image.  
    - Y shift to apply to input image.  
    - NY (supply for faster performences). See output.  
    - NX (supply for faster performences). See output.  
    - Nr (supply for faster performences). See output.  
    - Nc (supply for faster performences). See output.  
    Output : - Shifted image with the same size as the input image.  
    - NY  
    - NX  
    - Nr  
    - Nc  
    See also: ImUtil.Im.imagefft_shift_fft.m, SIM/image_shift_fft.m,  
    SIM/imagefft_shift_fft.m  
    License: GNU general public license version 3  
    Tested : Matlab R2015b  
    By : Eran O. Ofek                    May 2016  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: ShiftedImage=imUtil.trans.shift_fft(Image,1.22,-3.1);  
    Reliable: 2  
      
      
### imUtil.trans.shift_lanczos

Shift Image using the sub pixel Lanczos filter Package: imUtil.image Description: Shift an image using the Lanczos interpolation kernel. This is works for sub pixel shifts and can treat images which are larger then 9x9.


    
    Shift Image using the sub pixel Lanczos filter  
    Package: imUtil.image  
    Description: Shift an image using the Lanczos interpolation kernel.  
    This is works for sub pixel shifts and can treat images  
    which are larger then 9x9.  
    Input  : - An image (2D matrix), or a cube of images in which the image  
    index is the 3rd dimension.  
    - A two element vector of [ShiftX, ShiftY] in pixels.  
    If the first input is a cube, then this may be a two column  
    matrix with shift per image (in each row).  
    Otherwise all the images will be shifted by the same shift.  
    - Lanczos parameters (e.g., 2 or 3). Default is 3.  
    - IsCirc - use circular shift (true), or non (false).  
    Default is false.  
    - Pad value for non circular shift. Default is 0.  
    Output : - Shifted image with the same size as the input image.  
    License: GNU general public license version 3  
    Tested : Matlab R2015b  
    By : Eran O. Ofek                    Jun 2020  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: G=imUtil.kernel2.gauss(2,[31 31]);  
    imUtil.image.moment2(G,16,16)  
    ShiftedImage=imUtil.trans.shift_lanczos(G,[1.22,-2.1],3,'circ');  
    imUtil.image.moment2(ShiftedImage,16,16)  
    ShiftedImage=imUtil.trans.shift_lanczos(G,[1.22,-2.1],3,'noncirc');  
    imUtil.image.moment2(ShiftedImage,16,16)  
    Reliable:  
      
