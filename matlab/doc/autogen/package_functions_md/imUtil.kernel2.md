# Package: imUtil.kernel2


### imUtil.kernel2.annulus

create a matrix or a cube of 2D normalized annulus shape in each image Package: +imUtil.kernel2


    
    create a matrix or a cube of 2D normalized annulus shape in each image  
    Package: +imUtil.kernel2  
    Input  : - (Radii) A two column matrix of [Inner Outer] radius of the  
    annulus.  
    If not provided then the default is [7 11].  
    - Stamp size [X,Y]. Default is max(radii.*2+1)  
    - [X,Y] Position of the annulus center in the stamp.  
    Default is the ceil(stamp_size/2).  
    Output : - A matrix or a cube with the 2D annulus which sum is  
    normalized to 1.  
    If a cube, the third dimension corresponds to the template  
    index.  
    By: Eran O. Ofek                         May 2020  
    Example: imUtil.kernel2.annulus  
      
      
### imUtil.kernel2.circ

create a matrix or a cube of 2D normalized corcular shape in each image Package: +imUtil.kernel2


    
    create a matrix or a cube of 2D normalized corcular shape in each image  
    Package: +imUtil.kernel2  
    Input  : - (Radius) Vector of radii of circles.  
    If not provided then the default is [3].  
    - Stamp size [X,Y]. Default is [7 7].  
    - [X,Y] Position of the circle center in the stamp.  
    Default is the ceil(stamp_size/2).  
    Output : - A matrix or a cube with the 2D circle which sum is  
    normalized to 1.  
    If a cube, the third dimension corresponds to the template  
    index.  
    By: Eran O. Ofek                         May 2020  
    Example: imUtil.kernel2.circ  
      
      
### imUtil.kernel2.gauss

create a matrix or a cube of 2D normalized Gaussians Package: +imUtil.kernel2


    
    create a matrix or a cube of 2D normalized Gaussians  
    Package: +imUtil.kernel2  
    Input  : - (Sigma) A one column or three column matrix.  
    One row per Gaussian template (in the output cube).  
    The columns are: (SigmaX, [SigmaY, Rho]]).  
    SigmaX is the gaussian sigma in the X direction (=FWHMx/2.35)  
    If three elements are provided then the second element is  
    SigmaY (sigma in the Y direction), and the third is the corr.  
    coef. Rho.  
    If one element is given, then the default is SigmaY=SigmaX,  
    and Rho=0.  
    If not provided then the default is [2 2 0].  
    - Stamp size [X,Y]. Default is [15 15].  
    - [X,Y] Position of the Gaussian center in the stamp.  
    Default is the ceil(stamp_size/2).  
    Output : - A matrix or a cube with the 2D Gaussian which sum is  
    normalized to 1.  
    If a cube, the third dimension corresponds to the template  
    index.  
    By: Eran O. Ofek                         Apr 2020  
    Example: imUtil.kernel2.gauss  
    imUtil.kernel2.gauss([1;2;3;4]);  a template bank of Gaussians.  
      
      
### imUtil.kernel2.lanczos

create a matrix or a cube of 2D normalized lanczis filter in each image Package: +imUtil.kernel2


    
    create a matrix or a cube of 2D normalized lanczis filter in each image  
    Package: +imUtil.kernel2  
    Input  : - (A) A vector of the Lanczos parameters a=2 or a=3.  
    If not provided then the default is [2].  
    - Stamp size [X,Y]. Default is [7 7].  
    - [X,Y] Position of the circle center in the stamp.  
    Default is the ceil(stamp_size/2).  
    In order to generate an interpolation shift filter by sub  
    pixel phi, PosXY should be: ceil(stamp_size/2)+phi.  
    Output : - A matrix or a cube with the 2D Lanczos filter which sum is  
    normalized to 1.  
    If a cube, the third dimension corresponds to the template  
    index.  
    By: Eran O. Ofek                         May 2020  
    Example: imUtil.kernel2.lanczos(3,[8 8],[4.5 4.5])  
    imUtil.kernel2.lanczos([3;2],[8 8],[4.5 4.5])  
    imUtil.kernel2.lanczos([3;2],[8 8],[4.5 4.5; 2 2])  
      
      
### imUtil.kernel2.line

create a matrix or a cube of 2D normalized corcular shape in each image Package: +imUtil.kernel2


    
    create a matrix or a cube of 2D normalized corcular shape in each image  
    Package: +imUtil.kernel2  
    Input  : - (Par) A three or four column matrix of  
    [Length, Width, Angle, Gap] of line. Ifthree columns are given  
    then Gap is set to zero.  
    Length is the line length.  
    Width is the line width.  
    Angle is the angle in degrees as measured from the X-axis  
    anti-clock wise.  
    Gap is the radius of a gap in the center of the line.  
    Default is [11 1 0 0];  
    - Stamp size [X,Y]. Default is [11 11].  
    - [X,Y] Position of the circle center in the stamp.  
    Default is the ceil(stamp_size/2).  
    Output : - A matrix or a cube with the 2D lines which sum is  
    normalized to 1.  
    If a cube, the third dimension corresponds to the template  
    index.  
    By: Eran O. Ofek                         May 2020  
    Example: imUtil.kernel2.line  
      
      
### imUtil.kernel2.sersic

create a matrix or a cube of 2D normalized Sersic functions Package: +imUtil.kernel2


    
    create a matrix or a cube of 2D normalized Sersic functions  
    Package: +imUtil.kernel2  
    Input  : -  
      
    (Sigma) A one column or three column matrix.  
    One row per Gaussian template (in the output cube).  
    The columns are: (SigmaX, [SigmaY, Rho]]).  
    SigmaX is the gaussian sigma in the X direction (=FWHMx/2.35)  
    If three elements are provided then the second element is  
    SigmaY (sigma in the Y direction), and the third is the corr.  
    coef. Rho.  
    If one element is given, then the default is SigmaY=SigmaX,  
    and Rho=0.  
    If not provided then the default is [2 2 0].  
    - Stamp size [X,Y]. Default is [21 21].  
    - [X,Y] Position of the Gaussian center in the stamp.  
    Default is the ceil(stamp_size/2).  
    Output : - A matrix or a cube with the 2D Gaussian which sum is  
    normalized to 1.  
    If a cube, the third dimension corresponds to the template  
    index.  
    By: Eran O. Ofek                         May 2020  
    Example: imUtil.kernel2.sersic  
      
      
