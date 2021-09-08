# Package: astro.lensing


### astro.lensing.alpha_kspl_fast

Deflection for softened power law elliptical density Package: AstroUtil.lensing Description: Calculate deflection for softened power law elliptical density of the form:


    
    Deflection for softened power law elliptical density  
    Package: AstroUtil.lensing  
    Description: Calculate deflection for softened power law elliptical  
    density of the form:  
    kappa = 0.5*b^(2-alpha)/((s^2 + r^2)^(1-0.5*alpha))  
    Input  : - position in which to calculate deflection and  
    magnification [Theta_x,Theta_y], in pixels.  
    - X position of center of the mass density (pixels)  
    - Y position of center of the mass density (pixels)  
    - eccentricity of the potential = sqrt(1-B^2/A^2)  
    - position angle of the mass distribution elipse [radians].  
    - softend core.  
    - Power law  
    - Normalization  
    Output : - X Deflection  
    - Y Deflection  
    - Jacobian matrix dAlpha/dTheta _11  
    - Jacobian matrix dAlpha/dTheta _22  
    - Jacobian matrix dAlpha/dTheta _12  
    Tested : Matlab 7.0  
    By : Eran O. Ofek                    Mar 2005  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Reference : Keeton 2002 p.7  
    Example: [AlphaX,AlphaY] = alpha_gnfw([ThetaX, ThetaY],...  
    ModelPars(Icomp,ColX0),...  
    ModelPars(Icomp,ColY0),...  
    ModelPars(Icomp,ColE),...  
    ModelPars(Icomp,ColPA),...  
    ModelPars(Icomp,ColS),...  
    ModelPars(Icomp,ColGamma),...  
    ModelPars(Icomp,ColNorm));  
    Needed : compile alpha_kspl_fast_mp.f  alpha_kspl_fast_sp.f  
    mex alpha_kspl_fast_mp.f  
    mex alpha_kspl_fast_sp.f  
      
      
### astro.lensing.alpha_sis

Gravitational deflection for softend spherical isothermal sphere Package: AstroUtil.lensing Description: Calculate the gravitational lensing deflection angle for a softend isothermal sphere (SIS).


    
    Gravitational deflection for softend spherical isothermal sphere  
    Package: AstroUtil.lensing  
    Description: Calculate the gravitational lensing deflection angle for a  
    softend isothermal sphere (SIS).  
    Input  : - Image plane position [ThetaX, ThetaY] in which to calculate  
    the deflections  
    - Vector of X0 of point lenses positions.  
    - Vector of Y0 of point lenses positions.  
    - Softend core./core radius  
    - Normaliztaion. In the limit of singular (s=0) and spherical  
    (q=1) model, the normalization is the Einstein radius of the  
    model, and its related to the 1-d velocity dispersion \sigma by:  
    4*pi*(\sigma / c)^2 (Dls/Ds)  
    Output : - Deflection of x component (AlphaX).  
    - Deflection of y component (AlphaY).  
    - Jacobian matrix A_11 term.  
    - Jacobian matrix A_22 term.  
    - Jacobian matrix A_12 term.  
    - Gravitational potential Phi.  
    Tested : Matlab 7.0  
    By : Eran O. Ofek                    May 2005  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Reliable: 2  
      
      
      
### astro.lensing.alpha_spl

Gravitational deflection of softened power law elliptical potential Package: AsttroUtil.lensing Description: Calculate gravitational lensing deflection and magnification tensor for softened power law elliptical potential of the


    
    Gravitational deflection of softened power law elliptical potential  
    Package: AsttroUtil.lensing  
    Description: Calculate gravitational lensing deflection and magnification  
    tensor for softened power law elliptical potential of the  
    form: phi = b(s^2 + x^2 + y^2/q^2)^(alpha/2) - b/s^alpha  
    Input  : - position in which to calculate deflection and  
    magnification [Theta_x,Theta_y], in pixels.  
    - X position of center of the mass density (pixels)  
    - Y position of center of the mass density (pixels)  
    - eccentricity of the potential = 1-sqrt(1-B^2/A^2)  
    - position angle of the mass distribution elipse [radians].  
    - softend core.  
    - Power law  
    - Normalization  
    Output : - X Deflection  
    - Y Deflection  
    - Mapping matrix term A_11  
    - Mapping matrix term A_12  
    - Mapping matrix term A_12  
    - The potential  
    - Kappa, the two dimensional density  
    Tested : Matlab 6.5  
    By : Eran O. Ofek & Keren Sharon     Mar 2005  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Reference : Keeton 2002 p.7  
    Example: [AlphaX,AlphaY] = alpha_gnfw([ThetaX, ThetaY],...  
    ModelPars(Icomp,ColX0),...  
    ModelPars(Icomp,ColY0),...  
    ModelPars(Icomp,ColE),...  
    ModelPars(Icomp,ColPA),...  
    ModelPars(Icomp,ColS),...  
    ModelPars(Icomp,ColGamma),...  
    ModelPars(Icomp,ColNorm));  
    Reliable: 2  
      
      
      
### astro.lensing.beta_minimize

- beta_minimize function                                                glens Description: Given a model and approximate source position, find the best source position that minimize the


    
    -  
    beta_minimize function                                                glens  
    Description: Given a model and approximate source position,  
    find the best source position that minimize the  
    residuals in the image plane.  
    Input  : - Source position to start with [BetaX, BetaY].  
    - ModelPars (see calc_alpha.m).  
    - ModelType (see calc_alpha.m).  
    - Observed images position [ThetaX, ThetaY],  
    or [X, Y, ErrX, ErrY] (in that case return chi2 instead of RMS).  
    - Dls/Ds  
    - Minimization method:  
    'Direct'   - direct search using fminsearch.  
    Output : - Best source position that minimize the residuals in the image  
    plane [BetaX, BetaY].  
    - RMS or chi2.  
    - Difference between observed and calculated (best) image position,  
    x followed by y.  
    - Calculated (best) images position.  
    Tested : Matlab 7.0  
    By : Eran O. Ofek              April 2005  
    URL : http://wise-obs.tau.ac.il/~eran/matlab.html  
    -  
      
### astro.lensing.calc_magnification

Magnification from mapping matrix Package: AstroUtil.lensing Description: Given the gravitational lensing mapping matrix d\alpha/d\theta, calculate the magnification.


    
    Magnification from mapping matrix  
    Package: AstroUtil.lensing  
    Description: Given the gravitational lensing mapping matrix  
    d\alpha/d\theta, calculate the magnification.  
    Input  : - Mapping matrix, A_11, d\alpha/d\theta _11.  
    This can be a scalar, vector or matrix.  
    - Mapping matrix, A_22, d\alpha/d\theta _22.  
    This can be a scalar, vector or matrix.  
    - Mapping matrix, A_12, d\alpha/d\theta _12.  
    This can be a scalar, vector or matrix.  
    Output : - Magnification at each \theta.  
    Tested : Matlab 7.0  
    By : Eran O. Ofek                    May 2007  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
      
      
### astro.lensing.dls_ds_2z

Given D_ls/D_s ratio, z_l and cosmological parameters, solve for z_s. Package: AstroUtil.lensing Description: Given D_ls/D_s ratio, z_l and cosmological parameters, solve for z_s.


    
    Given D_ls/D_s ratio, z_l and cosmological parameters, solve for z_s.  
    Package: AstroUtil.lensing  
    Description: Given D_ls/D_s ratio, z_l and cosmological  
    parameters, solve for z_s.  
    Input  : - Vector of D_ls/D_s ratio.  
    - Vector of z_l  
    - Matrix of cosmological parameters [OmegaM, OmegaL].  
    Output : - Vector of z_s.  
    Tested : Matlab 6.5  
    By : Eran O. Ofek                    Feb 2005  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
      
      
### astro.lensing.find_images_regions

- find_images_regions function                                          glens Description:  Given a mass model, its parameters and a source position, search for images of


    
    -  
    find_images_regions function                                          glens  
    Description:  Given a mass model, its parameters and a  
    source position, search for images of  
    the source only in a predefind given regions  
    or starting points.  
    Input  : - A single source positions [X, Y].  
    - Model parameters (see: calc_alpha.m).  
    - Model type (see: calc_alpha.m).  
    - A single source positions [X, Y].  
    - "Guess" images position to start the search in, [X, Y]  
    or [X, Y, ErrX, ErrY] (in that case return chi2 instead of RMS).  
    - Dls/Ds  
    - Maximum distance to search within -  
    default is NaN.  
    - SearchMethod:  
    'Jacobian'   - Use the Jacobian to converge, default.  
    'Direct'     - Use 'fminsearch' (local minimum)  
    'Scan'       - Scan a region with a resolution given by Resolution.  
    - Convergence threshold for solution in actual units,  
    For 'Jacobian' default is 0.01.  
    For 'Scan'     default is 1.  
    Output : - RMS or Chi2.  
    Return NaN if solution not found within MaxNoIter iterations.  
    - Vector of distances between the observed images position  
    and the calculated images position  
    (x components followed by y components).  
    - Images position and Jacobian, corresponding to the source  
    [X, Y, A_11, A_22, A_12].  
    If an images wasn't found within the search distance  
    (or not converged) set the image position to NaN.  
    - Vector of convergence: 0 - if not converged; 1 - if converged  
    Tested : Matlab 7.0  
    By : Eran O. Ofek        April 2005  
    URL : http://wise-obs.tau.ac.il/~eran/matlab.html  
    -  
### astro.lensing.fit_astrometric_timedelay

SHORT DESCRIPTION HERE Package: AstroUtil.lensing Description:


    
    SHORT DESCRIPTION HERE  
    Package: AstroUtil.lensing  
    Description:  
    Input  : -  
    * Arbitrary number of pairs of arguments: ...,keyword,value,...  
    where keyword are one of the followings:  
    Output : -  
    License: GNU general public license version 3  
    By : Eran O. Ofek                    Jun 2017  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: [Res]=AstroUtil.lensing.fit_astrometric_timedelay  
    Reliable:  
      
      
### astro.lensing.generate_timedelay_lc

Generate random power-law power spectrum light curve and its time delays. Package: astro.lensing Description: Generate random power-law power spectrum light curve, its time delays, total flux, and weighted mean position.


    
    Generate random power-law power spectrum light curve and its time delays.  
    Package: astro.lensing  
    Description: Generate random power-law power spectrum light curve,  
    its time delays, total flux, and weighted mean position.  
    Input  : - Times in which to generate LC.  
    If no parameter is given then default is (1:1:500)'.  
    - A two column matrix of [X Y] lens image positions.  
    If no parameter is given then default is [0 0; 1 0].  
    - A vector of image relative fluxes.  
    If no parameter is given then default is [1 0.5].  
    - Vector of time delay per image.  
    If no parameter is given then default is [0 50].  
    * Arbitrary number of pairs of arguments: ...,keyword,value,...  
    where keyword are one of the followings:  
    'ErrMag'  - Vector of magnitude errors, one per image.  
    'ErrPos'  - Positional error.  
    'GalMag'  - Galaxy magnitude.  
    'GalPos'  - Galaxy [X,Y] position.  
    'GalErrMag' - Galaxy magnitude error.  
    'Cadence' - Observing cadence.  
    'PowerLawInd' - Minus of power-spectrum power-law index.  
    'StdNorm' - Power spectrum std normlization.  
    'InterpMethod' - Interpolation method.  
    'MeanMag' - Mean magnitude of first image.  
    'ZP' - Photometriz zero point [mag].  
    Output : - Vector of times.  
    - Matrix of lensed images flux. Column per image.  
    - Vector of total flux of images and galaxy.  
    - Galaxy flux.  
    - Weighted mean X position of blend.  
    - Weighted mean Y position of blend.  
    License: GNU general public license version 3  
    By : Eran O. Ofek                    Jul 2017  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: [T,F,TotalF,X,Y]=astro.lensing.generate_timedelay_lc  
    Reliable: 2  
      
      
### astro.lensing.integrand_jn_ellkappa

- integrand_jn_ellkappa function                                        glens Description: Calculate the integrand of J_n(x,y), for gravitational lensing softened elliptical mass distribution


    
    -  
    integrand_jn_ellkappa function                                        glens  
    Description: Calculate the integrand of J_n(x,y), for gravitational  
    lensing softened elliptical mass distribution  
    (See Keeton 2001, Eq. 15).  
    Input  : - U, integration parameter  
    - N, function order (e.g., J_n).  
    - X.^2  
    - Y.^2  
    - Q.^2, where Q is the ellipse axis ratio b/a.  
    - Power law slope (Alpha).  
    - S.^2, where S is the core radius.  
    Output : - Value of the integrand  
    Tested : Matlab 6.5  
    By : Eran O. Ofek                     March 2005  
    URL : http://wise-obs.tau.ac.il/~eran/matlab.html  
    Reliable: 2  
    -  
      
    Upsilon^2 (Keeton 2001, Eq. 15)  
### astro.lensing.iplane_rms

iplane_rms function                                                glens Description: Given a model parameters of a lens, and the images position, calculate the best source


    
      
    iplane_rms function                                                glens  
    Description: Given a model parameters of a lens, and the  
    images position, calculate the best source  
    position that minimize the residuals in  
    image plane (see also: iplane_rms_smart.m).  
    Input  : - Model parameters (see calc_alpha.m).  
    - Vector of Model Type (see calc_alpha.m).  
    - Images position cell array:  
    Each cell contains [ThetaX, ThetaY, ErrorX, ErrorY]  
    of the images of a single source [pixels units!].  
    It is recomended that the first image in each list  
    will be the faintest image corresponds to each source.  
    - Vector of Dls/Ds for each source.  
    Output : - Residuals in the image plane (x components for all images  
    of a source followed by the y component, followed  
    by next source...)  
    - RMS or chi2 for best image plane solution.  
    In case image plane solution not attempted set to NaN.  
    - Number of constraints.  
    - Best source position [X, Y].  
    In case the program didn't solve for source position  
    return [NaN, NaN].  
    Tested : Matlab 7.0  
    By : Eran Ofek              April 2005  
    URL : http://wise-obs.tau.ac.il/~eran/matlab.html  
    See Also: iplane_rms_smart.m, iplane_rms_norm.m  
      
      
### astro.lensing.iplane_rms_norm

iplane_rms_norm function                                           glens Description:   Given a model parameters of a lens, and the images position, calculate the best source


    
      
    iplane_rms_norm function                                           glens  
    Description:   Given a model parameters of a lens, and the  
    images position, calculate the best source  
    position that minimize the residuals in  
    image plane (see also: iplane_rms_smart.m).  
      
    This program is a duplicate of iplane_rms.m  
    The only difference is that this program get  
    the lens model normalization (for the first line  
    in the model - the rest of the lines are  
    normalized proportianly) as the first  
    input argument.  
      
    Input  : - Model Normalization (override the 7th column in ModelPars).  
    - Model parameters (see calc_alpha.m).  
    - Vector of Model Type (see calc_alpha.m).  
    - Images position cell array:  
    Each cell contains [ThetaX, ThetaY, ErrorX, ErrorY]  
    of the images of a single source [pixels units!].  
    It is recomended that the first image in each list  
    will be the faintest image corresponds to each source.  
    - Vector of Dls/Ds for each source.  
    Output : - RMS or chi2 for best image plane solution.  
    In case image plane solution not attempted set to NaN.  
    - Residuals in the image plane (x components for all images  
    of a source followed by the y component, followed  
    by next source...)  
    - Number of constraints.  
    - Best source position [X, Y].  
    In case the program didn't solve for source position  
    return [NaN, NaN].  
    Tested : Matlab 7.0  
    By : Eran Ofek              April 2005  
    URL : http://wise-obs.tau.ac.il/~eran/matlab.html  
    See Also: iplane_rms.m, iplane_rms_smart.m  
      
      
### astro.lensing.iplane_rms_smart

iplane_rms_smart function                                          glens Description: Given a model parameters of a lens, and the images position, calculate the best source


    
      
    iplane_rms_smart function                                          glens  
    Description: Given a model parameters of a lens, and the  
    images position, calculate the best source  
    position that minimize the residuals in  
    image plane.  
    The program starts with source plane fitting  
    and pseudo source plane fitting - if  
    successful proceed with image plane fitting.  
    Input  : - Model parameters (see calc_alpha.m).  
    - Vector of Model Type (see calc_alpha.m).  
    - Images position cell array:  
    Each cell contains [ThetaX, ThetaY, ErrorX, ErrorY]  
    of the images of a single source [pixels units!].  
    It is recomended that the first image in each list  
    will be the faintest image corresponds to each source.  
    - Vector of Dls/Ds for each source.  
    Output : - RMS or chi2 for best image plane solution.  
    In case image plane solution not attempted set to NaN.  
    - RMS for pseudo image plane solution.  
    In case image plane solution not attempted set to NaN.  
    - RMS for source plane solution.  
    - Residuals in the image plane (x components for all images  
    of a source followed by the y component, followed  
    by next source...).  
    - Residuals in the pseudo image plane (x components for all images  
    of a source followed by the y component, followed  
    by next source...).  
    In case image plane solution was achived return NaN.  
    - Residuals in the source plane (x components for all images  
    of a source followed by the y component, followed  
    by next source...).  
    In case image plane solution was achived return NaN.  
    - Number of constraints.  
    - Best source position [X, Y].  
    In case the program didn't solve for source position  
    return [NaN, NaN].  
    Tested : Matlab 7.0  
    By : Eran Ofek              April 2005  
    URL : http://wise-obs.tau.ac.il/~eran/matlab.html  
    See Also: iplane_rms.m, iplane_rms_norm.m  
      
      
### astro.lensing.kappa_from_alpha

- kappa_from_alpha function                                             glens Description:   Calculate the surface density of a lens numerically from the deflection field.


    
    -  
    kappa_from_alpha function                                             glens  
    Description:   Calculate the surface density of a lens  
    numerically from the deflection field.  
    Input  : - Matrix of deflection in X (AlphaX) - the deflection is in pixels,  
    assuming d_ls/d_s=1.  
    - Matrix of deflection in Y (AlphaY) - the deflection is in pixels,  
    assuming d_ls/d_s=1.  
    - PixelSize in arcsec (per pixel), default is [0.05 0.05] (HST-ACS).  
    Output : - The surface density of the lens in units of the critical  
    density for lensing.  
    Tested : Matlab 7.0  
    By : Eran O. Ofek               June 2005  
    URL : http://wise-obs.tau.ac.il/~eran/matlab.html  
    Example: Kappa=kappa_from_alpha(AlphaX.*DlsDs,AlphaY.*DlsDs);  
    -  
### astro.lensing.lensmodel_solvenorm

- lensmodel_solvenorm function                                        glens Description:    Given a lens model and images position solve for the best fit normalization


    
    -  
    lensmodel_solvenorm function                                        glens  
    Description:    Given a lens model and images position  
    solve for the best fit normalization  
    (7th column parameter in ModelPars).  
    The program (can) first find the best  
    fit solution in the source plane and then  
    use this to do the fit in the image plane.  
    Input  : - Model parameters (see calc_alpha.m).  
    - Vector of Model Type (see calc_alpha.m).  
    - Images position cell array:  
    Each cell contains [ThetaX, ThetaY, ErrorX, ErrorY]  
    of the images of a single source [pixels units!].  
    It is recomended that the first image in each list  
    will be the faintest image corresponds to each source.  
    - Vector of Dls/Ds for each source.  
    - Solve for normalization using image plane minimization {1 | 0},  
    default is 1 (otherwise use source plane minimization).  
    Output : - Model parameters (see calc_alpha.m) in which the normalization  
    (7th column) is the best fit source plane solution.  
    Tested : Matlab 7.0  
    By : Eran O. Ofek            April 2005  
    URL : http://wise-obs.tau.ac.il/~eran/matlab.html  
    -  
### astro.lensing.plot_lens_data

- plot_lens_data function                                           glens Description: Given Image position and corresponding source position plot the image and source


    
    -  
    plot_lens_data function                                           glens  
    Description: Given Image position and corresponding  
    source position plot the image and source  
    position with connecting lines.  
    Input  : - Image position [X, Y].  
    - Source position [X, Y].  
    - Numerical index for color 1..17  
    Output : null  
    Plot   : Source and images position.  
    Tested : Matlab 6.5  
    By : Eran O. Ofek     January 2005  
    URL : http://wise-obs.tau.ac.il/~eran/matlab.html  
    -  
### astro.lensing.predict_images

predict_images function                                        glens Description:


    
      
    predict_images function                                        glens  
    Description:  
    Input  : - Model parameters or AlphaX  
    - Model Type       or AlphaY  
    - Start position:  
    'I' - InPos is image position.  
    'S' - ImPos is source position.  
    - Image position [X, Y]  
    - ThetaX matrix to search in  
    - ThetaY matrix to search in  
    - Dls/Ds  
    - Image search threshold (distance between sources in source plane).  
    - Color  
    Output : -  
    Tested : Matlab 6.5  
    By : Eran O. Ofek       March 2005  
    URL : http://wise-obs.tau.ac.il/~eran/matlab.html  
      
### astro.lensing.s1plane_rms

s1plane_rms function                                               glens Description: Given a model parameters of a lens, and the images position corresponding to a single source,


    
      
    s1plane_rms function                                               glens  
    Description: Given a model parameters of a lens, and the  
    images position corresponding to a single source,  
    calculate the deflections  
    (and magnification) and the sources position,  
    and the best normalization.  
    The program calculates the rms of the sources  
    position in the source plane.  
    assuming Dls_Ds = 1.  
    Input  : - Model parameters (see calc_alpha.m).  
    - Vector of Model Type (see calc_alpha.m).  
    - Images position [ThetaX, ThetaY, ErrorX, ErrorY]  
    It is recomended that the first image in each list  
    will be the faintest image corresponds to each source.  
    Output : - Residual of each source corresoinding to an image, relative to the  
    mean position of the source.  
    - RMS in the source plane.  
    - Mean source position for each set of images.  
    - Best fit normalization (multiply the input normalization),  
    for each set of images.  
    Tested : Matlab 7.0  
    By : Eran Ofek          April 2005  
    URL : http://wise-obs.tau.ac.il/~eran/matlab.html  
    See Also: splane_rms_norm.m  
      
      
### astro.lensing.splane_rms

splane_rms function                                                glens Description: Given a model parameters of a lens, and the images position, calculate the deflections


    
      
    splane_rms function                                                glens  
    Description: Given a model parameters of a lens, and the  
    images position, calculate the deflections  
    (and magnification) and the sources position.  
    The program calculates the rms of the sources  
    position in the source plane.  
    Input  : - Model parameters (see calc_alpha.m).  
    - Vector of Model Type (see calc_alpha.m).  
    - Images position cell array:  
    Each cell contains [ThetaX, ThetaY, ErrorX, ErrorY]  
    of the images of a single source [pixels units!].  
    It is recomended that the first image in each list  
    will be the faintest image corresponds to each source.  
    - Vector of Dls/Ds for each source.  
    Output : - Residual of each source corresoinding to an image, relative to the  
    mean position of the source.  
    - RMS in the source plane.  
    - Mean source position for each set of images.  
    Tested : Matlab 7.0  
    By : Eran Ofek          April 2005  
    URL : http://wise-obs.tau.ac.il/~eran/matlab.html  
    See Also: splane_rms_norm.m  
      
      
### astro.lensing.splane_rms_n

splane_rms_n function                                              glens Description:       Given a model parameters of a lens, and the images position, calculate the deflections


    
      
    splane_rms_n function                                              glens  
    Description:       Given a model parameters of a lens, and the  
    images position, calculate the deflections  
    (and magnification) and the sources position,  
    and the best normalization.  
    The program calculates the rms of the sources  
    position in the source plane.  
    Input  : - Model parameters (see calc_alpha.m).  
    - Vector of Model Type (see calc_alpha.m).  
    - Images position cell array:  
    Each cell contains [ThetaX, ThetaY, ErrorX, ErrorY]  
    of the images of a single source [pixels units!].  
    It is recomended that the first image in each list  
    will be the faintest image corresponds to each source.  
    - Vector of Dls/Ds for each source.  
    Output : - Residual of each source corresoinding to an image, relative to the  
    mean position of the source.  
    - RMS in the source plane.  
    - Mean source position for each set of images.  
    - Best fit normalization (multiply the input normalization),  
    for each set of images.  
    Tested : Matlab 7.0  
    By : Eran Ofek          April 2005  
    URL : http://wise-obs.tau.ac.il/~eran/matlab.html  
    See Also: splane_rms_norm.m  
      
      
### astro.lensing.splane_rms_norm

splane_rms_norm function   Given a deflection field of a lens, and the images position, calculate the sources position. The program calculates the rms of the sources


    
      
    splane_rms_norm function   Given a deflection field of a lens, and the  
    images position, calculate the sources position.  
    The program calculates the rms of the sources  
    position in the source plane.  
      
    This program is a duplicate of splane_rms_norm1.m  
    The major difference is that this program get  
    the deflection field instead of the model  
    parameters.  
      
    Input  : - Model Normalization relative to the given deflection field.  
    - Cell array of deflection field [AlphaX, AlphaY],  
    corresponding to ImagesCell.  
    - Images position cell array:  
    Each cell contains [ThetaX, ThetaY, ErrorX, ErrorY]  
    of the images of a single source [pixels units!].  
    It is recomended that the first image in each list  
    will be the faintest image corresponds to each source.  
    - Vector of Dls/Ds for each source.  
    Output : - RMS in the source plane.  
    - Residual of each source corresoinding to an image, relative to the  
    mean position of the source.  
    - Mean source position for each set of images.  
    Tested : Matlab 7.0  
    By : Eran Ofek          April 2005  
    URL : http://wise-obs.tau.ac.il/~eran/matlab.html  
    See Also: splane_rms.m  
      
      
### astro.lensing.splane_rms_norm1

splane_rms_norm1 function                                          glens Description: Given a model parameters of a lens, and the images position, calculate the deflections


    
      
    splane_rms_norm1 function                                          glens  
    Description: Given a model parameters of a lens, and the  
    images position, calculate the deflections  
    (and magnification) and the sources position.  
    The program calculates the rms of the sources  
    position in the source plane.  
      
    This program is a duplicate of splane_rms.m  
    The major difference is that this program get  
    the lens model normalization (for the first line  
    in the model - the rest of the lines are  
    normalized proportianly) as the first  
    input argument.  
      
    Input  : - Model Normalization (override the 7th column in ModelPars).  
    - Model parameters (see calc_alpha.m).  
    - Vector of Model Type (see calc_alpha.m).  
    - Images position cell array:  
    Each cell contains [ThetaX, ThetaY, ErrorX, ErrorY]  
    of the images of a single source [pixels units!].  
    It is recomended that the first image in each list  
    will be the faintest image corresponds to each source.  
    - Vector of Dls/Ds for each source.  
    Output : - RMS in the source plane.  
    - Residual of each source corresoinding to an image, relative to the  
    mean position of the source.  
    - Mean source position for each set of images.  
    Tested : Matlab 7.0  
    By : Eran Ofek          April 2005  
    URL : http://wise-obs.tau.ac.il/~eran/matlab.html  
    See Also: splane_rms.m  
      
### astro.lensing.upsilon_u

- upsilon_u function                                                    glens Description: Calculate the upsilon(u) function (Eq. 15 in Keeton 2001).


    
    -  
    upsilon_u function                                                    glens  
    Description: Calculate the upsilon(u) function (Eq. 15 in Keeton 2001).  
    Input  : - u  
    - x  
    - y  
    - q, the prjected axis ratio = b/a  
    Output : - Function value  
    Tested : Matlab 6.5  
    By : Eran O. Ofek                     March 2005  
    URL : http://wise-obs.tau.ac.il/~eran/matlab.html  
    Reliable: 2  
    -  
      
