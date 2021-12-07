% Content of the +TimeDelay package:
%
% A_hat                       : A_hat operator
%                               A_hat_=A_hat(Tau,Alpha,X_i,w)
% chi2_xF                     : Calculate the chi^2 between the reconstructed combined light
%                               and the observed combined light.
%                               [Chi2,Nobs,CalcF_t,f_t]=chi2_xF(Pars,FitPar,varargin)
% combined_Ft                 : Reconstruct the combined light curve from the original light curve
%                               Ft=combined_Ft(t,ft,A0,Alpha,Tau,InterpMethod)
% dft_matrix                  : Return the discrete Fourier Transform matrix
%                               [Fjl]=dft_matrix(N)
% end_matching                : Apply end-matching to a light curve such that first and last point have the same flux
%                               [F_t,Slope,Diff]=TimeDelay.end_matching(T,F_t)
% end_matching_xt             : Apply end-matching to a center-of-light position consistent with light curve 
%                               [xtEM,FtEM]=TimeDelay.end_matching(T,xt,Ft,x0,x,A0,A,ErrF_t,Errx_t)
% equal_diagonals_matrix      : Populate a vector along the diagonals of a matrix (Toplitz matrix)
%                               Mat=TimeDelay.equal_diagonals_matrix(Vec);
% fft_freq                    : Return the frequencies off fft with N points and Delta time step D.
%                               Freq=TimeDelay.fft_freq(N,D);
% fit_astrometric_flux        : Fit the 2 images astrometric-flux time delay model to observations
% fit_astrometric_flux_simple : Fit 2-D lensed images using the simple flux/astrometry method (no red noise)
% fit_flux                    : Fit the 2 images time delay using the flux-only method
% fit_fluxBPL                 : DO NOT USE    
% logdet                      : Calculate the log of the determinant of a matrix                     
% logl_BPL                    : Calculate the log-likelihood for a broken power-law model            
% logl_F                      : Flux only method - Calculate the log-likelihood of F, Sigma_F, and Sigma_phi                    
% logl_x_given_F              : old version. DO NOT USE                
% logl_xF                     : Return the -logL of x,F - of the astrometric-flux time-delay method
% plots_for_paperI            : scripts for plotting paper I figures.             
% plots_for_paperII           : scripts for plotting paper II figures.
% power_spectrum_sections     : Calculate the power spectrum of unevenly spaced time series with gaps
% rand_lensed                 : Generate evenly spaced combined ligt curve and position of lensed quasar
% rand_lensed_uneq            : unequally spaced lensed quasar light curves and center of light position           
% rand_psd                    : Generate a random vector with distribution following some power spectrum.
% reconstruct_ft              : Reconstruct f(t) of a lensed quasar from the combined light and position           
% sigma_phi                   : Calculate Sigma_phi and Sigma_F              
% witt_timedelay_formula      : Calculate the time delay for isothermal elliptical potential (Witt formula)      
