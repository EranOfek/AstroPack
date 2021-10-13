# Package: astro.mag


### astro.mag.magVega

Return the Vega magnitude adopted by GAIA (0.023\pm0.008) Example: Mag=astro.mag.magVega


    
    Return the Vega magnitude adopted by GAIA (0.023\pm0.008)  
    Example: Mag=astro.mag.magVega  
      
### astro.mag.photonIntegral

Calculate the number of photons from a spectrum given transmission.


    
    Calculate the number of photons from a spectrum given transmission.  
    Input  : - Spectrum [Wave, F_lambda] in units of [Ang, erg/cm^2/s/A].  
    - Transmission [Wave, T], in units of [Ang, cm^2]  
    * ...,key,val,...  
    'Method' 0 Interpolation method. Default is 'linear'.  
    Author : Eran Ofek (Oct 2021)  
    Example: Trans=[4000 0; 4001 1; 9100 1;9101 0];  
    Spec = astro.mag.specVega;  
    Nph = astro.mag.photonIntegral(Spec, Trans)  
      
### astro.mag.specVega

Return the Vega (Alpha Lyr) standar spectrum for Vega mag calculations


    
    Return the Vega (Alpha Lyr) standar spectrum for Vega mag calculations  
    Input  : - Spectrum type:  
    'alpha_lyr_mod_004' - STSCI model 4 from CALSPEC  
    Default.  
    'dovi' - 'vega_spec.mat' file from Dovi.  
    Output : - Vega s[pectrum [Wave [Ang], f_lambda [erg/cm^2/s/A]].  
    Author : Eran Ofek (Oct 2021)  
    Example: Spec = astro.mag.specVega  
      
### astro.mag.synMag

Calculate the synthetic magnitude of a spectrum and transmission in the Vega, AB, or STMAG systems.


    
    Calculate the synthetic magnitude of a spectrum and transmission  
    in the Vega, AB, or STMAG systems.  
    Input  : - Spectra: [Wave(Ang), Flux, Flux,...], where flux in  
    erg/cm^2/s/A.  
    - Transmission [Wave(Ang), RelTran]  
    - Mag sys: ['AB'] | 'Vega' | 'STMAG'  
    Output : - Magnitude per column of flux.  
    Author : Eran Ofek (Oct 2021)  
    Example: Trans = [6000 0; 6001 1; 7000 1; 7001 0];  
    W=(3000:1:1e4)'; [~,~,Fl] = astro.spec.black_body(5700,W);  
    Spec  = [W, Fl.*(constant.SunR.^2)./(10.*constant.pc).^2];  
    Mag = astro.mag.synMag(Spec, Trans)  
    Mag = astro.mag.synMag([Spec, Spec(:,2)], Trans)  
    Mag = astro.mag.synMag([Spec, Spec(:,2)], Trans, 'AB')  
    Mag = astro.mag.synMag([Spec, Spec(:,2)], Trans, 'STMAG')  
      
