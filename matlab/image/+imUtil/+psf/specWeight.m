function Result = specWeight(SpecSrc, RadSrc, PSFdata, Args)
    % Make source PSFs at certain pixel distances on the detector weighted with the given spectra of the sources
    % Package: imUtil.psf
    % Description: make source PSFs at certain pixel distances on the detector weighted with the given spectra of the sources
    % Input  : - SpecSrc (SrcNum, Intensity) : an array of source spectra 
    %          - RadSrc  (SrcNum, RadDistance): an array of radial distances of the sources from the detector tile reference point
    %          - PSFdata: a 4-D matrix of experimental PSF: (X, Y, wavelength, radius) 
    %          - Args.Rad: a vector of radial distances of the lab PSFs 
    %          - Args.Lambda: a vector of wavelengths of the lab PSFs 
    %          - Args.SpecLam: a vector of wavelengths of the source spectra
    % Output : - Result(X,Y, SrcNum): A 3-D matrix of spectrum-weighted PSFs
    %            
    % Tested : Matlab R2020b
    %     By : A. Krassilchtchikov et al.    Feb 2023
    % Example: PSF = imUtil.psf.specWeight( PSFdata, RadSrc, Rad, Spec );

    arguments
        
        SpecSrc 
        
        RadSrc 
        
        PSFdata 
        
        Args.Rad  
        
        Args.Lambda  = 0;   % if Args.Lambda and Args.SpecLam are not put in
                            % that implies that the spectral grid in the
                            % PSFdata array and in the input source spectra
                            % SpecSrc is the same ! 
        Args.SpecLam = 0;
        
    end
    
    % rescale the data array to the actual source positions:
    
    X       = 1:size(PSFdata,1);  
    Y       = 1:size(PSFdata,2);
    Lam     = 1:size(PSFdata,3);
    
    PSFdataS = interpn(X,Y,Lam, Args.Rad, PSFdata, X,Y,Lam, RadSrc);
    
    % regrid the input spectra if the spectral grids in PSFdata and in SpecSrc are not the same 
    
    if Args.SpecLam ~= 0 && Args.Lambda ~= 0   % regrid the spectra
        
        Spec = interp1(Args.SpecLam, SpecSrc, Args.Lambda);
        
    else                                        % the spectral grids in PSFdata and in SpecSrc are the same 
         
        Spec = SpecSrc;
        
    end
    
    % put the spectrum into the right dimension
    % NOTE that the Spec array should be transposed before reshaping! 
    
    Spec2 = reshape(Spec',[1 1 size(Spec,2) size(Spec,1)]);
    
    % multiply the PSF array sampled by the source positions by the source spectra
        
    Wcube = PSFdataS .* Spec2;
           
    % sum over the wavelengths and delete the degenerate dimension 
    
    Result  = squeeze( sum(Wcube,3) ); 
    
    % normalize 
    Result = Result ./ sum( Result, [1,2] );

end
