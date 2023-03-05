function Result = specWeight2a(SpecSrc, RadSrc, PSFdata, Args)
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
    
    if Args.SpecLam ~= 0 && Args.Lambda ~= 0   % regrid the spectra
        
        Spec = interp1(Args.SpecLam, SpecSrc, Args.Lambda);
        
    else                                        % the spectral grids in PSFdata and in SpecSrc are the same 
         
        Spec = SpecSrc;
        
    end
    
    NSrc = size(Spec,1); 
    Nx   = size(PSFdata,1);
    Ny   = size(PSFdata,2);
    Nlam = size(PSFdata,3);
    
    X    = 1:Nx; 
    Y    = 1:Ny;
    Lam  = 1:Nlam;
    
    Result = zeros(Nx,Ny,NSrc);
    
    for Isrc = 1:1:NSrc
        
        Spec2 = reshape(Spec(Isrc,:),[1 1 Nlam]);
        Wcube = PSFdata .* Spec2;
        
        ActCube = interpn(X,Y,Lam, Args.Rad, Wcube, X,Y,Lam, RadSrc(Isrc) );
        
        Result(:,:,Isrc) = squeeze( sum(ActCube,3) );
        
    end   
        
    % normalize 
    Result = Result ./ sum( Result, [1,2] );

end
