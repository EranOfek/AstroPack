function Result = specWeight(SpecSrc, RadSrc, PSFdata, Args)
    % Make source PSFs at certain pixel distances on the detector weighted with the given spectra of the sources
    % Package: imUtil.psf
    % Description: make source PSFs at certain pixel distances on the detector weighted with the given spectra of the sources
    % Input  : - SpecSrc (SrcNum, Intensity) : an array of source spectra 
    %          - RadSrc  (SrcNum, RadDistance): an array of radial distances of the sources from the detector tile reference point
    %          - PSFdata: a 4-D matrix of experimental PSF: (X, Y, wavelength, radius) 
    %          * ...,key,val,...
    %          'Rad'        - a vector of radial distances of the lab PSFs 
    %          'Lambda'     - a vector of wavelengths of the lab PSFs 
    %          'SpecLam'    - a vector of wavelengths of the source spectra
    %          'SizeLimit'  - the maximal array size in Gb determines
    %                         the calculation method: a too large array may cause out of
    %                         memory error, while for a smaller array a faster algorithm
    %                         can be employed (see below)
    % Output : - Result(X,Y, SrcNum): A 3-D matrix of spectrum-weighted PSFs
    %            
    % Tested : Matlab R2020b
    % Author : A. Krassilchtchikov et al. (Feb 2023)
    % Examples: PSF = imUtil.psf.specWeight( SpecSrc, RadSrc, PSFdata);
    %           PSF = imUtil.psf.specWeight( SpecSrc, RadSrc, PSFdata, ...
    %           'Rad', Rad, 'SizeLimit',Args.ArraySizeLimit, 'Lambda', WavePSF, 'SpecLam', Wave);

    arguments       
        SpecSrc        
        RadSrc     
        PSFdata     
        Args.Rad      
        Args.Lambda  = 0;   % if Args.Lambda and Args.SpecLam are not put in
        Args.SpecLam = 0;   % that implies that the spectral grid in the
                            % PSFdata array and in the input source spectra
                            % SpecSrc is the same !         
        Args.SizeLimit = 8; % [Gb] the maximal array size determines the calculation method   
    end
    
    Tiny = 1e-30;
    
    Nx      = size(PSFdata,1);  
    Ny      = size(PSFdata,2);
    NLam    = size(PSFdata,3);
    
    X       = 1:Nx;  
    Y       = 1:Ny;
    Lam     = 1:NLam;
    
    NumSrc  = size(RadSrc,1);
    NumWave = size(SpecSrc,2);
        
    % check that the number of sources and the number of input spectra are the same:
    if size(SpecSrc,1) ~= NumSrc       
        cprintf('err','Number of source radii and number of spectra in specWeight do not match, exiting...\n');
        return      
    end
    
    % regrid the input spectra if the spectral grids in PSFdata and in SpecSrc are not the same 
    if numel(Args.SpecLam) > 1 && numel(Args.Lambda) > 1   % regrid the spectra
        SrcNum = 1:NumSrc;
        if NumSrc > 1
            Spec = interpn(SrcNum, Args.SpecLam', SpecSrc, SrcNum, Args.Lambda','linear',Tiny);
            NumWave = size(Spec,2);
        else % if there is only 1 source
            Spec = interp1(Args.SpecLam', SpecSrc, Args.Lambda','linear',Tiny);
            NumWave = size(Spec,1);
        end
    elseif NumWave == NLam                               % the spectral grids in PSFdata and in SpecSrc are of the same size
        Spec = SpecSrc;
    else
        cprintf ('err', 'Spectral grid mismatch in specWeight, need to input the grids, exiting...\n');
        return
    end 
    
    Result = zeros(Nx,Ny,NumSrc);
    ArraySizeGb = 8 * Nx * Ny * NLam * NumSrc / (1024^3);
    
    if ArraySizeGb < Args.SizeLimit  % if the number of sources is not too high, we can use a faster algorithm
        % rescale the data array to the actual source positions:
        PSFdataS = interpn(X,Y,Lam, Args.Rad, PSFdata, X,Y,Lam, RadSrc, 'linear', Tiny);
        % put the spectrum into the right dimension
        % NOTE that the Spec array should be transposed before reshaping! 
        Spec2 = reshape(Spec',[1 1 NumWave NumSrc]);
        % multiply the PSF array sampled by the source positions by the source spectra
        Wcube = PSFdataS .* Spec2;
        % sum over the wavelengths and delete the degenerate dimension 
        SumL  = squeeze( sum( Wcube,3 ) ); 
        % normalize 
        Result = SumL ./ sum( SumL, [1,2] );
    else                             % if the number of sources is high, we can not operate on the full array
        for Isrc = 1:1:NumSrc
            % rescale the data array to the actual source positions
            PSFdataS = interpn(X,Y,Lam, Args.Rad, PSFdata, X,Y,Lam, RadSrc(Isrc), 'linear', Tiny);
            % put the spectrum into the right dimension
            % NOTE that the Spec array should be transposed before reshaping! 
            Spec2 = reshape(Spec(Isrc,:)',[1 1 NumWave]);
            % multiply the PSF array sampled by the source positions by the source spectra
            Wcube = PSFdataS .* Spec2;
            % sum over the wavelengths and delete the degenerate dimension 
            SumL  = squeeze( sum(Wcube,3) ); 
            % normalize 
            Result(:,:,Isrc) = SumL ./ sum( SumL, [1,2] );
        end     
    end  
end
