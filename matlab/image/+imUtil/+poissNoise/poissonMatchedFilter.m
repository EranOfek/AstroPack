function Pp = poissonMatchedFilter(P, B, F)
    % Return the Poisson Matched filter (Ofek & Zackay 2018)
    % Input  : - A PSF, or a cube of PSF in which the PSF index is in the
    %            third dimension.
    %          - Vector of Backgrounds. 
    %          - Vector of Fluxes. 
    % Output : - Poisson noise matched filter: ln(1+ P.*F./B)
    %            The filter index is in the 3rd dimension and is given by
    %            the maximum length of the background, flux, or PSF.
    % Author : Eran Ofek (Feb 2023)
    % Example: P = imUtil.kernel2.gauss(1.5); B=0.1; F=3;
    %          Pp = imUtil.poissNoise.poissonMatchedFilter(P, B, F)
    %          P = imUtil.kernel2.gauss([1.5;2]); B=0.1; F=3;
    %          Pp = imUtil.poissNoise.poissonMatchedFilter(P, B, F)
    
    Np = size(P,3);
    Nb = numel(B);
    Nf = numel(F);
    
    Nmax = max(Nb, Nf);
    
    if Nmax>1
        B = reshape(B,[1 1 Nb]);
        F = reshape(F,[1 1 Nf]);
    end
                
    Pp = log(1+ P.*F./B);
    
end
