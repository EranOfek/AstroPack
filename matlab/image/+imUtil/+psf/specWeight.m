function Result = specWeight(PSFdata, RadSrc, Rad, Spec)

% Make source PSFs at certain pixel distances on the detector weighted with the given spectra of the sources
% Package: imUtil.psf
% Description: make source PSFs at certain pixel distances on the detector weighted with the given spectra of the sources
% Input  : - PSFdata: a 4-D matrix of experimental PSF: 2D at given radius and wavelength
%          - RadSrc: an array of radial distances of the sources from the detector tile reference point
%          - Rad: an array of radial distances sampled on the lab PSFs 
%          - Spec: 3D array of source spectra 
% Output : - A 3-D matrix of spectrum-weighted PSFs
%            
% Tested : Matlab R2020b
%     By : A. Krassilchtchikov et al.    Feb 2023
% Example: PSF = imUtil.psf.specWeight( PSFdata, RadSrc, Rad, Spec );

    % check input parameters:

    NSrc    = size(Spec,1);
    Nwave   = size(Spec,2);

    Nx      = size(PSFdata,1);
    Ny      = size(PSFdata,2);
    
    if size(PSFdata,3) ~= Nwave
        fprintf('Warning: Wavelength dimensions do not match!\n');
    end

    if size(PSFdata,4) ~= size(Rad)
        fprintf('Warning: Radial dimensions do not match!\n');
    end
    
    Result  = zeros( Nx, Ny, NSrc );
        
    % make weighted PSFs for each of the sources:
    
    for ISrc = 1:1:NSrc
        
        % find a nearest (left) grid node and interpolate the PSF matrix:
        Ir = find (Rad  <= RadSrc(ISrc),  1, 'last');
        
        % linearly interpolate the PSF matrix:
        
        PSFint = zeros(Nx, Ny, Nwave); 
                        
        PSFint(:,:,:) = ( 1. / ( Rad(Ir+1) - Rad(Ir) ) ) * ...
            ( PSFdata(:,:,:,Ir) * ( Rad(Ir+1)-RadSrc(ISrc) ) + PSFdata(:,:,:,Ir+1) * ( RadSrc(ISrc)-Rad(Ir) ) ) ;
                
        for Iw = 1:1:Nwave
            
            Result( :, :, ISrc) = Result( :, :, ISrc) + PSFint(:, :, Iw) .* Spec(ISrc,Iw);
            
        end
        
        % normalize the PSF to unity:
        
        Result( :, :, ISrc) = Result( :, :, ISrc) / sum( Result( :, :, ISrc), 'all' );
                        
    end



end