function [Rad, Stamp] = quantileRadius (PSFin, Args)
    % Measure the radius of signal containment in a PSF stamp at a given level
    % Package: imUtil.psf
    % Description: Measure the radius of signal containment in a PSF stamp at a given level 
    % Input: - PSFin: a 2D array containing the PSF stamp 
    %         * ...,key,val,... 
    %         'Level' signal containment level [0-1], 0.5 is default
    % Output : - Rad  : containment radius in pixels
    %          - Stamp: the PSF stamp within the containment radius
    % Tested : Matlab R2020b
    % Author : A. Krassilchtchikov et al. (Feb 2023)
    % Example: Rad = imUtil.psf.quantileRadius (PSF, 'Level', 0.8)
    arguments        
        PSFin                    % the input PSF stamp        
        Args.Level     =    0.5; % 50% containment        
    end    
    % normalize the PSF stamp to 1:    
    PSF = PSFin / sum(PSFin,'all');    
    % find the smaller size of an M x N array:  
    [SizeX, SizeY] = size(PSF);
    Size       = min (SizeX, SizeY);
    HalfSize   = floor( (Size+1)/2 );        
    Isc      = 0;
    Encircle = 0;
    while Isc < HalfSize && Encircle < Args.Level   
        Isc   = Isc + 1;            
        Bl      = max( HalfSize - Isc, 1 );
        Br      = min( HalfSize + Isc, Size);           
        Stamp   = PSF(Bl:Br,Bl:Br);
        Encircle = sum( Stamp, 'all' );     
    end  
    Rad = Isc;     
    if Encircle < Args.Level
        fprintf('Warining! The required level not reached, probably, due to a rectangular PSF matrix!\n');
        fprintf('%s%4.1f\n','The encircled fraction is only',Encircle);
    end   
    % make a cropped PSF stamp
    if rem(SizeX,2) == 0
        CutX = SizeX/2-Rad+1:SizeX/2+Rad;
    else
        CutX = (SizeX+1)/2-Rad:(SizeX+1)/2+Rad;
    end
    if rem(SizeY,2) == 0
        CutY = SizeY/2-Rad+1:SizeY/2+Rad;
    else
        CutY = (SizeY+1)/2-Rad:(SizeY+1)/2+Rad;
    end    
    Stamp = PSF(CutX,CutY);
end