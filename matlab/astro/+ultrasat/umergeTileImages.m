function Image = umergeTileImages (Args)
    % Make a 4-tile ULTRASAT image from 4 separate tile images     
    % Package: ultrasat
    % Description: Make a 4-tile ULTRASAT image from 4 separate tile images
    %          - Args.A      : name of the .mat object with a presimulated image of tile A
    %          - Args.B      : name of the .mat object with a presimulated image of tile B
    %          - Args.C      : name of the .mat object with a presimulated image of tile C
    %          - Args.D      : name of the .mat object with a presimulated image of tile D
    %          
    % Output : - Image: a 2D array containing the resulting source image 
    %                   
    %            
    % Tested : Matlab R2020b
    %     By : A. Krassilchtchikov et al.    Feb 2023
    % Example: Image = umergeTileImages (Args)
    %                                        
    
    arguments
        
    Args.A = 'SimImage_tileA.mat';
    Args.B = 'SimImage_tileB.mat';
    Args.C = 'SimImage_tileC.mat';
    Args.D = 'SimImage_tileD.mat';
    
    end
    
    TileSizeX  = 4738;
    TileSizeY  = 4738;
    
    PixelSize   = 9.5;  % pixel size in microns
    
    GapMm       = 2.4;  % gap width in mm
        
    Ngap        = ceil( 1e3 * GapMm / PixelSize); 
    
    Nx          = 2 * TileSizeX + Ngap;
    Ny          = 2 * TileSizeY + Ngap;
    
    Image = zeros(Nx, Ny);
    
    % read in the data
    
    imA = load(Args.A);
    imB = load(Args.B);
    imC = load(Args.C);
    imD = load(Args.D);
    
    % pad the quarter arrays with zeros
    
    imA = padarray(imA,[TileSizeX+Ngap 0             ],'post');
    imA = padarray(imA,[0              TileSizeY+Ngap],'pre' );
    
    imB = padarray(imB,[TileSizeX+Ngap TileSizeY+Ngap],'pre' );
    
    imC = padarray(imC,[TileSizeX+Ngap 0             ],'pre' );
    imC = padarray(imC,[0              TileSizeY+Ngap],'post');
    
    imD = padarray(imD,[TileSizeX+Ngap TileSizeY+Ngap],'post');
    
    % sum the arrays
    
    Image = Image + imA + imB + imC + imD;
    
    % output: a matlab object and a fits file
    
    
    
end