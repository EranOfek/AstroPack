function [Image, SubtractedImage] = injectStamps(XY, PSF, Flux, Args)
    % Construct an image from object stamps + some background 
    %     Optional detailed description
    % Input  : - an array of XY positions of the PSF's central pixels
    %          - a cell array of PSF stamps
    %          - an array of source fluxes
    %          - 
    %          - 
    %          * ...,key,val,... 
    %          'InputImage' - an input image to be added or subracted 
    %          'Subtract' - whether to subtract the constructed image from
    %                       the InputImage
    %          'Back' - a background level
    %          'AddNoise' - whether to add Poisson noise
    % Output : - an artificial image  
    % Author : A.M. Krassilchtchikov (2024 Apr) 
    % Example: 

    arguments
        XY
        PSF
        Flux
        Args.Back              = [];
        Args.AddNoise logical  = true;
                
        Args.InputImage        = [];
        Args.Subtract logical  = false;
    end

    SubtractedImage = [];
    
   % construct a new artificial image
   
   if Args.AddNoise
       
   end
   
   % subtract the new image from InputImage or add to it (if requested)   
   if ~isempty(Args.InputImage)
       if Args.Subtract
           
       else
           
       end       
   end
    
end
