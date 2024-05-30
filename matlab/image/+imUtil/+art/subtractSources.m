function Image = subtractSources(Image, SrcPSF, XY, Args)
    % Subtract odd-sized fluxed source images (PSFs) into whole pixel positions of an image  
    %     NB: first one needs to prepare fluxed and shifted source PSFs with imUtil.art.createSourceCube 
    % Input  : - an image matrix  
    %          - a prepared cube of fluxed source PSFs  
    %          - a prepared list of whole pixel positions   
    %          * ...,key,val,... 
    %          'Size' - [X Y] a forced size of the resulting image [used only if any(size(Image) < 2)]   
    % Output : - an image with subtracted source PSFs  
    % Author : A.M. Krassilchtchikov (2024 May) 
    % Example: 
    % 
    arguments
        Image      
        SrcPSF
        XY
        Args.Size  = [];         
    end
    %
    Image = imUtil.art.injectSources(Image,SrcPSF,XY,'Subtract',true,'Size',Args.Size);
end
