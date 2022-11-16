function [Flux, FluxErr, Npix, BackErr] = polygonFlux(Image, XV, YV, Args)
    % Calculate total flux within polygons in an image
    % Input  : - An image (matrix).
    %          - Vector of X verteces, or a cell array of verteces for
    %            multipule polygons.
    %          - Vector of Y verteces, or a cell array of verteces for
    %            multipule polygons.
    %          * ...,key,val,...
    %            'Back' - Background image to subtract from image prior to flux
    %                   measurment. If empty don't subtract background.
    %                   Default is [].
    %            'Var' - Variance image from which to estimate error.
    %                   Default is [].
    % Output : - Vector of fluxes (one per polygon).
    %          - Vector of flux error estimates as the sqrt of the flux.
    %          - Vector of the number of pixels in each polygon.
    %          - Vector of backgroud error estimated from the sqrt of the
    %            sum of the variance map at the polygon position.
    % Author : Eran Ofek (Nov 2022)
    % Example: [Flux, FluxErr, Npix, BackErr] = imUtil.sources.polygonFlux(Image, XV, YV)
    
    arguments
        Image
        XV
        YV
        Args.Back = [];
        Args.Var  = [];
    end
    
    if ~isempty(Args.Back)
        Image = Image - Args.Back;
    end
    
    if nargout>3 && isempty(Args.Var)
        error('For BackErr Var must be supplied');
    end
        
    
    [SY,SX] = size(Image);
    
    [MatX,MatY] = meshgrid((1:1:SX), (1:1:SY));
    
    if ~iscell(XV)
        XV = {XV};
    end
    if ~iscell(YV)
        YV = {YV};
    end
    
    N    = numel(XV);
    Flux = zeros(N,1);
    Npix = nan(N,1);
    for I=1:1:N
        Flag = inpolygon(MatX, MatY, XV{I}, YV{I});
        
        Flux(I) = sum(Image(:).*Flag(:),'omitnan');
        
        if nargout>2
            Npix(I) = sum(Flag(:));
            
            if nargout>3
                BackErr(I) = sqrt(sum(Args.Var(:).*Flag(:),'omitnan'));
            end
        end
                            
    end
    FluxErr = sqrt(Flux);
    
end
    
