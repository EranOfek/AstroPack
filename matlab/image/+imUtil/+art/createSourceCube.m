function [CubePSF, XY] = createSourceCube(PSF, X1Y1, Flux, Args)
    % Build a cube / cell array of fluxed PSF stamps, optionally rescaled, rotated, and shifted to whole pixel positions
    %     This is a low-level function to be used for source injection into
    %     an astronomical image or source removal therefrom 
    % Input  : - a cube or a cell array of PSF stamps or a single PSF stamp
    %          - a 2-column (X, Y) table of injection positions
    %          - a vector of source flux values or a single value for all the sources
    %          * ...,key,val,... 
    %          'Oversample' - PSF scaling factor(s) (1 scalar, 2 scalars, 1 vector, 
    %                    a 2-column matrix of scaling factors), do not rescale if Oversample = 0 (default) 
    %          'RotAngle' - PSF rotation angle(s): a scalar or a vector  
    %          'Recenter' - true (shift the stamps according to X1Y1) or 
    %                       false (just round the X1Y1 values to XY and do not shift the PSF) 
    %          'RecenterMethod' - 'fft' or 'nearest'; usually 'nearest' goes with Oversmapling > 1
    %          'PositivePSF' - logical, whether to improve the PSF wings (edges)
    %          'FunEdge'     - a handle of a 2D function used to impove the edges 
    %          'FunEdgePars' - parameters of 'FunEdge'
    % Output : - a cube / cell array of shifted, rescaled and fluxed PSF stamps
    %          - a 2-column (X, Y) table of whole pixel injection positions
    % Author : A.M. Krassilchtchikov (2024 May) 
    % Example: for i = 1:10; P(:,:,i) = imUtil.kernel2.gauss([4 4 0],[24 24]) + 1e-2*rand(24,24); end
    %          X1Y1 = 100.*rand(10,2); Flux = 100.*rand(10,1);
    %          [CubePSF, XY] = imUtil.art.createSourceCube(P, X1Y1, Flux, 'Recenter', false, 'Oversample', 3, 'PositivePSF', true);
    %
    %          for i = 1:3; P{i} = imUtil.kernel2.gauss([4 4 0],[21+3*i 21+3*i]) + 1e-2*rand(21+3*i,21+3*i); end
    %          X1Y1 = 100.*rand(3,2); Flux = 100.*rand(3,1);
    %          [CubePSF, XY] = imUtil.art.createSourceCube(P, X1Y1, Flux, 'Recenter', false, 'Oversample', 3, 'PositivePSF', true);
    arguments
        PSF
        X1Y1
        Flux
        Args.Oversample          = 0;
        Args.RotAngle            = [];
        Args.Recenter    logical = true;
        Args.RecenterMethod      = '';  % fft or 
        Args.PositivePSF logical = false;
        Args.FunEdge             = @imUtil.kernel2.cosbell;
        Args.FunEdgePars         = [4 6];
    end
        
    Nsrc = size(X1Y1,1);           % the number of input sources
    
    % whole pixel coordinates and subpixel shifts 
    XY      = max(round(X1Y1), 1); % the rounding should not produce 0
    XYshift = X1Y1 - XY;
    
    % check the number of input flux values 
    if numel(Flux) == 1 
        Flux = repmat(Flux, 1, Nsrc)'; 
    elseif numel(Flux) ~= Nsrc 
        error ('The size of the source flux vector does not match that of the coordinate matrix');
    end
    
    % check the size and type of PSF stamps
    if numel(PSF) == 1
        PSF = repmat(PSF, [1 1 Nsrc]);
    elseif iscell(PSF)
        if numel(PSF) ~= Nsrc
            error ('The size of the PSF array does not match that of the coordinate matrix');
        end
    else 
        if size(PSF,3) ~= Nsrc
           error ('The size of the PSF stack does not match that of the coordinate matrix');
        end 
    end        
    
    % shift and resample the PSF stamps, forcing odd-sized and normalized stamps  
    if Args.Recenter || all(Args.Oversample > 0)
        PSF = imUtil.psf.shiftResampleRotate(PSF,XYshift,Args.Oversample,Args.RotAngle,...
            'ForceOdd',true,'Recenter',Args.Recenter,'RecenterMethod',Args.RecenterMethod,'Renorm',true);
    end        
    
    % eliminate negative PSF edges 
    if Args.PositivePSF
        if iscell(PSF)
            M = cellfun(@size, PSF, 'UniformOutput', false);
            for Isrc = 1:Nsrc
                EdgeFunPars = ceil( Args.FunEdgePars .* M{Isrc}(1) / 15);    % empiric, should somehow depend on M
                SupressedEdges = Args.FunEdge( EdgeFunPars, size(PSF{Isrc}) ) .* PSF{Isrc};
                PSF{Isrc} = SupressedEdges ./ sum(SupressedEdges,'all');     % renormalize
            end
        else
            [M,~,~] = size(PSF); 
            for Isrc = 1:Nsrc
                EdgeFunPars = ceil( Args.FunEdgePars .* M / 15);             % empiric, should somehow depend on M
                SupressedEdges = Args.FunEdge( EdgeFunPars, [M M] ) .* PSF(:,:,Isrc);
                PSF(:,:,Isrc) = SupressedEdges ./ sum(SupressedEdges,'all'); % renormalize
            end
        end
    end
    
    % make fluxed PSF cubes  
    if iscell(PSF)
        CubePSF = cell(Nsrc,1);
        for Ipsf = 1:Nsrc
            CubePSF{Ipsf} = Flux(Ipsf) .* PSF{Ipsf};
        end
    else
        CubePSF = reshape(Flux, 1, 1, Nsrc) .* PSF;
    end
end
