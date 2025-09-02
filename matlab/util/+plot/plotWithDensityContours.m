function [HScatter,HContour,N,XEdges,YEdges] = plotWithDensityContours(X, Y, Args)
    % Plot X–Y points, but switch to density contours in bins whose point count exceeds a threshold.
    %   The density is calculated in DX, DY bins provided by the user.
    % Input: - Vector of X coordinates of points.
    %        - Vector of Y coordinates of points.
    %        * ...,key,val,...
    %          'DX' - Width of X-bin used for desity calculation. Default is 0.1.
    %          'DY' - Width of Y-bin used for desity calculation. Default is 0.1.
    %          'DensityThresh' - DEnsity threshold. Default is 30.
    %          'Levels' - Contours levels.
    %               You may want to verify that the first contour level is
    %               equal to the DEnsityThresh parameter.
    %               Default is logspace(log10(30), log10(1e5), 12)
    %          'PlotArgs' - A cell array of additional arguments to pass to
    %               plot function. Default is {'MarkerSize',1}
    %          'LogZ' - A logical indicating if to use log10(Density) contours.
    %               Default is false.
    %          'AddColorBar' - Add colorbar. Default is true.
    %
    % Output: - HScatter - handle to scatter object (empty if none plotted)
    %         - HContour - handle(s) to contour object(s) (empty if none plotted)
    %         - N        - 2D histogram counts (size: numXbins × numYbins)
    %         - XEdges   - bin edges along X
    %         - YEdges   - bin edges along Y
    % Author : ChatGPT, Eran Ofek (Sep 2025)
    % Example: plot.plotWithDensityContours(T.mag_psf,T.r_mag_psf);
    %          plot.plotWithDensityContours(T.mag_psf,T.r_mag_psf,'LogZ',true);

    
    arguments
        X 
        Y 
        Args.DX    = 0.1;
        Args.DY    = 0.1;
        Args.DensityThresh  = 30;
        Args.Levels         = logspace(log10(30), log10(1e5), 12);
        Args.PlotArgs       = {'MarkerSize',1};
        Args.LogZ           = false;
        Args.AddColorBar    = true;
    end
    
    % Remove NaNs/Infs
    Valid = isfinite(X) & isfinite(Y);
    X = X(Valid);  Y = Y(Valid);
    
    % Bin edges (pad by half a bin so edge points are included comfortably)
    DX = Args.DX;  DY = Args.DY;
    XMin = min(X); XMax = max(X);
    YMin = min(Y); YMax = max(Y);
    XPad = 0.5*DX;  YPad = 0.5*DY;
    XEdges = (floor((XMin - XPad)/DX)*DX) : DX : (ceil((XMax + XPad)/DX)*DX);
    YEdges = (floor((YMin - YPad)/DY)*DY) : DY : (ceil((YMax + YPad)/DY)*DY);
    
    % 2D histogram
    [N, XEdges, YEdges] = histcounts2(X, Y, XEdges, YEdges);
    
    % Decide which bins are "dense"
    DensityThresh = Args.DensityThresh;
    MaskHigh = (N >= DensityThresh);
    
    % Select sparse points only (so we don't overplot where it's dense)
    %   1) Bin-index each point
    [Ix,~] = discretize(X, XEdges);
    [Iy,~] = discretize(Y, YEdges);
    InRange = ~isnan(Ix) & ~isnan(Iy);
    Ix = Ix(InRange); Iy = Iy(InRange);
    XShow = X(InRange); YShow = Y(InRange);
    
    %   2) Keep points whose bin is below threshold
    Lin = sub2ind(size(N), Ix, Iy);
    Keep = ~MaskHigh(Lin);
    XShow = XShow(Keep);  YShow = YShow(Keep);
    
    % Prepare grid for contour (bin centers)
    XCenters = 0.5*(XEdges(1:end-1) + XEdges(2:end));
    YCenters = 0.5*(YEdges(1:end-1) + YEdges(2:end));
    Z = N;                   % size: numel(XCenters) × numel(YCenters)
    %Z(~MaskHigh) = NaN;      % only draw contours over dense bins
    
    % Plot
    HScatter = gobjects(0);
    HContour = gobjects(0);
    holdState = ishold;
    
    if ~isempty(XShow)
        %HScatter = scatter(XShow, YShow, 6, 'filled', 'MarkerFaceAlpha', 0.5, 'MarkerEdgeColor', 'none', 'MarkerSize',1);

        HScatter = plot(XShow, YShow, '.', Args.PlotArgs{:});
        hold on
    end
    
    if any(MaskHigh,'all')
        % contour expects Z as (Ny × Nx), so transpose
        [Xg, Yg] = meshgrid(XCenters, YCenters);

        % use log Z scale
        if Args.LogZ
            Z = log10(Z);
            Args.Levels = log10(Args.Levels);
        end

        HContour = contour(Xg, Yg, Z.',Args.Levels, 'ShowText', 'off');  % use contourf if you prefer filled
        hold on
        if Args.AddColorBar
            colorbar;
        end
    end
    
    box on
        
    if ~holdState
        hold off;
    end
    
end
