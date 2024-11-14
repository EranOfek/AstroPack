function [Integral] = integralShiftedGaussianInCircle(r, sigma, VecD)
    % calculate the integral of a 2-D Gaussian within a circle shifted from the Gaussian center.
    % Input  : - Circle radius (scalar).
    %          - Gaussian sigma (scalar)/
    %          - Distance between Gaussian and circle centers (vector).
    % Output : - Integral
    % Author : Eran Ofek (2024 Nov) 
    % Example: [Integral] = tools.math.geometry.integralShiftedGaussianInCircle(1.5, 1.4./2.35, 0.3)

    
    % Define the Gaussian function
    gaussian = @(x, y) (1 / (2 * pi * sigma^2)) * exp(-(x.^2 + y.^2) / (2 * sigma^2));

    Nd = numel(VecD);
    Integral = zeros(Nd,1);
    for Id=1:1:Nd
        d = VecD(Id);
        % Define the circular region as a logical mask for integration limits
        circular_region = @(x, y) ((x - d).^2 + y.^2 <= r^2);

        % Set up the integration over the region defined by the circle
        Integral(Id) = integral2(@(x, y) gaussian(x, y) .* circular_region(x, y), ...
                             -r - d, r + d, -r, r, ...
                             'Method', 'auto', 'AbsTol', 1e-4, 'RelTol', 1e-4);
    end
    
end
