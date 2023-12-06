function Fun = smooth_downsample(Fun0, ResFactor, Args)
    % smooth a 1D function with a Gaussian kernel and then downsample by it a whole factor 
    % this is needed, e.g., to digest collections of high-resolution spectra 
    % and produce more compact archives at lower resolution without loosing 
    % the form and keeping the total energy and its balance between the bands
    % Input: - a 1D function as [X Y]
    %        - the downsampling factor (a natural number)
    % Output: - the smoothed and downsampled function as [X Y] 
    % Author: A.M. Krassilchtchikov (Oct 2023)
    % Example: X = 1:10000; Y = normrnd(X,sqrt(X)); Fun0 = [X' Y'];
    %          Fun = tools.math.filter.smooth_downsample(Fun,10,'Kernsize',2);
    arguments
        Fun0
        ResFactor
        Args.Kernwidth = 2; % smoothing width in the units of the downsampling factor
        Args.Kernsize  = 6; % Gaussian kernel size in sigma units
    end    
    Sigma = Args.Kernwidth * ResFactor;  % 
    Kernel_size = Args.Kernsize * Sigma; % 
    Kernel = normpdf(-(Kernel_size-1)/2:(Kernel_size-1)/2, 0, Sigma);
    % Normalize the kernel to keep the total flux
    Kernel = Kernel / sum(Kernel);
    % Apply Gaussian smoothing
    F_smoothed = conv(Fun0(:,2), Kernel, 'same');
    % Downsample the smoothed data
    Fun(:,1) = Fun0(1:ResFactor:end,1);
    Fun(:,2) = F_smoothed(1:ResFactor:end);
end
