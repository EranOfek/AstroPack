function IL = information_loss(M0, M1)
    % a measure of information content lost when using M1 PSF stamp instead of M0 stamp
    % (from the appendix of Zackay & Ofek, 2017)
    % Input : - "original" PSF stamp
    %       : - "changed" PSF stamp
    %
    % Output: - a real number showing how much information has been lost (percent)
    %
    % Author : A. Krassilchtchikov (Jun 2023)
    % Example: M0 = rand(15,15); 
    %          M1 = M0; M1(7,7) = 0.6; M1(2,1) = 0.1; 
    %          IC = imUtil.psf.information_loss(M0, M1)
    %
    arguments
        M0
        M1
    end
    
    if numel(M0) ~= numel(M1) 
        fprintf('Warning! The numbers of elements in the input stamps differ, the resulting measure may be irrelevant!\n');
    end
    
    Rat0 = ( sum (M0 .* M0, 'all') )^2. / sum (M0^2. .* M0, 'all'); % information content measure
    Rat1 = ( sum (M0 .* M1, 'all') )^2. / sum (M0^2. .* M1, 'all');

    IL = abs( 1 - Rat1/Rat0 );

end