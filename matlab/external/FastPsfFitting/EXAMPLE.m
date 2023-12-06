% This is a simple demonstration of using psfFit_Image. It is recommended
% to look into the full function description with all features before using
% it in your project

% 1) Create an image to test the fitting 

% Note: Overlapping emitters are problematic for the fitting and will cause
% the result to deviate from the true values even without noise.

imsize = 128;
BG = 100;
sigma = 1;
nr_emitters = 10;
with_noise = false;

[x,y] = meshgrid(1:imsize,1:imsize);
psfFunc = @(x0,y0,A,sigma) A*exp(-(x-x0).^2/(2*sigma^2)-(y-y0).^2/(2*sigma^2));

% Emitters vary in position, brightness and standard deviation.
em_pos = 5+rand(nr_emitters,2)*(imsize-5);
em_amp = 300+rand(nr_emitters,1)*100;
em_sig = sigma + rand(nr_emitters,1)*0.5;

img = zeros(imsize) + BG;
for iEm = 1:nr_emitters
    img = img + psfFunc(em_pos(iEm,1) , em_pos(iEm,2), em_amp(iEm), em_sig(iEm));
end

if(with_noise)
    img = poissrnd(img);
end


% 2) Fit the candidates
% As intial guess we add noise to the true positions
par_init = em_pos + 1.5*(2*rand(nr_emitters,2)-1); 
par_init = par_init';

% This is the simplest possible call, where we only supply the candidate position guesses as initial parameters. 
% See psfFit_Image for all options.
result_params = psfFit_Image( img, par_init ); 

% 3) Display the result
figure; imagesc(img); colormap hot; axis image;
hold on;
  plot(em_pos(:,1), em_pos(:,2),'kx');
  plot(par_init(1,:), par_init(2,:),'c.');
  plot(result_params(1,:), result_params(2,:),'co');
hold off;
legend('true pos', 'init guess', 'fitted pos')
