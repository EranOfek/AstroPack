function [ params, exitflag ] = psfFit( img, varargin )
% Short usage: params = psfFit( img );  (fits an isotropic gaussian)
% Full usage: [ params, exitflag ] = psfFit( img, param_init, param_optimizeMask, useIntegratedGauss, useMLErefine)
%  Fit a gaussian point spread function model to the given image.
% 
% Coordinate convention: Integer coordinates are centered in pixels. I.e.
% the position xpos=3, ypos=5 is exactly the center of pixel img(5,3). Thus
% pixel center coordinates range from 1 to size(img,2) for x and 1 to
% size(img,1) for y coordinates.
%
% Use empty matrix [] for parameters you don't want to specify.
%
% Gaussian fitting covers three basic cases:
%   - fitting isotropic gaussian  (sigma_y and angle initial values not specified and they should not be optimized)
%   - fitting anisotropic gaussian  (angle initial value not specified and should not be optimized)
%   - fitting anisotropic rotated gaussian
%
% The fitting case is selected based on the set of specified initial
% parameters together with the set of parameters that should be optimized.
% The angle input/output should be in degree.
%
% Input:
%   img - NxM image to fit to. (internally converted to double)
%   param_init - Initial values for parameters. You can specify up to [xpos;ypos;A;BG;sigma_x,sigma_y,angle].
%                If negative or no values are given, the fitter estimates a value for that parameter if neccessary, 
%                with the exception of the angle, which is estimated if angle==0 and if it should be optimized.
%                If parameters are not optimized (see next entry) their values are kept constant during optimization.
%   param_optimizeMask - Must be true(1)/false(0) for every parameter [xpos,ypos,A,BG,sigma_x,sigma_y,angle].
%                Parameters with value 'false' are not fitted. | default: [1,1,1,1,1,0,0] -> 'optimize x,y,A,BG,sigma' (isoptric gaussian)
%   useIntegratedGauss - Wether to use pixel integrated gaussian or not 
%                        not supported for non-isotropic arbitrarily roated gaussians | default: false
%   useMLErefine - Use Poissonian noise based maximum likelihood estimation after
%                  least squares fit. Make sure to input image intensities in photons 
%                  for this to make sense. | default: false
%
% Output
%   params - Final parameters [xpos,ypos,A,BG,sigma_x,sigma_y,angle].
%   exitflag - Return state of optimizer. Positive = 'good'. Negative = 'bad'.
%         1 - CONVERGENCE
%        -1 - NO_CONVERGENCE
%        -2 - FAILURE
%
% Optimization is done using the ceres-solver library.
% http://ceres-solver.org, New BSD license.
% Copyright 2015 Google Inc. All rights reserved.
%
% Authors: Simon Christoph Stein and Jan Thiart
% Date: March 2016
% E-Mail: scstein@phys.uni-goettingen.de
% 


% -- Licensing --

% Copyright (c) 2016, Simon Christoph Stein
% All rights reserved.
% 
% Redistribution and use in source and binary forms, with or without
% modification, are permitted provided that the following conditions are met:
% 
% 1. Redistributions of source code must retain the above copyright notice, this
%    list of conditions and the following disclaimer.
% 2. Redistributions in binary form must reproduce the above copyright notice,
%    this list of conditions and the following disclaimer in the documentation
%    and/or other materials provided with the distribution.
% 
% THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
% ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
% WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
% DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR
% ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
% (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
% LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
% ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
% (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
% SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
% 
% The views and conclusions contained in the software and documentation are those
% of the authors and should not be interpreted as representing official policies,
% either expressed or implied, of the FreeBSD Project.


% License ceres-solver:
%
% Copyright 2015 Google Inc. All rights reserved.
%
% Redistribution and use in source and binary forms, with or without modification, 
% are permitted provided that the following conditions are met:
% 
%     1. Redistributions of source code must retain the above copyright notice, this list 
% of conditions and the following disclaimer.
%     2. Redistributions in binary form must reproduce the above copyright notice, this list
% of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution.
%     3. Neither the name of Google Inc., nor the names of its contributors may be used to
% endorse or promote products derived from this software without specific prior written permission.
%
% This software is provided by the copyright holders and contributors “AS IS” and any express or 
% implied warranties, including, but not limited to, the implied warranties of merchantability and 
% fitness for a particular purpose are disclaimed. In no event shall Google Inc. be liable for any 
% direct, indirect, incidental, special, exemplary, or consequential damages (including, but not limited
% to, procurement of substitute goods or services; loss of use, data, or profits; or business interruption)
% however caused and on any theory of liability, whether in contract, strict liability, or tort (including
% negligence or otherwise) arising in any way out of the use of this software, even if advised of the
% possibility of such damage.

% Make sure logicals are passed as correct datatype
if numel(varargin) >= 2 && ~isempty(varargin{2});  varargin{2} = logical(varargin{2});  end
if numel(varargin) >= 3 && ~isempty(varargin{3});  varargin{3} = logical(varargin{3});  end
if numel(varargin) >= 4 && ~isempty(varargin{4});  varargin{4} = logical(varargin{4});  end

% Set default optimization x,y,A,BG,sigma
if( numel(varargin)<2 || isempty(varargin{2}) )
    varargin{2} = logical([1,1,1,1,1,0,0]);
end

% Convert img to double if neccessary
[ params, exitflag ] = mx_psfFit( double(img), varargin{:});

end

