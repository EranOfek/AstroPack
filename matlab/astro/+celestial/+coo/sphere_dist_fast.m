function [Dist,Ang,PA]=sphere_dist_fast(RA_1,Dec_1,RA_2,Dec_2)
% Calculate the angular distance between two points on the celestial sphere.
% Description: Calculate the angular distance between two points on the
%              celestial sphere. See sphere_dist.m (and built in distance.m)
%                for a more general function. This function is ~10 time
%              faster than sphere_dist.m, but it works only with radians
%                and calculate only the distance.
%   Known issue: In cases that the input is mixture of single and doubles
%   the ouput may be truncated. See issue 252.
%   It is recomended to use inputs which are double.
% Input  : - Matrix of logitudes for the first point [radian].
%          - Matrix of latitudes for the first point [radian].
%          - Matrix of logitudes for the second point [radian].
%          - Matrix of latitudes for the second point [radian].
% Output : - Matrix of distances between points [radian].
%          - Matrix of position angles between points [radian].
%            Measured westward. Take 2*pi-PA to get the PA measured
%            Eastward.
%          - Matrix of P.A. of the first point relative to the second point
%            (eastwrad from the north).
% Tested : Matlab 2011b
%     By : Eran O. Ofek                    Feb 2013
%    URL : http://weizmann.ac.il/home/eofek/matlab/
% See also: sphere_dist.m, sphere_dist_cosd.m (and built in distance.m).
% Example: D=celestial.coo.sphere_dist_fast(RA1,Dec1,RA2,Dec2);
% Reliable: 1
%--------------------------------------------------------------------------
 

%     arguments
%         RA_1
%         Dec_1
%         RA_2
%         Dec_2
%         UseMex logical = true;     	% True: Use MEX implementation, False: Use MATLAB implementaion
%         UseMP logical  = true;      	% True: Use threading with OpenMP multi-threading library
%     end
% 
% 
%     % MATLAB implementation
%     if ~UseMex
%         Dist = double(acos(sin(Dec_1).*sin(Dec_2) + cos(Dec_1).*cos(Dec_2).*cos(RA_1-RA_2)));
%         return;
%     end
%     
%     % MEX implementation
%     % Call function according to input data type
%     C1 = lower(class(RA_1));
%     C2 = lower(class(RA_2));
%     C3 = lower(class(Dec_1));
%     C4 = lower(class(Dec_2));
%     if isequal(C1,C2) || isequal(C2,C3) || isequal(C3,C4)    
%         switch C1
%             case {'single'}
%                 Dist = celestial.coo.mex.mex_sphere_dist_fast_single(RA_1,Dec_1,RA_2,Dec_2);
%             case {'double'}
%                 Dist = celestial.coo.mex.mex_sphere_dist_fast_double(RA_1,Dec_1,RA_2,Dec_2);
%             otherwise
%                 error('celestial.coo.mex.mex_sphere_dist_fast - Unsupported data type');
%         end
%     else
%         error('celestial.coo.mex.mex_sphere_dist_fast - data types of input are ambigious');
%     end
%     
% end
% 



%Dist = acos(sin(Dec_1).*sin(Dec_2) + sqrt(1-sin(Dec_1).^2).*sqrt(1-sin(Dec_2).^2).*cos(RA_1-RA_2));  % this is more accurate

%Dist = acos(sin(Dec_1).*sin(Dec_2) + cos(Dec_1).*cos(Dec_2).*cos(RA_1-RA_2));  % this is more accurate
%Dist = real(Dist);

% haversine formulae - faster and nore stable
Dist = 2.*asin( sqrt(sin(0.5.*(Dec_2-Dec_1)).^2 + cos(Dec_1).*cos(Dec_2).*sin(0.5.*(RA_2-RA_1)).^2 ));

Dist = real(Dist);


if (nargout>1)
    dRA = RA_1-RA_2;
    SinPA = sin(dRA).*cos(Dec_2)./sin(Dist);
    CosPA = (sin(Dec_2).*cos(Dec_1) - cos(Dec_2).*sin(Dec_1).*cos(dRA))./sin(Dist);

    Ang    = atan2(real(SinPA),real(CosPA));
    %PA(PA<0) = 2.*pi + PA(PA<0);

    I     = Ang<0;
    Ang(I) = 2.*pi + Ang(I);

    if nargout>2
        PA    = atan2(real(SinPA),-real(CosPA));
        PA(PA<0) = PA(PA<0) + 2.*pi;
    end

end
