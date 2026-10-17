function R = tlogg2picklesClass(Teff,logg)
    % spectral class and luminosity class to be used with AstroSpec.specStarsPickles
    % Input: - Teff - eff. temp. in K
    %        - logg - log(g)
    % Output: - a structure containing spectral class (R.class) and luminosity class (R.lumclass)
    % Author: A.M. Krassilchtchikov (Sep 2023)
    % Example: R = astro.stars.tlogg2picklesClass(6e3,4.6);
    %          Spec = AstroSpec.specStarsPickles(R.class,R.lumclass);
    %
    % https://sites.uni.edu/morgans/astro/course/Notes/section2/spectraltemps.html
    % https://www.pas.rochester.edu/~emamajek/EEM_dwarf_UBVIJHK_colors_Teff(i).txt
    % (not complete, very crude)
    for i = 1:numel(Teff)
        for j = 1:numel(logg)
            if logg(j) > 4.5 % dwarves
                R(i,j).lumclass = 'v';
                if Teff(i) > 5.40e4
                    R(i,j).class = 'o5';
                elseif Teff(i) > 3.78e4
                    R(i,j).class = 'o9';
                elseif Teff(i) > 2.92e4
                    R(i,j).class = 'b0';
                elseif Teff(i) > 2.30e4
                    R(i,j).class = 'b1';
                elseif Teff(i) > 1.76e4
                    R(i,j).class = 'b3';
                elseif Teff(i) > 1.35e4
                    R(i,j).class = 'b57';
                elseif Teff(i) > 1.23e4
                    R(i,j).class = 'b8';
                elseif Teff(i) > 1.14e4
                    R(i,j).class = 'b9';
                elseif Teff(i) > 9.60e3
                    R(i,j).class = 'a0';
                elseif Teff(i) > 9.04e3
                    R(i,j).class = 'a2';
                elseif Teff(i) > 8.75e3
                    R(i,j).class = 'a3';
                elseif Teff(i) > 8.31e3
                    R(i,j).class = 'a5';
                elseif Teff(i) > 7.92e3
                    R(i,j).class = 'a7';
                elseif Teff(i) > 7.35e3
                    R(i,j).class = 'f0';
                elseif Teff(i) > 7.05e3
                    R(i,j).class = 'f2';
                elseif Teff(i) > 6.70e3
                    R(i,j).class = 'f5';
                elseif Teff(i) > 6.55e3
                    R(i,j).class = 'f6';
                elseif Teff(i) > 6.30e3
                    R(i,j).class = 'f8';
                elseif Teff(i) > 6.05e3
                    R(i,j).class = 'g0';
                elseif Teff(i) > 5.80e3
                    R(i,j).class = 'g2';
                elseif Teff(i) > 5.66e3
                    R(i,j).class = 'g5';
                elseif Teff(i) > 5.44e3
                    R(i,j).class = 'g8';
                elseif Teff(i) > 5.24e3
                    R(i,j).class = 'k0';
                elseif Teff(i) > 4.96e3
                    R(i,j).class = 'k2';
                elseif Teff(i) > 4.80e3
                    R(i,j).class = 'k3';
                elseif Teff(i) > 4.60e3
                    R(i,j).class = 'k4';
                elseif Teff(i) > 4.40e3
                    R(i,j).class = 'k5';
                elseif Teff(i) > 4.00e3
                    R(i,j).class = 'k7';
                elseif Teff(i) > 3.75e3
                    R(i,j).class = 'm0';
                elseif Teff(i) > 3.70e3
                    R(i,j).class = 'm1';
                elseif Teff(i) > 3.60e3
                    R(i,j).class = 'm2';
                elseif Teff(i) > 3.50e3
                    R(i,j).class = 'm3';
                elseif Teff(i) > 3.40e3
                    R(i,j).class = 'm4';
                elseif Teff(i) > 3.20e3
                    R(i,j).class = 'm5';
                elseif Teff(i) > 3.10e3
                    R(i,j).class = 'm6';
                else
                    R(i,j).class = 'm6'; % a temporary solution
                    %             error('star_picklesclass: no data for such a low T_eff');
                end
            elseif logg(j) > 3 % giants
                R(i,j).lumclass = 'iii';
                if Teff(i) > 3.00e4
                    R(i,j).class = 'o8';
                elseif Teff(i) > 1.40e4
                    R(i,j).class = 'b12';
                elseif Teff(i) > 1.28e4
                    R(i,j).class = 'b3';
                elseif Teff(i) > 1.15e4
                    R(i,j).class = 'b5';
                elseif Teff(i) > 9.70e3
                    R(i,j).class = 'b9';
                elseif Teff(i) > 9.40e3
                    R(i,j).class = 'a0';
                elseif Teff(i) > 8.80e3
                    R(i,j).class = 'a3';
                elseif Teff(i) > 8.30e3
                    R(i,j).class = 'a5';
                elseif Teff(i) > 8.00e3
                    R(i,j).class = 'a7';
                elseif Teff(i) > 7.50e3
                    R(i,j).class = 'f0';
                elseif Teff(i) > 7.20e3
                    R(i,j).class = 'f2';
                elseif Teff(i) > 6.80e3
                    R(i,j).class = 'f5';
                elseif Teff(i) > 5.80e3
                    R(i,j).class = 'g0';
                elseif Teff(i) > 5.10e3
                    R(i,j).class = 'g5';
                elseif Teff(i) > 5.05e3
                    R(i,j).class = 'g8';
                elseif Teff(i) > 4.90e3
                    R(i,j).class = 'k0';
                elseif Teff(i) > 4.70e3
                    R(i,j).class = 'k1';
                elseif Teff(i) > 4.50e3
                    R(i,j).class = 'k2';
                elseif Teff(i) > 4.30e3
                    R(i,j).class = 'k3';
                elseif Teff(i) > 4.10e3
                    R(i,j).class = 'k4';
                elseif Teff(i) > 3.75e3
                    R(i,j).class = 'k5';
                elseif Teff(i) > 3.66e3
                    R(i,j).class = 'm0';
                elseif Teff(i) > 3.60e3
                    R(i,j).class = 'm1';
                elseif Teff(i) > 3.50e3
                    R(i,j).class = 'm2';
                elseif Teff(i) > 3.30e3
                    R(i,j).class = 'm3';
                elseif Teff(i) > 9.60e3
                    R(i,j).class = 'm4';
                elseif Teff(i) > 3.10e3
                    R(i,j).class = 'm5';
                elseif Teff(i) > 2.95e3
                    R(i,j).class = 'm6';
                    %         elseif Teff(i) > 2.60e3
                    %             R(i,j).class = 'm7';
                    %         elseif Teff(i) > 2.60e3
                    %             R(i,j).class = 'm8';
                    %         elseif Teff(i) > 2.60e3
                    %             R(i,j).class = 'm9';
                    %         elseif Teff(i) > 2.60e3
                    %             R(i,j).class = 'm10';
                else
                    R(i,j).class = 'm7'; % a temporary solution
                    %             error('star_picklesclass: no data for such a low T_eff');
                end
            else
                error('star_picklesclass: supergiants currently not implemented');
            end
        end
    end
end
    
%%%%%%%%%%%%%


% ukb0i.mat
% ukb1i.mat
% ukb3i.mat
% ukb5i.mat
% ukb8i.mat
% 
% ukb2ii.mat
% ukb5ii.mat
% 
% ukb2iv.mat
% ukb6iv.mat
% 
% uka0i.mat
% uka2i.mat
% ukf5i.mat
% ukf8i.mat
% 
% uka0iv.mat
% uka47iv.mat
% ukf5iv.mat
% 
% ukf0i.mat
% 
% ukf0ii.mat
% ukf2ii.mat
% 
% ukf02iv.mat
% ukf8iv.mat
% 
% ukg0i.mat
% ukg2i.mat
% ukg5i.mat
% ukg8i.mat
% 
% ukg5ii.mat
% 
% ukg0iv.mat
% ukg2iv.mat
% ukg5iv.mat
% ukg8iv.mat
% 
% ukk2i.mat
% ukk3i.mat
% ukk4i.mat
% 
% ukk01ii.mat
% ukk34ii.mat
% 
% ukk0iv.mat
% ukk1iv.mat
% ukk3iv.mat
% 
% ukm2i.mat
% 
% ukm3ii.mat

