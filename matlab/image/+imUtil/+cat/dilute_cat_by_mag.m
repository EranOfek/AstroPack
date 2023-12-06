function [Flag,Cat]=dilute_cat_by_mag(Cat,ColCell,MagCol,Density,TargetDensity)
% Remove faint sources from a catalog to have a specific surface density
% Package: +imUtil.cat
% Description: Dilute sources from a acatalog based on sources magnitude.
%              Remove the faintest sources and sources which have NaN
%              magnitude, such that the surface density of the diluted
%              catalog will be equal to some target surface density.
% Input  : - The catalog, in which one of the columns is a magnitude
%            column.
%          - A cell array of column names. If empty, then the next input
%            argument must be numeric.
%          - The index of the magnitude column name in the input catalog,
%            or the magnitude column name (string).
%            The name will be searched in the cell array of column names,
%            first using strcmp, and if not sucessful using strfind.
%          - The surface density of the input catalog.
%          - The target (required) density of the catalog. The surafce
%            density of the selected sources will be equal to this surface
%            density.
% Output : - A vector of logical flags of selected sources (rows).
%          - The diluted catalog.
%      By: Eran O. Ofek                         Apr 2020
% Example: [Flag,Cat]=imUtil.cat.dilute_cat_by_mag(rand(100,3),{'RA','Dec','Mag_G'},'Mag',100,20)

if Density>TargetDensity
    % dilute

    % Find column name
    if ~isnumeric(MagCol)
        % assume MagCol is a string
        Flag = strcmp(MagCol,ColCell);
        if ~any(Flag)
            % attempt strfind
            Tmp = strfind(ColCell,MagCol);
            Flag = cellfun(@(x) ~isempty(x),Tmp);
        end
        if sum(Flag)~=1
            error('Found %d relevant column names - should be exactly 1',sum(Flag));
        end

    else
        Flag = MagCol;
        
    end     
    % select Mag column
    Mag = Cat(:,Flag);
    
    Threshold = quantile(Mag,TargetDensity./Density);
    
    Flag = Mag<Threshold & ~isnan(Mag);
    
    if nargout>1
        Cat = Cat(Flag,:);
    end
    
else
    Nsrc = size(Cat,1);
    Flag = true(Nsrc,1);
end

