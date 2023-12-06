function [Cat,Ref,FlagCat,FlagRef,Summary]=prep_cat_for_astrometry(Cat, Ref, Args)
% Clean two catalogs and equalize their surface density
% Package: imUtil.patternMatch
% Description: Given two catalogs (e.g., Cat and Ref), clean the catalogs
%              by removing NaN coordinates,
%              imUtil.cat.flag_overdense_colrow, imUtil.cat.flag_overdense,
%              estimate their density using imUtil.cat.surface_density, and
%              equalize their surface density by removing the faintest
%              sources in one of the catalogs using
%              imUtil.cat.dilute_cat_by_mag
% Input  : - A catalog with [X,Y,Mag] columns.
%          - A Ref catalog wiyh [X,Y,Mag] columns.
%          * Pairs of ...,key,val,... Possible keywords include:
%            'CatRemoveNaN' - A logical indicating if to remove NaN
%                   coordinates from the Cat. Default is true.
%            'CatRemoveBadColRow' - A logical indicating if to remove
%                   source in overdense columns/rows
%                   from the Cat. Default is true.
%            'CatRemoveOverDense' - A logical indicating if to remove
%                   source in overdense regions
%                   from the Cat. Default is true.
%            'RefRemoveNaN'  - A logical indicating if to remove NaN
%                   coordinates from the Ref. Default is false.
%            'RefRemoveBadColRow' - A logical indicating if to remove
%                   source in overdense columns/rows
%                   from the Ref. Default is true.
%            'RefRemoveOverDense' - A logical indicating if to remove
%                   source in overdense regions
%                   from the Ref. Default is false.
%            'EqualizeDensity' - A logical indicating if to equalize the
%                   surface density of the two catalogs.
%            'DiluteThreshold' - If the surface density of the Ref minus
%                   Cat divided by Cat (abs value) is larger than this
%                   factor then applay source diluation.
%                   Default is 0.5.
%            'ColRowPar' - A cell array of addotional parameters to pass to
%                   imUtil.cat.flag_overdense_colrow.
%                   Default is {}.
%            'OverdensePar' - A cell array of addotional parameters to pass to
%                   imUtil.cat.flag_overdense
%                   Default is {}.
%            'CatHalfSize' - Either radius, or [half width, half height] of
%                   the Cat catalog. If empty, then estimate area using
%                   convex hull. Default is empty.
%            'RefHalfSize' - Either radius, or [half width, half height] of
%                   the Ref catalog. If empty, then estimate area using
%                   convex hull. Default is empty.
%            'ColCatX' - X coordinates column index in the Cat.
%                   Default is 1.
%            'ColCatY' - Y coordinates column index in the Cat.
%                   Default is 2.
%            'ColCatMag' - Mag column index in the Cat.
%                   Default is 3.
%            'ColRefX' - X coordinates column index in the Ref.
%                   Default is 1.
%            'ColRefY' - Y coordinates column index in the Ref.
%                   Default is 2.
%            'ColRefMag' - Mag column index in the Ref.
%                   Default is 3.
% Output : - A new clean/diluted catalog.
%          - A new clean/diluted Ref catalog.
%          - A vector of logicals indicating the selected sources in Cat.
%          - A vector of logicals indicating the selected sources in Ref.
%          - Summary of number of sources survived after each step.
% License: GNU general public license version 3
% Tested : Matlab R2015b
%     By : Eran O. Ofek                    Apr 2016
%    URL : http://weizmann.ac.il/home/eofek/matlab/
% Example:
% [Cat,Ref]=imUtil.patternMatch.prep_cat_for_astrometry(rand(100,3).*1000,rand(200,3).*1000);
% Reliable: 2


arguments
    Cat
    Ref
    Args.CatRemoveNaN(1,1) logical         = true;
    Args.CatRemoveBadColRow(1,1) logical   = true;
    Args.CatRemoveOverDense(1,1) logical   = true;
    Args.RefRemoveNaN(1,1) logical         = false;
    Args.RefRemoveBadColRow(1,1) logical   = false;
    Args.RefRemoveOverDense(1,1) logical   = true;
    Args.EqualizeDensity(1,1) logical      = true;
    Args.DiluteThreshold                   = 0.5;
    Args.ColRowPar cell                    = {};
    Args.OverdensePar cell                 = {};
    Args.CatHalfSize                       = [];
    Args.RefHalfSize                       = [];
    Args.ColCatX(1,1)                      = 1;
    Args.ColCatY(1,1)                      = 2;
    Args.ColCatMag(1,1)                    = 3;
    Args.ColRefX(1,1)                      = 1;
    Args.ColRefY(1,1)                      = 2;
    Args.ColRefMag(1,1)                    = 3;
end

% clean catalog

NsrcCat = size(Cat,1);
NsrcRef = size(Ref,1);
FlagCat = true(NsrcCat,1);
FlagRef = true(NsrcRef,1);

Summary.Cat.Start = NsrcCat;
Summary.Ref.Start = NsrcRef;

% remove NaNs:
if Args.CatRemoveNaN
    FlagCat = FlagCat & ~isnan(Cat(:,Args.ColCatX));
end
if Args.RefRemoveNaN
    FlagRef = FlagRef & ~isnan(Ref(:,Args.ColRefX));
end
Summary.Cat.RemoveNaN = sum(FlagCat);
Summary.Ref.RemoveNaN = sum(FlagRef);


% remove sources acculated on overdense bad column/rows

if Args.CatRemoveBadColRow
    FlagCat = FlagCat & ~imUtil.cat.flag_overdense_colrow(Cat(:,[Args.ColCatX,Args.ColCatY]),Args.ColRowPar{:},'Dim',1);
    FlagCat = FlagCat & ~imUtil.cat.flag_overdense_colrow(Cat(:,[Args.ColCatX,Args.ColCatY]),Args.ColRowPar{:},'Dim',2);
end
if Args.RefRemoveBadColRow
    FlagRef = FlagRef & ~imUtil.cat.flag_overdense_colrow(Cat(:,[Args.ColRefX,Args.ColRefY]),Args.ColRowPar{:},'Dim',1);
    FlagRef = FlagRef & ~imUtil.cat.flag_overdense_colrow(Cat(:,[Args.ColRefX,Args.ColRefY]),Args.ColRowPar{:},'Dim',2);
end
Summary.Cat.RemoveBadColRow = sum(FlagCat);
Summary.Ref.RemoveBadColRow = sum(FlagRef);


% remove sources in overdense regions:
if Args.CatRemoveOverDense
    FlagCat = FlagCat & ~imUtil.cat.flag_overdense(Cat(:,[Args.ColCatX,Args.ColCatY]),Args.OverdensePar{:});
    FlagCat = FlagCat & ~imUtil.cat.flag_overdense(Cat(:,[Args.ColCatX,Args.ColCatY]),Args.OverdensePar{:});
end
if Args.RefRemoveOverDense
    FlagRef = FlagRef & ~imUtil.cat.flag_overdense(Ref(:,[Args.ColRefX,Args.ColRefY]),Args.OverdensePar{:});
    FlagRef = FlagRef & ~imUtil.cat.flag_overdense(Ref(:,[Args.ColRefX,Args.ColRefY]),Args.OverdensePar{:});
end
Summary.Cat.RemoveOverDense = sum(FlagCat);
Summary.Ref.RemoveOverDense = sum(FlagRef);

Cat = Cat(FlagCat,:);
Ref = Ref(FlagRef,:);

% dilute by magnitude
if Args.EqualizeDensity
    if ~isempty(Cat) && ~isempty(Ref)


        [DensityCat,AreaCat] = imUtil.cat.surface_density(Cat(:,[Args.ColCatX,Args.ColCatY]) ,Args.CatHalfSize);
        [DensityRef,AreaRef] = imUtil.cat.surface_density(Ref(:,[Args.ColRefX,Args.ColRefY]) ,Args.RefHalfSize);
 
        
        Ratio = abs(DensityRef - DensityCat)./DensityCat;
        if Ratio>Args.DiluteThreshold
            % difference in density between Cat and Ref is larger than the
            % threshold

            if DensityCat>DensityRef
                % dilute Cat
                FlagCatD = imUtil.cat.dilute_cat_by_mag(Cat,{},Args.ColCatMag,DensityCat,DensityRef);
                FlagCatT = true(size(FlagCat));
                FlagCatT(FlagCat) = FlagCatD;
                FlagCat = FlagCat & FlagCatT;
                
                Cat = Cat(FlagCatD,:);
            else
                FlagRefD = imUtil.cat.dilute_cat_by_mag(Ref,{},Args.ColRefMag,DensityRef,DensityCat);
                FlagRefT = true(size(FlagRef));
                FlagRefT(FlagRef) = FlagRefD;
                FlagRef = FlagRef & FlagRefT;
                
                Ref = Ref(FlagRefD,:);
            end
        end
        
        Summary.Cat.EqualizeDensity = sum(FlagCat);
        Summary.Ref.EqualizeDensity = sum(FlagRef);  
        Summary.AreaCat = AreaCat;
        Summary.AreaRef = AreaRef;
        Summary.DensityDEviation   = Ratio;   % 0 means equal density

    end    
end
    

    
    

