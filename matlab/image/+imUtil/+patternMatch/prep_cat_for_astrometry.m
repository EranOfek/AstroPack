function [Cat,Ref,FlagCat,FlagRef]=prep_cat_for_astrometry(Cat,Ref,varargin)
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
% License: GNU general public license version 3
% Tested : Matlab R2015b
%     By : Eran O. Ofek                    Apr 2016
%    URL : http://weizmann.ac.il/home/eofek/matlab/
% Example:
% [Cat,Ref]=imUtil.patternMatch.prep_cat_for_astrometry(rand(100,3).*1000,rand(200,3).*1000);
% Reliable: 2


InPar = inputParser;
addOptional(InPar,'CatRemoveNaN',true);
addOptional(InPar,'CatRemoveBadColRow',true);
addOptional(InPar,'CatRemoveOverDense',true);
addOptional(InPar,'RefRemoveNaN',false);
addOptional(InPar,'RefRemoveBadColRow',false);
addOptional(InPar,'RefRemoveOverDense',true);
addOptional(InPar,'EqualizeDensity',true);
addOptional(InPar,'DiluteThreshold',0.5);
addOptional(InPar,'ColRowPar',{});
addOptional(InPar,'OverdensePar',{});

addOptional(InPar,'CatHalfSize',[]);
addOptional(InPar,'RefHalfSize',[]);

addOptional(InPar,'ColCatX',1);
addOptional(InPar,'ColCatY',2);
addOptional(InPar,'ColCatMag',3);
addOptional(InPar,'ColRefX',1);
addOptional(InPar,'ColRefY',2);
addOptional(InPar,'ColRefMag',3);


parse(InPar,varargin{:});
InPar = InPar.Results;



% clean catalog

NsrcCat = size(Cat,1);
NsrcRef = size(Ref,1);
FlagCat = true(NsrcCat,1);
FlagRef = true(NsrcRef,1);

% remove NaNs:
if InPar.CatRemoveNaN
    FlagCat = FlagCat & ~isnan(Cat(:,InPar.ColCatX));
end
if InPar.RefRemoveNaN
    FlagRef = FlagRef & ~isnan(Ref(:,InPar.ColRefX));
end

% remove sources acculated on overdense bad column/rows

if InPar.CatRemoveBadColRow
    FlagCat = FlagCat & ~imUtil.cat.flag_overdense_colrow(Cat(:,[InPar.ColCatX,InPar.ColCatY]),InPar.ColRowPar{:},'Dim',1);
    FlagCat = FlagCat & ~imUtil.cat.flag_overdense_colrow(Cat(:,[InPar.ColCatX,InPar.ColCatY]),InPar.ColRowPar{:},'Dim',2);
end
if InPar.RefRemoveBadColRow
    FlagRef = FlagRef & ~imUtil.cat.flag_overdense_colrow(Cat(:,[InPar.ColRefX,InPar.ColRefY]),InPar.ColRowPar{:},'Dim',1);
    FlagRef = FlagRef & ~imUtil.cat.flag_overdense_colrow(Cat(:,[InPar.ColRefX,InPar.ColRefY]),InPar.ColRowPar{:},'Dim',2);
end


% remove sources in overdense regions:
if InPar.CatRemoveOverDense
    FlagCat = FlagCat & ~imUtil.cat.flag_overdense(Cat(:,[InPar.ColCatX,InPar.ColCatY]),InPar.OverdensePar{:});
    FlagCat = FlagCat & ~imUtil.cat.flag_overdense(Cat(:,[InPar.ColCatX,InPar.ColCatY]),InPar.OverdensePar{:});
end
if InPar.RefRemoveOverDense
    FlagRef = FlagRef & ~imUtil.cat.flag_overdense(Ref(:,[InPar.ColRefX,InPar.ColRefY]),InPar.OverdensePar{:});
    FlagRef = FlagRef & ~imUtil.cat.flag_overdense(Ref(:,[InPar.ColRefX,InPar.ColRefY]),InPar.OverdensePar{:});
end



Cat = Cat(FlagCat,:);
Ref = Ref(FlagRef,:);

% dilute by magnitude
if InPar.EqualizeDensity
    if ~isempty(Cat) && ~isempty(Ref)


        [DensityCat,AreaCat] = imUtil.cat.surface_density(Cat(:,[InPar.ColCatX,InPar.ColCatY]) ,InPar.CatHalfSize);
        [DensityRef,AreaRef] = imUtil.cat.surface_density(Ref(:,[InPar.ColRefX,InPar.ColRefY]) ,InPar.RefHalfSize);
 
        
        Ratio = abs(DensityRef - DensityCat)./DensityCat;
        if Ratio>InPar.DiluteThreshold
            % difference in density between Cat and Ref is larger than the
            % threshold

            if DensityCat>DensityRef
                % dilute Cat
                FlagCatD = imUtil.cat.dilute_cat_by_mag(Cat,{},InPar.ColCatMag,DensityCat,DensityRef);
                FlagCatT = true(size(FlagCat));
                FlagCatT(FlagCat) = FlagCatD;
                FlagCat = FlagCat & FlagCatT;
                
                Cat = Cat(FlagCatD);
            else
                FlagRefD = imUtil.cat.dilute_cat_by_mag(Ref,{},InPar.ColRefMag,DensityRef,DensityCat);
                FlagRefT = true(size(FlagRef));
                FlagRefT(FlagRef) = FlagRefD;
                FlagRef = FlagRef & FlagRefT;
                
                Ref = Ref(FlagRefD);
            end
        end
    end
end
    
    
    
    

