function [Result] = overlapSources(AI, Args)
    % compare source characteristics from overlapping image crops
    %     Optional detailed description
    % Input  : - an AstroImage containing all the crops (proc or coadd)    
    %          * ...,key,val,... 
    %         'MagCut' - a limiting magnitude employed for the comparison
    %         'Prop'   - a list of columns to compare
    %         'MatchRadius' - match radius in arcsec
    % Output : - a struct with statistics for each Crop
    % Author : A.M. Krassilchtchikov (2026 Feb) 
    % Example: R = pipeline.last.quality.overlapSources(Coadd);
    %
    arguments
        AI      
        Args.MagCut = 17;  
        Args.Prop   = {'RA', 'Dec', 'MAG_APER_3', 'MAG_PSF'};
        Args.MatchRadius = 3; % arcsec
    end
    % read the list of overlap interfaces:
    Ind   = LASToverlapsNew;
    Nvrlp = size(Ind,1);
    % loop over all the possible pairs of crops
    for Ivrlp = 1:Nvrlp
        MS = imProc.match.match(AI(Ind(Ivrlp,1)).CatData, AI(Ind(Ivrlp,2)).CatData, 'Radius', Args.MatchRadius);
        FlagMag = MS.Table.MAG_APER_3 < Args.MagCut;
        if sum(FlagMag) > 0
            fprintf('%d overlap sources found between crops %d and %d\n',sum(FlagMag),Ind(Ivrlp,1), Ind(Ivrlp,2));
            for Iprop = 1:numel(Args.Prop)
                Prop = Args.Prop{Iprop};
                Diff = MS.Table.(Prop) - AI(Ind(Ivrlp,2)).CatData.Table.(Prop);
                Result.(Prop).MedianDiff(Ivrlp) = nanmedian(Diff(FlagMag), 1);
                Result.(Prop).StdDiff(Ivrlp)    = nanstd(Diff(FlagMag),[],1);
            end
        else
            fprintf('No overlap sources found between crops %d and %d\n',Ind(Ivrlp,1), Ind(Ivrlp,2));
            for Iprop = 1:numel(Args.Prop)
                Prop = Args.Prop{Iprop};
                Result.(Prop).MedianDiff(Ivrlp) = NaN;
                Result.(Prop).StdDiff(Ivrlp)    = NaN;
            end
        end
    end    
end
%
function Ind = LASToverlapsNew 
% NB: this is LAST-specific!
    Ind = [1 2;   2  3;  3  4; ...
           5 6;   6  7;  7  8; ...
           9 10; 10 11; 11 12; ...
          13 14; 14 15; 15 16; ...
          17 18; 18 19; 19 20; ...
          21 22; 22 23; 23 24; ...
           ...
           1 5; 5  9;  9 13; 13 17; 17 21; ...
           2 6; 6 10; 10 14; 14 18; 18 22; ...
           3 7; 7 11; 11 15; 15 19; 19 23; ...
           4 8; 8 12; 12 16; 16 20; 20 24];   
end

function Ind = LASToverlapsOld 
% NB: this is LAST-specific!
    Ind = [1 7;   7 13; 13 19; ...
           2 8;   8 14; 14 20; ...
           3 9;   9 15; 15 21; ...
           4 10; 10 16; 16 22; ...
           5 11; 11 17; 17 23; ...
           6 12; 12 18; 18 24; ...
           ...
           1 2; 2 3; 3 4; 4 5; 5 6; ...
           7 8; 8 9; 9 10; 10 11; 11 12; ...
           13 14; 14 15; 15 16; 16 17; 17 18; ...
           19 20; 20 21; 21 22; 22 23; 23 24];   
end