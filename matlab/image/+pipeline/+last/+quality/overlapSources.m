function [Result] = overlapSources(AI, Args)
    % compare source characteristics from overlapping image crops
    %     Optional detailed description
    % Input  : - an AstroImage containing all the crops (proc or coadd)    
    %          * ...,key,val,... 
    %         'MagCut' - a limiting magnitude employed for the comparison
    %         'Prop'   - a list of columns to compare
    %         'MatchRadius' - match radius in arcsec
    % Output : - a struct with statistics:
    % Author : A.M. Krassilchtchikov (2026 Feb) 
    % Example: R = pipeline.last.quality.overlapSources(Coadd);
    %
    arguments
        AI      
        Args.MagCut = 16;  
        Args.Prop   = {'RA', 'Dec', 'MAG_APER_3', 'MAG_PSF'};
        Args.MatchRadius = 3; % arcsec
    end
    % read the list of overlap interfaces:
    Ind   = LASToverlaps;
    Nvrlp = numel(Ind);
    % loop over all the input properties
    for Iprop = 1:numel(Args.Prop)
        Prop = Args.Prop{Iprop};
        % loop over all the possible pairs of crops        
        for Ivrlp = 1:Nvrlp
            MS = imProc.match.match(AI(Ind(Ivrlp,1)).CatData, AI(Ind(Ivrlp,2)).CatData, 'Radius', Args.MatchRadius);
            FlagMag = MS.Table.MAG_APER_3 < Args.MagCut;
            Diff = MS.Table.(Prop) - AI(Ind(Ivrlp,2)).CatData.Table.(Prop);
            Result(Ivrlp).(Prop).MedianDiff = nanmedian(Diff(FlagMag), 1);
            Result(Ivrlp).(Prop).StdDiff    = nanstd(Diff(FlagMag),[],1);           
        end
    end
end
%
function Ind = LASToverlaps 
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