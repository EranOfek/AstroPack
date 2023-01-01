function [ZP, Filters] = survey_ZP(DR, Type)
    % return zero points of different surveys
    % Input  : - Version. Default is 'GAIAEDR3'.
    %          - Mag type: 'Vega' | ['AB'] | 'VegaMinusAB'
    % Output : - ZP for filters
    %          - Filter names
    % AUthor : Eran Ofek (Oct 2021)
    % Example: ZP = astro.mag.survey_ZP
   
    arguments
        DR     = 'GAIAEDR3';
        Type   = 'AB';  % 'Vega' | 'AB' | 'VegaMinusAB'
        %Filter = {'G','Bp','Rp'};
    end
    
    switch lower(DR)
        case {'gaiaedr3','gaiadr3'}
            Vega = [25.6874	25.3385	24.7479];
            AB   = [25.8010 25.3540 25.1040];
            Filters = {'G','Bp','Rp'};
        otherwise
                error('Unknown DR option');
    end
            
    
    switch lower(Type)
        case 'ab'
            ZP = AB;
        case 'vega'
            ZP = Vega;
        case 'vegaminusab'
            ZP = Vega - AB;
        otherwise
            error('Unknown Type option');
    end
            
    
end