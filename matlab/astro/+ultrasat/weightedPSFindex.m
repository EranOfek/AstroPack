function Result = weightedPSFindex(Args)
    % index of spectra used for ULTRASAT PSF weighting
    % Input: - 
    %        * ...,key,val,...
    %        'SpecName' - a cell with spectrum name: or a stellar class
    %        {'a3','i'} or a temperature of a BB spectrum {5700}
    % Output: - an array of AstroSpec or a single AstroSpec
    arguments
        Args.SpecName = {};
        Args.AllSpec logical = false;
    end
    
    % first come the Pickles' spectra:
    Ind{1}     = {'o5','v'};
    Ind{end+1} = {'o8','iii'};
    Ind{end+1} = {'o9','v'};
    Ind{end+1} = {'b0','i'};
    Ind{end+1} = {'b0','v'};
    Ind{end+1} = {'b12','iii'};
    Ind{end+1} = {'b1','i'};
    Ind{end+1} = {'b1','v'};
    Ind{end+1} = {'b2','ii'};
    Ind{end+1} = {'b2','iv'};
    Ind{end+1} = {'b3','iii'};
    Ind{end+1} = {'b3','i'};
    Ind{end+1} = {'b3','v'};
    Ind{end+1} = {'b57','v'};
    Ind{end+1} = {'b5','iii'};
    Ind{end+1} = {'b5','ii'};
    Ind{end+1} = {'b5','i'};
    Ind{end+1} = {'b6','iv'};
    Ind{end+1} = {'b8','i'};
    Ind{end+1} = {'b8','v'};
    Ind{end+1} = {'b9','iii'};
    Ind{end+1} = {'b9','v'};
    Ind{end+1} = {'a0','iii'};
    Ind{end+1} = {'a0','i'};
    Ind{end+1} = {'a0','iv'};
    Ind{end+1} = {'a0','v'};
    Ind{end+1} = {'a2','i'};
    Ind{end+1} = {'a3','iii'};
    Ind{end+1} = {'a3','v'};
    Ind{end+1} = {'a47','iv'};
    Ind{end+1} = {'a5','iii'};
    Ind{end+1} = {'a5','v'};
    Ind{end+1} = {'a7','iii'};
    Ind{end+1} = {'a7','v'};
    Ind{end+1} = {'f02','iv'};
    Ind{end+1} = {'f0','iii'};
    
    NPick = numel(Ind);
    
    Temp = [2e3 ,4e3 ,6e3 ,8e3 ,1e4 ,2e4 ,3e4 ,4e4, 5e4, 6e4, 7e4];
    
    % build all the spectra
    if Args.AllSpec
        
        Spec = AstroSpec.specStarsPickles('G2','V');
        for Temp = [2e3 ,4e3 ,6e3 ,8e3 ,1e4 ,2e4 ,3e4 ,4e4, 5e4, 6e4, 7e4]
            Spec(end+1) = AstroSpec.blackBody(WavePSF,Temp);
        end    
        Result = Spec;
    else
        Ind = 1; % regexp(Args.SpecName,Ind)
        Result = Spec(Ind);
    end
    
end