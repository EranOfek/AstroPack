function Result = weightedPSFindex(Args)
    % 1. build an array of spectra for ULTRASAT PSF weighting or 2. find the index of a particular spectrum in this array
    % Input: - 
    %        * ...,key,val,...
    %        'SpecName' - a cell with spectrum name: or a stellar class:
    %                    {'a3','i'} or a temperature of a BB spectrum: 5700
    %        'BuildSpec' - whether to produce a set of spectra to be used by ultrasat.weightedPSF
    % Output: - an array of AstroSpec or an index of a particular spectrum in the list
    % Author: A.M. Krassilchtchikov (Oct 2023)
    % Example: Ind = ultrasat.weightedPSFindex('SpecName',2e3);
    %          Ind = ultrasat.weightedPSFindex('SpecName',{'a47','iv'});
    %          Spec = ultrasat.weightedPSFindex('BuildSpec','true'); 
    arguments
        Args.SpecName = {};
        Args.BuildSpec logical = false;
    end
    
    % The Pickles' spectra:
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
    Ind{end+1} = {'f0','ii'};
    Ind{end+1} = {'f0','i'};
    Ind{end+1} = {'f0','v'};
    Ind{end+1} = {'f2','iii'};
    Ind{end+1} = {'f2','ii'};
    Ind{end+1} = {'f2','v'};
    Ind{end+1} = {'f5','iii'};
    Ind{end+1} = {'f5','iv'};
    Ind{end+1} = {'f5','v'};
    Ind{end+1} = {'f6','v'};
    Ind{end+1} = {'f8','i'};
    Ind{end+1} = {'f8','iv'};
    Ind{end+1} = {'f8','v'};
    
    Ind{end+1} = {'g0','iii'};
    Ind{end+1} = {'g0','i'};
    Ind{end+1} = {'g0','iv'};
    Ind{end+1} = {'g0','v'};
    Ind{end+1} = {'g2','i'};
    Ind{end+1} = {'g2','iv'};
    Ind{end+1} = {'g2','v'};
    Ind{end+1} = {'g5','iii'};
    Ind{end+1} = {'g5','i'};
    Ind{end+1} = {'g5','iv'};
    Ind{end+1} = {'g5','v'};
    Ind{end+1} = {'g8','iii'};
    Ind{end+1} = {'g8','i'};
    Ind{end+1} = {'g8','iv'};
    Ind{end+1} = {'g8','v'};
    
    Ind{end+1} = {'k01','ii'};
    Ind{end+1} = {'k0','iii'};
    Ind{end+1} = {'k0','iv'};
    Ind{end+1} = {'k0','v'};
    Ind{end+1} = {'k1','iii'};
    Ind{end+1} = {'k1','iv'};
    Ind{end+1} = {'k2','iii'};
    Ind{end+1} = {'k2','i'};
    Ind{end+1} = {'k2','v'};
    Ind{end+1} = {'k34','ii'};
    Ind{end+1} = {'k3','iii'};
    Ind{end+1} = {'k3','i'};
    Ind{end+1} = {'k3','iv'};
    Ind{end+1} = {'k4','iii'};
    Ind{end+1} = {'k4','i'};
    Ind{end+1} = {'k4','v'};
    Ind{end+1} = {'k5','iii'};
    Ind{end+1} = {'k5','v'};
    Ind{end+1} = {'k7','v'};
    
    Ind{end+1} = {'m0','iii'};
    Ind{end+1} = {'m0','v'};
    Ind{end+1} = {'m10','iii'};
    Ind{end+1} = {'m1','iii'};
    Ind{end+1} = {'m1','v'};
    Ind{end+1} = {'m2','iii'};
    Ind{end+1} = {'m2','i'};
    Ind{end+1} = {'m2p5','v'};
    Ind{end+1} = {'m2','v'};
    Ind{end+1} = {'m3','iii'};
    Ind{end+1} = {'m3','ii'};
    Ind{end+1} = {'m3','v'};
    Ind{end+1} = {'m4','iii'};
    Ind{end+1} = {'m4','v'};
    Ind{end+1} = {'m5','iii'};
    Ind{end+1} = {'m5','v'};
    Ind{end+1} = {'m6','iii'};
    Ind{end+1} = {'m6','v'};
    Ind{end+1} = {'m7','iii'};
    Ind{end+1} = {'m8','iii'};
    Ind{end+1} = {'m9','iii'};
    
    NPick = numel(Ind);
    
    % Blackbody spectra:
    Wave = 2000:11000;
    Temp = [2e3 ,4e3 ,6e3 ,8e3 ,1e4 ,2e4 ,3e4 ,4e4, 5e4, 6e4, 7e4];
    for iT = 1:numel(Temp)
        Ind{end+1} = Temp(iT);
    end
    
    % Some other type of spectra:
   
    % 1. Build all the spectra from the list OR
    % 2. For a given set of spectral parameters find its number (index) in the list
    if Args.BuildSpec          
        Spec = repmat(AstroSpec,1,numel(Ind));        
        for i = 1:NPick
            Spec(i) = AstroSpec.specStarsPickles(Ind{i}{1},Ind{i}{2});
        end        
        for i = NPick+1:NPick+numel(Temp)
            Spec(i) = AstroSpec.blackBody(Wave,Ind{i});
        end            
        Result = Spec;
    else 
        Elem = cellfun(@(x) isequal(x, Args.SpecName), Ind);
        Result = find(Elem);
    end
    
end