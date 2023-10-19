function Result = weightedPSFindex(Args)
    % build an array of spectra for ULTRASAT PSF weighting or find the index of a particular spectrum in this array
    % Input: - 
    %        * ...,key,val,...
    %        'SpecName' - a cell with spectrum name: or a stellar class:
    %                    {'a3','i'} or a temperature of a BB spectrum: 5700
    %        'AllSpec' - whether to produce a set of spectra to be used by ultrasat.weightedPSF
    % Output: - an array of AstroSpec or an index of a particular spectrum in the list
    % Author: A.M. Krassilchtchikov (Oct 2023)
    % Example: Ind = ultrasat.weightedPSFindex('SpecName',2e3);
    %          Ind = ultrasat.weightedPSFindex('SpecName',{'a47','iv'});
    %          Spec = ultrasat.weightedPSFindex('AllSpec','true'); 
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
    
    % Blackbody spectra:
    Wave = 2000:11000;
    Temp = [2e3 ,4e3 ,6e3 ,8e3 ,1e4 ,2e4 ,3e4 ,4e4, 5e4, 6e4, 7e4];
    for iT = 1:numel(Temp)
        Ind{end+1} = Temp(iT);
    end
    
    % build all the spectra
    if Args.AllSpec
        
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