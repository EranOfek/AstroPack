function getJPL_allOrbitalElements(Args)
    % Retrieve orbital elements as a function of time for all minor planets
    %   Retrieve from JPL horizons, from StartTime to EndTime.
    %   data is save is Dir.
    % Input  : * ...,key,val,... 
    %            'OrbEl' - A celestial.OrbitalEl object. Elements as a function
    %                   of epoch will be retrieved for each source in this list.
    %                   If emptyy, then use: celestial.OrbitalEl.loadSolarSystem([]);
    %                   In this case comets are ignored (Comet names are
    %                   not recognized from some reasons - need to debug).
    %                   Default is [].
    %            'StartTime' - Default is [1 1 2000]
    %            'StopTime' - Default is [1 1 2030];
    %            'StepSize' - Default is 100.
    %            'StepUnits' - Default is 'd'.
    %            'ElementsInFile' - Default is 5000.
    %            'Dir' - Directory in which to save the files
    %                   Default is
    %                   '/home/eran/matlab/data/SolarSystem/MinorPlanetsET/'
    % Output : null
    %          Files named OrbElET_%d_%8d_%8d.mat',I, StartInd, EndInd
    %          and OrbEl.mat (the element file from JPL).
    % Author : Eran Ofek (2023 Nov) 
    % Example: celestial.SolarSys.getJPL_allOrbitalElements
    
    arguments
        Args.OrbEl             = [];
        Args.StartTime         = [1 1 2010];
        Args.StopTime          = [1 1 2040];
        Args.StepSize          = 100;
        Args.StepUnits         = 'd';
        
        Args.ElementsInFile    = 10; %5000;
        
        Args.Dir               = '/home/eran/matlab/data/SolarSystem/MinorPlanetsET/';
    end

    
    PWD = pwd;
    cd(Args.Dir);
    
    if isempty(Args.OrbEl)
        OrbEl = celestial.OrbitalEl.loadSolarSystem([]);
        if numel(OrbEl)>2
            OrbEl = OrbEl(1:2);
        end
    else
        OrbEl = Args.OrbEl;
    end
    save -v7.3 OrbEl.mat OrbEl
    
    
    
    for I=1:1:numel(OrbEl)
        Nel = OrbEl(I).numEl;
        StartInd = 1;
        OrbElET  = [];
        for Iel=1:1:Nel
            [I, Iel, Nel]
            if isempty(OrbEl(I).Number)
                Name = OrbEl(I).Designation{Iel};
            else
                Name = sprintf('%d;',OrbEl(I).Number(Iel));
            end
                
            
            [OrbElET] = celestial.SolarSys.getJPL_ephem(Name,'EPHEM_TYPE','ELEMENTS','TimeScale','TDB',...
                                              'StartTime',Args.StartTime,...
                                              'StopTime',Args.StopTime,...
                                              'StepSize',Args.StepSize,...
                                              'StepUnits',Args.StepUnits,...
                                              'OutType','OrbitalEl',...
                                              'OrbEl',OrbElET);
            %
            if (Iel./Args.ElementsInFile)==floor(Iel./Args.ElementsInFile) || Iel==Nel
                FileName = sprintf('OrbElET_%d_%08d_%08d.mat',I, StartInd, Iel);
                save('-v7.3', FileName, 'OrbElET');
                OrbElET = [];
                StartInd = Iel + 1;
            end
                
        end
    end
    
    cd(PWD)
    
end
