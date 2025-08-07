function [Result,Tagg, Ephem] = searchVisitsSolarSys(ObjectName, Args)
    % One line description
    %     Optional detailed description
    % Input  : - 
    %          - 
    %          * ...,key,val,... 
    % Output : - 
    % Author : Eran Ofek (2025 Jul) 
    % Example: [R,Tagg,Ephem]=pipeline.last.queryDB.searchVisitsSolarSys('90004917', 'StartJD',2450814.5, 'StopJD',2450830.5);

    arguments
        ObjectName     = '90004917';  % C/2025 N1 (ATLAS)
        Args.StartJD   = ceil(celestial.time.julday())+0.5-100;
        Args.StopJD    = ceil(celestial.time.julday())+0.5-10;
        Args.StepSize    = 1;  
        Args.StepUnits   = 'd';
        Args.MagLimit  = 21; 
        Args.Constraints       = {'fwhm',[1.0 5.0]; 'airmass',[1 3.5]; 'ph_rms',[0 0.1]; 'limmag',[19 23]};
    end

    if nargout>2
        AddEphem = true;
    else
        AddEphem = false;
    end


    [Tephem] = celestial.SolarSys.getJPL_ephem(ObjectName,'EPHEM_TYPE','OBSERVER', 'TimeScale','UT', 'StartTime',Args.StartJD, 'StopTime',Args.StopJD);
    % sky motion [arcsec/min]


    if tools.table.isColumn(Tephem, 'APmag')
        Mag = Tephem.APmag;
    elseif tools.table.isColumn(Tephem, 'Tmag')
        Mag = Tephem.Tmag;
    else
        error('Unknown Mag option');
    end

    FlagMag = Mag<Args.MagLimit;
    Tephem  = Tephem(FlagMag,:);
    Nephem  = size(Tephem,1);
    Counter = 0;
    CounterEp = 0;
    Result = [];
    for Iephem=1:1:Nephem
        %[Iephem, Nephem]

        Tvisit = pipeline.last.queryDB.searchVisitsByCoo(Tephem.RA(Iephem), Tephem.Dec(Iephem), 'RangeJD',Tephem.Date(Iephem)+[-0.5, 0.5], 'QueryMethod','upix', 'Constraints',Args.Constraints);
        [Iephem, Nephem, size(Tvisit{1},1)]

        if ~isempty(Tvisit{1})
            Counter = Counter + 1;

            if AddEphem
                Nvisit = size(Tvisit{1},1);
                for Ivisit=1:1:Nvisit
                    CounterEp = CounterEp + 1;
                    JD = Tvisit{1}.jd_start(Ivisit);
                    [Tep1] = celestial.SolarSys.getJPL_ephem(ObjectName,'EPHEM_TYPE','OBSERVER', 'TimeScale','UT', 'StartTime',JD, 'StopTime',JD+0.1);
                    if CounterEp==1
                        Ephem = Tep1;
                    else
                        Ephem = [Ephem; Tep1];
                    end
                end
            end

            if Counter==1
                Tagg = Tvisit{1};
            else
                Tagg = [Tagg; Tvisit{1}];
            end
        end
    end


    [UnT, UnI] = unique(Tagg.id_visit);
    Result=Tagg(UnI,:);
    Ephem = Ephem(UnI,:);


end
