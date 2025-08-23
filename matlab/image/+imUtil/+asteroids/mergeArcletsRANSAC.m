function [Result] = mergeArcletsRANSAC(Data, Args)
    % One line description
    %     Optional detailed description
    % Input  : - 
    %          - 
    %          * ...,key,val,... 
    % Output : - 
    % Author : Eran Ofek (2025 Aug) 
    % Example: 

    arguments
        Data
        Args.ColT      = 1;
        Args.ColRA     = 2;
        Args.ColDec    = 3;
        Args.ColRAPM   = 4;
        Args.ColDecPM  = 5;
        Args.AngUnits  = 'deg';

        Args.RefT      = [];
        
    end

    T     = Data(:,Args.ColT);
    RA    = Data(:,Args.ColRA);
    Dec   = Data(:,Args.ColDec);
    RAPM  = Data(:,Args.ColRAPM);
    DecPM = Data(:,Args.ColDecPM);

    Convfactor = convert.angular(Args.AngUnits, 'deg');
    RA         = RA.*ConvFactor;
    Dec        = Dec.*ConvFactor;
    RAPM       = RAPM.*ConvFactor;
    DecPM      = DecPM.*ConvFactor;

    if isempty(Args.RefT)
        RefT = (min(T) + max(T)).*0.5;
    end
    T = T - RefT;


    % fit all

    % If large residuals
    % fot all with RANSAC

    



end
