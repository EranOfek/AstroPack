function prepMPC_report(Cat, Args)
    %
    % Format description:
    % https://www.minorplanetcenter.net/iau/info/OpticalObs.html
    %
    
    
    arguments
        Cat 
        Args.JD                     % if given, override Args.JD
        Args.ColName
        Args.ColDesig
        Args.ColJD
        Args.ColRA
        Args.ColDec
        Args.ColMag
        
        Args.StackedImage logical  = false;  % if true, set Note1 to 'K'
        Args.Note1                 = 'K  
        Args.Note2                 = 'B';  % 'C' - CCD; 'B' - CMOS
        Args.Band                  = [];
        Args.ObsCode               = [];
        
    end
    
    if isa(Cat, 'AstroCatalog')
        Name  = getCol(Cat, Args.ColName);
        Desig = getCol(Cat, Args.ColDesig);
        Data  = getCol(Cat, {Args.ColJD, Args.ColRA, Args.ColDec, Args.ColMag});
        JD    = Data(:,1);
        RA    = Data(:,2);
        Dec   = Data(:,3);
        Mag   = Data(:,4);
    elseif istable(Cat)
        Name  = Cat.(Args.ColName);
        Desig = Cat.(Args.ColDesig);
        JD    = Cat.(Args.ColJD);
        RA    = Cat.(Args.ColRA);
        Dec   = Cat.(Args.ColDec);
        Mag   = Cat.(Args.ColMag);
    else
        error('Unknown Cat type option');
    end
    
    Nobs   = numel(RA);
    if ~isempty(Args.JD)
        JD = Args.JD(:).*ones(Nobs,1);
    end
    
    % for objects without Name use Designation
    for Iobs=1:1:Nobs
        if isnan(Name(Iobs))
            if isempty(Desig{Iobs})
                
            else
                % use Desig
                fprintf('     %12s%1s%04d %02d %9.6f%02d %02d %06.3f%+02d %02d %05.2f%5.2f%1s      %3s',  Desig{Iobs}, Discovery, Args.Note1, Args.Note2, Date, RA, Dec, Mag, Band, ObsCode);
            end
        else
            if isnan(
            fprintf('%05d',
    
    
    
    
end