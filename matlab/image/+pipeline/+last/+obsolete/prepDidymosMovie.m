function CoaddAI=prepDidymosMovie(CropAI,Args)
    %
    % Example: CoaddAI=pipeline.last.prepDidymosMovie(CropAI)
    
    arguments
        CropAI
        
        Args.ZP                = [];
        Args.MaxTime           = 3800;
        Args.JD0               = celestial.time.julday([26 9 2022 23 15 1.9]);
        Args.TimeStep          = 10;   % s
        Args.BinTime           = 0.1;
        Args.TimeBinAfter0     = @(TimeSec) max(60, TimeSec.*0.1);   % s
        Args.Filter            = imUtil.kernel2.gauss(1.2);
        Args.MinNimages        = 4;
        Args.coaddArgs cell    = {'StackMethod','sigmaclip', 'StackArgs',{'MeanFun',@tools.math.stat.nanmedian, 'MaxIter',1, 'Nsigma',[3 3]}};  %{'StackMethod','sigmaclip','StackArgs',{'MeanFun',@median, 'StdFun',@tools.math.stat.nanstd, 'Nsigma',[2 2], 'MaxIter',1}};
        Args.PlotDS9 logical   = false;
    end
    RAD        = 180./pi;
    ARCSEC_DEG = 3600;
    SEC_DAY    = 86400;
    
    FlagGood = CropAI.sizeImage>0;
    CropAI   = CropAI(FlagGood);
    
    JD = CropAI.julday;
    
    % subtract background
    %BAI = imProc.background.background(CropAI, 'SubSizeXY',[], 'CreateNewObj',true,'SubBack',true);
    %BAI = BAI.filter(Args.Filter);
    BAI = CropAI;
    
    
    TimeSince0 = (JD - Args.JD0).*SEC_DAY;   % [s]
    StartTime  = min(TimeSince0);
    EndTime    = max(TimeSince0) - 0.5.*Args.BinTime.*max(TimeSince0);
    
    TimeVec = (StartTime:Args.TimeStep:EndTime).';
    Time    = TimeVec(1);
    Icoadd = 0;
    Istep  = 0;
    while Time<Args.MaxTime && Istep<numel(TimeVec)
        Istep = Istep + 1;
        Time  = TimeVec(Istep);
        
        Icoadd
        Flag = abs(TimeSince0-Time)<Args.TimeBinAfter0(Time);
        IndFlag  = find(Flag);
        
               
        NinCoadd = numel(IndFlag);
        
        if NinCoadd>Args.MinNimages
            Icoadd = Icoadd + 1;
            
            if isempty(Args.ZP)
                
                ZP       = zeros(NinCoadd,1);

                for IIf=1:1:NinCoadd
                    if isempty(CropAI(IndFlag(IIf)).UserData.ZP)
                        ZP(IIf) = NaN;
                    else
                        ZP(IIf) = CropAI(IndFlag(IIf)).UserData.ZP(3);
                    end
                end
            else
                ZP = Args.ZP;
            end
            
            FluxFactor = 10.^(0.4.*ZP(Flag));
            %FAI = BAI(Flag).copy;
           
            %FlagNN = ~isnan(ZP);
            %ZP = ZP(FlagNN);
            
           
            
            CoaddAI(Icoadd) = imProc.stack.coadd(BAI(Flag), 'PreNorm',1./FluxFactor, Args.coaddArgs{:}, 'ReplaceNaN','none');
            
%             CoaddAI(Icoadd) = imProc.stack.coadd(BAI(Flag), 'PreNorm',FluxFactor,...
%                 'StackMethod','sigmaclip',...
%                 'StackArgs',{'MeanFun',@tools.math.stat.nanmedian, 'MaxIter',1, 'Nsigma',[3 3]},...
%                 'ReplaceNaN','none');

            
            
            if Args.PlotDS9
                ds9(CoaddAI(Icoadd),Icoadd)
            end
        end
    end
    
    
end
