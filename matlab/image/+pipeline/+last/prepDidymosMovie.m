function CoaddAI=prepDidymosMovie(CropAI,Args)
    %
    % Example: CoaddAI=pipeline.last.prepDidymosMovie(CropAI)
    
    arguments
        CropAI
        
        Args.JD0               = celestial.time.julday([26 9 2022 23 15 0]);
        Args.TimeStep          = 30;   % s
        Args.BinTime           = 0.025;
        Args.TimeBinAfter0     = @(TimeSec) max(60, TimeSec.*0.025);   % s
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
    BAI = imProc.background.background(CropAI, 'SubSizeXY',[], 'CreateNewObj',true,'SubBack',true);
    BAI = BAI.filter(Args.Filter);
    
    
    TimeSince0 = (JD - Args.JD0).*SEC_DAY;   % [s]
    StartTime  = min(TimeSince0);
    EndTime    = max(TimeSince0) - 0.5.*Args.BinTime.*max(TimeSince0);
    
    Icoadd = 0;
    for Time = StartTime:Args.TimeStep:EndTime
        
        Icoadd
        Flag = abs(TimeSince0-Time)<Args.TimeBinAfter0(Time);
        IndFlag  = find(Flag);
               
        NinCoadd = numel(IndFlag);
        
        if NinCoadd>Args.MinNimages
            Icoadd = Icoadd + 1;
            ZP       = zeros(NinCoadd,1);
        
            for IIf=1:1:NinCoadd
                if isempty(CropAI(IndFlag(IIf)).UserData.ZP)
                    ZP(IIf) = NaN;
                else
                    ZP(IIf) = CropAI(IndFlag(IIf)).UserData.ZP(3);
                end
            end
            FluxFactor = 10.^(0.4.*ZP);
            %FAI = BAI(Flag).copy;
           
            FlagNN = ~isnan(ZP);
            ZP = ZP(FlagNN);
            
            CoaddAI(Icoadd) = imProc.stack.coadd(BAI(Flag), 'PreNorm',FluxFactor, Args.coaddArgs{:}, 'ReplaceNaN','none');
            
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
