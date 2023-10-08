function AllT=wgetList_ztf_phot(Cat, Args)
    % Run wget_ztf_phot on a list of targets
    %   See VO.ZTF.wget_ztf_phot for details.
    % Input  : - A catalog containing J2000 RA and Dec columns.
    %          * ...,key,val,...
    %            'Radius' - Search radius. Default is 1 (arcsec).
    %            'Band' - Default is [1 2].
    %            'ColRA' - RA column index. Default is 1.
    %            'ColDec' - Dec column index. Default is 2.
    %            'CooUnits' - Coordinates units. Default is 'rad'.
    %            'wget_ztf_photArgs' - A cell array of additional arguments
    %                   to pass to VO.ZTF.wget_ztf_phot.
    %                   Default is {}.
    %            'UseFlags' - Select catflags==0 before calculating
    %                   statistics. Default is true.
    % Output : - A structure array which size is: 
    %            [Num src, Num bands] wit a field LC containing
    %            the ZTF photometry table.
    %            Additional fields containing LC mag statistics are
    %            available.
    % Author : Eran Ofek (Sep 2023)
    % Example: AllT=VO.ZTF.wgetList_ztf_phot(Cat.Cat(1:10,1:2));
    
    arguments
        Cat
        Args.Radius                   = 1;
        Args.Band                     = [1 2];
        Args.ColRA                    = 1;
        Args.ColDec                   = 2;
        Args.CooUnits                 = 'rad';
        Args.wget_ztf_photArgs cell   = {};
        Args.UseFlags logical         = true;
    end
   
    CatCoo = Cat(:,[Args.ColRA, Args.ColDec]);
    CatCoo = convert.angular(Args.CooUnits, 'deg', CatCoo);
    
    Ncat = size(CatCoo,1);
    Nband = numel(Args.Band);
    for Icat=1:1:Ncat
        [Icat, Ncat]
        for Iband=1:1:Nband
            [Table,Str] = VO.ZTF.wget_ztf_phot(CatCoo(Icat,1),CatCoo(Icat,2), Args.Band(Iband),...
                                               'Radius',Args.Radius,...
                                               Args.wget_ztf_photArgs{:});
            %
            AllT(Icat,Iband).LC = Table;
            
            % add statistics
            if ~isempty(Table)
                if Args.UseFlags
                    Fg = AllT(Icat,Iband).LC.catflags==0;
                    CatG = AllT(Icat,Iband).LC(Fg,:);
                else
                    catG = AllT(Icat,Iband);
                end
                
                AllT(Icat,Iband).RStd   = tools.math.stat.rstd(CatG.mag);
                AllT(Icat,Iband).MedMag = median(CatG.mag,1,'omitnan');
                Z = (CatG.mag - AllT(Icat,Iband).MedMag)./sqrt(CatG.magerr.^2 + CatG.magzprms.^2);
                
                AllT(Icat,Iband).NeclispeZ5 = sum(Z>5);
                AllT(Icat,Iband).NflareZ5   = sum(Z<-5);
                AllT(Icat,Iband).NeclispeZ8 = sum(Z>8);
                AllT(Icat,Iband).NflareZ8   = sum(Z<-8);
                
                % deviations in units of rstd
                
                Resid = Table.mag - AllT(Icat,Iband).MedMag;
                ZDev  = Resid./AllT(Icat,Iband).RStd;
                
                AllT(Icat,Iband).MaxDevNeg  = max(Resid);
                AllT(Icat,Iband).MinDevNeg  = min(Resid);
                AllT(Icat,Iband).MaxZDevNeg = max(ZDev);
                AllT(Icat,Iband).MinZDevNeg = min(ZDev);
                AllT(Icat,Iband).Npt        = sum(~isnan(Table.mag));
            else
                AllT(Icat,Iband).Npt        = 0;
                ALlT(Icat,Iband).NeclispeZ5 = NaN;
                ALlT(Icat,Iband).NeclispeZ8 = NaN;
                ALlT(Icat,Iband).NflareZ5 = NaN;
                ALlT(Icat,Iband).NflareZ8 = NaN;
                
            end
        end
    end
            
    
end
