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
        Args.ColDec                   = 1;
        Args.CooUnits                 = 'rad';
        Args.wget_ztf_photArgs cell   = {};
    end
   
    CatCoo = Cat(:,[Args.ColRA, Args.ColDec]);
    CatCoo = convert.angular(Args.CooUnits, 'deg', CatCoo);
    
    Ncat = size(CatCoo,1);
    Nband = numel(Args.Band);
    for Icat=1:1:Ncat
        for Iband=1:1:Nband
            [Table,Str] = VO.ZTF.wget_ztf_phot(CatCoo(Icat,1),CatCoo(Icat,2), Args.Band(Iband),...
                                               'Radius',Args.Radius,...
                                               Args.wget_ztf_photArgs{:});
            %
            AllT(Icat,Iband).LC = Table;
            
            % add statistics
            if ~isempty(Table)
                AllT(Icat,Iband).RStd   = tools.math.stat.rstd(Table.mag);
                AllT(Icat,Iband).MedMag = median(Table.mag,1,'omitnan');
                % deviations in units of rstd
                Resid = Table.mag - AllT(Icat,Iband).MedMag;
                ZDev  = Resid./AllT(Icat,Iband).RStd;
                
                AllT(Icat,Iband).MaxDevNeg  = max(Resid);
                AllT(Icat,Iband).MinDevNeg  = min(Resid);
                AllT(Icat,Iband).MaxZDevNeg = max(ZDev);
                AllT(Icat,Iband).MinZDevNeg = min(ZDev);
                AllT(Icat,Iband).Npt        = sum(~isnan(Table.mag));
            end
        end
    end
            
    
end
