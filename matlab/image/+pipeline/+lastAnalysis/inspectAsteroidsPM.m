function inspectAsteroidsPM(Files, Args)
    % Inspect all asteroid candidates in  proc dirs
    % Input  : - File template to load.
    %            Default is 'LAST*_sci_merged_Asteroids_*.mat'.
    %          * ...,key,val,...
    %            'Dir' - Dir from which to execute the recursive search.
    %                   If empty, use current dir.
    %                   Default is empty.
    %            'MinSN' - Minmal S/N (PSF 3) to select.
    %                   Default is 7.
    % Output : - 
    % Author : Eran Ofek (Sep 2022)
    % Example: pipeline.lastAnalysis.inspectAsteroidsPM
   
    
    arguments
        Files          = 'LAST*_sci_merged_Asteroids_*.mat';   
        Args.Dir       = '';  % local dir
        Args.MinSN     = 7;
    end
    
    PWD = pwd;
    
    if isempty(Args.Dir)
        % search template in local dir
        
    else
        cd(Args.Dir);
    end
    %%
    BD=BitDictionary;
    
    DirF  = io.files.rdir(Files);
    Nlist = numel(DirF);   
    List  = {DirF.name};
    
    Nsub = 24;
    State = false(ceil(Nlist./Nsub), Nsub);
    for Ilist=1:1:Nlist
        AstMat = io.files.load2(List{Ilist});
        
        
        Ncand = numel(AstMat.AstCrop);
        for Icand=1:1:Ncand
            %AstMat.AstCrop(Icand)
            
            Icand
            
            TT = AstMat.AstCrop(1).SelectedCatPM.toTable;
            
            if TT.Mean_SN_3>Args.MinSN
                TT(:,{'RA','Dec','Nobs','Noutlier','StdRA','StdDec','PM_RA','PM_Dec','PM_TdistProb','FLAGS','Mean_SN_3','Std_SN_3', 'Mean_MAG_CONV_3','Std_MAG_CONV_3'})


                Nstamps = numel(AstMat.AstCrop(Icand).Stamps);
                Is(3) = Nstamps;
                Is(1) = 1;
                Is(2) = ceil(Nstamps./2);

                for Iframe=1:1:numel(Is)
                    ds9(AstMat.AstCrop(Icand).Stamps(Is(Iframe)), Iframe);
                    ds9.zoom('to fit');
                    ds9.match_wcs
                end


                Ans = input('Any key to continue','s');
            end
        end
    end
        
end