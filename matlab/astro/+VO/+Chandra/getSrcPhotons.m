function Data=getSrcPhotons(RA, Dec, Args)
    % Get Chandra photons of a single source over all ObsIDs.
    % Input  : - J2000.0 RA [deg or sexagesimal string]
    %          - J2000.0 Dec [deg or sexagesimal string]
    %          * ...,key,val,...
    %            'ObsSearchRadius' - Search radius for ObsID
    %                   Default is 1200.
    %            'SearchRadius' - Search radius for source photons.
    %                   Default is 5.
    %            'SearchRadiusUnits' - Search radii units.
    %                   Default is 'arcsec'.
    %            'EnergyRange' - Energy range [eV] to select.
    %                   Default is [200 8000].
    %            See code for additional arguments
    % Output : - A structure array with the photons in each ObsID.
    %            Element per ObsID.
    % Author : Eran Ofek (Oct 2023)
    % Example: Data = VO.Chandra.getSrcPhotons;
   
    arguments
        RA      = '23:23:27.8';
        Dec     = '+58:48:42';
        Args.ObsSearchRadius      = 1200;
        Args.SearchRadius         = 5;
        Args.Annulus              = [15 30];
        Args.SearchRadiusUnits    = 'arcsec';
        Args.EnergyRange          = [200 8000];  % [eV]
        
        Args.BaseChandraObs       = '/raid/eran/projects/transients/x/chandra/ObsID';
        Args.FileTemplate         = 'acis*_evt2.fits';
        Args.ColRA                = 'RA';
        Args.ColDec               = 'Dec';
        Args.ColTime              = 'time';
        Args.ColEnergy            = 'energy';
        Args.ColCCDID             = 'ccd_id';
        Args.ColChipX             = 'chipx';
        Args.ColChipY             = 'chipy';
        Args.ColX                 = 'x';
        Args.ColY                 = 'y';
        Args.ColPHA               = 'pha';
    end
    CatProp = 'Cat';
    
    if ischar(RA)
        RA = celestial.coo.convertdms(RA, 'SH','r');
    end
    
    if ischar(Dec)
        Dec = celestial.coo.convertdms(Dec, 'SD','R');
    end
    
    ObsSearchRadius = convert.angular(Args.SearchRadiusUnits, 'rad', Args.ObsSearchRadius);
    SearchRadius    = convert.angular(Args.SearchRadiusUnits, 'rad', Args.SearchRadius);
    Annulus         = convert.angular(Args.SearchRadiusUnits, 'rad', Args.Annulus);
    
    CatChandra = cats.X.ChandraObs;pi.
    

    DistF  = celestial.coo.sphere_dist_fast(RA, Dec,CatChandra.(CatProp).RA, CatChandra.(CatProp).Dec);
    IndObs = find(DistF<ObsSearchRadius);
    Nobs   = numel(IndObs);

    PWD = pwd;
    %Data = struct('File',cell(Nobs,1), 'ObsID',cell(Nobs,1), 'RA',cell(Nobs,1), 'Dec',cell(Nobs,1));
    K = 0;
    for Iobs=1:1:Nobs
        [Iobs, Nobs]
        Folder = sprintf('%s/%s/%s/%d/primary',Args.BaseChandraObs,...
                                              CatChandra.(CatProp).AO{IndObs(Iobs)},...
                                              CatChandra.(CatProp).Cat{IndObs(Iobs)},...
                                              CatChandra.(CatProp).ObsID(IndObs(Iobs)));
                                          
        cd(Folder);
        Dir = dir(Args.FileTemplate);
        if numel(Dir)==1
            P = PhotonsList(Dir.name);
            %
            P.populateBadTimes;
            P.removeBadTimes;
            P.selectEnergy(Args.EnergyRange);
            % Add RA/Dec to catalog
            P.addSkyCoo;
            PhotonsRADec = P.Events.getLonLat('rad');
            
            D = celestial.coo.sphere_dist_fast(RA, Dec, PhotonsRADec(:,1), PhotonsRADec(:,2));
            Iph = find(D<SearchRadius);
            
            Ibck = find(D<Annulus(2) & D>Annulus(1));
            
            K = K + 1;
            Data(K).File      = Dir.name;
            Data(K).ObsID     = CatChandra.(CatProp).ObsID(IndObs(Iobs));
            Data(K).Nph       = numel(Iph);
            Data(K).Dist      = D(Iph);
            Data(K).RA        = P.Events.getCol(Args.ColRA, false, false, 'SelectRows',Iph);
            Data(K).Dec       = P.Events.getCol(Args.ColDec, false, false, 'SelectRows',Iph);
            Data(K).Time      = P.Events.getCol(Args.ColTime, false, false, 'SelectRows',Iph);
            Data(K).Energy    = P.Events.getCol(Args.ColEnergy, false, false, 'SelectRows',Iph);
            Data(K).CCDID     = P.Events.getCol(Args.ColCCDID, false, false, 'SelectRows',Iph);
            Data(K).ChipX     = P.Events.getCol(Args.ColChipX, false, false, 'SelectRows',Iph);
            Data(K).ChipY     = P.Events.getCol(Args.ColChipY, false, false, 'SelectRows',Iph);
            Data(K).X         = P.Events.getCol(Args.ColX, false, false, 'SelectRows',Iph);
            Data(K).Y         = P.Events.getCol(Args.ColY, false, false, 'SelectRows',Iph);
            Data(K).PHA       = P.Events.getCol(Args.ColPHA, false, false, 'SelectRows',Iph);
            Data(K).BckEnergy = P.Events.getCol(Args.ColEnergy, false, false, 'SelectRows',Ibck);
            
            Data(K).BadTimes = P.BadTimes;
            %PhList = [PhList; P.Events.Catalog(Iph,[1 15])];
        
        end
        

end

    
end
