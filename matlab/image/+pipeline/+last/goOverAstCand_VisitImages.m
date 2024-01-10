function [Result] = goOverAstCand_VisitImages(Path, Args)
    % One line description
    %     Optional detailed description
    % Input  : - 
    %          - 
    %          * ...,key,val,... 
    % Output : - 
    % Author : Eran Ofek (2024 Jan) 
    % Example: pipeline.last.goOverAstCand_VisitImages('/marvin/LAST.01.10.01/2023/11/06/proc')

    arguments
        Path                   = pwd;
       
        Args.AstFileTemp = '*merged_Asteroids*.mat';

        Args.SearchRadGAIA     = 10;

        Args.MagGAIA           = 20;
        Args.A                 = [];
        Args.B                 = [];
    end
    RAD = 180./pi;

    PWD = pwd;
    cd(Path)

    % search all *merged_Asteroids*.mat
    Files = io.files.rdir(Args.AstFileTemp);
    Nf    = numel(Files);

    for If=1:1:Nf
        cd(Files(If).folder);
        AstData = io.files.load2(Files(If).name);

        Ncrop = numel(AstData.AstCrop);
        for Icrop = 1:1:Ncrop

            RA = AstData.AstCrop(Icrop).
        [GaiaCat,Col] = catsHTM.cone_search('GAIADR3',AstData.AstCrop(Icrop).SelectedCatPM.Table.RA./RAD,...
                                                      AstData.AstCrop(Icrop).SelectedCatPM.Table.Dec./RAD,...
                                                      Args.SearchRadGAIA, 'OutType','astrocatalog');
        GaiaCat.sphere_dist

        if min(GaiaCat.Table.phot_bp_mean_mag)<Args.MagGAIA     


    cd(PWD);

end
