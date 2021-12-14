function pairOrientationMatching
    %
    
    arguments
        Cat        = [];
        Ref        = [];
        Args.MinR2 = 100;
        Args.MaxR2 = 10000;
    end
    RAD = 180./pi;
    
    if isempty(Cat)
        Cat = rand(2000,2).*1600;
        
        Ref = rand(4000,2).*1600;
        Ref = [Ref; Cat(1:200,:)+30];
    end
    
    CatX = Cat(:,1);
    CatY = Cat(:,2);
    RefX = Ref(:,1);
    RefY = Ref(:,2);
    
    % Build a list of all pairs
    CatDX = CatX(:) - CatX(:).';
    CatDY = CatY(:) - CatY(:).';
    CatR2 = CatDX.^2 + CatDY.^2;
    % select in range
    FlagR2 = CatR2(:)>Args.MinR2 & CatR2(:)<Args.MaxR2;
    CatR2  = CatR2(FlagR2);
    CatT   = atan2(CatDY(FlagR2), CatDX(FlagR2)).*RAD;
    Ncat   = numel(CatR2);
    
    RefDX = RefX(:) - RefX(:).';
    RefDY = RefY(:) - RefY(:).';
    RefR2 = RefX.^2 + RefY.^2;
    % select in range
    FlagR2 = RefR2(:)>Args.MinR2 & RefR2(:)<Args.MaxR2;
    RefR2  = RefR2(FlagR2);
    RefT   = atan2(RefDY(FlagR2), RefDX(FlagR2)).*RAD;
    Nref   = numel(RefR2);
    
    Tree = kdtree_build([RefR2, RefT]);
    
    % select random pair from Cat
    Nsim = 10000;
    for Isim=1:1:Nsim
        Ipair = randi(Ncat,1,1);
    
        Range = [(sqrt(CatR2(Ipair)) +[-2 2]).^2; CatT(Ipair)+[-2 2]];
        [Idxs] = kdtree_range_query(Tree, Range);
        if ~isempty(Idxs)
            Ipair
            Idxs
        end
    end
    
end