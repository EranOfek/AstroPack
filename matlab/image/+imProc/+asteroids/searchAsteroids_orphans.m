function searchAsteroids_orphans(Obj, Args)
    %
    
    arguments
        Obj AstroCatalog
        
        Args.OrphanFlag          = [];
    end
    
    
    R = tools.math.fit.ransacLinear2d(TXY(:,2:3),TXY(:,1))
    
    
end