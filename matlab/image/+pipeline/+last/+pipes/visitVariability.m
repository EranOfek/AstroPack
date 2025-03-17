function [Result] = visitVariability(X, Y, Args)
    % One line description
    %     Optional detailed description
    % Input  : - 
    %          - 
    %          * ...,key,val,... 
    % Output : - 
    % Author : Eran Ofek (2025 Mar) 
    % Example: 

    arguments
        X
        Y
        Args.A                 = [];
        Args.B                 = [];
    end


    DB = db.Db;
    DB.connect;
    DB.useDB('last');

    JD = celestial.time.julday([1 1 2025]);
    T = DB.query(sprintf('SELECT * FROM visit_images WHERE midjd>%10.1f',JD));

    PWD = pwd;

    tic;
    K = 0;
    for I=1:1:100
        %FN = pipeline.last.queryDB.table2path(T(I,:));
        %Dir = FN.genPath('AddSubDir',1);
        %cd(Dir);

        MS=pipeline.last.queryDB.loadProducts(T(I,:),'merged','MergedMat');

        [AC(I)] = lcUtil.variabilityAnalysis(MS);
        
        if AC(I).sizeCatalog>0
            K = K + 1;
            if K==1
                Table = AC(I).Catalog;
            else
                Table = [Table; AC(I).Catalog];
            end
        end

    end
    toc

end
