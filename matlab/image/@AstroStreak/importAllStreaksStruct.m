function A=importAllStreaksStruct(~,AllStreaks)
    % importer - translator from AllStreaks structure, as saved by
    %  pipelineI
    
    E=zeros(1,numel(AllStreaks));
    for i=1:numel(AllStreaks)
        E(i)=numel(AllStreaks(i).Streaks.Flux);
    end
    
    N=sum(E);
    k=0;
    A=AstroStreak.empty(0,N);
    for i=find(E>0)
        % for the Streaks part, we have an importer
        A(k+1:k+E(i))=A.importImUtilAstroStreak(AllStreaks(i).Streaks);
        % populate the only bits of information which are not included in
        %  the Streaks part, i.e. the X,Y offsets
        try
            ORIGSEC=AllStreaks(i).ORIGSEC;
            LIGHTSEC=AllStreaks(i).LIGHTSEC;
        catch
            % hardcoded, if absent in AllStreaks
            O = imUtil.cut.gridSubImage([6388 9576], [1716 1716]);
            ORIGSEC = O(AllStreaks(i).Crop,:);
            LIGHTSEC = [0 0 24 0]; % zero elements not used
        end
        for j=k+1:k+E(i)
            A(j).Xoffset = ORIGSEC(1) - 1;
            A(j).Yoffset = ORIGSEC(3) + LIGHTSEC(3) - 1;
        end
        k=k+E(i);
   end
end
