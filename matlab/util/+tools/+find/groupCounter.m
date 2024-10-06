function Gr = groupCounter(Counter, Args)
    % Group a vector of counters into successive numbers.
    %   Given a vector of integer counters, generate groups of successive
    %   increasing counters. each group must contain at least MinInGroup
    %   elements and MaxInGroup elements.
    % Input  : - Vector of integers.
    %          * ...,key,val,...
    %            'MinInGroup' - Minimum number of elements in group.
    %                   Smaller groups will be discarded.
    %                   Default is 10.
    %            'MaxInGroup' - Break groups, such that this is the maximum
    %                   size of groups. Use Inf if no breaking is needed.
    %                   Default is 20.
    % Output : - A structure array of groups containing the following
    %            fields:
    %            .I1 - Starting index of group.
    %            .I2 - Ending index of group.
    %            .Ind - Vector of indices in group.
    %            .N - Number of elements in group.
    % Author : Eran Ofek (Sep 2022)
    % Example: Counter=[1 1 1 1 2 3 4 5 1:20, 1 1, 1:20];
    %          Gr=tools.find.groupCounter(Counter);
    %          Counter=[1 1 1 1 2 3 4 5 1:20, 1 1, 1:20, 1, 1:50];
    %          Gr=tools.find.groupCounter(Counter);
    
    arguments
        Counter
        Args.MinInGroup    = 10;
        Args.MaxInGroup    = 20;
        Args.Algo          = 1;
    end
    
    if Args.Algo==1
        if isempty(Counter)
            Gr = [];
        else
            Nc          = numel(Counter);
            DiffCounter = [diff([Counter(:); 1]); -1];
    
            D1 = [diff(Counter(:));1];
            D1(D1>1) = 0;
    
            %Diff = [Counter.',[diff(Counter),1].', [0;diff([diff(Counter),1].')], (1:1:Nc).'];
            Diff = [Counter,D1, [0;diff(D1)], (1:1:Nc).'];
    
            IgroupStart = [1; find(Diff(:,3)>0)];
            Ng          = numel(IgroupStart);
            for Ig=1:1:Ng
                I1 = IgroupStart(Ig);
                In = find(Diff(I1+1:end,3)<0, 1, 'first');
                Ip = find(Diff(I1+1:end,3)>0, 1, 'first');
                if isempty(In) && isempty(Ip)
                    I2 = Nc;
                else
                    if isempty(Ip)
                        I2 = In + I1;
                    else
                        if Ip<In
                            % skip
                            I2 = [];
                        else
                            I2 = In + I1;
                        end
                    end
                end
                if isempty(I2)
                    I2 = NaN;
                end
                Gr(Ig).I1 = I1;
                Gr(Ig).I2 = I2;
                Gr(Ig).Ind = (I1:1:I2).';
                Gr(Ig).N   = numel(Gr(Ig).Ind);
    
            end
    
            NinGroup = [Gr.I2] - [Gr.I1] + 1;
            Fgood    = NinGroup>=Args.MinInGroup;
            Gr       = Gr(Fgood);
            NinGroup = [Gr.I2] - [Gr.I1] + 1;
            Ngr      = numel(Gr);
    
            Itoomany = find(NinGroup>Args.MaxInGroup);
            K        = Ngr;
            for Itm=1:1:numel(Itoomany)
                Igr = Itoomany(Itm);
    
    
                Nint = (1 + Gr(Igr).I2 - Gr(Igr).I1);
                Nsub = ceil(Nint./Args.MaxInGroup);  % number of sub groups in current long group
                IntCounter = (1:1:Nint);
                ceil(IntCounter./Args.MaxInGroup);
                for Ii=1:1:Nsub
                    K = K + 1;
    
                    Gr(K).I1 = Gr(Igr).I1 + (Ii-1).*Args.MaxInGroup;
                    Gr(K).I2 = Gr(Igr).I1 + min(Ii.*Args.MaxInGroup, Nint)-1;
                    Gr(K).Ind = (Gr(K).I1:1:Gr(K).I2).';
                    Gr(K).N   = numel(Gr(K).Ind);
                end
                Gr(Igr).I2 = NaN;
            end
            Fgood = ~isnan([Gr.I2]);
            Gr    = Gr(Fgood);
        end
        
    else
        error('Unknown Algo option');
    end
end