function [Result] = mergeArclets(JD, RA, Dec, MuRA, MuDec, Args)
    % One line description
    %     Optional detailed description
    % Input  : - 
    %          - 
    %          * ...,key,val,... 
    % Output : - 
    % Author : Eran Ofek (2025 Jun) 
    % Example: 

    arguments
        JD
        RA
        Dec
        MuRA
        MuDec

        Args.CooUnits              = 'deg';
        Args.ErrRA                 = 1./3600;
        Args.ErrDec                = 1./3600;
        Args.ErrMuRA               = 2.4;  
        Args.ErrMuDec              = 2.4;
    end

    [JD, SI] = sort(JD);
    RA       = RA(SI);
    Dec      = Dec(SI);
    MuRA     = MuRA(SI);
    MuDec    = MuDec(SI);
    N        = numel(JD);
    if numel(Args.ErrRA)==1
        Args.ErrRA     = Args.ErrRA.*ones(N,1);
    else
        Args.ErrRA     = Args.ErrRA(SI);
    end
    if numel(Args.ErrDec)==1
        Args.ErrDec    = Args.ErrDec.*ones(N,1);
    else
        Args.ErrDec    = Args.ErrDec(SI);
    end
    if numel(Args.ErrMuRA)==1
        Args.ErrMuRA   = Args.ErrMuRA.*ones(N,1);
    else
        Args.ErrMuRA   = Args.ErrMuRA(SI);
    end
    if numel(Args.ErrMuDec)==1
        Args.ErrMuDec  = Args.ErrMuDec.*ones(N,1);
    else
        Args.ErrMuDec  = Args.ErrMuDec(SI);
    end

    I1 = 1;
    for I=2:1:N
        

        R_RA  = tools.math.fit.fitPoly_PointsAndSlopes([JD(I1); JD(I)], [RA(I1); RA(I)], [MuRA(I1), MuRA(I)], 'Orders',[0 1 2], 'SubT',true);
        R_Dec = tools.math.fit.fitPoly_PointsAndSlopes([JD(I1); JD(I)], [Dec(I1); Dec(I)], [MuDec(I1), MuDec(I)], 'Orders',[0 1 2], 'SubT',true);
        % the slope at T=0 (mid time):
        R_RA.Par(2)
    end



    Found = false(N,1);
    Cont  = true;
    K     = 0;






    while Cont


        Ind = find(~Found);
        Nind = numel(Ind);
        if Nind==0
            Cont = false;
        elseif Nind==1
            K = K + 1;
            Result(K).IndList = Ind;
            Found(Ind) = true;
        else
            % multiple entries in List,
            
            CurrentInd = Ind(Iind);

            K = K + 1;
            Result(K) = CurrentInd;
            Found(CurrentInd) = true;

            Ind = find(~Found);
            Nind = numel(Ind);
            for Iind=1:1:Nind
                % propagate CurrentInd to Ind(Iind)
                 

                IndTest = Ind(Iind);

                tools.math.fit.fitPoly_PointsAndSlopes(T, X, Xdot);


                PropRA  = RA(CurrentInd)  + (JD(IndTest) - JD(CurrentInd)).*MuRA(CurrentInd);
                PropDec = Dec(CurrentInd) + (JD(IndTest) - JD(CurrentInd)).*MuDec(CurrentInd);


                



            end


        end
    end

    
    

end
