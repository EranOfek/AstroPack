function Msg = generateReportMPC(Obj, Args)
    %
    % PROBLEM: The stamp catalpog contains multi sources and not clear
    % which one is the asteroid...
    %
    % Example:
    % imProc.asteroids.generateReportMPC(AstData.AstCrop(1).Stamps)

    arguments
        Obj     % AstroCatalog or AstroImage array
        Args.Msg                      = [];  % if given than concat to existing report
        Args.Ind                      = [];  % empty, scalar, or vector of element per image
        Args.ErrorIfMultiLine logical = false;

        Args.AstIndex                 = 1;
        Args.Filter                   = 'C';
        Args.generateReportMPCArgs cell = {};
        

        Args.ColRA                    = 'RA';
        Args.ColDec                   = 'Dec';
        Args.ColMag                   = 'MAG_PSF';
    end


    if isempty(Args.Msg)
        AddHeader = true;
    else
        AddHeader = false;
    end

    Nind = numel(Args.Ind);
    Nobj = numel(Obj);
    Table = zeros(0,6);
    for Iobj=1:1:Nobj
        if isa(Obj(Iobj), 'AstroImage')
            Cat = Obj(Iobj).CatData;
        elseif isa(Obj(Iobj), 'AstroCatalog')
            Cat = Obj(Iobj);
        else
            error('Unknown input object class - must be AstroImage or AstroCatalog');
        end

        if isempty(Args.Ind)
            % verify that the catalog contains a single line
            Nline = sizeCatalog(Cat);
            if Nline==0
                % skip
                Ind = [];
            elseif Nline>1
                if Args.ErrorIfMultiLine
                    error('Object %d contains a catalog with %d lines', Iobj, Nline);
                end
                % skip or error
                Ind = [];
            else
                % Nline==1
                Ind = 1;
            end
        else
            Ind = Args.Ind(min(Nind,Iobj));
        end

        if ~isempty(Ind)
            % [JD, RA, Dec, Mag, Filter, AstIndex]
            Table = [Table; [Cat.JD, Cat.getCol(Args.ColRA,'SelectRows',Ind),...
                             Cat.getCol(Args.ColDec,'SelectRows',Ind),...
                             Cat.getCol(Args.ColMag,'SelectRows',Ind),...
                             NaN, Args.AstIndex]];
        end
        
    end
    if isempty(Table)
        Msg = '';
    else
        Msg = imUtil.asteroids.generateReportMPC(Table, 'Filter',Args.Filter, 'AddHeader',AddHeader);
    end


end
