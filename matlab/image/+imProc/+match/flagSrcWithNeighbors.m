function [Flag, Obj] = flagSrcWithNeighbors(Obj, Args)
    % Flag sources in AstroCatalog which have neighbors within a radius
    %   Optionaly, remove sources with neighboors.
    % Input  : - A multi-element AstroCatalog object.
    %          * ...,key,val,...
    %            'CooType'
    %            'Radius'
    %            'RadiusUnits'
    %            'ColNamesX'
    %            'ColNamesY'
    %            'ColNamesRA'
    %            'ColNamesDec'
    % Outout : - A vector of logical indicating sources with neighboors
    %            within search radius. If multi-element AstroCatalog, then
    %            only the vector corresponding to last object is returned.
    %          - Modified object (original copy is modified!).
    %            The AstroCatalog object after removing sources with
    %            neighboors. If nargout<2 then the original object is not
    %            modified.
    % Author : Eran Ofek (Jul 2021)
    % Example: AC = AstroCatalog({rand(100,2).*1024},'ColNames',{'X','Y'});
    %          Flag = imProc.match.flagSrcWithNeighbors(AC)
    
    arguments
        Obj AstroCatalog
        Args.CooType               = 'pix';
        Args.Radius                = 10;
        Args.RadiusUnits           = 'arcsec';
        
        Args.ColNamesX             = AstroCatalog.DefNamesX;
        Args.ColNamesY             = AstroCatalog.DefNamesY;
        Args.ColNamesRA            = AstroCatalog.DefNamesRA;
        Args.ColNamesDec           = AstroCatalog.DefNamesDec;
    end
    
    Nobj = numel(Obj);
    for Iobj=1:1:Nobj
    
        switch lower(Args.CooType)
            case 'sphere'
                DistFun = @celestial.coo.sphere_dist_fast;
                [ColInd1] = colnameDict2ind(Obj(Iobj), Args.ColNamesRA);
                [ColInd2] = colnameDict2ind(Obj(Iobj), Args.ColNamesDec);
                if ~Obj(Iobj).IsSorted
                    % sort by Y/Dec
                    Obj(Iobj).sortrows(ColInd2);
                end
                Coo     = getLonLat(Obj(Iobj), 'rad');

                RadiusRad = convert.angular(Args.RadiusUnits, 'rad', Args.Radius);
            case 'pix'
                DistFun = @tools.math.geometry.plane_dist;
                [ColInd1] = colnameDict2ind(Obj(Iobj), Args.ColNamesX);
                [ColInd2] = colnameDict2ind(Obj(Iobj), Args.ColNamesY);
                if ~Obj(Iobj).IsSorted
                    % sort by Y/Dec
                    Obj(Iobj).sortrows(ColInd2);
                end
                Coo    = getXY(Obj(Iobj));

                RadiusRad = Args.Radius;
            otherwise
                error('Unknown CooType option');
        end   


        [IndTable] = VO.search.search_sortedlat_multi(Coo,...
                                                      Coo(:,1), Coo(:,2), RadiusRad, [], DistFun);

        Flag = [IndTable.Nmatch]>1;
        Flag = Flag(:);

        if nargout>1
            % select sources
            Obj(Iobj).Catalog = Obj(Iobj).Catalog(~Flag,:);
        end
    end
end