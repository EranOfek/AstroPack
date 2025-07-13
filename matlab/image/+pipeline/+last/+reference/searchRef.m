function [Result] = searchRef(New, RefTable, Args)
    % for each image in the input AI, list the overlapping reference images
    %     Optional detailed description
    % Input  : - an AstroImage with the new image (or a stack)
    %          * ...,key,val,... 
    % Output : - 
    % Author : A.M. Krassilchtchikov (2025 Jul) 
    % Example: RefTable = DB.query('select fieldid, mountnum, camnum,cropid from last.ref_images;');
    %          Res = pipeline.last.reference.searchRef(New, RefTable);
    arguments
        New                   % a new AI       
        RefTable              % the table of reference images        
        Args.X          = []; % 
    end
    %
    Nobj = numel(New);

    for Iobj = 1:Nobj
        FieldID = New(Iobj).getStructKey('FIELDID').FIELDID;
        CamNum  = New(Iobj).getStructKey('CAMNUM').CAMNUM;
        P0 = New(Iobj).getStructKey({'RA1', 'Dec1', 'RA2', 'Dec2', 'RA3', 'Dec3', 'RA4', 'Dec4'});        

        Idx = find(RefTable.FIELDID == FieldID & RefTable.CAMNUM == CamNum);
        NCrops = numel(Idx);
        Crops  = cell(NCrops);
        for ICrop = 1:NCrops
            Crops{ICrop} = [RefTable.RA1(Idx(ICrop)), RefTable.Dec1(Idx(ICrop));...
                            RefTable.RA2(Idx(ICrop)), RefTable.Dec2(Idx(ICrop));...
                            RefTable.RA3(Idx(ICrop)), RefTable.Dec3(Idx(ICrop));...
                            RefTable.RA4(Idx(ICrop)), RefTable.Dec4(Idx(ICrop))];
        end
        
        Intersect = celestial.polygon.polygon_boolean_operations(P0, {P1,P2,P3,P4,P5});

    end

% The function will search by fieldid using: tools.find.binarySearch
% Search for camnum
% next will check for overlaps with all crops in field/camnum.
% Each output will be called a "cut" - we expect up to four cuts in this scheme.
end
