function [Result] = searchRef(New, RefTable)
    % for each image in the input AI, list the overlapping reference images
    %     Optional detailed description
    % Input  : - an AstroImage with the new image (or a stack)
    %          - the reference images table
    %          * ...,key,val,... 
    % Output : - indexes of the overlapping crops in the reference images table
    % Author : A.M. Krassilchtchikov (2025 Jul) 
    % Example: RefTable = D.query('select id_ref, fieldid, mountnum, camnum, cropid, ra1, dec1, ra2, dec2, ra3, dec3, ra4, dec4 from last.ref_images;');
    %          New = AstroImage('/home/sasha/LAST/LAST.01.06.04_20250418.190502.407_clear_663_000_001_024_sci_coadd_Image_1.fits');
    %          Res = pipeline.last.reference.searchRef(New, RefTable);
    %          RefTable(Res{1},:)
    arguments
        New                   % a new AI       
        RefTable              % the table of reference images                
    end
    %
    Nobj   = numel(New);
    Result = cell(1,Nobj);
    
    for Iobj = 1:Nobj
        FieldID = string(New(Iobj).getStructKey('FIELDID').FIELDID);
        CamNum  = New(Iobj).getStructKey('CAMNUM').CAMNUM;
        Corn    = New(Iobj).getStructKey({'RA1', 'DEC1', 'RA2', 'DEC2', 'RA3', 'DEC3', 'RA4', 'DEC4'});    
        P0      = [Corn.RA1, Corn.DEC1; Corn.RA2, Corn.DEC2; Corn.RA3, Corn.DEC3; Corn.RA4, Corn.DEC4];

        Idx = find(strcmp(RefTable.fieldid, FieldID) & RefTable.camnum == CamNum);
        NCrops = numel(Idx);
        Crops  = cell(1,NCrops);
        for ICrop = 1:NCrops
            Crops{ICrop} = [RefTable.ra1(Idx(ICrop)), RefTable.dec1(Idx(ICrop));...
                            RefTable.ra2(Idx(ICrop)), RefTable.dec2(Idx(ICrop));...
                            RefTable.ra3(Idx(ICrop)), RefTable.dec3(Idx(ICrop));...
                            RefTable.ra4(Idx(ICrop)), RefTable.dec4(Idx(ICrop))];
        end
        
        Cr = celestial.polygon.polygon_boolean_operations(P0, Crops);
        Result{Iobj} = Idx(Cr.Intersect>0);
    end
end
