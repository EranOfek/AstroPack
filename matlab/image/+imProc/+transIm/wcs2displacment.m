function [DispField, OutWCS, FieldName]=wcs2displacment(Obj, Trans, Args)
    % Generate a displacment field (for imwarp) from one WCS to another.
    %   The displacment field indicat by how much each pixel is shifted in
    %   X and Y in order to convert one WCS to the second WCS.
    % Input  : - An AstroImage object that contains a WCS object or an
    %            Header with a WCS data.
    %          - Either a WCS object or AnstroImage object containing a WCS
    %            object, or WCS information in the header.
    %            The number of elements in this object is either 1 or equal
    %            to the number of elemenst in the first input argument.
    %          * ...,key,val,...
    %            'Sampling' - In order to save time the step size in which
    %                   the te X/Y coordinate grid is calculated on is
    %                   controlled via this argument. Default is 10.
    %            'FieldName' - Field name in the output structure array
    %                   that contains the displacment cube per each elemnt of the
    %                   input AstroWCS/AstroImage.
    %                   Default is 'DF'.
    % Output : - A structure array of dispalcment field. Each element in
    %            the structure describes the displacment that need to
    %            convert the first input WCS to the second input WCS.
    %            In each element of the structure array there is a a field
    %            name which is specified by the 'FieldName' input argument
    %            (default is 'DF').
    %            The dispalcment field is a 3-D cube (see imwarp).
    %          - An array of (a new copy) WCS corresponding to the output
    %            transformation (first input after displacment).
    %          - Field name in the output structure array containing the
    %            dispalcment field cube.
    % Author : Eran Ofek (May 2023)
    % Example: D=imProc.transIm.wcs2displacment(Obj, W)
    
    arguments
        Obj AstroImage
        Trans              % AstroWCS or AstroImage
        Args.Sampling     = 10;
        Args.FieldName    = 'DF';
    end
    
    Nobj   = numel(Obj);
    Ntran  = numel(Trans);
    if ~(Nobj==Ntran || Ntran==1)
        error('Number of elements in Trans object (2nd input arg) must be 1 or equal to the number of elements in the 1st input argument');
    end    
    
    [ImageSizeY, ImageSizeX]   = sizeImage(Obj);
            
    if isa(Trans, 'AstroWCS')
        % Convert AstroWCS to displacment field

        DispField = struct(Args.FieldName, cell(Nobj,1));
        for Iobj=1:1:Nobj
            Itran = min(Ntran, Iobj);
            DispField(Iobj).(Args.FieldName)  = xy2refxyDisp(Obj(Iobj).WCS, [1, ImageSizeX(Iobj), 1, ImageSizeY(Iobj)], Trans(Itran), 'Sampling', Args.Sampling);
        end
        OutWCS = Trans.copy;
    elseif isa(Trans, 'AstroImage')
        % Convert AstroWCS in AstroImage to displacment field
        IsDisplacment   = true;

        DispField = struct(Args.FieldName, cell(Nobj,1));

        if ~Trans.WCS.Success
            Trans.populateWCS;
        end 

        
        for Iobj=1:1:Nobj
            if ~isempty(Obj(Iobj).WCS) && Obj(Iobj).WCS.Success
                DataWCS = Obj(Iobj).WCS;
            else
                % attempt to generate WCS from header
                Obj(Iobj).populateWCS;
                DataWCS = Obj(Iobj).WCS;
            end
            
            DispField(Iobj).(Args.FieldName)  = xy2refxyDisp(DataWCS, [1, ImageSizeX(Iobj), 1, ImageSizeY(Iobj)], Trans.WCS, 'Sampling', Args.Sampling);
            Itran        = min(Ntran, Iobj);
            OutWCS(Iobj) = Trans(Itran).WCS.copy;
        end
        
    else
        error('Unknown Trans (2nd input argument) object option'); 
    end
    FieldName = Args.FieldName;
    
end