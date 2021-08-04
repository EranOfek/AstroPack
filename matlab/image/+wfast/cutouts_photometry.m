function cutouts_photometry(Input, Args)
    %
   
    arguments
        Input                                    % AstroImage or file name
        Args.read2AstroImageArgs cell          = {'Calibrate',true,'InterpOverNan',false};
    end
    
    if ~isa(Input, AstroImage)
        % read FileName into an AstroImage object
        [AI, CalibObj] = read2AstroImage(Input, Args.read2AstroImageArgs{:},'ReadType','cutouts');
    else
        AI = Input;
    end
    
    Nobj = numel(Obj);
    for Iobj=1:1:Nobj
        SizeC = size(AI(Iobj).Image);
        Ncut  = prod(SizeC(3:end));
        Cube = reshape(AI(Iobj).Image, SizeC(1), SizeC(2), Ncut);
        X    = ceil(SizeC(2).*0.5).*ones(Ncut,1);
        Y    = ceil(SizeC(1).*0.5).*ones(Ncut,1);
        Cube = single(Cube);
        
        % with 1st moment estimation
        [M1,M2,Aper] = imUtil.image.moment2(Cube, X, Y, 'NoWeightFirstIter',false);
    
        % without 1st moment estimation
        [M1,M2,Aper] = imUtil.image.moment2(Cube, X, Y, 'NoWeightFirstIter',false,'MaxIter',-1);
        
    end
end