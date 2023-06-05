function [H, ColNames]=calibDesignMatrix(Args)
    % Build design matrix for relative photometric calibration
    %   The design matrix is of the form: H=[Z_i, M_s, ...]
    %   and it can be used to solve: m_is = H*Par
    % Input  : - 
    % Output : - 
    % Author : Eran Ofek (Jun 2023)
    % Example: H=imUtil.calib.calibDesignMatrix('Nstar',3,'Nimage',5)
    %          H=imUtil.calib.calibDesignMatrix('Nstar',3,'Nimage',5,'StarProp',{[2 3 4]})
    %          H=imUtil.calib.calibDesignMatrix('Nstar',3,'Nimage',5,'StarProp',{[2 3 4],[11 12 13]})
    %          [H,CN]=imUtil.calib.calibDesignMatrix('Nstar',3,'Nimage',5,'StarProp',{[2 3 4]}, 'ImageProp', {[1 2 3 4 5]});
    %          H1=imUtil.calib.calibDesignMatrix('Nstar',3,'Nimage',5,'StarProp',{[2 3 4]}, 'ImageProp', {[1 2 3 4 5]}, 'Sparse',false);
    %          all(H1==H,'all')
    
    
    
    arguments
        Args.Nstar
        Args.Nimage
        Args.Sparse logical       = true;
        
        Args.ZP_PrefixName        = 'Z';
        Args.MeanMag_PrefixName   = 'M';
        Args.StarPropNames        = {};
        Args.StarProp             = {};  % one vector of properties per star - e.g., color
        
        Args.ImagePropNames       = {};
        Args.ImageProp            = {};  % one vector of properties per image - e.g., airmass
        
        Args.AllStarProp          = {};
    end
    
    
        
    % the Z_i block:
    % of the form: [1 0 0 ...; 1 0 0 ...; ...; 0 1 0 0 ...; ...]
    
    NpropStar  = numel(Args.StarProp);
    NpropImage = numel(Args.ImageProp);
    
    % StarProp column names:
    if isempty(Args.StarPropNames)
        StarPropNames = tools.cell.cellNumericSuffix('SP',(1:1:NpropStar));
    else
        if ischar(Args.StarPropNames)
            StarPropNames = tools.cell.cellNumericSuffix(Args.StarPropNames, (1:1:NpropStar));
        else
            % assume already a cell of column names:
            StarPropNames = Args.StarPropNames;
        end
    end
    
    % ImageProp column names:
    if isempty(Args.ImagePropNames)
        ImagePropNames = tools.cell.cellNumericSuffix('IP',(1:1:NpropImage));
    else
        if ischar(Args.ImagePropNames)
            ImagePropNames = tools.cell.cellNumericSuffix(Args.ImagePropNames, (1:1:NpropImage));
        else
            % assume already a cell of column names:
            ImagePropNames = Args.ImagePropNames;
        end
    end
    

    
    % allocate the design matrix:
    Nline = Args.Nimage.*Args.Nstar;
    if Args.Sparse
        % define sparse design matrix 
        Npopulated = Nline .* (2 + NpropStar + NpropImage);
        H = sparse([],[],[], Nline, Args.Nimage + Args.Nstar + NpropStar + NpropImage, Npopulated);
    else
        % not sparse
        H = zeros(Nline, Args.Nimage + Args.Nstar + NpropStar + NpropImage);
    end
    
    ColNames = [tools.cell.cellNumericSuffix(Args.ZP_PrefixName,      [1:1:Args.Nimage]), ...
                tools.cell.cellNumericSuffix(Args.MeanMag_PrefixName, [1:1:Args.Nstar]), ...
                StarPropNames,...
                ImagePropNames];
            
    
            
    VecIm = (1:1:Args.Nimage).';
    %VecSt = Args.Nimage+(1:1:Args.Nstar).';
    for Ist=1:1:Args.Nstar
        Iline = VecIm + (Ist-1).*Args.Nimage;
        H(Iline, VecIm) = diag(ones(Args.Nimage,1));
        
        H(Iline, Args.Nimage+Ist) = ones(Args.Nimage,1);
        for Ip=1:1:NpropStar
            H(Iline, Args.Nimage+Args.Nstar + Ip) = Args.StarProp{Ip}(Ist).*ones(Args.Nimage,1);
        end
        
        for Ip=1:1:NpropImage
            H(Iline,  Args.Nimage+Args.Nstar + NpropStar + Ip) = Args.ImageProp{Ip}(:);
        end
            
    end
    
      
   
    
    
    
    
end
