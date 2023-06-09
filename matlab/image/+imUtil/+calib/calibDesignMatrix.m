function [H, ColNames]=calibDesignMatrix(Nimage, Nstar, Args)
    % Build design matrix for relative photometric calibration
    %   The design matrix is of the form: H=[Z_i, M_s, ...]
    %   and it can be used to solve: m_is = H*Par
    % Input  : - Number of images (epochs).
    %          - Number of stars
    %          * ...,key,val,...
    %            'Sparse' - Logical indicating if to construct a sparse
    %                   matrix.
    %                   Default is true.
    %            'ZP_PrefixName' - In the column names cell, this is the
    %                   prefix of the images zero point.
    %            'MeanMag_PrefixName' - In the column names cell, this is the
    %                   prefix of the stars mean magnitudes.
    %            'StarProp' - A cell array of vectors. Each vector
    %                   must be of the length equal to the number of stars.
    %                   Each vector in the cell array will generate a new
    %                   column in the design matrix with a property per
    %                   star (e.g., color of each star).
    %                   Default is {}.
    %            'StarPropNames' - A cell array of names for the StarProp
    %                   column names. If this is a string than will be the
    %                   string prefix, with added index. Default is 'SP'.
    %            'ImageProp' - Like StarProp but for the images.
    %                   E.g., airmass.
    %                   Default is {}.
    %            'ImagePropNames' - Like StarPropNames, but for the images.
    %                   Default is 'IP'.
    %            'AddCalibBlock' - A logical indicating if to add a block
    %                   of magnitude calibration, at the bottom of the
    %                   design matrix.
    % Output : - The design matrix.
    %          - A cell array of column names in the design matrix.
    % Author : Eran Ofek (Jun 2023)
    % Example: H=imUtil.calib.calibDesignMatrix(5,3);
    %          H=imUtil.calib.calibDesignMatrix(5,3,5,'StarProp',{[2 3 4]})
    %          H=imUtil.calib.calibDesignMatrix(5,3'StarProp',{[2 3 4],[11 12 13]})
    %          [H,CN]=imUtil.calib.calibDesignMatrix(5,3,'StarProp',{[2 3 4]}, 'ImageProp', {[1 2 3 4 5]});
    %          H1=imUtil.calib.calibDesignMatrix(5,3,'StarProp',{[2 3 4]}, 'ImageProp', {[1 2 3 4 5]}, 'Sparse',false);
    %          all(H1==H,'all')
        
    arguments
        Nimage
        Nstar
        Args.Sparse logical        = true;
        
        Args.ZP_PrefixName         = 'Z';
        Args.MeanMag_PrefixName    = 'M';
        
        Args.StarProp              = {};  % one vector of properties per star - e.g., color
        Args.StarPropNames         = 'SP';
        
        Args.ImageProp             = {};  % one vector of properties per image - e.g., airmass
        Args.ImagePropNames        = 'IP';
        
        Args.AddCalibBlock logical = false;
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
    Nline = Nimage.*Nstar;
    if Args.Sparse
        % define sparse design matrix 
        Npopulated = Nline .* (2 + NpropStar + NpropImage) + Nstar.*double(Args.AddCalibBlock);
        H = sparse([],[],[], Nline, Nimage + Nstar + NpropStar + NpropImage, Npopulated);
    else
        % not sparse
        H = zeros(Nline, Nimage + Nstar + NpropStar + NpropImage);
    end
    
    ColNames = [tools.cell.cellNumericSuffix(Args.ZP_PrefixName,      [1:1:Nimage]), ...
                tools.cell.cellNumericSuffix(Args.MeanMag_PrefixName, [1:1:Nstar]), ...
                StarPropNames,...
                ImagePropNames];
                        
    VecIm = (1:1:Nimage).';
    %VecSt = Nimage+(1:1:Nstar).';
    for Ist=1:1:Nstar
        Iline = VecIm + (Ist-1).*Nimage;
        H(Iline, VecIm) = diag(ones(Nimage,1));
        
        H(Iline, Nimage+Ist) = ones(Nimage,1);
        for Ip=1:1:NpropStar
            H(Iline, Nimage+Nstar + Ip) = Args.StarProp{Ip}(Ist).*ones(Nimage,1);
        end
        
        for Ip=1:1:NpropImage
            H(Iline,  Nimage+Nstar + NpropStar + Ip) = Args.ImageProp{Ip}(:);
        end
            
    end
    
    if Args.AddCalibBlock
        Iline = Nimage.*Nstar + (1:1:Nstar).';
        Irow  = Nimage + (1:1:Nstar);
        H(Iline,Irow) = diag(ones(Nstar,1));
    end
   
end
