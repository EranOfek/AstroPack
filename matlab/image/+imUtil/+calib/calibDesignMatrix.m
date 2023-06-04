function H=calibDesignMatrix(Args)
    % Build design matrix for relative photometric calibration
    %   The design matrix is of the form: H=[Z_i, M_s, ...]
    %   and it can be used to solve: m_is = H*Par
    %
    % Example: H=imUtil.calib.calibDesignMatrix('Nstar',3,'Nimage',5)
    %          H=imUtil.calib.calibDesignMatrix('Nstar',3,'Nimage',5,'StarProp',{[2 3 4]})
    %          H=imUtil.calib.calibDesignMatrix('Nstar',3,'Nimage',5,'StarProp',{[2 3 4],[11 12 13]})
    
    
    arguments
        Args.Nstar
        Args.Nimage
        Args.StartPtopNames = {};
        Args.StarProp       = {};  % one vector of properties per star - e.g., color
        Args.ImageProp      = {};  % one vector of properties per image - e.g., airmass
    end
    
    % the Z_i block:
    % of the form: [1 0 0 ...; 1 0 0 ...; ...; 0 1 0 0 ...; ...]
    
    NpropStar = numel(Args.StarProp);
    % allocate the design matrix:
    H = zeros(Args.Nimage.*Args.Nstar, Args.Nimage + Args.Nstar + NpropStar);
    
    VecIm = (1:1:Args.Nimage).';
    %VecSt = Args.Nimage+(1:1:Args.Nstar).';
    for Ist=1:1:Args.Nstar
        Iline = VecIm + (Ist-1).*Args.Nimage;
        H(Iline, VecIm) = diag(ones(Args.Nimage,1));
        
        H(Iline, Args.Nimage+Ist) = ones(Args.Nimage,1);
        for Ip=1:1:NpropStar
            H(Iline, Args.Nimage+Args.Nstar + Ip) = Args.StarProp{Ip}(Ist).*ones(Args.Nimage,1);
        end
    end
      
   
    
    
    
    
end
