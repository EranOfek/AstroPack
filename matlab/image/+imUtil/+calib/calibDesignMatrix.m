function H=calibDesignMatrix(Args)
    % Build design matrix for relative photometric calibration
    %   The design matrix is of the form: H=[Z_i, M_s, ...]
    %   and it can be used to solve: m_is = H*Par
    %
    % Example: H=imUtil.calib.calibDesignMatrix('Nstar',3,'Nimage',5)
    
    arguments
        Args.Nstar
        Args.Nimage
    end
    
    % the Z_i block:
    % of the form: [1 0 0 ...; 1 0 0 ...; ...; 0 1 0 0 ...; ...]
    
    % allocate the design matrix:
    H = zeros(Args.Nimage.*Args.Nstar, Args.Nimage + Args.Nstar);
    
    VecIm = (1:1:Args.Nimage).';
    VecSt = Args.Nimage+(1:1:Args.Nstar).';
    for Iim=1:1:Args.Nimage
        Iline = VecIm + (Iim-1).*Args.Nimage;
        H(Iline, VecIm) = diag(ones(Args.Nimage,1));
        
        H(Iline, Args.Nimage+Iim) = ones(Args.Nimage,1);
    end
      
   
    
    
    
    
end
