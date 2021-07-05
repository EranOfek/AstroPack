function translient
    %
    
    arguments
        
    end
   
    
    m     = size(Pn_hat);
    z_Num = conj(Pn_hat).*conj(Pr_hat).*(Pn_hat.*R_hat - Pr_hat.*N_hat);
    z_Dom = abs(Pr_hat).^2 .* sigma_n.^2 + abs(Pn_hat).^2 .*sigma_r.^2;
    
    
    
    
end