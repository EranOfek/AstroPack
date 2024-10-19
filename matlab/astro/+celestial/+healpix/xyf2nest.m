function [Pix] = xyf2nest(NSide, Face, X, Y)
    % 
    %     Optional detailed description
    % Input  : - 
    %          - 
    %          * ...,key,val,... 
    % Output : - 
    % Author : Eran Ofek (2024 Oct) 
    % Example: 
   
    error('incorrect');
    % Initialize the nested pixel index
    Pix = zeros(size(X), 'uint64');
    
    % Loop over the number of bits (log2 of nside) to interleave bits of ix and iy
    for I = 0:log2(NSide)-1
        I = uint64(I);
        Pix = bitor(Pix, bitshift(bitand(X, 2.^I), 2*I));
        Pix = bitor(Pix, bitshift(bitand(Y, 2.^I), 2*I+1));
    
    end
    
    % Add the face contribution to the pixel index
    Pix = Pix + Face * (NSide.^2);
end
