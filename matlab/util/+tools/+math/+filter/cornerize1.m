function Template = cornerize1(Template, Length, Dim, InCorner, PadVal)
    % Pad a templare an put the template center at the corner, or center.
    % Input  : - A 1D template.
    %            This can be a vector or a matrix of templates in which the
    %            templates can be along the 1st or 2nd dimension.
    %          - The required size of the template.
    %          - Dimension along the template exist.
    %            Default is 1.
    %          - A logical indicating if to put the template at the corner
    %            or center. Default is true (in corner).
    %          - Pad value. Default is 0.
    % Output : - Padded and in corber (or cemter) templates.
    % Author : Eran Ofek (Feb 2022)
    % Example: T = [1 2 3 2 1];
    %          T1 = tools.math.filter.cornerize1(T,10)
    %          T1 = tools.math.filter.cornerize1(T.',10)
    %          T1 = tools.math.filter.cornerize1([T.', T.'],10)
    %          T1 = tools.math.filter.cornerize1([T; T],10,2)
    
    arguments
        Template
        Length
        Dim                = 1;
        InCorner logical   = true;
        PadVal             = 0;
    end
    
    if size(Template,1)==1
        Template = Template.';
        Trans = true;
    else
        Trans = false;
    end
    
    LenT   = size(Template, Dim);
    Nextra = Length - LenT;
    if Nextra<0
        error('Template must be equal or smaller than Length');
    end
    
    if Dim==1
        PadSize = [Nextra, 0];
    elseif Dim==2
        PadSize = [0, Nextra];
    else
        error('Dim must be 1 or 2');
    end
    
    Nhalf = floor(LenT.*0.5);
    Template = padarray(Template, PadSize, PadVal, 'post');
    Template = circshift(Template, -Nhalf, Dim);
    
    
    if ~InCorner
        Template = fftshift(Template);
    end
    
    if Trans
        Template = Template.';
    end
    
end