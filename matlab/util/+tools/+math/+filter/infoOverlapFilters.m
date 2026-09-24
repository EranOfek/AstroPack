function [Info] = infoOverlapFilters(G1, G2)
    % Calculate the information-content overlap between two weighned templates under H0
    % Input  : - A single template (an array of any dimension)
    %          - Single or multiple templates.
    %            If a single template then this has the same size as the
    %            first input template. If multiple tempates, then they are
    %            store in an extra dimension.
    % Output : - A vector of information-content overlap between the first
    %            template and the (multiple) second templates.
    % Author : Eran Ofek (2025 Nov) 
    % Example: G1=imUtil.kernel2.gauss(1.5);
    %          G2=imUtil.kernel2.gauss([3;4]);
    %          InfoLoss=tools.math.filter.infoOverlapFilters(G1,G2)

    arguments
        G1
        G2
    end


    if ndims(G1)==ndims(G2) && all(size(G1)==size(G2))
        Info = dot(G1(:),G2(:))./(norm(G1).*norm(G2));
    else
        SizeG2 = size(G2);
        SizeF  = SizeG2(1:end-1);
        G2a=reshape(G2,[prod(SizeF),SizeG2(end)]);
        Info = sum(G1(:).*G2a,1)./(norm(G1,2).*sqrt(sum(G2a.^2,1)));
    end
end
