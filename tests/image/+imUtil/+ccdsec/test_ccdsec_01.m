function [Result] = unitTest()
    % UnitTest for : imUtil.ccdsec
    % Example: imUtil.ccdsec.unitTest
   

    CCDSEC1 = imUtil.ccdsec.remove_edge_section([100 200],[1 10 1 100],2);
    CCDSEC2 = imUtil.ccdsec.remove_edge_section([100 200],[1 10 1 100]);
    if CCDSEC1~=CCDSEC2
        error('Error in remove_edge_section');
    end
    if ~all(CCDSEC1==[11 200 1 100])
        error('Error in remove_edge_section');
    end
    
    CCDSEC = imUtil.ccdsec.remove_edge_section([100 200],[1 200 91 100],[]);
    if ~all(CCDSEC==[1 200 1 90])
        error('Error in remove_edge_section');
    end


    Result = true;
    
end
