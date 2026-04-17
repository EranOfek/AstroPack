function [Result] = unitTest()
    % unitTest for tools.sum package

    %% tools.sum.mex.sumPowers_mex

    R = rand(1e4,1);
    a1=tools.sum.mex.sumPowers_mex(R,3);
    aa=[sum(R(:)), sum(R(:).^2), sum(R(:).^3)];

    if max(abs(a1-aa),[],'all')>1e-11
        error('Problem with tools.sum.mex.sumPowers_mex');
    end


    %% tools.sum.mex.sum2_mex

    R = rand(1e3,1e3);
    S0 = sum(R.^2, 1, 'omitnan');
    S1 = tools.sum.mex.sum2_mex(R, 1);

    if max(abs(S0-S1),[],'all')>1e-12
        error('Problem with tools.sum.mex.sum2_mex');
    end


    %%

    Result = true;

end
