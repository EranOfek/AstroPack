function s = safeText(val)
    % Required because AppDesigner’s Label.Text does not accept empty or non-text values.
    % Usage: app.MyLabe.Text = ultrasat.planner.guiutils.safeText(MyVar);
    
    if isempty(val)
        s = '';
    elseif isstring(val) || ischar(val)
        s = char(val);
    else
        s = char(string(val));
    end
end
