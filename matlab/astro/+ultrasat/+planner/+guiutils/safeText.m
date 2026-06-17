function s = safeText(val)
    % Return a char string safe for App Designer Label.Text (never empty/non-text crash)
    % Usage: app.MyLabe.Text = ultrasat.planner.guiutils.safeText(MyVar);
    
    % Map empty to blank string
    if isempty(val)
        s = '';
    elseif isstring(val) || ischar(val)
        % Already textual — pass through as char
        s = char(val);
    else
        % Coerce numeric or other types to char via string()
        s = char(string(val));
    end
end
