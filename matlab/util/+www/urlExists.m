function Tf = urlExists(Url)
% URLEXISTS Test if one or more URLs exist without downloading the file.
% Input  : - List of URLs. A char vector (single URL), or a string scalar,
%            or a string array, or a cell array of char.
% Output : - A logical vector, same size as input:
%            true  = URL exists (HTTP 200 or valid redirect)
%            false = does not exist, timeout, or error.
% Author : ChatGPT + Eran Ofek (Dec 2025)

% Notes:
%   Uses matlab.net.http.* to send HEAD requests (weboptions does not support HEAD).
%
% Example:
%   Tf = www.urlExists("https://dr18.sdss.org/.../spec-0350-51691-0239.fits");
%   Tf = www.urlExists(["url1","url2","url3"]);

    % Normalize input to string array
    if ischar(Url)
        Url = string(Url);
    elseif iscell(Url)
        Url = string(Url(:));
    elseif isstring(Url)
        % ok
    else
        error('Url must be char, string, or cell array of chars.');
    end

    N = numel(Url);
    Tf = false(size(Url));

    import matlab.net.*
    import matlab.net.http.*

    for i = 1:N
        U = strtrim(Url(i));

        try
            req = RequestMessage('head');   % HEAD request (no file download)
            resp = req.send(U);

            % Accept the following as "exists":
            %   200 OK
            %   301 / 302 / 303 / 307 / 308 (redirects)
            if isa(resp.StatusCode,'matlab.net.http.StatusCode')
                code = double(resp.StatusCode);
            else
                code = resp.StatusCode;
            end

            if ismember(code,[200 301 302 303 307 308])
                Tf(i) = true;
            else
                Tf(i) = false;
            end

        catch
            Tf(i) = false;
        end
    end
end
