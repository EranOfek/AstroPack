function [Files,Links,Prop,Cat]=wget_ztf_images_irsa(RA,Dec,Args)
    % Query and retrieve ZTF images from the IRSA archive
    % Package: VO.ZTF
    % Description: Query and retrieve ZTF images from the IRSA archive.
    % Input  : - J2000.0 R.A. [radians, [H M S], or sexagesimal string], or
    %            a string containing object name (e.g., 'm31').
    %            If empty, then the query is done without position (only the
    %            WHERE clause).
    %          - J2000.0 Dec. [radians, [sign D M S], or sexagesimal string].
    %          * ...,key,val,...
    %            'GetFiles'- Get files (true) or just query images (false).
    %                        Default is true.
    %            'GetN'    - Get specific image {'all','last','first'}.
    %                        Default is 'all'.
    %            'ImType'  - ZTF image type to query: 'sci' | 'raw' | 'cal'.
    %                        Default is 'sci'.
    %            'Product' - Image product type:
    %                        'log'|'mask'|'image'|'scilog'|'sex'|'dao'|'daopsf'|'diff'|'diffpsf'|...
    %                        Default is 'image'.
    %            'Where'   - String containing WHERE clause (e.g.,
    %                        'field=600 and ccdid=2'). Default is ''.
    %            'Intersect'- Options are: 'COVERS' | 'ENCLOSED' | 'CENTER' |
    %                        'OVERLAPS'.
    %                        Default is 'OVERLAPS'.
    %            'Size'    - Position search size (height, [width]) [deg].
    %                        Default is 0.
    %            'QueryPar'- Cell array of additional key,val parameters to
    %                        pass to VO.ZTF.irsa_query_ztf_images.
    %            'constructPar'- Cell array of additional key,val parameters to
    %                        pass to VO.ZTF.irsa_image_link.
    %            'User'    - IRSA/IPAC user name (char), or a cell containing
    %                        a two-line user/pass file for
    %                        io.files.read_user_pass_file.
    %                        If empty (default), credentials are read from
    %                        PasswordsManager using 'PassProject'.
    %            'Pass'    - IRSA/IPAC password. If empty (default) and
    %                        'User' is not a file cell, use PasswordsManager.
    %            'PassProject'- Passwords.yml project name.
    %                        Default is 'ZTF_IPAC'.
    %            'PassFile'- Path to a two-line user/pass file. If given,
    %                        credentials are read with
    %                        io.files.read_user_pass_file (old method).
    %                        Default is [].
    %            'pwgetExtra'- Extra parameter to pass to www.pwget.
    %                        Default is '--no-check-certificate --load-cookies=cookies.txt'.
    %            'MaxGet'  - Max parallel wget to pass to www.pwget.
    %                        Default is 15.
    % Output : - Cell array of retrieved file names.
    %          - Cell array of links.
    %          - Structure array of image properties.
    %          - AstCat object containing the queried table.
    % License: GNU general public license version 3
    %     By : Eran O. Ofek                    Nov 2017
    %    URL : http://weizmann.ac.il/home/eofek/matlab/
    % Example: [Files,Links,Prop,Cat]=VO.ZTF.wget_ztf_images_irsa(358./RAD,23./RAD);
    %          Files = VO.ZTF.wget_ztf_images_irsa([],[],'Where','field=600 and ccdid=2','ImType','raw');
    %          [Files,Links] = VO.ZTF.wget_ztf_images_irsa([],[],'Where','field=600 and ccdid=2','ImType','sci','Product','diff','GetFiles',false);
    %          % old two-line password file:
    %          Files = VO.ZTF.wget_ztf_images_irsa(358./RAD,23./RAD,'PassFile','~/matlab/passwords/ztf_ipac_pass');
    % Reliable: 2
    %--------------------------------------------------------------------------

    arguments
        RA                       = []
        Dec                      = []
        Args.GetFiles logical    = true
        Args.GetN                = 'all'
        Args.ImType              = 'sci'
        Args.Product             = 'image'
        Args.Where               = ''
        Args.Intersect           = 'OVERLAPS'
        Args.Size                = 0
        Args.QueryPar cell       = {}
        Args.constructPar cell   = {}
        Args.User                = []
        Args.Pass                = []
        Args.PassProject         = 'ZTF_IPAC'
        Args.PassFile            = []
        Args.pwgetExtra          = '--no-check-certificate --load-cookies=cookies.txt'
        Args.MaxGet              = 15
    end

    [User, Pass] = get_irsa_user_pass(Args);

    Cat=VO.ZTF.irsa_query_ztf_images(RA,Dec,'Where',Args.Where,'Intersect',Args.Intersect,'Size',Args.Size,...
                                     'User',User,'Pass',Pass,...
                                     Args.QueryPar{:});

    if isempty(Cat.Cat)
       % If Cat is empty than exit
       Files = [];
       Links = [];
       Prop  = [];
    else

        Prop  = VO.ZTF.irsa_table2prop(Cat,'ImType',Args.ImType,'Product',Args.Product);

        Links = VO.ZTF.irsa_image_link(Prop,Args.constructPar{:});

        if (Args.GetFiles)
            switch lower(Args.GetN)
                case 'all'
                    LinksR = Links;
                case 'first'
                    LinksR = Links{1};
                case 'last'
                    LinksR = Links{end};
                otherwise
                    error('Unknown GetN option');
            end

            Files = www.pwget(LinksR, Args.pwgetExtra, Args.MaxGet);
        else
            Files = {};
        end
    end
end

function [User, Pass] = get_irsa_user_pass(Args)
    % Resolve IRSA user/password from PasswordsManager or a two-line file.

    if ~isempty(Args.PassFile)
        [User, Pass] = io.files.read_user_pass_file(Args.PassFile);
    elseif iscell(Args.User)
        [User, Pass] = io.files.read_user_pass_file(Args.User{1});
    elseif ~isempty(Args.User) && ~isempty(Args.Pass)
        User = Args.User;
        Pass = Args.Pass;
    else
        PM = PasswordsManager;
        if isempty(Args.User)
            [User, Pass] = PM.getUserPassword(Args.PassProject);
        else
            [User, Pass] = PM.getUserPassword(Args.PassProject, Args.User);
        end
    end
end
