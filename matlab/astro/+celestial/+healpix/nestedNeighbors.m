function Neigh = nestedNeighbors(NSide, Pix, Args)
    % Return 8 neighbors of NESTED HEALPix pixel(s).
    %   Each healpix may have 7 or 8 neighboors. So the same neighboor may
    %   appear twice.
    %
    % Input  : - (NSide) scalar power-of-two
    %          - (Pix) scalar or array of NESTED pixel indices
    %          * ...,key,val,...
    %            'IncludeCenter' - Include central pix. Default is false.
    %
    % Output : - Array of neighbor pixel indices [8 x numel(Npix)]
    %            Neighbor order:
    %            1 2 3
    %            4   5
    %            6 7 8
    %            i.e.
    %               (-1,-1) (0,-1) (1,-1)
    %               (-1, 0)         (1, 0)
    %               (-1, 1) (0, 1) (1, 1)
    %            If center pixel is included, then it appears at the edn.
    % Author : ChatGPT + Eran Ofek (Feb 2026)
    % Example: Neigh = nestedNeighbors(NSide, Pix)
    

    arguments
        NSide %(1,1) {mustBeInteger, mustBePositive}
        Pix %{mustBeInteger, mustBeNonnegative}
        Args.IncludeCenter     = false
    end
    
%error('BUG!')

Pix = uint64(Pix(:).');
Npix = numel(Pix);

[LonC, LatC] = celestial.healpix.pix2ang(NSide, Pix);

ThetaPix = sqrt(4*pi / (12*double(NSide)^2));
Step = 0.55 * ThetaPix;

[Xc, Yc, Zc] = sph2cart(LonC, LatC, 1);

Zaxis = [0;0;1];

Neigh = zeros(8, Npix, 'uint64');

Dirs = int32([
   -1 -1
    0 -1
    1 -1
   -1  0
    1  0
   -1  1
    0  1
    1  1
]);

for I = 1:Npix

    C = [Xc(I); Yc(I); Zc(I)];

    E = cross(Zaxis, C);
    if norm(E) < 1e-12
        E = cross([1;0;0], C);
    end
    E = E ./ norm(E);

    N = cross(C, E);
    N = N ./ norm(N);

    for K = 1:8

        Dx = double(Dirs(K,1));
        Dy = double(Dirs(K,2));

        Scale = 1 / hypot(Dx, Dy);
        Dx = Dx * Scale;
        Dy = Dy * Scale;

        V = C + Step*(Dx*E + Dy*N);
        V = V ./ norm(V);

        Lon = atan2(V(2), V(1));
        Lat = asin(V(3));

        Neigh(K,I) = celestial.healpix.ang2pix(NSide, Lon, Lat);

    end




    % Pix  = Pix(:).';
    % Npix = numel(Pix);
    % 
    % [X, Y, Face] = celestial.healpix.nest2xyf(NSide, Pix);
    % 
    % % ---- Convert to signed int32 for arithmetic ----
    % X    = int32(X);
    % Y    = int32(Y);
    % Face = int32(Face);
    % NS   = int32(NSide);
    % 
    % 
    % Offsets = int32([
    %    -1 -1
    %     0 -1
    %     1 -1
    %    -1  0
    %     1  0
    %    -1  1
    %     0  1
    %     1  1
    % ]);
    % 
    % Neigh = zeros(8, Npix, 'uint64');
    % 
    % for K = 1:8
    % 
    %     dX = Offsets(K,1);
    %     dY = Offsets(K,2);
    % 
    %     Xn = X + dX;
    %     Yn = Y + dY;
    %     Fn = Face;
    % 
    %     CrossLeft  = Xn < 0;
    %     CrossRight = Xn >= NS;
    %     CrossDown  = Yn < 0;
    %     CrossUp    = Yn >= NS;
    % 
    %     Cross = CrossLeft | CrossRight | CrossDown | CrossUp;
    % 
    %     if any(Cross)
    % 
    %         Idx = find(Cross);
    % 
    %         for I = Idx
    % 
    %             Xi = X(I);
    %             Yi = Y(I);
    % 
    %             Xcand = Xi + dX;
    %             Ycand = Yi + dY;
    % 
    %             % --- determine crossed edge ---
    %             if dX ~= 0   % horizontal has priority
    % 
    %                 if Xcand < 0
    %                     Edge = 'left';
    %                     Xlocal = NS - 1;
    %                     Ylocal = Ycand;
    %                 else
    %                     Edge = 'right';
    %                     Xlocal = 0;
    %                     Ylocal = Ycand;
    %                 end
    % 
    %             else   % purely vertical move
    % 
    %                 if Ycand < 0
    %                     Edge = 'down';
    %                     Ylocal = NS - 1;
    %                     Xlocal = Xcand;
    %                 else
    %                     Edge = 'up';
    %                     Ylocal = 0;
    %                     Xlocal = Xcand;
    %                 end
    % 
    %             end
    % 
    %             % Wrap the coordinate that did not define the edge
    %             if Xlocal < 0
    %                 Xlocal = NS - 1;
    %             elseif Xlocal >= NS
    %                 Xlocal = 0;
    %             end
    % 
    %             if Ylocal < 0
    %                 Ylocal = NS - 1;
    %             elseif Ylocal >= NS
    %                 Ylocal = 0;
    %             end
    % 
    %             [NewFace, T] = celestial.healpix.faceTransition(Fn(I), Edge);
    %             Fn(I) = int32(NewFace);
    % 
    %             [Xt, Yt] = celestial.healpix.applyFaceTransform( ...
    %                 NSide, Xlocal, Ylocal, T);
    % 
    %             Xn(I) = int32(Xt);
    %             Yn(I) = int32(Yt);
    % 
    %         end
    %     end
    % 
    %     Neigh(K,:) = celestial.healpix.xyf2nest( ...
    %                          NSide, uint32(Xn), uint32(Yn), uint32(Fn));
    % end

        % 
        % 
        % 
        % 
        % 
        % Xn = X + dX;
        % Yn = Y + dY;
        % Fn = Face;
        % 
        % % --- Boundary detection ---
        % CrossLeft  = Xn < 0;
        % CrossRight = Xn >= NSide;
        % CrossDown  = Yn < 0;
        % CrossUp    = Yn >= NSide;
        % 
        % Cross = CrossLeft | CrossRight | CrossDown | CrossUp;
        % 
        % if any(Cross)
        % 
        %     Idx = find(Cross);
        % 
        %     for I = Idx
        % 
        %         if CrossLeft(I)
        %             Edge = 'left';
        %             Xn(I) = NSide - 1;
        %         elseif CrossRight(I)
        %             Edge = 'right';
        %             Xn(I) = 0;
        %         elseif CrossDown(I)
        %             Edge = 'down';
        %             Yn(I) = NSide - 1;
        %         else
        %             Edge = 'up';
        %             Yn(I) = 0;
        %         end
        % 
        %         [NewFace, T] = celestial.healpix.faceTransition(Fn(I), Edge);
        %         Fn(I) = NewFace;
        % 
        %         [Xn(I), Yn(I)] = celestial.healpix.applyFaceTransform( ...
        %             NSide, Xn(I), Yn(I), T);
        % 
        %     end
        % end
        % 
        % Neigh(K,:) = celestial.healpix.xyf2nest(NSide, Xn, Yn, Fn);
        % 
    %end
    
    if Args.IncludeCenter
        Neigh = [Pix; Neigh];
    end
    

end