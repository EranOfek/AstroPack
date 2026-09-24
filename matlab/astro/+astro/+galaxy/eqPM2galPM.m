function [mu_l, mu_b, l, b] = eqPM2galPM(ra, dec, mu_ra, mu_dec)
    % Convert J2000 equatorial proper motions to Galactic (mu_l, mu_b).
    %   Convert proper motions from J2000 equatorial coordinates (RA,Dec)
    %   into Galactic proper motions (mu_l, mu_b).
    % Input  : - Right Ascension, α [radians, J2000]
    %          - Declination, δ [radians, J2000]
    %          - Proper motion in RA*, μ_{α*} = μ_α cos(δ) [arcsec/yr]
    %            (i.e. **cos(δ) is already included**).
    %          - Proper motion in Dec, μ_δ [arcsec/yr]
    % Output : - Proper motion in Galactic longitude*, μ_{l*} [arcsec/yr]
    %            This is μ_l cos(b), i.e. includes cos(b), in the same
    %            convention as μ_{α*}.
    %          - Proper motion in Galactic latitude, μ_b [arcsec/yr]
    %          - Galactic longitude [rad].
    %          - Galactic latitude [rad].
    %
    % CONVENTIONS:
    %   All proper motions are on the tangent plane and defined with the
    %     "star" convention:
    %         μ_{α*} = μ_α cos(δ),   μ_{l*} = μ_l cos(b).
    %    This function outputs μ_{l*} (mu_l) and μ_b.
    %    Units of proper motion are preserved: arcsec/yr in, arcsec/yr out.
    %
    % NOTES:
    %   Uses the standard J2000 (ICRS) to Galactic rotation matrix.
    %   Works for scalar or vector input.
    %
    % Author : ChatGPT + Eran Ofek (Dec 2025)
    % Example:
    %       % RA, Dec of a star (J2000), degrees -> radians:
    %       ra  = deg2rad(180.0);
    %       dec = deg2rad( 30.0);
    %       mu_ra  = 0.005;   % "/yr   (5 mas/yr)   μ_{α*}
    %       mu_dec = -0.003;  % "/yr   (-3 mas/yr)  μ_δ
    %
    %       [mu_l, mu_b, l, b] = astro.galaxy.pm_eq_to_gal(ra, dec, mu_ra, mu_dec);
    %

    % Ensure column vectors for internal computation
    ra     = ra(:);
    dec    = dec(:);
    mu_ra  = mu_ra(:);   % μ_{α*}
    mu_dec = mu_dec(:);  % μ_δ

    N = numel(ra);

    %--------------------------------------------------------------
    % J2000 Equatorial (ICRS) -> Galactic rotation matrix
    % (columns: equatorial basis, rows: Galactic basis)
    %--------------------------------------------------------------
    R = [ ...
        -0.0548755604162154  -0.8734370902348850  -0.4838350155487132 ; ...
         0.4941094278755837  -0.4448296299600112   0.7469822444972189 ; ...
        -0.8676661490190047  -0.1980763734312015   0.4559837761750669 ];

    %--------------------------------------------------------------
    % Unit direction vector in equatorial coordinates
    %--------------------------------------------------------------
    cd = cos(dec);  sd = sin(dec);
    ca = cos(ra);   sa = sin(ra);

    % r_eq: 3 x N
    r_eq = [cd.*ca.'; cd.*sa.'; sd.'];

    %--------------------------------------------------------------
    % Equatorial tangent basis vectors (orthonormal)
    %--------------------------------------------------------------
    % p_eq: direction of increasing α at constant δ
    % q_eq: direction of increasing δ
    %
    % These are standard:
    %   p_eq = [-sinα,  cosα,  0]^T
    %   q_eq = [-cosα sinδ, -sinα sinδ, cosδ]^T
    p_eq = [-sa.';  ca.';  zeros(1,N)];
    q_eq = [-ca.'.*sd.';  -sa.'.*sd.';  cd.'];

    %--------------------------------------------------------------
    % Transform r, p, q to Galactic frame
    %--------------------------------------------------------------
    r_gal = R * r_eq;   % 3 x N
    p_gal = R * p_eq;
    q_gal = R * q_eq;

    xg = r_gal(1,:).';
    yg = r_gal(2,:).';
    zg = r_gal(3,:).';

    % Galactic longitude/latitude
    l = atan2(yg, xg);  % radians
    b = asin(zg);       % radians

    % Optionally wrap l to [0, 2π), if you prefer:
    % l = mod(l, 2*pi);

    cb = cos(b);
    sb = sin(b);
    cl = cos(l);
    sl = sin(l);

    %--------------------------------------------------------------
    % Galactic tangent basis vectors (orthonormal)
    %--------------------------------------------------------------
    % p_gal_prime: direction of increasing l (longitude)
    % q_gal_prime: direction of increasing b (latitude)
    %
    %   p'_gal = [-sin l,  cos l,  0]^T
    %   q'_gal = [-cos l sin b, -sin l sin b, cos b]^T
    l_row = l.';  b_row = b.';
    p_gal_prime = [-sin(l_row);          cos(l_row);                   zeros(1,N)];
    q_gal_prime = [-cos(l_row).*sin(b_row); -sin(l_row).*sin(b_row);   cos(b_row)];

    %--------------------------------------------------------------
    % Proper motion vector on the sky in Galactic frame
    %--------------------------------------------------------------
    % Tangential motion vector in the equatorial basis:
    %   v_t = μ_{α*} * p_eq + μ_δ * q_eq   (in "/yr, direction cosines)
    %
    % Then rotate v_t to Galactic frame via the same R.
    %
    mu_ra_row  = mu_ra.';   % 1 x N
    mu_dec_row = mu_dec.';  % 1 x N

    v_t_eq = p_eq .* mu_ra_row  +  q_eq .* mu_dec_row;  % 3 x N, "/yr
    v_t_gal = R * v_t_eq;                                % 3 x N, "/yr

    %--------------------------------------------------------------
    % Decompose v_t_gal onto (p_gal_prime, q_gal_prime)
    %--------------------------------------------------------------
    % Since (p_gal_prime, q_gal_prime) are orthonormal,
    %   μ_{l*} = p'_gal · v_t_gal
    %   μ_b    = q'_gal · v_t_gal
    %
    mu_l  = sum(p_gal_prime .* v_t_gal, 1).';   % N x 1, "/yr
    mu_b  = sum(q_gal_prime .* v_t_gal, 1).';   % N x 1, "/yr

    % mu_l is μ_{l*} = μ_l cos(b), analogous to μ_{α*}.
    % If you ever want plain μ_l, you can do: mu_l_plain = mu_l ./ cos(b);

end
