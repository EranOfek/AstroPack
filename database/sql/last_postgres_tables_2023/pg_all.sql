create table public.raw_images
(
    pk          bigint generated always as identity
        primary key,
    filename    varchar(256),
    xxhash      varchar(80),
    ra          double precision default 0,
    dec         double precision default 0,
    jd          double precision default 0,
    mount       smallint         default 0,
    camnum      smallint         default 0,
    imtype      varchar(80),
    bitpix      smallint         default 0,
    naxis1      smallint         default 0,
    naxis2      smallint         default 0,
    object      varchar          default ''::character varying,
    expmode     varchar(80)      default ''::character varying,
    counter     integer          default 0,
    exptime     real             default 0,
    gain        real             default 0,
    readnoi     real             default 0,
    darkcur     real             default 0,
    saturval    real             default 0,
    nonlin      real             default 0,
    binx        smallint         default 0,
    biny        smallint         default 0,
    camname     varchar(80)      default ''::character varying,
    camtemp     real             default 0,
    camcool     real             default 0,
    cammode     smallint         default 0,
    camgain     smallint         default 0,
    camoffs     smallint         default 0,
    projname    varchar(256)     default ''::character varying,
    obslon      real             default 0,
    obslat      real             default 0,
    obsalt      real             default 0,
    lst         real             default 0,
    date_obs    varchar(80)      default ''::character varying,
    m_ra        double precision default 0,
    m_dec       double precision default 0,
    m_ha        double precision default 0,
    m_jra       double precision default 0,
    m_jdec      double precision default 0,
    m_jha       double precision default 0,
    ha          double precision default 0,
    equinox     real             default 0,
    m_az        real             default 0,
    m_alt       real             default 0,
    az          real             default 0,
    alt         real             default 0,
    airmass     real             default 0,
    trk_ra      real             default 0,
    trk_dec     real             default 0,
    mnttemp     real             default 0,
    focus       real             default 0,
    prvfocus    real             default 0,
    procstat    varchar(256)     default ''::character varying,
    procversion smallint         default 0
);

comment on column public.raw_images.procstat is 'Additional user data';

alter table public.raw_images
    owner to postgres;

create index raw_images_idx_filename
    on public.raw_images (filename);

create index raw_images_idx_xxhash
    on public.raw_images (xxhash);

create index raw_images_idx_ra
    on public.raw_images (ra);

create index raw_images_idx_dec
    on public.raw_images (dec);

create index raw_images_idx_jd
    on public.raw_images (jd);

create index raw_images_idx_mount
    on public.raw_images (mount);

create index raw_images_idx_camnum
    on public.raw_images (camnum);

create index raw_images_idx_imtype
    on public.raw_images (imtype);

create table public.proc_images
(
    pk           bigint generated always as identity
        primary key,
    filename     varchar(256),
    xxhash       varchar(80),
    ra           double precision default 0,
    dec          double precision default 0,
    jd           double precision default 0,
    mount        smallint         default 0,
    camnum       smallint         default 0,
    imtype       varchar(80),
    bitpix       smallint         default 0,
    naxis1       smallint         default 0,
    naxis2       smallint         default 0,
    object       varchar          default ''::character varying,
    expmode      varchar(80)      default ''::character varying,
    counter      integer          default 0,
    exptime      real             default 0,
    gain         real             default 0,
    readnoi      real             default 0,
    darkcur      real             default 0,
    saturval     real             default 0,
    nonlin       real             default 0,
    binx         smallint         default 0,
    biny         smallint         default 0,
    camname      varchar(80)      default ''::character varying,
    camtemp      real             default 0,
    camcool      real             default 0,
    cammode      smallint         default 0,
    camgain      smallint         default 0,
    camoffs      smallint         default 0,
    projname     varchar(256)     default ''::character varying,
    obslon       real             default 0,
    obslat       real             default 0,
    obsalt       real             default 0,
    lst          real             default 0,
    date_obs     varchar(80)      default ''::character varying,
    m_ra         double precision default 0,
    m_dec        double precision default 0,
    m_ha         double precision default 0,
    m_jra        double precision default 0,
    m_jdec       double precision default 0,
    m_jha        double precision default 0,
    ha           double precision default 0,
    equinox      real             default 0,
    m_az         real             default 0,
    m_alt        real             default 0,
    az           real             default 0,
    alt          real             default 0,
    airmass      real             default 0,
    trk_ra       real             default 0,
    trk_dec      real             default 0,
    mnttemp      real             default 0,
    focus        real             default 0,
    prvfocus     real             default 0,
    procstat     varchar(256)     default ''::character varying,
    procversion  smallint         default 0,
    fieldid      varchar(80)      default ''::character varying,
    timezone     real             default 0,
    ccdid        real             default 0,
    cropid       real             default 0,
    level        varchar(80)      default ''::character varying,
    version      varchar(80)      default ''::character varying,
    subdir       varchar(80)      default ''::character varying,
    overscan     varchar(80)      default ''::character varying,
    origgain     real             default 0,
    ccdsec       varchar(80)      default ''::character varying,
    origsec      varchar(80)      default ''::character varying,
    origusec     varchar(80)      default ''::character varying,
    uniqsec      varchar(80)      default ''::character varying,
    meanbck      double precision default 0,
    medbck       double precision default 0,
    stdbck       double precision default 0,
    meanvar      double precision default 0,
    medvar       double precision default 0,
    ast_nsrc     real             default 0,
    ast_arms     double precision default 0,
    ast_errm     double precision default 0,
    wcsaxes      smallint         default 0,
    radesys      varchar(80)      default ''::character varying,
    lonpole      real             default 0,
    latpole      real             default 0,
    ctype1       varchar(80)      default ''::character varying,
    ctype2       varchar(80)      default ''::character varying,
    cunit1       varchar(80)      default ''::character varying,
    cunit2       varchar(80)      default ''::character varying,
    crpix1       double precision default 0,
    crpix2       double precision default 0,
    crval1       double precision default 0,
    crval2       double precision default 0,
    cd1_1        double precision default 0,
    cd1_2        double precision default 0,
    cd2_1        double precision default 0,
    cd2_2        double precision default 0,
    ra1          double precision default 0,
    ra2          double precision default 0,
    ra3          double precision default 0,
    ra4          double precision default 0,
    dec1         double precision default 0,
    dec2         double precision default 0,
    dec3         double precision default 0,
    dec4         double precision default 0,
    rau1         double precision default 0,
    rau2         double precision default 0,
    rau3         double precision default 0,
    rau4         double precision default 0,
    decu1        double precision default 0,
    decu2        double precision default 0,
    decu3        double precision default 0,
    decu4        double precision default 0,
    ph_zp        double precision default 0,
    ph_col1      double precision default 0,
    ph_medc      double precision default 0,
    ph_rms       double precision default 0,
    ph_nsrc      real             default 0,
    ph_magsy     varchar(80)      default ''::character varying,
    linmag       double precision default 0,
    backmag      double precision default 0,
    fwhm         double precision default 0,
    med_a        double precision default 0,
    med_b        double precision default 0,
    med_th       double precision default 0,
    pipever      varchar(80)      default ''::character varying,
    raw_image_id integer          default 0
);

comment on column public.proc_images.procstat is 'Additional user data';

alter table public.proc_images
    owner to postgres;

create index proc_images_idx_filename
    on public.proc_images (filename);

create index proc_images_idx_xxhash
    on public.proc_images (xxhash);

create index proc_images_idx_ra
    on public.proc_images (ra);

create index proc_images_idx_dec
    on public.proc_images (dec);

create index proc_images_idx_jd
    on public.proc_images (jd);

create index proc_images_idx_mount
    on public.proc_images (mount);

create index proc_images_idx_camnum
    on public.proc_images (camnum);

create index proc_images_idx_imtype
    on public.proc_images (imtype);

create table public.coadd_images
(
    pk           bigint generated always as identity
        primary key,
    filename     varchar(256),
    xxhash       varchar(80),
    ra           double precision default 0,
    dec          double precision default 0,
    jd           double precision default 0,
    mount        smallint         default 0,
    camnum       smallint         default 0,
    imtype       varchar(80),
    bitpix       smallint         default 0,
    naxis1       smallint         default 0,
    naxis2       smallint         default 0,
    object       varchar          default ''::character varying,
    expmode      varchar(80)      default ''::character varying,
    counter      integer          default 0,
    exptime      real             default 0,
    gain         real             default 0,
    readnoi      real             default 0,
    darkcur      real             default 0,
    saturval     real             default 0,
    nonlin       real             default 0,
    binx         smallint         default 0,
    biny         smallint         default 0,
    camname      varchar(80)      default ''::character varying,
    camtemp      real             default 0,
    camcool      real             default 0,
    cammode      smallint         default 0,
    camgain      smallint         default 0,
    camoffs      smallint         default 0,
    projname     varchar(256)     default ''::character varying,
    obslon       real             default 0,
    obslat       real             default 0,
    obsalt       real             default 0,
    lst          real             default 0,
    date_obs     varchar(80)      default ''::character varying,
    m_ra         double precision default 0,
    m_dec        double precision default 0,
    m_ha         double precision default 0,
    m_jra        double precision default 0,
    m_jdec       double precision default 0,
    m_jha        double precision default 0,
    ha           double precision default 0,
    equinox      real             default 0,
    m_az         real             default 0,
    m_alt        real             default 0,
    az           real             default 0,
    alt          real             default 0,
    airmass      real             default 0,
    trk_ra       real             default 0,
    trk_dec      real             default 0,
    mnttemp      real             default 0,
    focus        real             default 0,
    prvfocus     real             default 0,
    procstat     varchar(256)     default ''::character varying,
    procversion  smallint         default 0,
    fieldid      varchar(80)      default ''::character varying,
    timezone     real             default 0,
    ccdid        real             default 0,
    cropid       real             default 0,
    level        varchar(80)      default ''::character varying,
    version      varchar(80)      default ''::character varying,
    subdir       varchar(80)      default ''::character varying,
    overscan     varchar(80)      default ''::character varying,
    origgain     real             default 0,
    ccdsec       varchar(80)      default ''::character varying,
    origsec      varchar(80)      default ''::character varying,
    origusec     varchar(80)      default ''::character varying,
    uniqsec      varchar(80)      default ''::character varying,
    meanbck      double precision default 0,
    medbck       double precision default 0,
    stdbck       double precision default 0,
    meanvar      double precision default 0,
    medvar       double precision default 0,
    ast_nsrc     real             default 0,
    ast_arms     double precision default 0,
    ast_errm     double precision default 0,
    wcsaxes      smallint         default 0,
    radesys      varchar(80)      default ''::character varying,
    lonpole      real             default 0,
    latpole      real             default 0,
    ctype1       varchar(80)      default ''::character varying,
    ctype2       varchar(80)      default ''::character varying,
    cunit1       varchar(80)      default ''::character varying,
    cunit2       varchar(80)      default ''::character varying,
    crpix1       double precision default 0,
    crpix2       double precision default 0,
    crval1       double precision default 0,
    crval2       double precision default 0,
    cd1_1        double precision default 0,
    cd1_2        double precision default 0,
    cd2_1        double precision default 0,
    cd2_2        double precision default 0,
    ra1          double precision default 0,
    ra2          double precision default 0,
    ra3          double precision default 0,
    ra4          double precision default 0,
    dec1         double precision default 0,
    dec2         double precision default 0,
    dec3         double precision default 0,
    dec4         double precision default 0,
    rau1         double precision default 0,
    rau2         double precision default 0,
    rau3         double precision default 0,
    rau4         double precision default 0,
    decu1        double precision default 0,
    decu2        double precision default 0,
    decu3        double precision default 0,
    decu4        double precision default 0,
    ph_zp        double precision default 0,
    ph_col1      double precision default 0,
    ph_medc      double precision default 0,
    ph_rms       double precision default 0,
    ph_nsrc      real             default 0,
    ph_magsy     varchar(80)      default ''::character varying,
    linmag       double precision default 0,
    backmag      double precision default 0,
    fwhm         double precision default 0,
    med_a        double precision default 0,
    med_b        double precision default 0,
    med_th       double precision default 0,
    pipever      varchar(80)      default ''::character varying,
    raw_image_id integer          default 0,
    ncoadd       real             default 0,
    coaddop      varchar(80)      default ''::character varying,
    avncoadd     real             default 0,
    mincoadd     smallint         default 0,
    midjd        double precision default 0,
    minjd        double precision default 0,
    maxjd        double precision default 0,
    sublevel     varchar(80)      default ''::character varying,
    gm_ratex     double precision default 0,
    gm_stdx      double precision default 0,
    gm_ratey     double precision default 0,
    gm_stdy      double precision default 0
);

comment on column public.coadd_images.procstat is 'Additional user data';

alter table public.coadd_images
    owner to postgres;

create index coadd_images_idx_filename
    on public.coadd_images (filename);

create index coadd_images_idx_xxhash
    on public.coadd_images (xxhash);

create index coadd_images_idx_ra
    on public.coadd_images (ra);

create index coadd_images_idx_dec
    on public.coadd_images (dec);

create index coadd_images_idx_jd
    on public.coadd_images (jd);

create index coadd_images_idx_mount
    on public.coadd_images (mount);

create index coadd_images_idx_camnum
    on public.coadd_images (camnum);

create index coadd_images_idx_imtype
    on public.coadd_images (imtype);

create table public.proc_src_catalog
(
    pk             bigint generated always as identity
        primary key,
    filename       varchar(256),
    xxhash         varchar(80),
    mount          smallint         default 0,
    camnum         smallint         default 0,
    node           smallint         default 0,
    xpeak          smallint         default 0,
    ypeak          smallint         default 0,
    x1             real             default 0,
    y1             real             default 0,
    x2             real             default 0,
    y2             real             default 0,
    xy             real             default 0,
    sn_1           real             default 0,
    sn_2           real             default 0,
    sn_3           real             default 0,
    sn_4           real             default 0,
    sn_5           real             default 0,
    back_im        real             default 0,
    var_im         real             default 0,
    back_annulus   real             default 0,
    std_annulus    real             default 0,
    flux_aper_1    double precision default 0,
    flux_aper_2    double precision default 0,
    flux_aper_3    double precision default 0,
    fluxerr_aper_1 double precision default 0,
    fluxerr_aper_2 double precision default 0,
    fluxerr_aper_3 double precision default 0,
    mag_aper_1     double precision default 0,
    mag_aper_2     double precision default 0,
    mag_aper_3     double precision default 0,
    magerr_aper_1  double precision default 0,
    magerr_aper_2  double precision default 0,
    magerr_aper_3  double precision default 0,
    flux_conv_1    double precision default 0,
    flux_conv_2    double precision default 0,
    flux_conv_3    double precision default 0,
    flux_conv_4    double precision default 0,
    flux_conv_5    double precision default 0,
    mag_conv_1     double precision default 0,
    mag_conv_2     double precision default 0,
    mag_conv_3     double precision default 0,
    mag_conv_4     double precision default 0,
    mag_conv_5     double precision default 0,
    magerr_conv_1  double precision default 0,
    magerr_conv_2  double precision default 0,
    magerr_conv_3  double precision default 0,
    magerr_conv_4  double precision default 0,
    magerr_conv_5  double precision default 0,
    flags          integer          default 0,
    x              real             default 0,
    y              real             default 0,
    flux_psf       double precision default 0,
    mag_psf        double precision default 0,
    magerr_psf     double precision default 0,
    psf_chi2dof    real             default 0,
    sn             double precision default 0,
    ra             double precision default 0,
    dec            double precision default 0,
    mergedcatmask  integer          default 0,
    nobs           real             default 0,
    procstat       varchar(256)     default ''::character varying,
    pipever        varchar(80)      default ''::character varying,
    exptime        real             default 0,
    jd             double precision default 0
);

comment on column public.proc_src_catalog.procstat is 'Additional user data';

alter table public.proc_src_catalog
    owner to postgres;

create index proc_src_catalog_idx_filename
    on public.proc_src_catalog (filename);

create index proc_src_catalog_idx_xxhash
    on public.proc_src_catalog (xxhash);

create index proc_src_catalog_idx_mount
    on public.proc_src_catalog (mount);

create index proc_src_catalog_idx_camnum
    on public.proc_src_catalog (camnum);

create index proc_src_catalog_idx_node
    on public.proc_src_catalog (node);

create index proc_src_catalog_idx_mag_psf
    on public.proc_src_catalog (mag_psf);

create index proc_src_catalog_idx_sn
    on public.proc_src_catalog (sn);

create index proc_src_catalog_idx_ra
    on public.proc_src_catalog (ra);

create index proc_src_catalog_idx_dec
    on public.proc_src_catalog (dec);

create index proc_src_catalog_idx_jd
    on public.proc_src_catalog (jd);

create table public.coadd_src_catalog
(
    pk             bigint generated always as identity
        primary key,
    filename       varchar(256),
    xxhash         varchar(80),
    mount          smallint         default 0,
    camnum         smallint         default 0,
    node           smallint         default 0,
    xpeak          smallint         default 0,
    ypeak          smallint         default 0,
    x1             real             default 0,
    y1             real             default 0,
    x2             real             default 0,
    y2             real             default 0,
    xy             real             default 0,
    sn_1           real             default 0,
    sn_2           real             default 0,
    sn_3           real             default 0,
    sn_4           real             default 0,
    sn_5           real             default 0,
    back_im        real             default 0,
    var_im         real             default 0,
    back_annulus   real             default 0,
    std_annulus    real             default 0,
    flux_aper_1    double precision default 0,
    flux_aper_2    double precision default 0,
    flux_aper_3    double precision default 0,
    fluxerr_aper_1 double precision default 0,
    fluxerr_aper_2 double precision default 0,
    fluxerr_aper_3 double precision default 0,
    mag_aper_1     double precision default 0,
    mag_aper_2     double precision default 0,
    mag_aper_3     double precision default 0,
    magerr_aper_1  double precision default 0,
    magerr_aper_2  double precision default 0,
    magerr_aper_3  double precision default 0,
    flux_conv_1    double precision default 0,
    flux_conv_2    double precision default 0,
    flux_conv_3    double precision default 0,
    flux_conv_4    double precision default 0,
    flux_conv_5    double precision default 0,
    mag_conv_1     double precision default 0,
    mag_conv_2     double precision default 0,
    mag_conv_3     double precision default 0,
    mag_conv_4     double precision default 0,
    mag_conv_5     double precision default 0,
    magerr_conv_1  double precision default 0,
    magerr_conv_2  double precision default 0,
    magerr_conv_3  double precision default 0,
    magerr_conv_4  double precision default 0,
    magerr_conv_5  double precision default 0,
    flags          integer          default 0,
    x              real             default 0,
    y              real             default 0,
    flux_psf       double precision default 0,
    mag_psf        double precision default 0,
    magerr_psf     double precision default 0,
    psf_chi2dof    real             default 0,
    sn             double precision default 0,
    ra             double precision default 0,
    dec            double precision default 0,
    mergedcatmask  integer          default 0,
    nobs           real             default 0,
    procstat       varchar(256)     default ''::character varying,
    pipever        varchar(80)      default ''::character varying,
    jd             double precision default 0,
    exptime        real             default 0,
    distmp         real             default 0
);

comment on column public.coadd_src_catalog.procstat is 'Additional user data';

alter table public.coadd_src_catalog
    owner to postgres;

create index coadd_src_catalog_idx_filename
    on public.coadd_src_catalog (filename);

create index coadd_src_catalog_idx_xxhash
    on public.coadd_src_catalog (xxhash);

create index coadd_src_catalog_idx_mount
    on public.coadd_src_catalog (mount);

create index coadd_src_catalog_idx_camnum
    on public.coadd_src_catalog (camnum);

create index coadd_src_catalog_idx_node
    on public.coadd_src_catalog (node);

create index coadd_src_catalog_idx_mag_psf
    on public.coadd_src_catalog (mag_psf);

create index coadd_src_catalog_idx_sn
    on public.coadd_src_catalog (sn);

create index coadd_src_catalog_idx_ra
    on public.coadd_src_catalog (ra);

create index coadd_src_catalog_idx_dec
    on public.coadd_src_catalog (dec);

create index coadd_src_catalog_idx_jd
    on public.coadd_src_catalog (jd);

