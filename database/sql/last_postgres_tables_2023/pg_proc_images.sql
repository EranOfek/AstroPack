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

