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

