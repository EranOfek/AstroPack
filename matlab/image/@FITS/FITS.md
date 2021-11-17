# FITS

https://www.stsci.edu/instruments/wfpc2/Wfpc2_dhb/intro_ch23.html


### FITS File Format

Flexible Image Transport System (FITS) is a standard format for exchanging 
astronomical data between institutions, independent of the hardware platform 
and software environment. A data file in FITS format consists of a series of 
Header Data Units (HDUs), each containing two components: an ASCII text 
header and the binary data. The header contains a series of header keywords 
that describe the data in a particular HDU and the data component immediately 
follows the header.

The first header in a FITS file is known as the primary header, and any number 
of extensions can follow the primary HDU. The data unit following the primary 
header must contain either an image or no data at all, but each extension can 
contain one of several different data types, including images, binary tables, 
and ASCII text tables. The value of the XTENSION keyword in the extension's 
header identifies the type of data the extension contains. 

Figure 2.1 schematically illustrates the structure of a FITS file and its extensions. 


## Write Table

### matlab.io.fits.

	insertATbl(fptr,rowlen,   nrows,ttype, tbcol, tform,tunit,extname)

	insertBTbl(fptr,          nrows,ttype,        tform,tunit,extname,  pcount)

	createTbl (fptr,tbltype,  nrows,ttype,        tform,tunit,extname)



## Sample: asu.fit

### asu_h0.txt

	SIMPLE  =                    T / Standard FITS Format
	BITPIX  =                    8 / Character data
	NAXIS   =                    0 / No Image --- just extension(s)
	EXTEND  =                    T / There are standard extensions
	ORIGIN  = 'xml2fits_v1.95'     / Converted from XML-Astrores to FITS
							 e-mail: question@simbad.u-strasbg.fr
	COMMENT  ARG='-rm'
	LONGSTRN= 'OGIP 1.0'           / Long string convention (&/CONTINUE) may be used
	DATE    = '2021-03-15'         / Written on 2021-03-15:12:36:42 (GMT)
								 by: apache@vizier.u-strasbg.fr
			 **********************************************************
				 EXCERPT from catalogues stored in VizieR (CDS)
							with the following conditions:
			 **********************************************************

			 VizieR Astronomical Server vizier.u-strasbg.fr
			 Date: 2021-03-15T12:36:42 [V1.99+ (14-Oct-2013)]
			 Explanations and Statistics of UCDs: See LINK below
			 In case of problem, please report to: cds-question@unistra.fr

	INFO    = 'votable-version=1.99+ (14-Oct-2013)' / #
	INFO    = '-ref=VIZ604f53ea8bfd' / #
	INFO    = '-out.max=unlimited' / #
	Q-PARAMS=                   17 / Number of queryParameters (followed by list)
			 -oc.form=dec
			 -out.max=unlimited
			 #out.form=FITS (binary) Table
			 -nav=cat:B/wds&tab:{B/wds/wds}&tab:{B/wds/notes}&tab:{B/wds/refs}&key:s
			 ource=B/wds&HTTPPRM:&
			 -c.eq=J2000
			 -c.r= 2
			 -c.u=arcmin
			 -c.geom=r
			 -source=B/wds/wds,B/wds/notes,B/wds/refs
			 -order=I
			 -out=Obs1
			 -out=pa1
			 -out=sep1
			 -out=mag1
			 -out=mag2
			 -out=RAJ2000
			 -out=DEJ2000
			 #
	INFO    = 'CatalogsExamined=3'
			 3 catalogues with potential matches were examined.
	END


### asu_h1

	XTENSION= 'BINTABLE'           / Binary Table Extension
	BITPIX  =                    8 / binary data
	NAXIS   =                    2 / Simple 2-D matrix
	NAXIS1  =                   32 / Number of bytes per record
	NAXIS2  =               153989 / Number of records
	PCOUNT  =                    0 / Get rid of random parameters
	GCOUNT  =                    1 / Only one group (isn't it obvious?)
	TFIELDS =                    7 / Number of data fields (columns)
	CDS-CAT = 'B/wds   '           / Catalogue designation in CDS nomenclature
			 The Washington Visual Double Star Catalog (Mason+ 2001-2020)
	EXTNAME = 'B_wds_wds'          / Identification of the table
	CDS-NAME= 'B/wds/wds'          / Table name in METAtab
			 The Washington Double Star Catalog (main part)
	UCD__1  = 'time.epoch'         / ................................byte#0
	TDISP1  = 'I4      '           / Display Fortran Format
	TFORM1  = 'I       '           / 16-bit (short) integer
	TTYPE1  = 'Obs1    '           / ? Date of first satisfactory observation [NULL
									 integer written as an empty string]
	TUNIT1  = 'yr      '           / year
	TNULL1  =               -32768 / NULL definition
	UCD__2  = 'pos.posAng'         / ................................byte#2
	TDISP2  = 'I3      '           / Display Fortran Format
	TFORM2  = 'I       '           / 16-bit (short) integer
	TTYPE2  = 'pa1     '           / ? Position Angle at date Obs1 (2) [NULL integer
									 written as an empty string]
	TUNIT2  = 'deg     '           / degree
	TNULL2  =               -32768 / NULL definition
	UCD__3  = 'pos.angDistance;src.orbital' / .......................byte#4
	TDISP3  = 'F5.1    '           / Display Fortran Format
	TFORM3  = 'E       '           / 32-bit floating-point (simple precision)
	TTYPE3  = 'sep1    '           / ? Separation at date Obs1
	TUNIT3  = 'arcsec  '           / second of arc
	UCD__4  = 'phot.mag;em.opt'    / ................................byte#8
	TDISP4  = 'F6.3    '           / Display Fortran Format
	TFORM4  = 'E       '           / 32-bit floating-point (simple precision)
	TTYPE4  = 'mag1    '           / ? Magnitude of First Component
	TUNIT4  = 'mag     '           / magnitude
	UCD__5  = 'phot.mag;em.opt'    / ................................byte#12
	TDISP5  = 'F5.2    '           / Display Fortran Format
	TFORM5  = 'E       '           / 32-bit floating-point (simple precision)
	TTYPE5  = 'mag2    '           / ? Magnitude of Second Component
	TUNIT5  = 'mag     '           / magnitude
	UCD__6  = 'pos.eq.ra;meta.main' / ...............................byte#16
	TDISP6  = 'F11.5   '           / Display Fortran Format
	TFORM6  = 'D       '           / 64-bit floating-point (double precision)
	TTYPE6  = 'RAJ2000 '           / ? Right Ascension J2000 (Ep=J2000, hours)
	TUNIT6  = 'deg     '           / degree
	UCD__7  = 'pos.eq.dec;meta.main' / ..............................byte#24
	TDISP7  = 'F11.5   '           / Display Fortran Format
	TFORM7  = 'D       '           / 64-bit floating-point (double precision)
	TTYPE7  = 'DEJ2000 '           / ? Declination J2000 (Ep=J2000, degrees)
	TUNIT7  = 'deg     '           / degree
	END


