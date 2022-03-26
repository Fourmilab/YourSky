C CURDEF.INC (FORTRAN)
C  Add definitions to end of lists here if the database is altered to include 
C  additional parameters, but do not change or delete existing assignments.
C
C Data-fields currently defined and retrievable by calling DXREAD are listed
C below. Not all retrievable fields will be populated with data.
C
C Column "CODE" gives the integer used to request that quantity by its placement
C in the IR8ORD() array passed to DXINI. The requested value will be stored in
C the corresponding position of the R8OUT output array returned by DXREAD.
C
C All numeric data is returned to the user by DXREAD as 8-byte IEEE floating
C point, although the numeric data is physically stored in the database with 
C the different byte-level representations indicated below to reduce physical
C storage space. 
C 
C Symbols in the availability code:
C
C   a = field is defined for asteroid records
C   c = field is defined for comet records
C   3+= field is defined for DASTCOM3 and later database readers
C   4+= field is defined for DASTCOM4 and later database readers
C   5+= field is defined for DASTCOM5 and later database readers
C
C The classical orbital elements are heliocentric ecliptic. The ecliptic is
C specified by EQUNOX, which should now always be '2000', indicating J2000
C ecliptic system (IAU76 constants). 
C
C To transform from J2000 ecliptic to the original equatorial system used for 
C numerical integration (defined by PENAM), use an obliquity angle (epsilon)
C of 84381.448 arcsec. There are slightly different values of epsilon in
C different sets of constants, but this value was used to convert from the 
C precise planetary ephemeris equatorial system of PENAM, and so should be
C used to consistently recover the original numerically integrated equatorial 
C coordinates. If ecliptic coordinates are to be used directly for high 
C precision applications, convert-as-necessary to the intended ecliptic system.
C
C If EQUNOX is instead '1950', an epsilon of 84404.8362512 arc-seconds should
C be used to recover the original integrator equatorial state in the coordinate 
C frame of PENAM.
C
C The au->km conversion value for 'au' distance units should be taken from the 
C planetary ephemeris indicated by PENAM, along with other constants such 
C as mass parameters (GM's). For PENAMs of at least DE430 and later, 1 au is 
C 149597870.700 km, by IAU standard.
C
C Time units are in the coordinate time (CT) scale of general relativity, 
C defined by the independent variable in the barycentric planetary ephemeris 
C equations of motion. This is equivalent to the IAU TDB time-scale.
C
C "1 day" is defined to be 86400 SI seconds.
C
C NOTE #1: non-gravitational parameters A1, A2, A3 are stored in the database
C in units of 10^-8 au/d^2, but returned by this reader package with units of
C au/d^2. Conversion is done in subroutine DXNASGN. This handling is denoted 
C in the list below by the "[s:10^-8 au/day^2]" notation, where "s:" denotes 
C "stored as". For example, if the database store value of A1 is 1.3453, the
C value of A1 is 1.3453*10^-8 au/day^2, and that will be returned by this 
C reader package. A user-developed reader would need to perform the same units
C conversion.
C
C NOTE #2: values are physically stored with minimum appropriate byte-lengths 
C in suitable numerical data-types (i.e., integer, floating point), but are 
C converted to an array of 8-byte floating-point double-precision values when 
C returned to the user by DXREAD(). This can result in additional digits that 
C aren't meaningful; for example, when 4-byte floating-point values (seven
C decimal digits) are stored in the 8-byte return array (fifteen decimal 
C digits), the eight additional digits can be artifacts of floating point 
C representation. Calculations should take this into account and recognize 
C the original REAL*4 database values as having at most seven decimal digits 
C of precision.
C
C NOTE #3: The non-gravitational A1-A3, DT and related model parameters are as
C described in "Cometary Orbit Determination and Nongravitational Forces", 
C D.K. Yeomans, P.W. Chodas, G. Sitarski, S. Szutowicz, M Krolikowska, in 
C Comets II, University of Arizona Press (2004), pp. 137-151.
C
C Physically stored values REAL*8 (8-byte IEEE floating precision numeric)
C
C CODE avail   Label  Definition
C ---- ----- -------  ---------------------------------------------------------
C  801 ac/3+   EPOCH  Time of osc. orbital elements solution, JD (CT,TDB)
C  802 ac/3+  CALEPO  Time of osc. orbital elements solution, YYYYDDMM.ffff
C  803 ac/3+      MA  Mean anomaly at EPOCH, deg (elliptical & hyperbolic cases,
C                       "9.999999E99" if not available)
C  804 ac/3+       W  Argument of periapsis at EPOCH, J2000 ecliptic, deg.
C  805 ac/3+      OM  Longitude of ascending node at EPOCH, J2000 ecliptic,deg.
C  806 ac/3+      IN  Inclination angle at EPOCH wrt J2000 ecliptic, deg.
C  807 ac/3+      EC  Eccentricity at EPOCH
C  808 ac/3+       A  Semi-major axis at EPOCH, au
C  809 ac/3+      QR  Perihelion distance at EPOCH, au
C  810 ac/3+      TP  Perihelion date for QR at EPOCH, JD (CT,TDB)
C  811 ac/3+   TPCAL  Perihelion date for QR at EPOCH, format YYYYMMDD.fff
C  812 ac/5+  TPFRAC  Decimal (fractional) part of TP for extended precision
C  813 ac/4+  SOLDAT  Date orbit solution was performed, JD (CT,TDB)
C
C DERIVED from physically stored REAL*8 (8-byte IEEE floating precision numeric)
C
C CODE avail   Label  Definition
C ---- ----- -------  ---------------------------------------------------------
C  851 ac/3+   ADIST  Aphelion distance at EPOCH, au
C  852 ac/3+     PER  Sidereal orbit period @ EPOCH, Julian yr (365.25 d/Jul.yr)
C  853 ac/3+  ANGMOM  Specific angular momentum at EPOCH, au^2/D
C  854 ac/3+       N  Mean motion, deg/day (elliptical and hyperbolic cases,
C                       "9.999999E99" if not available)
C  855 ac/3+     DAN  Heliocentric distance of ascending node at EPOCH, au
C  856 ac/3+     DDN  Heliocentric distance of descending node at EPOCH, au
C  857 ac/3+       L  Ecliptic longitude of perihelion at EPOCH, deg.
C  858 ac/3+       B  Ecliptic latitude of perihelion at EPOCH, deg.
C
C Physically stored REAL*8 (8-byte IEEE floating precision numeric) 
C Requires multiple storage slots 
C
C CODE avail   Label  Definition
C ---- ----- -------  ---------------------------------------------------------
C  899 ac/4+ SRC(01)  Square root covariance vector. Vector-stored upper-
C                     triangular matrix with order {EC,QR,TP,OM,W,IN,{ESTL}}.
C                     Always reserve enough space (i.e., up to 55 slots) in 
C                     both request and output arrays to hold complete vector.
C
C                     SRC in matrix form (units are days, radians, au):
C
C                            EC    QR    TP    OM    W      IN  {ESTL}
C                        EC  e11   e12   e13   e14  e15    e16
C                        QR        e22   e23   e24  e25    e26
C                        TP              e33   e34  e35    e36
C                        OM                    e44  e45    e46
C                        W                          e55    e56
C                        IN                                e66
C                     {ESTL}
C 
C                     The SRC vector stores the matrix elements from row i and 
C                     column j (i<=j) in vector slot j*(j-1)/2 + i. i.e., upper 
C                     part only, stored by columns: 
C                           
C                              SRC(1,..)= e11,e12,e22,e13,e23 ...
C
C                     To obtain a covariance matrix, bottom-fill an SRC matrix
C                     with zeros to obtain matrix RI. Multiply RI by its
C                     transpose:
C
C                                           T
C                              COV = RI * RI
C
C Physically stored values REAL*4 (4-byte IEEE floating precision numeric)
C
C CODE avail  Label   Definition
C ---- ----- -------  ---------------------------------------------------------
C  401 ac/3+       H  Absolute visual magnitude (IAU H-G system) (99=unknown)
C  402 ac/3+       G  Mag. slope parm. (IAU H-G)(99=unknown & 0.15 not assumed) 
C  403  c/3+      M1  Total absolute magnitude, mag.
C  404  c/3+      M2  Nuclear absolute magnitue, mag.
C  405  c/4+      K1  Total absolute magnitude scaling factor
C  406  c/4+      K2  Nuclear absolute magnitude scaling factor
C  407  c/4+   PHCOF  Phase coefficient for K2= 5
C  408 ac/3+      A1  Non-grav. accel., radial component, [s:10^-8 au/day^2]
C  409 ac/3+      A2  Non-grav. accel., transverse component,[s:10^-8 au/day^2]
C  410 ac/4+      A3  Non-grav. accel., normal component, [s:10^-8 au/day^2]
C  411  c/4+      DT  Non-grav. lag/delay parameter, days
C  412 ac/5+      R0  Non-grav. model constant, normalizing distance, au
C  413 ac/5+     ALN  Non-grav. model constant, normalizing factor
C  414 ac/5+      NM  Non-grav. model constant, exponent m
C  415 ac/5+      NN  Non-grav. model constant, exponent n 
C  416 ac/5+      NK  Non-grav. model constant, exponent k
C  417  c/4+      S0  Center-of-light estimated offset at 1 au, km
C  418  c/5+     TCL  Center-of-light start-time offset, d since "ref.time"
C  419 a /5+     LGK  Surface thermal conductivity log_10(k), (W/m/K)
C  420 ac/5+     RHO  Bulk density, kg/m^3
C  421 ac/5+   AMRAT  Solar pressure model, area/mass ratio, m^2/kg 
C  422  c/5+     AJ1  Jet 1 acceleration, au/d^2 
C  423  c/5+     AJ2  Jet 2 acceleration, au/d^2
C  424  c/5+     ET1  Thrust angle, colatitude of jet 1, deg.
C  425  c/5+     ET2  Thrust angle, colatitude of jet 2, deg.
C  426  c/5+     DTH  Jet model diurnal lag angle, deg. (delta_theta)
C  427 ac/5+     ALF  Spin pole orientation, RA, deg.
C  428 ac/5+     DEL  Spin pole orientation, DEC, deg.
C  429 ac/5+  SPHLM3  Earth gravity sph. harm. model limit, Earth radii
C  430 ac/5+  SPHLM5  Jupiter grav. sph. harm. model limit, Jupiter radii
C  431 ac/3+      RP  Object rotational period, hrs
C  432 ac/3+      GM  Object mass parameter, km^3/s^2
C  433 ac/3+     RAD  Object mean radius, km
C  434 ac/5+  EXTNT1  Triaxial ellipsoid, axis 1/largest equat. extent, km
C  435 ac/5+  EXTNT2  Triaxial ellipsoid, axis 2/smallest equat. extent, km
C  436 ac/5+  EXTNT3  Triaxial ellipsoid, axis 3/polar extent, km
C  437 ac/4+    MOID  Earth MOID at EPOCH time, au; '99' if not computed
C  438 ac/3+  ALBEDO  Geometric visual albedo, 99 if unknown 
C  439 a /3+    BVCI  B-V color index, mag., 99 if unknown
C  440 a /5+    UBCI  U-B color index, mag., 99 if unknown
C  441 a /5+    IRCI  I-R color index, mag., 99 if unknown
C  442 ac/4+    RMSW  RMS of weighted optical residuals, arcsec
C  443 ac/5+    RMSU  RMS of unweighted optical residuals, arcsec
C  444 ac/5+    RMSN  RMS of normalized optical residuals
C  445 ac/5+   RMSNT  RMS of all normalized residuals
C  446 a /5+    RMSH  RMS of abs. visual magnitude (H) residuals, mag.
C  447  c/5+   RMSMT  RMS of MT estimate residuals, mag.
C  448  c/5+   RMSMN  RMS of MN estimate residuals, mag.
C
C Physically stored values INTEGER*4 (4-byte integer numeric)
C
C CODE avail   Label  Definition
C ---- ----- -------  ---------------------------------------------------------
C  201 ac/3+      NO  Logical record-number of this object in DASTCOM
C  202 ac/4+    NOBS  Number of observations of all types used in orbit soln.
C  203 ac/4+ OBSFRST  Start-date of observations used in fit, YYYYMMDD 
C  204 ac/4+ OBSLAST  Stop-date of observations used in fit, YYYYMMDD
C
C Physically stored values INTEGER*1 (1-byte signed integers)
C
C CODE avail   Label  Definition
C ---- ----- -------  ---------------------------------------------------------
C  101 ac/5+  PRELTV  Planet relativity "bit-switch" byte: bits 0-7 are set to
C                      1 if relativity for corresponding planet was computed, 
C                      0 if not. For example, if Earth & Jupiter, FORTRAN(95) 
C                      statement IBITS(PRELTV,J,1) should return 1 when J=2 or 
C                      J=4, but zero for every other J through 7. No provision
C                      for supporting Pluto relativity.
C  102 ac/5+  SPHMX3  Earth grav. model max. degree; 0=point-mass, 2= J2 only,
C                      3= up to J3 zonal, 22= 2x2 field, 33=3x3 field, etc.
C  103 ac/5+  SPHMX5  Jupiter grav. max. deg.; 0=point-mass, 2= J2 only, 
C                      3= up to J3 zonal, 22= 2x2 field, 33=3x3 field, etc.
C  104 ac/5+   JGSEP  Galilean satellites used as sep. perturbers; 0=no 1=yes
C  105 ac/5+  TWOBOD  Two-body orbit model flag; 0=no 1=yes
C  106 ac/5+   NSATS  Number of satellites; 99 if unknown.
C  107 ac/4+   UPARM  Orbit condition code; 99 if not computed
C  108 ac/4+    LSRC  Length of square-root cov. vector SRC (# elements used)
C   
C Physically stored values INTEGER*2 (2-byte integers)
C
C CODE avail   Label  Definition
C ---- ----- -------  ---------------------------------------------------------
C  151  c/3+    IPYR  Perihelion year (i.e., 1976, 2012, 2018, etc.)
C  152 ac/3+    NDEL  Number of radar delay measurements used in orbit soln.
C  153 ac/3+    NDOP  Number of radar Doppler measurements used in orbit soln.
C  154  c/5+  NOBSMT  Number of magnitude measurements used in total mag. soln.
C  155  c/5+  NOBSMN  Number of magnitude measurements used in nuc. mag. soln.
C  156  c/3+  COMNUM  IAU comet number (parsed from DESIG)
C 
C Physically stored character data return in CHOUT argument:
C 'length' is the maximum number of characters in the field
C 
C CODE avail  length  Label  Definition
C ---- ----- ------- ------  ---------------------------------------------------
C  001 ac/3+     4   EQUNOX  Equinox of orbital elements ('1950' or '2000')
C  002 ac/4+     6    PENAM  Planetary ephemeris ID/name
C  003 ac/3+    12    SBNAM  Small-body perturber ephemeris ID/name
C  004 a /3      5   SPTYPT  Tholen spectral type
C  005 a /4+     5   SPTYPS  SMASS-II spectral type
C  006 ac/3+     9     DARC  Data arc span (year-year, OR integer # of days)
C  007 a /3+    41   COMNT1  Asteroid comment line #1
C  008 a /3+    80   COMNT2  Asteroid comment line #2
C  009  c/3+    49   COMNT3  Comet comment line #1
C  010  c/3+    80   COMNT4  Comet comment line #2
C  011 ac/3+    13    DESIG  Object designation
C  012 ac/4+    14     ESTL  Dynamic parameter estimation list. Last symbol set
C                             to '+' if list is too long for field; check 
C                             object record comments field for full list.
C  013 ac/3+    10     IREF  Solution reference/ID/name
C  014 ac/3+    29     NAME  Object name
C
C** Define current database numeric quantity codes and label correspondences
C
C   New fields require additional entries in FLAB, FLABL, and FCODE 
C   lists below, with increments to RR8D, RR4D, RI4D, RI2D, RI1D, or NUMCH
C   parameter statements as appropriate. Other coding changes are necessary,
C   such as the physical read statements. 
C
C   New field codes MUST be inserted in proper numerical order (monotonically 
C   decreasing code list). For example, if a new REAL*8 field is installed in 
C   the physical database, it might be assigned a code of 814 and be placed 
C   in each of the three lists below, between 851 and 813. If it is a new 
C   derived REAL*8 parameter, it might be assigned the code 859 and placed in 
C   each of the three lists below between 899 and 858. 
C
C------------------------------------------------------------------------------
C Modification History:
C
C  DATE         Who  Change
C  -----------  ---  ----------------------------------------------------------
C  2013-Jul-15  JDG  Version 1.0
C  2014-Mar-28  JDG  Clarified wording in MA and N field descriptions
C
C Key:
C  JDG= Jon.D.Giorgini@jpl.nasa.gov
C
C Jet Propulsion Laboratory, NASA/California Institute of Technology,
C Solar System Dynamics Group, 4800 Oak Grove Drive, Pasadena, CA 91109 USA
C
C NOTICES:
C
C Copyright 2013, by the California Institute of Technology. ALL RIGHTS 
C RESERVED. United States Government Sponsorship acknowledged. Any commercial 
C use must be negotiated with the Office of Technology Transfer at the 
C California Institute of Technology.
C
C This software may be subject to U.S. export control laws. By accepting this
C software, the user agrees to comply with all applicable U.S. export laws and
C regulations. User has the responsibility to obtain export licenses, or other
C export authority as may be required before exporting such information to 
C foreign countries or providing access to foreign persons.
C------------------------------------------------------------------------------
C
      INTEGER       FCODE(NUMF)   ! Field code
      CHARACTER*8   FLAB(NUMF)    ! Field label
      INTEGER       FLABL(NUMF)   ! Length of field label
C
      DATA FLAB /
     &  'SRC(01)',
C FLABL|        7,
C FCODE|      899,
C
     &   'B',  'L', 'DDN','DAN',  'N','ANGMOM', 'PER', 'ADIST',
C FLABL|   1,    1,     3,    3,    1,       6,     3,       5,
C FCODE| 858,  857,   856,  855,  854,     853,   852,     851,
C
     &  'SOLDAT','TPFRAC', 'TPCAL',
C FLABL|       6,       6,       5,
C FCODE|     813,     812,     811,
C
     &  'TP','QR','A','EC','IN','OM', 'W','MA','CALEPO','EPOCH',
C FLABL|   2,   2,  1,   2,   2,   2,   1,   2,       6,      5,
C FCODE| 810, 809,808, 807, 806, 805, 804, 803,     802,    801,
C
     &  'RMSMN','RMSMT','RMSH','RMSNT','RMSN','RMSU','RMSW',
C FLABL|      5,      5,     4,      5,     4,     4,     4,
C FCODE|    448,    447,   446,    445,   444,   443,   442,
C
     &  'IRCI','UBCI','BVCI','ALBEDO','MOID','EXTNT3','EXTNT2','EXTNT1',
C FLABL|     4,     4,     4,       6,     4,       6,       6,       6,
C FCODE|   441,   440,   439,     438,   437,     436,     435,     434,
C
     &  'RAD','GM','RP','SPHLM5','SPHLM3','DEL','ALF','DTH','ET2','ET1',
C FLABL|    3,   2,   2,       6,       6,    3,    3,    3,    3,    3,
C FCODE|  433, 432, 431,     430,     429,  428,  427,  426,  425,  424,
C
     &  'AJ2','AJ1','AMRAT','RHO','LGK','TCL','S0','NK','NN','NM','ALN',
C FLABL|    3,    3,      5,    3,    3,    3,   2,   2,   2,   2,    3,
C FCODE|  423,  422,    421,  420,  419,  418, 417, 416, 415, 414,  413,
C
     &  'R0','DT', 'A3','A2','A1','PHCOF','K2','K1','M2','M1','G','H',
C FLABL|   2,   2,    2,   2,   2,      5,   2,   2,   2,   2,  1,  1,
C FCODE| 412, 411,  410, 409, 408,    407, 406, 405, 404, 403,402,401,
C
     &  'OBSLAST', 'OBSFRST', 'NOBS','NO',
C FLABL|        6,         6,     4,   2,
C FCODE|      204,       203,   202, 201,
C
     &  'COMNUM','NOBSMN','NOBSMT','NDOP','NDEL','IPYR',
C FLABL|       6,       6,       6,     4,     4,     4,
C FCODE|     156,     155,     154,   153,   152,   151,
C
     &  'LSRC','UPARM','NSATS','TWOBOD','JGSEP','SPHMX5','SPHMX3',
C FLABL|     4,      5,      5,       6,      5,       6,       6,
C FCODE|   108,    107,    106,     105,    104,     103,     102,
C
     &  'PRELTV',
C FLABL|       6,
C FCODE|     101,
C
     &  'NAME',   'IREF',
C FLABL|     4,        4,
C FCODE|    14,       13,
C
     &  'ESTL', 'DESIG', 'COMNT4', 'COMNT3', 'COMNT2', 'COMNT1',
C FLABL|     4,       5,        6,        6,        6,        6,
C FCODE|    12,      11,       10,        9,        8,        7,
C
     &  'DARC', 'SPTYPS', 'SPTYPT', 'SBNAM', 'PENAM', 'EQUNOX' /
C FLABL|     4,        6,        6,       5,       5,        6 /
C FCODE|     6,        5,        4,       3,       2,        1 /
C
C~~~~~
C
      DATA FLABL /
C FLAB |'SRC(01)',
     &          7,
C FCODE|      899,
C
C FLAB |'B',  'L', 'DDN','DAN',  'N','ANGMOM', 'PER', 'ADIST',
     &    1,    1,     3,    3,    1,       6,     3,       5,
C FCODE|858,  857,   856,  855,  854,     853,   852,     851,
C
C FLAB |'SOLDAT','TPFRAC', 'TPCAL',
     &         6,       6,       5,
C FCODE|     813,     812,     811,
C 
C FLAB |'TP','QR','A','EC','IN','OM', 'W','MA','CALEPO','EPOCH',
     &     2,   2,  1,   2,   2,   2,   1,   2,       6,      5,
C FCODE| 810, 809,808, 807, 806, 805, 804, 803,     802,    801,
C 
C FLAB |'RMSMN','RMSMT','RMSH','RMSNT','RMSN','RMSU','RMSW',
     &        5,      5,     4,      5,     4,     4,     4,
C FCODE|    448,    447,   446,    445,   444,   443,   442,
C
C FLAB |'IRCI','UBCI','BVCI','ALBEDO','MOID','EXTNT3','EXTNT2','EXTNT1',
     &       4,     4,     4,       6,     4,       6,       6,       6,
C FCODE|   441,   440,   439,     438,   437,     436,     435,     434,
C
C FLAB |'RAD','GM','RP','SPHLM5','SPHLM3','DEL','ALF','DTH','ET2','ET1',
     &      3,   2,   2,       6,       6,    3,    3,    3,    3,    3,
C FCODE|  433, 432, 431,     430,     429,  428,  427,  426,  425,  424,
C
C FLAB |'AJ2','AJ1','AMRAT','RHO','LGK','TCL','S0','NK','NN','NM','ALN',
     &      3,    3,      5,    3,    3,    3,   2,   2,   2,   2,    3,
C FCODE|  423,  422,    421,  420,  419,  418, 417, 416, 415, 414,  413,
C
C FLAB |'R0','DT', 'A3','A2','A1','PHCOF','K2','K1','M2','M1','G','H',
     &     2,   2,    2,   2,   2,      5,   2,   2,   2,   2,  1,  1,
C FCODE| 412, 411,  410, 409, 408,    407, 406, 405, 404, 403,402,401,
C
C FLAB |'OBSLAST', 'OBSFRST', 'NOBS','NO',
     &          6,         6,     4,   2,
C FCODE|      204,       203,   202, 201,
C
C FLAB |'COMNUM','NOBSMN','NOBSMT','NDOP','NDEL','IPYR',
     &         6,       6,       6,     4,     4,     4,
C FCODE|     156,     155,     154,   153,   152,   151,
C
C FLAB |'LSRC','UPARM','NSATS','TWOBOD','JGSEP','SPHMX5','SPHMX3',
     &       4,      5,      5,       6,      5,       6,       6,
C FCODE|   108,    107,    106,     105,    104,     103,     102,
C
C FLAB |'PRELTV',
     &         6,
C FCODE|     101,
C
C FLAB |'NAME',   'IREF',
     &       4,        4,
C FCODE|    14,       13,
C
C FLAB |'ESTL', 'DESIG', 'COMNT4', 'COMNT3', 'COMNT2', 'COMNT1',
     &       4,       5,        6,        6,        6,        6,
C FCODE|    12,      11,       10,        9,        8,        7,
C
C FLAB |'DARC', 'SPTYPS', 'SPTYPT', 'SBNAM', 'PENAM', 'EQUNOX' /
     &       4,        6,        6,       5,       5,        6 /
C FCODE|     6,        5,        4,       3,       2,        1 /
C
C~~~~~
C
      DATA FCODE /
C FLAB |'SRC(01)',
C FLABL|        7,
     &        899,
C
C FLAB | 'B',  'L', 'DDN','DAN',  'N','ANGMOM', 'PER', 'ADIST',
C FLABL|
     &   858,  857,   856,  855,  854,     853,   852,     851,
C
C FLAB |'SOLDAT','TPFRAC', 'TPCAL',
C FLABL|       6,       6,       5,
     &       813,     812,     811,
C
C FLAB |'TP','QR','A','EC','IN','OM', 'W','MA','CALEPO','EPOCH',
C FLABL|   2,   2,  1,   2,   2,   2,   1,   2,       6,      5,
     &   810, 809,808, 807, 806, 805, 804, 803,     802,    801,
C
C FLAB |'RMSMN','RMSMT','RMSH','RMSNT','RMSN','RMSU','RMSW',
C FLABL|      5,      5,     4,      5,     4,     4,     4,
     &      448,    447,   446,    445,   444,   443,   442,
C
C FLAB |'IRCI','UBCI','BVCI','ALBEDO','MOID','EXTNT3','EXTNT2','EXTNT1',
C FLABL|     4,     4,     4,       6,     4,       6,       6,       6,
     &     441,   440,   439,     438,   437,     436,     435,     434,
C
C FLAB |'RAD','GM','RP','SPHLM5','SPHLM3','DEL','ALF','DTH','ET2','ET1',
C FLABL|    3,   2,   2,       6,       6,    3,    3,    3,    3,    3,
     &    433, 432, 431,     430,     429,  428,  427,  426,  425,  424,
C
C FLAB |'AJ2','AJ1','AMRAT','RHO','LGK','TCL','S0','NK','NN','NM','ALN',
C FLABL|    3,    3,      5,    3,    3,    3,   2,   2,   2,   2,    3,
     &    423,  422,    421,  420,  419,  418, 417, 416, 415, 414,  413,
C
C FLAB |'R0','DT', 'A3','A2','A1','PHCOF','K2','K1','M2','M1','G','H',
C FLABL|   2,   2,    2,   2,   2,      5,   2,   2,   2,   2,  1,  1,
     &   412, 411,  410, 409, 408,    407, 406, 405, 404, 403,402,401,
C
C FLAB |'OBSLAST', 'OBSFRST', 'NOBS','NO',
C FLABL|        6,         6,     4,   2,
     &        204,       203,   202, 201,
C
C FLAB |'COMNUM','NOBSMN','NOBSMT','NDOP','NDEL','IPYR',
C FLABL|       6,       6,       6,     4,     4,     4,
     &       156,     155,     154,   153,   152,   151,
C
C FLAB |'LSRC','UPARM','NSATS','TWOBOD','JGSEP','SPHMX5','SPHMX3',
C FLABL|     4,      5,      5,       6,      5,       6,       6,
     &     108,    107,    106,     105,    104,     103,     102,
C
C FLAB |'PRELTV',
C FLABL|       6,
     &       101,
C
C FLAB |'NAME',   'IREF',
C FLABL|     4,        4,
     &      14,       13,
C
C FLAB |'ESTL', 'DESIG', 'COMNT4', 'COMNT3', 'COMNT2', 'COMNT1',
C FLABL|     4,       5,        6,        6,        6,        6,
     &      12,      11,       10,        9,        8,        7,
C
C FLAB |'DARC', 'SPTYPS', 'SPTYPT', 'SBNAM', 'PENAM', 'EQUNOX' /
C FLABL|     4,        6,        6,       5,       5,        6 /
     &       6,        5,        4,       3,       2,        1 /
C
