      SUBROUTINE D3READ( IOBJ, R8P, CP, LSRC, STAT )
      IMPLICIT NONE
      REAL*8       R8P(99)
      CHARACTER*1  CP*217
      INTEGER      IOBJ, LSRC, STAT
C
C     ENTRY D3INIT( FNAM, IU, XTRA, SPACE, TYP, IBND, BIAS, ISTAT )
      CHARACTER*1  FNAM*256, TYP
      LOGICAL      SPACE, XTRA
      INTEGER      IU, ISTAT, IBND(6), BIAS(3)
C
C     ENTRY D3EZIN( FNAM, IU, ISTAT )
C     CHARACTER*1  FNAM*256
C     INTEGER      IU, ISTAT
C
C     ENTRY D3ERMS( CERRMS )
      CHARACTER*80 CERRMS
C   
C     ENTRY D3CLOS
C
C------------------------------------------------------------------------------
C D3READ -- Read & return asteroid/comet parameters from DASTCOM3 database. 
C           Subroutine D3INIT (or D3EZIN) must be called prior to D3READ to 
C           initialize subroutine. Check STAT error code on return.
C
C           Orbital elements returned are heliocentric J2000-ecliptic.
C           osculating elements, with epoch in coordinate time ("ET", "TDB").
C           As of 2001, elements from the Minor Planet Center labelled
C           "TDT" are actually also in coordinate time despite label.
C
C           Subroutine detects and uses two types of DASTCOM3 databases:
C
C              Type 'A':  Standard DASTCOM3 database
C              Type 'C':  Expanded covariance format DASTCOM3
C
C           There are three groupings of records within DASTCOM3. The first
C           are numbered asteroids. The second, unnumbered asteroids.
C           The third are comets. Physical records numbers are offset 
C           from logical record numbers by bias parameters defined in 
C           the file header (REC=1).
C
C           A separate index (MI.DB) allows one to look-up the logical 
C           record numbers of some object by designation. D3READ will 
C           translate that into a physical record number and retrieve 
C           the data. 
C
C Inputs:
C   IOBJ  --  DASTCOM3 object logical record number.
C                numbered asteroids   : IOBJ = asteroid number
C                un-numbered asteroids: IOBJ >= BND(5), IOBJ < BND(2)
C                comets               : IOBJ >= BND(6), IOBJ < BND(3)
C
C            ... where BND() is returned by the D3INIT initialization call.
C
C Output:
C   R8P() -- Double precision array of extracted values. A brief map to 
C              the return array is below. Comet record if IOBJ >= BND(6).
C              See assignment statements in code for details of parameter.
C
C             DIRECT quantities:
C
C                           Asteroid                    Comet
C                           --------                    -----
C                 R8P(1)    A                           A
C                 R8P(2)    E                           E
C                 R8P(3)    RI                          RI
C                 R8P(4)    OM                          OM
C                 R8P(5)    RNODE                       RNODE
C                 R8P(6)    RMO                         RMO
C                 R8P(7)    EPOCH                       EPOCH
C                 R8P(8)    CALEPO                      CALEPO
C                 R8P(9)    RAD                         RAD
C                 R8P(10)   GM                          GM
C                 R8P(11)   Q                           Q
C                 R8P(12)   BVT                         M1
C                 R8P(13)   H                           M2
C                 R8P(14)   ALBEDO                      A1
C                 R8P(15)   G                           A2
C                                                       A3    [ R8P(39) ]
C                                                       DT    [ R8P(36) ]
C                 R8P(16)   RP                          PYR
C                 R8P(17)   PERJD                       PERJD
C                 R8P(18)   PERCAL                      PERCAL
C
C             DERIVED/PARSED (filled if D3INIT call XTRA=.TRUE., else 0.D0):
C
C                           Asteroid                    Comet
C                           --------                    -----
C                 R8P(19)   ADIST                       ADIST
C                 R8P(20)   PER                         PER   
C                 R8P(21)   ANGMOM                      ANGMOM
C                 R8P(22)   N                           N
C                 R8P(23)   DAN                         DAN
C                 R8P(24)   DDN                         DDN
C                 R8P(25)   L                           L
C                 R8P(26)   B                           B
C                 R8P(27)   NOBS                        NOBS
C                 R8P(28)   NO                          NO
C                 R8P(29)   0.D0                        K1
C                 R8P(30)   0.D0                        K2
C                 R8P(31)   0.D0                        PHCOF
C                 R8P(32)   0.D0                        COMNUM
C                 R8P(33)   NDEL                        NDEL
C                 R8P(34)   NDOP                        NDOP
C               * R8P(35)   NSCT                      * NSCT
C                 R8P(36)   0.D0                        DT
C                 R8P(37)   0.D0                        S0
C
C             Covariance DASTCOM3 quantities (fill if type 'C' database):
C
C                           Asteroid                    Comet
C                           --------                    -----
C                 R8P(38)   RMSW                        RMSW
C                 R8P(39)   RMSH                        A3
C                 R8P(40)   MOID                        MOID
C                 R8P(41)   UPARM                       UPARM
C                 R8P(42)   OBSTRT                      OBSTRT
C                 R8P(43)   OBSTOP                      OBSTOP
C                 R8P(44)   SOLDAT                      SOLDAT
C                 R8P(45)   SRC(1)                      SRC(1)
C                     .         .                         .
C                 R8P(65)   SRC(21)                       .
C               * R8P(66)   OQP                           .
C               * R8P(67)   CEU                           .
C               * R8P(68)   SIG1                          .
C               * R8P(69)   LEAK1                         .
C               * R8P(70)   SLEAK1                        .
C               * R8P(71)   LEAK5                         .
C               * R8P(72)   SLEAK5                        .
C                 R8P(73)   0.D0                          .
C                     .      .                            .
C                 R8P(99)   0.D0                        SRC(55)
C
C            * NOTES: 
C                - Asteroid positions 66-72 assigned for possible 
C                   future use only. 
C                - Ast/com position 35 (# of spacecraft obs) assigned
C                   for possible future use only.
C
C   CP   --  Character parameters. Packed string. Parse return variable 
C            as follows (comet record if IOBJ >= BND(6)):
C
C                 Asteroid                    Comet
C                 --------                    -----
C                 NAME  = CP(1:24)            NAME  = CP(1:24)
C                 DESIG = CP(25:37)           DESIG = CP(25:37)
C                 IREF  = CP(38:45)           IREF  = CP(38:45)
C                 EQUNOX= CP(46:49)           EQUNOX= CP(46:49)
C                 SPTYPE= CP(50:55)           DARC  = CP(56:64)
C                 DARC  = CP(56:64)           COMNT3= CP(65:113)
C                 COMNT1= CP(65:105)          COMNT2= CP(114:193)
C                 COMNT2= CP(106:185)       * SBNAM = CP(194:205)
C               * SBNAM = CP(186:197)       * PENAM = CP(206:211)  
C               * PENAM = CP(198:203)         EST   = CP(212:217)
C
C            * Notes: 
C                  - SBNAM (perturbation marker/file) MAY not be filled 
C                     for non-JPL solutions in type 'A' DASTCOM3. 
C                  - PENAM is NOT filled in type 'A' DASTCOM3.
C                  - PENAM and SBNAM are both filled for type 'C'
C                     covariance DASTCOM3.
C
C   LSRC --  Length of SRC (covariance) vector. LSRC= 0 if no SRC.
C
C   STAT --  Status (check value on subroutine return); 
C             -3 = Failure to call D3INIT initialization.
C             -4 = IOBJ record out of bounds or place-holder record. 
C             -6 = Hyperobla's semi-major axis, a, is positive.
C              0 = OK
C             >0 = IOSTAT error code.
C
C Modification History:
C
C  DATE         Who   Change 
C  -----------  ----  ---------------------------------------------------------
C  2001-Jan-26  JDG   Version 1.0 written.
C  2001-Feb-06  JDG   BIAS() array return added to D3INIT.
C  2001-Feb-14  JDG   Added D3EZIN entry point.
C  2001-Feb-20  JDG   Added STAT=-6 error code, removed dupl. declaration.
C  2001-Aug-14  JDG   Added support for PC pre-header, new D3BND entry point.
C  2003-Oct-02  JDG   Added A3 to database, DT gets separate field.
C  2003-Oct-13  JDG   Change criteria for placeholder record.
C  2004-Jan-14  JDG   Fixed request-within-zones bound check.
C  2004-Jun-28  JDG   Change parabolic mean motion (n) to 0 deg/day.
C  2007-Jan-11  JDG   Add support for EST list in DASTCOM3_C (from ORB file)
C
C Key:
C  JDG= Jon.Giorgini@jpl.nasa.gov
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
C D3INIT -- Open DASTCOM3, initialize subroutine (D3EZIN is alternate).
C 
C Inputs:
C   FNAM   -- File name (256 characters max).
C   IU     -- Unit number to assign database file. IU must be >= 10.
C   XTRA   -- Logical flag requesting additional (derived)  quantities.
C   SPACE  -- Logical flag; .TRUE. = change NULL symbols to SPACE.
C                           .FALSE.= no change.
C   
C Outputs:
C   TYP    -- File type code:
C               'A' denotes standard DASTCOM3   (395 byte record length)
C               'C' denotes expanded DASTCOM3_C (823 byte record length) 
C
C   BND()  -- DASTCOM3 zone boundaries.
C               BND(1)  = Last numbered asteroid record
C               BND(2)  = Last unnumbered asteroid record
C               BND(3)  = Last comet record
C               BND(4)  = First numbered asteroid record
C               BND(5)  = First unnumbered asteroid record
C               BND(6)  = First comet record
C
C   BIAS() -- Record bias parameters. Physical_record = IOBJ + BIAS(x)
C               BIAS(1) = numbered asteroids,    IOBJ>=BND(4), IOBJ<BND(1) 
C               BIAS(2) = un-numbered asteroids, IOBJ>=BND(5), IOBJ<BND(2)
C               BIAS(3) = comets,                IOBJ>=BND(6), IOBJ<BND(3)
C
C   ISTAT  -- Integer error code (check value on subroutine return):
C               >0      = (IOSTAT open/read error code).
C                0      = OK,
C               -1      = Previous file not closed by calling D3CLOS.
C               -2      = Unknown file type.
C               -5      = Input unit number (IU) less than 10, cannot open.
C
C------------------------------------------------------------------------------
C
C D3EZIN -- Open DASTCOM3, initialize subroutine. Alternative to D3INIT
C  for those who don't need data about file structure returned. Returns
C  all derived/parsed quantities (XTRA= .TRUE.), converts nulls to spaces 
C  (SPACE= .TRUE.).
C 
C Inputs:
C   FNAM   -- File name (256 characters max).
C   IU     -- Unit number to assign database file. IU must be >= 10.
C   
C Outputs:
C   ISTAT  -- Integer error code (check value on subroutine return):
C               >0      = (IOSTAT open/read error code).
C                0      = OK,
C               -1      = Previous file not closed by calling D3CLOS.
C               -2      = Unknown file type.
C               -5      = Input unit number (IU) less than 10, cannot open.
C------------------------------------------------------------------------------
C
C D3BND --   Retrieve object category bounds. DASTCOM3 must have been 
C  been previously initialized by calling D3INIT or D3EZIN.
C
C Inputs:
C  None
C
C Outputs:
C  BND(1..6) -- Array of object boundary records (last/first).
C
C------------------------------------------------------------------------------
C
C D3ERMS -- Retrieved text of most recent error message in D3READ routine.
C
C Inputs:
C   None
C
C Outputs:
C   ERRMSG -- Text of last error message.
C
C------------------------------------------------------------------------------
C
C D3CLOS -- Close file, release unit number. 
C
C------------------------------------------------------------------------------
C
C** Declare local variables
      REAL*8       A32, C1, C2, C3, C4, C5, C6, EDELT
      INTEGER      MTS, ISUB, I, J, BND(6)
      INTEGER      L1, U, J0, J1, J2, IOS
      LOGICAL      D3INI, LEXTR, LSPACE
      CHARACTER*80 ERRMS
C
C** Declare database header record unique variables.
      INTEGER      IREC, IBIAS0, IBIAS1, IBIAS2
      CHARACTER*1  BEGINP*18, ENDPT*18, FILLER*105
C
C** Declare database record variables.
      INTEGER      NO, NOBS, UPARM
      REAL*8       EPOCH, CALEPO, RMO, RNODE, OM, RI, E, A, PERJD
      REAL*8       PERCAL, Q, OBSTRT, OBSTOP, SOLDAT, SRC(55)
      REAL*8       SIG1, LEAK1, SLEAK1, LEAK5, SLEAK5, PYR
      REAL         GM, RAD, H, G, BVT, RP, ALBEDO, RMSW, RMSH, MOID
      REAL         OQP, CEU, A1, A2, A3, DT, M1, M2
      CHARACTER*1  ASTNAM*18, EQUNOX*4, IREF*8, SPTYPE*6, FTYP
      CHARACTER*1  DARC*9, COMNT1*41, COMNT2*80, SBNAM*12, PENAM*6
      CHARACTER*1  COMNAM*24, COMNT3*49, NAME*24, DESIG*13, EST*6
C
C** Declare parameters; SGM10 is SQRT(GM of Sun in AU^3/d^2).
      REAL*8       TWOPI, SGM10, PP, DPR
      PARAMETER(TWOPI= 6.2831853071795862D+00)
      PARAMETER(SGM10= 1.7202098950000D-02 )
      PARAMETER(PP   = TWOPI / SGM10 )
      PARAMETER(DPR  = 360.D0 / TWOPI ) 
C
C** Declare local parameters.
      INTEGER       RECLA, RECLC
      PARAMETER( RECLA= 395 )   !DASTCOM3   record length
      PARAMETER( RECLC= 823 )   !DASTCOM3_C record length
C
C** Initialize flag. 
      DATA  D3INI   / .FALSE. /
C
C** Define some small and large numbers. 
      REAL*8 RMAX
      DATA   RMAX   / 9.999999D+99  /
C
C** Set tolerance for conic discrimination.
      REAL*8 TOL
      DATA   TOL    / 1.D-8 /
      SAVE   U, BND, IBIAS0, IBIAS1, IBIAS2, LSPACE, LEXTR, FTYP, ERRMS
C
C** Check for proper call to D3INIT.
      IF ( .NOT. D3INI ) THEN
       STAT= -3
       ERRMS= 'D3READ: database not initialized with call to D3INIT.' 
       RETURN
      END IF
C
C** Check IOBJ for quick return cases. If one is doing a top-to-bottom
C** scan of DASTCOM without regard to boundary records, this error only
C** indicates an interstitial record, not a fatal error.
      STAT= -4
      ERRMS= 'D3READ: requested IOBJ out of bounds, no action taken.' 
      IF ( (IOBJ .LE. 0) .OR. (IOBJ .GT. BND(3)) ) RETURN
      IF ( (IOBJ .GT. BND(1)) .AND. (IOBJ .LT. BND(5))) RETURN
      IF ( (IOBJ .GT. BND(2)) .AND. (IOBJ .LT. BND(6))) RETURN
C
C** Clear possible previous assignments and set default return values.
      ERRMS = ' '
      CP    = ' '
      NAME  = ' '
      SBNAM = ' '
      PENAM = ' '
      RMSW  = 0.0
      RMSH  = 0.0
      MOID  = 0.0
      UPARM = 0
      LSRC  = 0
      OBSTRT= 0.D0
      OBSTOP= 0.D0
      SOLDAT= 0.D0
      OQP   = 0.0
      CEU   = 0.0
      SIG1  = 0.D0
      LEAK1 = 0.D0
      SLEAK1= 0.D0
      LEAK5 = 0.D0
      SLEAK5= 0.D0
      DO I= 1,55
       SRC(I)= 0.D0
       R8P(I)= 0.D0
      END DO
      DO I= 56,99
       R8P(I)= 0.D0
      END DO
C
C** Set record bias.
      IF ( IOBJ .GE. BND(6) ) THEN
       ISUB= IBIAS2   !Comets w/apparitions
      ELSE IF ( IOBJ .GE. BND(5) ) THEN
       ISUB= IBIAS1   !Unnumbered asteroid
      ELSE IF ( IOBJ .GE. BND(4) ) THEN
       ISUB= IBIAS0   !Numbered asteroid
      END IF
      L1= IOBJ + ISUB
C
C** Process request for COMET record. 
      IF (IOBJ .GE. BND(6)) THEN
       IF ( FTYP .EQ. 'A' ) THEN
        READ( U, REC=L1, IOSTAT=STAT) 
     +        NO,COMNAM,EPOCH,CALEPO,EQUNOX,IREF,DESIG,
     +        RMO,RNODE,OM,RI,E,A,
     +        PERJD,PERCAL,Q,GM,RAD,A1,A2,
     +        M1,M2,PYR,DARC,COMNT3,COMNT2,
     +        SBNAM
       ELSE
        READ( U, REC=L1, IOSTAT=STAT) 
     +        NO,COMNAM,EPOCH,CALEPO,EQUNOX,IREF,DESIG,
     +        RMO,RNODE,OM,RI,E,A,
     +        PERJD,PERCAL,Q,GM,RAD,A1,A2,
     +        M1, M2, PYR, DARC, COMNT3, COMNT2,
     +        SBNAM, PENAM, RMSW, MOID, UPARM,
     +        OBSTRT, OBSTOP, SOLDAT, A3, DT, EST,
     +        LSRC, (SRC(I),I=1,55)
       END IF
       IF ( STAT .NE. 0 ) THEN 
        WRITE(ERRMS,*) 
     +   'D3READ: IOSTAT error code ',STAT,' reading comet record:',IOBJ
        RETURN
       END IF
C 
C** Set 'NULL' characters to 'SPACE', if requested.
       IF ( LSPACE ) THEN
        COMNAM(1:1)= ' '
        IF ( ICHAR(COMNT3(1:1)) .EQ. 0 ) COMNT3= ' ' 
        IF ( ICHAR(COMNT2(1:1)) .EQ. 0 ) COMNT2= ' ' 
        DO MTS= 1,9
         IF ( ICHAR(DARC(MTS:MTS)) .EQ. 0 ) DARC(MTS:MTS)= ' '
        END DO  
       END IF
C 
C** Return zeros if place-holder record.
C      IF (((INDEX(COMNAM,'omet' ).NE.0) .AND. 
C    +      (INDEX(COMNAM,'ecord').NE.0)) .OR.
C    +      (INDEX(COMNAM,'NUMBERED').NE.0)) THEN
       IF ((EPOCH.EQ.0.D0) .OR.(EPOCH.EQ.1.D0).OR.
     +     (CALEPO.EQ.0.D0).OR.(Q.EQ.0.D0)   ) THEN
        STAT= -4
        ERRMS= 'D3READ: comet place-holder record (non-fatal).'
        RETURN
       END IF
C
C** Assign double-precision values.
       R8P(1) = A               !Semi-major axis, AU
       R8P(2) = E               !Eccentricity
       R8P(3) = RI              !Inclination, DEG
       R8P(4) = OM              !Long. of Ascending Node, DEG
       R8P(5) = RNODE           !Argument of Perifocus, DEG
       R8P(6) = RMO             !Mean anomaly, DEG
       R8P(7) = EPOCH           !Julian date of osc. elements
       R8P(8) = CALEPO          !Epoch calendar date; YYYYMMDD.ffff
       R8P(9) = RAD             !Object radius, km
       R8P(10)= GM              !Mass parameter, km^3/s^2
       R8P(11)= Q               !Perihelion dist, AU
       R8P(12)= M1              !Total abs. mag., M1
       R8P(13)= M2              !Nuclear abs. mag, M2
       R8P(14)= A1 * 1.D-8      !Non-grav (radial)    , units:10^-8 AU/d^2
       R8P(15)= A2 * 1.D-8      !Non-grav (transverse), units:10^-8 AU/d^2
       R8P(39)= A3 * 1.D-8      !Non-grav (normal)    , units:10^-8 AU/d^2
       R8P(36)= DT              !Delay of peak        , units: days
       R8P(16)= PYR             !Perihelion year (DASTCOM3), YYYY.00000
       R8P(17)= PERJD           !Perihelion Julian date
       R8P(18)= PERCAL          !Perihelion calendar date; YYYYMMDD.ffff 
C
C** Assign character variables.
       NAME       = COMNAM
       CP(1:24)   = NAME        !Object name
       CP(25:37)  = DESIG       !Object designation
       CP(38:45)  = IREF        !Orbit solution name
       CP(46:49)  = EQUNOX      !Equinox; '1950' or '2000'
       CP(50:55)  = ' '         !(null; SPTYPE placeholder)
       CP(56:64)  = DARC        !Data arc (span)
       CP(65:113) = COMNT3      !Comments, comet
       CP(114:193)= COMNT2      !Comments, comet
       CP(194:205)= SBNAM       !Small-body perturber flag/file.
       CP(206:211)= PENAM       !Planetary ephemeris model.
       CP(212:217)= EST         !Non-grav estimation list
C
C** Assign derived "extra"/optional quantities.
       IF ( LEXTR ) THEN
        C1     = Q * ( 1.D0 + E )                 !  P, semi-latus rec, AU 
        C2     = R8P(5) / DPR                     !  APF in radians
        C3     = COS( C2 ) 
        C4     = SIN( C2 )
        C5     = E * C3                           !  E * COS(APF)
        C6     = RI / DPR                         !  Inclination in radians
        R8P(21)= SQRT( C1 ) * SGM10               !Spec. ang. mom., AU^2/D
        R8P(23)= C1 / ( 1.D0 + C5 )               !Helio Dist asc. node, AU
        R8P(24)= C1 / ( 1.D0 - C5 )               !Helio Dist des. node, AU
        R8P(25)= R8P(4)+ATAN2(C4*COS(C6),C3)*DPR  !Eclip. long. peri, deg.
        R8P(25)= MOD( R8P(25)+360.D0, 360.D0 )    ! (correct angle range)
        R8P(26)= ASIN( C4 * SIN(C6) )*DPR         !Eclip. lat. peri, deg.
C
C** Set conic type criteria.
        EDELT= 1.D0 - E
        IF (DABS(EDELT) .GE. TOL) THEN ! Non-parabolic orbit detected ...
         IF ( EDELT .GT. 0.D0 ) THEN  !   Elliptical/circular
          A32    =  A ** 1.5D0   
          R8P(19)= 2.D0 * A - Q                   !Aphelion distance
          R8P(20)= PP * A32 / 365.25              !Period, Julian yrs
         ELSE                         !   Hyperbolic
          STAT= -6
          IF ( A .GE. 0.D0 ) RETURN
          A32    = (-A) ** 1.5D0
          R8P(19)= RMAX                           !Aphelion distance
          R8P(20)= RMAX                           !Period, Julian yrs
         END IF
         R8P(22)= (SGM10 / A32) * DPR             !Mean motion (n), deg/day
        ELSE                           !   Parabolic
         R8P(19)= RMAX                            !Aphelion distance
         R8P(20)= RMAX                            !Period, Julian yrs
         R8P(22)= 0.0D0                           !Parabolic mean motion (n)
        END IF
C
C** For comets, first symbols of COMNT3 can be integer number of obs.
        READ( COMNT3, *, IOSTAT=IOS ) NOBS
        IF ( IOS .NE. 0 ) NOBS= 0
C
C** Store other important values in returned array.
        R8P(27)= NOBS                             !Number obs. in soln.
        R8P(28)= NO                               !Record number
C
C** Interpreted quantities (comets only). If read error, returned as zero.
        J0= INDEX( CP, 'k1=' )
        J1= INDEX( CP, 'k2=' )
        J2= INDEX( CP, 'phase coef.=' )
        IF ( J0 .NE. 0 ) READ(CP(J0+3:), *, IOSTAT=IOS) R8P(29) !Store k1
        IF ( J1 .NE. 0 ) READ(CP(J1+3:), *, IOSTAT=IOS) R8P(30) !Store k2
        IF ( J2 .NE. 0 ) READ(CP(J2+12:),*, IOSTAT=IOS) R8P(31) !phase coef
C        
C** Determine comet number, if possible (return 0 if failure).
        I= 0
        READ( DESIG, *, IOSTAT= IOS ) I
        R8P(32)= I
C
C** Extract radar astrometry counts, if any.
        J0= INDEX( CP, 'adar(' ) 
        IF ( J0 .NE. 0 ) THEN
         I= 0
         READ( CP(J0+5:), *, IOSTAT=IOS ) I
         R8P(33)= I                       !Number of delay measurements.
         I= 0
         J0= INDEX( CP, 'Dop' ) - 3
         READ( CP(J0:), *, IOSTAT=IOS ) I 
         R8P(34)= I                       !Number of Doppler measurements.
        END IF
C
C** Extract comet parameter DT, if present.
C       J0= INDEX( CP, 'DT=' )
C       IF ( J0 .NE. 0 ) READ( CP(J0+3:), *, IOSTAT= IOS ) R8P(36)
C   
C** Extract comet parameter S0, if present.
        J0= INDEX( CP, 'S0=' )
        IF ( J0 .NE. 0 ) READ( CP(J0+3:), *, IOSTAT= IOS ) R8P(37)
       END IF
C
C** Store additional parameters if covariance DASTCOM3 file.
       IF ( FTYP .EQ. 'C' ) THEN
        R8P(38)= RMSW
        R8P(40)= MOID
        R8P(41)= UPARM
        R8P(42)= OBSTRT
        R8P(43)= OBSTOP
        R8P(44)= SOLDAT
        DO I= 1, LSRC
         R8P(44+I)= SRC(I)
        END DO
       END IF
C
C------------------------------------------------------------------------------
C** End of COMET RECORD PROCESSING
C------------------------------------------------------------------------------
C
      ELSE
C
C** Process request for ASTEROID record.
       IF ( FTYP .EQ. 'A' ) THEN
        READ( U, REC=L1, IOSTAT= STAT )
     +       NO,ASTNAM,EPOCH,CALEPO,EQUNOX,IREF,DESIG,
     +       RMO,RNODE,OM,RI,E,A,
     +       PERJD,PERCAL,Q,GM,RAD,H,G,BVT,
     +       RP,ALBEDO,SPTYPE,DARC,NOBS,COMNT1,COMNT2,
     +       SBNAM
       ELSE 
         READ( U, REC=L1, IOSTAT=STAT ) 
     +       NO,ASTNAM,EPOCH,CALEPO,EQUNOX,IREF,DESIG,
     +       RMO,RNODE,OM,RI,E,A,
     +       PERJD,PERCAL,Q,GM,RAD,H,G,BVT,
     +       RP,ALBEDO,SPTYPE,DARC,NOBS,COMNT1,COMNT2,
     +       SBNAM,PENAM,RMSW,RMSH,MOID,UPARM,
     +       OBSTRT,OBSTOP,SOLDAT,
     +       LSRC,(SRC(I),I=1,21),
     +       OQP,CEU,SIG1,LEAK1,SLEAK1,LEAK5,SLEAK5
       END IF
       IF ( STAT .NE. 0 ) THEN 
        WRITE(ERRMS,*) 'D3READ: IOSTAT error code ',STAT,
     +                 ' reading asteroid record ',IOBJ 
        RETURN
       END IF
 
C** Set all 'NULL' characters to 'SPACE'.
       IF ( LSPACE ) THEN
        ASTNAM(1:1)= ' '
        IF ( ICHAR(COMNT1(1:1)) .EQ. 0 ) COMNT1= ' '
        IF ( ICHAR(COMNT2(1:1)) .EQ. 0 ) COMNT2= ' ' 
        DO 30 MTS= 1,9
         IF ( ICHAR(DARC(MTS:MTS)) .EQ. 0 ) DARC(MTS:MTS)= ' '
         IF ( MTS .GT. 6) GOTO 30
         IF ( ICHAR(SPTYPE(MTS:MTS)) .EQ. 0 ) SPTYPE(MTS:MTS)= ' '
 30     CONTINUE
       END IF
C
C** Return zeros if place-holder record.
C      IF (((INDEX(ASTNAM,'steroid' ).NE.0) .AND. 
C    +      (INDEX(ASTNAM,'ecord'   ).NE.0)) .OR.
C    +      (INDEX(ASTNAM,'NUMBERED').NE.0)) THEN
       IF ((EPOCH.EQ.0.D0) .OR.(EPOCH.EQ.1.D0).OR.
     +     (CALEPO.EQ.0.D0).OR.(Q.EQ.0.D0)   ) THEN
        STAT= -4
        ERRMS= 'D3READ: asteroid place-holder record (non-fatal).'
        RETURN
       END IF
C
C** Assign double-precision variables.
       R8P(1) = A               !Semi-major axis, AU
       R8P(2) = E               !Eccentricity
       R8P(3) = RI              !Inclination, DEG
       R8P(4) = OM              !Long. of Ascending Node, DEG
       R8P(5) = RNODE           !Argument of Perifocus, DEG
       R8P(6) = RMO             !Mean anomaly, DEG
       R8P(7) = EPOCH           !Julian date of osc. elements
       R8P(8) = CALEPO          !Epoch calendar date; YYYYDDMM.ffff
       R8P(9) = RAD             !Object radius, km
       R8P(10)= GM              !Mass parameter, km^3/s^2
       R8P(11)= Q               !Perihelion dist, AU
       R8P(12)= BVT             !Color index (b-v)  
       R8P(13)= H               !Abs. vis. mag., H
       R8P(14)= ALBEDO          !Albedo
       R8P(15)= G               !Slope parameter, G
       R8P(16)= RP              !Rotational period, hrs
       R8P(17)= PERJD           !Perihelion Julian date
       R8P(18)= PERCAL          !Perihelion calendar date; YYYYMMDD.ffff 
C
C** Assign character variables.
       NAME       = ASTNAM
       CP(1:24)   = NAME        !Object name
       CP(25:37)  = DESIG       !Object designation
       CP(38:45)  = IREF        !Orbit solution name
       CP(46:49)  = EQUNOX      !Equinox; '1950' or '2000'
       CP(50:55)  = SPTYPE      !Spectral type
       CP(56:64)  = DARC        !Data arc (span)
       CP(65:105) = COMNT1      !Comments, asteroid
       CP(106:185)= COMNT2      !Comments, asteroid
       CP(186:197)= SBNAM       !Small-body perturber flag/file.
       CP(198:203)= PENAM       !Planetary ephemeris model.
C
C** Assign derived "extra"/optional quantities.
       IF ( LEXTR ) THEN
        C1     = Q * ( 1.D0 + E )                 !P, semi-latus rec, AU 
        C2     = R8P(5) / DPR                     !APF in radians
        C3     = COS( C2 ) 
        C4     = SIN( C2 )
        C5     = E * C3                           !E * COS(APF)
        C6     = RI / DPR                         !Inclination, radians
        R8P(21)= SQRT( C1 ) * SGM10               !Spec. ang. mom., AU^2/D
        R8P(23)= C1 / ( 1.D0 + C5 )               !Helio Dist asc. node, AU
        R8P(24)= C1 / ( 1.D0 - C5 )               !Helio Dist des. node, AU
        R8P(25)= R8P(4)+ATAN2(C4*COS(C6),C3)*DPR  !Eclip. long. peri, deg.
        R8P(25)= MOD( R8P(25)+360.D0, 360.D0 )    ! (correct angle range)
        R8P(26)= ASIN( C4 * SIN(C6) )*DPR         !Eclip. lat. peri, deg.
C
C** Set conic type criteria.
        EDELT= 1.D0 - E
        IF (DABS(EDELT) .GE. TOL) THEN ! Non-parabolic orbit detected ...
         IF ( EDELT .GT. 0.D0 ) THEN  !   Elliptical/circular
          A32    = A ** 1.5D0   
          R8P(19)= 2.D0 * A - Q                   !Aphelion distance
          R8P(20)= PP * A32 / 365.25D0            !Period, Julian yrs
         ELSE                         !   Hyperbolic
          STAT= -6
          IF ( A .GE. 0.D0 ) RETURN
          A32    = (-A) ** 1.5D0
          R8P(19)= RMAX                           !Aphelion distance
          R8P(20)= RMAX                           !Period, Julian yrs
         END IF
         R8P(22)= (SGM10 / A32) * DPR             !Mean motion (n), deg/day
        ELSE                           !   Parabolic
         R8P(19)= RMAX                            !Aphelion distance
         R8P(20)= RMAX                            !Period, Julian yrs
         R8P(22)= 0.D0                            !Parabolic mean motion (n)
        END IF
C  
C** Store other useful values in returned array.
        R8P(27)= NOBS                             !Number obs. in soln.
        R8P(28)= NO                               !Record number
C
C** Extract radar astrometry counts, if any.
        J0= INDEX( CP, 'adar(' ) 
        IF ( J0 .NE. 0 ) THEN
         I= 0
         READ( CP(J0+5:), *, IOSTAT=IOS ) I
         R8P(33)= I                       !Number of delay measurements.
         I= 0
         J0= INDEX( CP, 'Dop' ) - 3
         READ( CP(J0:), *, IOSTAT=IOS ) I 
         R8P(34)= I                       !Number of Doppler measurements.
        END IF
       END IF
C
C** Store additional parameters if covariance DASTCOM3 file.
       IF ( FTYP .EQ. 'C' ) THEN
        R8P(38)= RMSW                     !Sol. weighted residual RMS, arcsec
        R8P(39)= RMSH                     !Total magnitude estimate RMS.
        R8P(40)= MOID                     !Minimum orbital intrsct. dist, AU
        R8P(41)= UPARM                    !Uncertainty parameter.
        R8P(42)= OBSTRT                   !JD of first obs in solution.
        R8P(43)= OBSTOP                   !JD of last obs in solution.
        R8P(44)= SOLDAT                   !Date of solution.
        DO I= 1, LSRC                     !Square root covariance vector.
         R8P(44+I)= SRC(I)
        END DO
        R8P(66)= OQP   
        R8P(67)= CEU                    
        R8P(68)= SIG1
        R8P(69)= LEAK1
        R8P(70)= SLEAK1
        R8P(71)= LEAK5
        R8P(72)= SLEAK5
       END IF
      END IF
      STAT= 0
      ERRMS= ' '
 100  RETURN
C
C------------------------------------------------------------------------------
C
      ENTRY D3INIT( FNAM, IU, XTRA, SPACE, TYP, IBND, BIAS, ISTAT )
C
C** Check allowed unit numbers.
      IF ( IU .LT. 10 ) THEN
       ISTAT= -5
       ERRMS= 'D3INIT: input unit number out of range (IU < 10).'
       RETURN
      END IF
C      
C** Check if already open.
      IF ( D3INI ) THEN
       FTYP= ' '
       ISTAT= -1
       ERRMS='D3INIT: may not initialize without closing previous file.'
       RETURN
      END IF
C
C** Initialize
      ISTAT= 0
      IREC= 1
      U= IU
      ERRMS= ' '
C
C** Load DASTCOM3 file name, open file.
      OPEN( U, FILE=FNAM, STATUS='OLD', ACCESS='DIRECT', 
     +      FORM='UNFORMATTED', RECL=RECLA, IOSTAT=ISTAT )  
      IF ( ISTAT .NE. 0 ) THEN
       WRITE(ERRMS,*) 
     +  'D3INIT: IOSTAT error code ',ISTAT,' opening file.'
       RETURN
      END IF
C
C** Read new DASTCOM3 header.
 1    READ( U, REC=IREC, IOSTAT=ISTAT ) 
     +   IBIAS1,ENDPT,epoch,calepo,equnox,
     +   BEGINP,FILLER,IBIAS2,comnt1,comnt2
      IF ( ISTAT .NE. 0 ) THEN
       WRITE(ERRMS,*) 
     +  'D3INIT: IOSTAT error code ',ISTAT,' reading header.'
       RETURN
      END IF
C
C** Extract boundary ends. Cycle back until valid header/start found.
      READ( ENDPT, '(3I6)', IOSTAT=ISTAT ) BND(1), BND(2), BND(3)
      IF ( ISTAT .NE. 0 ) THEN
       IREC= IREC + 1
       IF ( IREC .LT. 3 ) GOTO 1
       WRITE(ERRMS,*) 
     +  'D3INIT: IOSTAT error code ',ISTAT,' reading bounds.'
       RETURN
      END IF
C
C** Determine input file type.
      READ( U, REC= IREC, IOSTAT=ISTAT ) FILLER 
      IF ( ISTAT .NE. 0 ) THEN
       WRITE(ERRMS,*) 
     +  'D3INIT: IOSTAT error code ',ISTAT,' reading header.'
       RETURN
      END IF
      FTYP= FILLER(80:80)
C
C** Close and reopen, if necessary for database type.
      IF ( FTYP .EQ. 'A' ) THEN
      ELSE IF (FTYP .EQ. 'C') THEN
       CLOSE( U ) 
       OPEN( U, FILE=FNAM, STATUS='OLD', ACCESS='DIRECT', 
     +       FORM='UNFORMATTED', RECL=RECLC, IOSTAT=ISTAT )  
       IF ( ISTAT .NE. 0 ) THEN
        WRITE(ERRMS,*) 
     +   'D3INIT: IOSTAT error code ',ISTAT,' reading type.'
        RETURN
       END IF
      ELSE IF ( ICHAR(FTYP) .EQ. 0 ) THEN
       FTYP= 'A'
      ELSE
       ISTAT= -2
       ERRMS= 'D3INIT: unknown database header type-code "'//FTYP//'"'
       FTYP= ' '
       RETURN    
      END IF
C
C** Extract zone boundary "record" numbers.
      READ( BEGINP, '(3I6)', IOSTAT=ISTAT ) BND(4), BND(5), BND(6)
      IF ( ISTAT .NE. 0 ) THEN
       WRITE(ERRMS,*) 
     +  'D3INIT: IOSTAT error code ',ISTAT,' reading bounds.'
       RETURN
      END IF
C
C** Successful open. Assign remaining parameters, save flags for 
C** later D3READ calls.
      DO J= 1,6
       IBND(J)= BND(J)
      END DO
      TYP= FTYP
      BIAS(1)= IREC
      BIAS(2)= BIAS(1) - IBIAS1 - 1
      BIAS(3)= BIAS(1) - IBIAS2 - 1
      IBIAS0 = BIAS(1)
      IBIAS1 = BIAS(2)
      IBIAS2 = BIAS(3)
      LSPACE= SPACE
      LEXTR= XTRA
      D3INI= .TRUE.
      RETURN
C
C------------------------------------------------------------------------------
C
      ENTRY D3EZIN( FNAM, IU, ISTAT )
C 
C** Check allowed unit numbers.
      IF ( IU .LT. 10 ) THEN
       ISTAT= -5
       ERRMS= 'D3EZIN: input unit number out of range (IU < 10).'
       RETURN
      END IF
C
C** Check if already open.
      IF ( D3INI ) THEN
       FTYP= ' '
       ISTAT= -1
       ERRMS='D3EZIN: may not initialize without closing previous file.'
       RETURN
      END IF
C
C** Initialize
      ISTAT= 0
      IREC= 1
      U= IU
      ERRMS= ' '
C
C** Load DASTCOM3 file name, open file.
      OPEN( U, FILE=FNAM, STATUS='OLD', ACCESS='DIRECT',
     +         FORM='UNFORMATTED', RECL=RECLA, IOSTAT=ISTAT )
      IF ( ISTAT .NE. 0 ) THEN
       WRITE(ERRMS,*) 
     +  'D3EZIN: IOSTAT error code ',ISTAT,' opening file.'
       RETURN
      END IF
C
C** Read new DASTCOM3 header.
 2    READ( U, REC=IREC, IOSTAT=ISTAT )
     +   IBIAS1,ENDPT,epoch,calepo,equnox,
     +   BEGINP,FILLER,IBIAS2,comnt1,comnt2
      IF ( ISTAT .NE. 0 ) THEN
       WRITE(ERRMS,*) 
     +  'D3EZIN: IOSTAT error code ',ISTAT,' reading header.'
       RETURN
      END IF
C
C** Extract boundary ends. Cycle back until valid header/start found.
      READ( ENDPT, '(3I6)', IOSTAT=ISTAT ) BND(1), BND(2), BND(3)
      IF ( ISTAT .NE. 0 ) THEN
       IREC= IREC + 1
       IF ( IREC .LT. 3 ) GOTO 2
       WRITE(ERRMS,*) 
     +  'D3EZIN: IOSTAT error code ',ISTAT,' reading bounds.'
       RETURN
      END IF
C
C** Determine input file type.
      READ( U, REC= IREC, IOSTAT=ISTAT ) FILLER
      IF ( ISTAT .NE. 0 ) THEN
       WRITE(ERRMS,*) 
     +  'D3EZIN: IOSTAT error code ',ISTAT,' reading header.'
       RETURN
      END IF
      FTYP= FILLER(80:80)
C
C** Close and reopen, if necessary for database type.
      IF ( FTYP .EQ. 'A' ) THEN
      ELSE IF (FTYP .EQ. 'C') THEN
       CLOSE( U )
       OPEN( U, FILE=FNAM, STATUS='OLD', ACCESS='DIRECT',
     +       FORM='UNFORMATTED', RECL=RECLC, IOSTAT=ISTAT )
       IF ( ISTAT .NE. 0 ) THEN
         WRITE(ERRMS,*) 
     +    'D3EZIN: IOSTAT error code ',ISTAT,' reading type.'
         RETURN
       END IF
      ELSE IF ( ICHAR(FTYP) .EQ. 0 ) THEN
       FTYP= 'A'
      ELSE
       ISTAT= -2
       ERRMS='D3EZIN: unknown database header type-code "'//FTYP//'"'
       FTYP= ' '
       RETURN
      END IF
C
C** Extract zone boundary "record" numbers.
      READ( BEGINP, '(3I6)', IOSTAT=ISTAT ) BND(4), BND(5), BND(6)
      IF ( ISTAT .NE. 0 ) THEN
       WRITE(ERRMS,*) 
     +  'D3EZIN: IOSTAT error code ',ISTAT,' reading bounds.'
       RETURN
      END IF
      IBIAS0= IREC
      IBIAS1= IBIAS0 - IBIAS1 - 1
      IBIAS2= IBIAS0 - IBIAS2 - 1
      LEXTR= .TRUE.
      LSPACE= .TRUE.
      D3INI= .TRUE.
      RETURN
C
C------------------------------------------------------------------------------
C
      ENTRY D3BND( IBND ) 
      DO I= 1,6
       IBND(I)= 0      
       IF ( D3INI ) IBND(I)= BND(I) 
      END DO
      RETURN
C
C------------------------------------------------------------------------------
C
      ENTRY D3ERMS( CERRMS ) 
      CERRMS= ERRMS
      RETURN
C
C------------------------------------------------------------------------------
C
      ENTRY D3CLOS
      IF ( D3INI ) CLOSE( U )
      D3INI= .FALSE.
      ERRMS= ' '
      END
