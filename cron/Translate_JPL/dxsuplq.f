      SUBROUTINE DXSUPLQ( ZONE,R8P,R4P,I4P,I2P,I1P,CXP,ERRMS,ISTAT )
      IMPLICIT NONE
C
C** Declare dummy arguments. Older compilers that don't support 
C   "INTEGER*1" may support alternatives "LOGICAL*1", or "BYTE"
C      LOGICAL*1     I1P(*)
C      BYTE          I1P(*)
      INTEGER*1      I1P(*)
      LOGICAL        ZONE(3)
      REAL*8         R8P(*)
      REAL*4         R4P(*)
      INTEGER*4      I4P(*)
      INTEGER*2      I2P(*)
      CHARACTER*(*)  CXP, ERRMS
      INTEGER        ISTAT
C
C------------------------------------------------------------------------------
C DXSUPLQ - DASTCOMx SUPplemental Quantities.  Returns parameters closely
C  related to those read from DASTCOMx database records through calculation 
C  and comment-field parsing.
C
C  Historically, some parameters were added to DASTCOM3/DASTCOM4 comment 
C  fields, instead of being assigned their own field in the data record, thus 
C  the need to parse them from comments.
C
C  In the future, if new quantities are similarly placed in comments without 
C  altering the database structure, this is the subroutine where it could be 
C  handled. The database physical record has already been read; the approach 
C  is to see if an empty (zero-filled) numeric slot can be filled another way.
C
C  Users should not directly call this subroutine.
C
C Inputs:
C  ZONE() .. Logical array indicating type of object record
C
C             ZONE(1) = IAU-numbered asteroid 
C             ZONE(2) = asteroid
C             ZONE(3) = comet
C
C  R8P() ... Internal array of DX-slotted values already read from DASTCOM 
C
C             Elements used here for calculations (heliocentric ecliptic):
C               R8P(4)= RNODE, argument of perihelion, degrees
C               R8P(5)= OM, longitude of ascending node, degrees
C               R8P(6)= RI, inclination, degrees
C               R8P(7)= E, eccentricity
C               R8P(8)= A, semi-major axis, au
C               R8P(9)= Q, perihelion distance, au
C
C  R4P() ... Internal array of DX-slotted values already read from DASTCOM
C  I4P() ... Internal array of DX-slotted values already read from DASTCOM
C  I2P() ... Internal array of DX-slotted values already read from DASTCOM
C  I1P() ... Internal array of DX-slotted values already read from DASTCOM
C             (passed for future use)
C  CXP ..... Internal character DX-format data block already read from DASTCOM
C
C Outputs:
C  R8P() ... Input array augmented to include DERIVED supplementary quantities
C
C               R8P(51)= ADIST, Aphelion distance, au
C               R8P(52)= PER, Sidereal orbit period, Julian years
C               R8P(53)= ANGMOM, Specific angular momentum, au^2/d
C               R8P(54)= N, Mean motion (N), deg/day
C               R8P(55)= DAN, Heliocentric distance of ascending node, au
C               R8P(56)= DDN, Heliocentric distance of descending node, au
C               R8P(57)= L, Ecliptic longitude of perihelion, degrees
C               R8P(58)= B, Ecliptic latitude of perihelion, degrees
C
C  R4P() ... Input array possibly augmented with additional (PARSED) values
C
C             If legacy record & comet:
C               R4P(1) = H      (pre-implementation)
C               R4P(2) = G      (pre-implementation)
C               R4P(38)= ALBEDO (pre-implementation)
C               R4P(5) = K1
C               R4P(6) = K2
C               R4P(7) = PHCOF, phase coefficient
C               R4P(11)= DT, delay parameter, d (if in comments but not rec.)
C               R4P(17)= SO, center-of-light offset at 1 au, km (if present)
C
C  I4P    .. Input array possibly augmented with additional (PARSED) values
C
C             If legacy record & comet:
C               I4P(2)= NOBS, number of observations of all types
C
C  I2P ..... Input array possibly augmented with additional (PARSED) values
C
C             If legacy:
C               I2P(2)= NDEL, number of delay measurements 
C               I2P(3)= NDOP, number of Doppler measurements
C
C             If comet record:
C               I2P(6)= COMNUM, IAU comet number
C
C  I1P ..... Input array (no change; future use)
C
C  ERRMS ... Error message
C
C  ISTAT ... Integer error code,
C              0= No error
C             -7= Hyperbola's semi-major axis, a, is positive
C
C Modification History:
C
C  DATE         Who  Change
C  -----------  ---  ----------------------------------------------------------
C  2013-Jul-15  JDG  Version 1.0
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
C** Declare local variables
      REAL*8       A32, C1, C2, C3, C4, C5, C6, EDELT
      INTEGER      I, J, IOS
C
C** Declare parameters; SGM10 is SQRT(GM of Sun in au^3/d^2). IAU resolution
C   2012-B2 removes this (Gauss's constant) as a fixed value. In the future,
C   it is to be defined by the planetary ephemeris solution, but as of JPL
C   DE-431 (2013), is not different at double-precision significance levels.
C
      REAL*8       TWOPI, SGM10, PP, DPR
      PARAMETER( TWOPI= 6.2831853071795862  )
      PARAMETER( SGM10= 1.7202098950000D-02 )
      PARAMETER( PP   = TWOPI / SGM10       )
      PARAMETER( DPR  = 360.D0 / TWOPI      )
C
C** Define a large number 
      REAL*8       RMAX
      DATA         RMAX   / 9.999999D+99  /
C
C** Set tolerance for conic discrimination
      REAL*8       TOL
      DATA         TOL    / 1.D-8 /
C
C** Initialize
      ISTAT= 0
      ERRMS= ' '
C
C** Compute values (at EPOCH) common to all objects
      C1     = R8P(9) * ( 1.D0 + R8P(7) )      !  P, semi-latus rec, au 
      C2     = R8P(4) / DPR                    !  APF, radians
      C3     = COS( C2 ) 
      C4     = SIN( C2 )
      C5     = R8P(7) * C3                     !  E * COS(APF)
      C6     = R8P(6) / DPR                    !  Inclination, radians
      R8P(53)= SQRT( C1 ) * SGM10              ! Specific ang. mom., au^2/d
      R8P(55)= C1 / ( 1.D0 + C5 )              ! Helio. dist. asc. node, au
      R8P(56)= C1 / ( 1.D0 - C5 )              ! Helio. dist. des. node, au
      R8P(57)= R8P(5)+ATAN2(C4*COS(C6),C3)*DPR ! Eclip. long. peri., deg.
      R8P(57)= MOD( R8P(57)+360.D0, 360.D0 )   !  (correct angle domain)
      R8P(58)= ASIN( C4 * SIN(C6) )*DPR        ! Eclip. lat. peri., deg.
C
C~~ Compute values (at EPOCH) that are dependent on conic type.
      EDELT= 1.D0 - R8P(7)
      IF (DABS(EDELT) .GE. TOL) THEN ! NON-PARABOLIC orbit detected ...
       IF ( EDELT .GT. 0.D0 ) THEN   !   Elliptical/circular
        A32    = R8P(8) ** 1.5D0   
        R8P(51)= 2.D0 * R8P(8) - R8P(9)       !Aphelion distance, au
        R8P(52)= PP * A32 / 365.25D0          !Sidereal orb. period, Julian yrs
       ELSE                          !   Hyperbolic
        IF ( R8P(8) .GE. 0.D0 ) THEN
         ERRMS= 'DXSUPLQ: hyperbola semi-major axis is positive'
         ISTAT= -7
         RETURN
        END IF
        A32    = (-R8P(8)) ** 1.5D0
        R8P(51)= RMAX                         !Aphelion distance, au
        R8P(52)= RMAX                         !Sidereal orb. period, Julian yrs
       END IF
       R8P(54)= (SGM10 / A32) * DPR           !Mean motion (n), deg/day
      ELSE                           ! PARABOLIC orbit
       R8P(51)= RMAX                          !Aphelion distance, au
       R8P(52)= RMAX                          !Sidereal orb. period, Julian yrs
       R8P(54)= 2.D0*SGM10/(C1**1.5D0) * DPR  !Parabolic mean motion (n), deg/d
      END IF
C       
C** Determine comet IAU-number from DESIG [CXP(161:173)], if possible 
C   (returns 0 if failure).
      IF ( ZONE(3) ) THEN
       I= 0
       READ( CXP(161:173), *, IOSTAT= IOS ) I
       I2P(6)= I
      END IF
C
C** Handle comment field parsing for DASTCOM3/4 legacy records
C
C~~ Extract radar astrometry counts, if any exist in comments and values
C   haven't been assigned already.
      J= INDEX( CXP, 'adar(' ) 
      IF ( J .NE. 0 ) THEN
       IF ( I2P(2) .EQ. 0 ) THEN
        I= 0
        READ( CXP(J+5:), *, IOSTAT=IOS ) I
        I2P(2)= I                       !Number of delay measurements.
       END IF
       IF ( I2P(3) .EQ. 0 ) THEN
        I= 0
        J= INDEX( CXP, 'Dop' ) - 3
        READ( CXP(J:), *, IOSTAT=IOS ) I 
        I2P(3)= I                       !Number of Doppler measurements.
       END IF
      END IF
C
C~~ Handle legacy COMET record issues
      IF ( ZONE(3) ) THEN
C
C~~ For D3/4 legacy comet records, first symbols of COMNT3 [CXP(32:80)] can 
C   be the integer number of observations (NOBS)
       IF ( I4P(2) .EQ. 0 ) READ( CXP(32:80), *, IOSTAT=IOS ) I4P(2)
C
C~~ Extract comet parameter K1 from comments, if present and needed.
       IF ( R4P(5) .EQ. 0. ) THEN
        J= INDEX( CXP, 'k1=' )
        IF ( J .NE. 0 ) READ(CXP(J+3:), *, IOSTAT=IOS) R4P(5)  !Store k1
       END IF
C
C~~ Extract comet parameter K2 from comments, if present and needed.
       IF ( R4P(6) .EQ. 0. ) THEN
        J= INDEX( CXP, 'k2=' )
        IF ( J .NE. 0 ) READ(CXP(J+3:), *, IOSTAT=IOS) R4P(6)  !Store k2
       END IF
C
C~~ Extract comet parameter PHCOF from comments, if present and needed.
C   Field in COMNT terminated with semi-colon.
       IF ( R4P(7) .EQ. 0. ) THEN
        J= INDEX( CXP, 'phase coef.=' )
        IF (J.NE.0) READ(CXP(J+12:),'(F4.2)',IOSTAT=IOS) R4P(7) !phase coef
       END IF
C
C~~ Extract comet parameter DT from comments, if present and needed.
       IF ( R4P(11) .EQ. 0. )  THEN
        J= INDEX( CXP, 'DT=' )
        IF ( J .NE. 0 ) READ( CXP(J+3:), *, IOSTAT= IOS ) R4P(11) 
       END IF
C   
C~~ Extract comet parameter S0 from comments, if present and needed.
       IF ( R4P(17) .EQ. 0. ) THEN
        J= INDEX( CXP, 'S0=' )
        IF ( J .NE. 0 ) READ( CXP(J+3:), *, IOSTAT= IOS ) R4P(17)
       END IF
C
C~~ Not implemented as of this writing, but allow for future store
C   of H-G & albedo data for comet inactive phase in comments.
       IF ( R4P(1) .EQ. 0. ) THEN
        J= INDEX( CXP, 'H=' )
        IF ( J .NE. 0 ) READ( CXP(J+2:), *, IOSTAT= IOS ) R4P(1)
       END IF
       IF ( R4P(2) .EQ. 0. ) THEN
        J= INDEX( CXP, 'G=' )
        IF ( J .NE. 0 ) READ( CXP(J+2:), *, IOSTAT= IOS ) R4P(2)
       END IF
       IF ( R4P(38) .EQ. 0. ) THEN
        J= INDEX( CXP, 'alb.=' )
        IF ( J .NE. 0 ) READ( CXP(J+5:), *, IOSTAT= IOS ) R4P(38)
       END IF
C
      END IF
C
      END
