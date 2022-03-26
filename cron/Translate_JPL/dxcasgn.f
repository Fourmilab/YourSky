      SUBROUTINE DXCASGN( NCH, DXORD, ZONE, CXP, CHOUT ) 
      IMPLICIT NONE
C
C** Include standard array definitions. 
      INCLUDE 'dxparms.inc'
C
C** Declare dummy arguments
      INTEGER           NCH, DXORD(NUMCH)
      LOGICAL           ZONE(3)
      CHARACTER*80      CHOUT(NUMCH)
C
C------------------------------------------------------------------------------
C DXCASGN -- DastcomX Character output ASiGNment. Uses previously filled 
C  standard database character-block along with user requested output-order 
C  arrays to fill the character return array. DXORD() must already be
C  vetted for problems.
C
C  Called by DXREAD. Users should not directly call this subroutine.
C
C  Inputs:
C   NCH ...... Logical length of DXORD() array (no. requested char. variables)
C   DXORD()... Array of integer codes for user-requested output values
C   ZONE() ... Logical array vector in database record
C               ZONE(1)= .TRUE. for IAU-numbered asteroids
C               ZONE(2)= .TRUE. for unnumbered asteroids
C               ZONE(3)= .TRUE. for comets
C              Only one element should be .TRUE., the others  .FALSE.
C   CXP   .... Internal string of previously loaded CHARACTER database values
C
C  Output:
C   CHOUT()... Output array of CHARACTER value(s) to return to user
C               
C Modification History:
C               
C  DATE         Who   Change
C  -----------  ----  ---------------------------------------------------------
C  2013-Jul-15  JDG   Version 1.0
C  2016-Mar-02  JDG   Fixed character output array order to pay attention to
C                      user specification.
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
      INTEGER           I
      CHARACTER*80      CH(NUMCH)
C
C** Clear local temporary storage arrays
      DO I= 1, NUMCH
       CH(I)= ' '
      END DO
C
C** Unpack character block into standard array slots. If additional
C   character fields are added to a future database, new assignments 
C   must be added here.
C
C~~ Common assignments
      CH(1)= CXP(1:4)       ! EQUNOX
      CH(2)= CXP(5:10)      ! PENAM
      CH(3)= CXP(11:22)     ! SBNAM
C
C~~ Comet assignments
      IF ( ZONE(3) ) THEN
       CH(6) = CXP(23:31)   ! DARC  
       CH(9) = CXP(32:80)   ! COMNT3
       CH(10)= CXP(81:160)  ! COMNT4
       CH(11)= CXP(161:173) ! DESIG
       CH(12)= CXP(174:187) ! COMEST
       CH(13)= CXP(188:197) ! IREF
       CH(14)= CXP(198:226) ! COMNAM
C
C~~ Asteroid assignments
      ELSE
       CH(4) = CXP(23:27)   ! SPTYPT
       CH(5) = CXP(28:32)   ! SPTYPS
       CH(6) = CXP(33:41)   ! DARC
       CH(7) = CXP(42:82)   ! COMNT1
       CH(8) = CXP(83:162)  ! COMNT2
       CH(11)= CXP(163:175) ! DESIG
       CH(12)= CXP(176:183) ! ASTEST
       CH(13)= CXP(184:193) ! IREF
       CH(14)= CXP(194:211) ! ASTNAM
      END IF
C
C** Make user-requested assignments for individual array element return
      DO I= 1,NCH 
       IF ( DXORD(I) .NE. 0 ) CHOUT(I)= CH( DXORD(I) )
      END DO

      END
