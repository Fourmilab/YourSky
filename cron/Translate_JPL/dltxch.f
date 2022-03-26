      SUBROUTINE DLTXCH( CHOUT, DTYP, IZONE, NUMCH, CXARR )
      IMPLICIT NONE
C
C** Declare dummy arguments
      CHARACTER*1    CHOUT*(NUMCH*80)
      INTEGER        DTYP, IZONE, NUMCH
      CHARACTER*1    CXARR(NUMCH)*80 
C
C------------------------------------------------------------------------------
C DLTXCH -- DASTCOM Legacy To DASTCOMX CHaracter assignment. Subroutine takes
C  input legacy DASTCOM4/D3READ character block and returns it in a standard
C  DASTCOMX array of elements.
C
C Inputs:
C  CHOUT ...... Standard legacy D3READ subroutine character-block,
C                such as returned when IR8ORD(1)= -3 or IR8ORD(1)= -4
C  DTYP ....... Integer-type of legacy database output
C                -3= DASTCOM3
C                -4= DASTCOM4
C  IZONE ...... Integer indicating type of object
C                 1= numbered asteroid
C                 2= unnumbered asteroid
C                 3= comet  
C  NUMCH ...... Number of all DEFINED character fields (as specified in 
C                'dxparms.inc'); logical length of CXARR().               
C
C Output:
C  CXARR ...... Character array containing individual fields, one per 
C                element of the array. Must be dimensioned to length NUMCH.
C
C  NOTE: If DTYP, IZONE, or NUMCH are invalid, CXARR will be returned blank. 
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
      INTEGER       I
C
C** Check for improper input; return if no action possible.
      IF ( NUMCH .LT. 14 )                       RETURN
      IF ( (DTYP .NE. -3) .AND. (DTYP .NE. -4) ) RETURN
      IF ( (IZONE .LT. 1) .OR. (IZONE .GT. 3) )  RETURN
C
C** Initialize return array
      DO I= 1, NUMCH
       CXARR(I)= ' '
      END DO
C
C** Break out the input character block into individual return elements
C~~ First: elements common to asteroid & comet records of both D3 and D4
      CXARR(1)= CHOUT(46:49)                        ! EQUNOX
      CXARR(6)= CHOUT(56:64)                        ! DARC
      CXARR(11)= CHOUT(25:37)                       ! DESIG
      CXARR(13)= CHOUT(38:45)                       ! IREF
      CXARR(14)= CHOUT(1:24)                        ! NAME
C
C~~ Next: remaining database/object specific assignments
      IF ( IZONE .EQ. 3 ) THEN                      ! Comet-specific
       IF (DTYP .EQ. -4) CXARR(2)= CHOUT(206:211)   !  PENAM (D4 only)
       CXARR(3)= CHOUT(194:205)                     !  SBNAM
C      CXARR(4)=                                    !  (blank; SPTYPT)
C      CXARR(5)=                                    !  (blank; SPTYPS)
C      CXARR(7)=                                    !  (blank; COMNT1)
       CXARR(8)= CHOUT(114:193)                     !  COMNT2
       CXARR(9)= CHOUT(65:113)                      !  COMNT3
C      CXARR(10)=                                   !  (blank; COMNT4)
       IF (DTYP .EQ. -4) CXARR(12)= CHOUT(212:217)  !  ESTL (D4 only)
      ELSE                                          ! Asteroid-specific
       IF (DTYP .EQ. -4) CXARR(2)= CHOUT(198:203)   !  PENAM (D4 only)
       CXARR(3)= CHOUT(186:197)                     !  SBNAM
       IF (DTYP .EQ. -3) CXARR(4)= CHOUT(50:55)     !  SPTYPT (D3 only)
       IF (DTYP .EQ. -4) CXARR(5)= CHOUT(50:55)     !  SPTYPS (D4 only)
       CXARR(7)= CHOUT(65:105)                      !  COMNT1 
       CXARR(8)= CHOUT(106:185)                     !  COMNT2
C      CXARR(9)=                                    !  (blank; COMNT3)
C      CXARR(10)=                                   !  (blank; COMNT4)
C      CXARR(12)=                                   !  (blank; ESTL)
      END IF
C
      END
