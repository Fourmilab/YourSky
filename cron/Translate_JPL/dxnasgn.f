      SUBROUTINE DXNASGN( NR8,DXORD,LSRC,R8P,R4P,I4P,I2P,I1P,R8OUT ) 
      IMPLICIT NONE
C
C** Include standard array definitions.
      INCLUDE 'dxparms.inc'
C
C** Declare dummy arguments
      INTEGER           NR8, DXORD(NUMNS), LSRC
      REAL*8            R8P(*), R8OUT(*)
      REAL*4            R4P(*)
      INTEGER*4         I4P(*)
      INTEGER*2         I2P(*)
      INTEGER*1         I1P(*)
C
C------------------------------------------------------------------------------
C DXNASGN -- DastcomX Numeric output ASiGNment. Uses previously filled standard
C  database arrays along with user requested output-order arrays to fill the 
C  numeric return array. DXORD() must already vetted for problems.
C
C  Converts units on A1, A2, and A3 parameters from physically stored
C  10^-8 au/d^2 to au/d^2, which is returned to the calling user.
C
C  Users should not directly call this subroutine.
C
C  Inputs:
C   NR8 ...... Logical length of DXORD() array
C   DXORD()... Array of integer codes for user-requested output values
C   LSRC ..... Length of SRC vector in database record
C   R8P() .... Internal array of previously loaded REAL*8 database values
C   R4P() .... Internal array of previously loaded REAL*4 database values
C   I4P() .... Internal array of previously loaded INTEGER*4 database values
C   I2P() .... Internal array of previously loaded INTEGER*2 database values
C   I1P() .... Internal array of previously loaded INTEGER*1 database values
C
C  Output:
C   R8OUT()... Output array of REAL*8 numeric values to return to user
C               
C Modification History:
C               
C  DATE         Who   Change
C  -----------  ----  ---------------------------------------------------------
C  2013-Jul-15  JDG   Version 1.0
C  2013-Aug-07  JDG   Restore ALN full-precision for nominal value
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
      INTEGER           I, J, K
C
C** Make assignments
      I= 1
      DO WHILE ( I .LE. NR8 ) 
C
       IF  ( (DXORD(I) .EQ. 899) .AND. (LSRC .GT. 0) ) THEN
        DO J= 0,LSRC-1
         K= I + J
         R8OUT(K)= R8P( 99+J )
        END DO
        I= I + LSRC - 1
C
       ELSE IF ( DXORD(I) .GT. 800 ) THEN
        R8OUT(I)= R8P( DXORD(I) - 800 )
C
       ELSE IF ( DXORD(I) .GT. 400 ) THEN
        R8OUT(I)= R4P( DXORD(I) - 400 )
C
C  Scale units on A1, A2, A3 from stored 10^-8 au/day^2 to return au/day^2
        IF ( DXORD(I) .EQ. 408 ) THEN      ! A1
         R8OUT(I)= R8OUT(I) * 1.D-8
        ELSE IF ( DXORD(I) .EQ. 409 ) THEN ! A2
         R8OUT(I)= R8OUT(I) * 1.D-8
        ELSE IF ( DXORD(I) .EQ. 410 ) THEN ! A3
         R8OUT(I)= R8OUT(I) * 1.D-8
        ELSE IF ( DXORD(I) .EQ. 413 ) THEN ! ALN
         IF ( ABS(R8OUT(I)-0.1112620426) .LT. 1.D-7 ) 
     &    R8OUT(I)= 0.1112620426D0
        END IF
C
       ELSE IF ( DXORD(I) .GT. 200 ) THEN
        R8OUT(I)= I4P( DXORD(I) - 200 )
C
       ELSE IF ( DXORD(I) .GT. 150 ) THEN
        R8OUT(I)= I2P( DXORD(I) - 150 )
C
       ELSE IF ( DXORD(I) .GT. 100 ) THEN
        R8OUT(I)= I1P( DXORD(I) - 100 )
C
       END IF
       I= I + 1
      END DO
C
      END
