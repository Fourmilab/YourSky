      SUBROUTINE GETUNI2( U )
      IMPLICIT NONE
C
C** Declare dummy argument.
      INTEGER       U 
C
C---------------------------------------------------------------------------
C GETUNI2 -- Find an unused logical unit number.
C
C Input:  None
C
C Output:
C   U ..... Unattached logical unit number 
C
C Modification History:
C
C  DATE         Who  Change
C  -----------  ---  -------------------------------------------------------
C  1996-???-??  JDG  Version 1.0 written.
C  2013-Jul-15  JDG  Version 2.0
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
C---------------------------------------------------------------------------
C
C** Declare subroutine variables
      LOGICAL       OPND  
C
C** Declare local parameters
      INTEGER    T1, T2
      PARAMETER( T1= 20 )  ! Lowest unit to check
      PARAMETER( T2= 99 )  ! Highest unit to check (100-102 defined in f90+)
C
C** Initialize variables.
      U= T1 - 1
      OPND= .TRUE.
C
C** Loop in search of first unopened logical unit number 
      DO WHILE ( OPND .AND. (U .LT. T2) )
       U= U + 1
       INQUIRE( UNIT= U, OPENED= OPND )
      END DO
C  
C** In the case where no logical unit T1 <= X <= T2 free ...
      IF ( OPND ) THEN 
       WRITE( 6, '(A)' ) 
     & 'GETUNI2 cannot find an open logical unit number, halting'
       STOP
      END IF
C
      END
