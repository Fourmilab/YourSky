      SUBROUTINE GETFLNM( TDIR, FNAM )
      IMPLICIT NONE
C
C** Declare dummy arguments.
      CHARACTER*(*)  TDIR, FNAM
C
C---------------------------------------------------------------------------
C GETFLNM -- Return an unused file name. The name is of form "scratch_##",
C  appended to the input TDIR, where ## is an integer from 15 to 99. 
C
C Input: 
C   TDIR ... Directory & file prefix; i.e., '/tmp/' or './' 
C
C Output:
C   FNAM ... String containing available file name
C
C Modification History:
C
C  DATE         Who  Change
C  -----------  ---  -------------------------------------------------------
C  1996-???-??  JDG  Version 1.0
C  2013-Jul-13  JDG  Removed process ID component to generalize
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
C** Declare local variables
      INTEGER        I, L
      LOGICAL        EXISTS
C
C** Store local data
      INTEGER        FRST, LAST
      DATA           FRST, LAST / 15, 99 /
C
C** Declare external functions.
      INTEGER        LENTRM, LFTTRM
C
C** Handle null input from user
      L= LENTRM( TDIR )
      IF ( L .EQ. 0 ) THEN
       TDIR= '/tmp/'
       L= 5
      END IF
C
C** Check for input variable problem
      IF ( LEN( FNAM ) .LT. L+10 ) THEN
       WRITE(0,'(A,I3)') 
     &  'GETFLNM: input argument FNAM length must be >= ',L+10
       STOP
      END IF
C 
C** Initialize loop variables.
      I= FRST
      EXISTS= .TRUE.
C
C** Begin loop to search for non-existing file TDIR{pid}.xx
  10  IF ( EXISTS .AND. (I .LE. LAST) ) THEN    
       WRITE( FNAM,'(A,I2.2)' ) TDIR(1:L)//'scratch_',I
       INQUIRE( FILE= FNAM(1:LENTRM(FNAM)), EXIST=EXISTS )
       I= I + 1
       IF ( EXISTS ) GOTO 10 
      END IF
C  
C** In no available file names can be identified ...
      IF ( EXISTS ) THEN
       WRITE(0,'(A)') 
     &  'GETFLNM: cannot allocate temporary file, halting'
       STOP
      END IF
C
      END
