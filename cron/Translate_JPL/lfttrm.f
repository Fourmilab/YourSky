      INTEGER FUNCTION LFTTRM( STR )
      IMPLICIT NONE
C
C** Declare dummy argument
      CHARACTER*(*)  STR
C
C------------------------------------------------------------------------------
C  LFTTRM -- Portably (i.e., include f77) returns the first non-blank position 
C   in specified string.  If the string is all spaces, the declared length of 
C   STR+1 is returned. 
C
C  Inputs:
C   STR  ................. Input string to assess
C
C  Output:
C   <value of function> .. Position of first non-blank character in STR,
C                           or LEN(STR)+1, if STR is all spaces.
C
C  DATE         Who  Change
C  -----------  ---  ----------------------------------------------------------
C  ????-???-??  PWC  Version 1.0
C  1994-???-??  JDG  Style adaption & comments for OSOD/Horizons (?)
C
C Key:
C  PWC= Paul.Chodas@jpl.nasa.gov
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
      INTEGER  I
C
C** Loop through string from start to maximum dimensioned length until
C** finding first non-blank position.
      DO I= 1, LEN(STR)
       IF ( STR(I:I) .NE. ' ' ) GOTO 10
      END DO
   10 IF ( (I .EQ. LEN(STR)) .AND. (STR(I:I) .EQ. ' ') ) I= I + 1 
      LFTTRM= I
C
      END
