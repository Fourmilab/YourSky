      INTEGER FUNCTION LENTRM( ISTR )
      IMPLICIT NONE
C
C** Declare dummy argument
      CHARACTER*(*)  ISTR
C
C------------------------------------------------------------------------------
C  LENTRM - Portably (i.e., include f77) returns the length of a specified
C   string, excluding trailing spaces.  If the string is all spaces, a length
C   of 0 is returned as the function value.
C
C   This function is now available from the f95 intrinsic LEN_TRIM.
C
C  Inputs:
C   ISTR  ................ Input string to 'measure'
C
C  Output:
C   <value of function> .. Number of characters in ISTR, excluding trailing 
C                          spaces. If ISTR is all spaces, a zero is returned
C
C Modification History:
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
      INTEGER        I
C
C** Loop through input string backward from maximum dimensioned length to
C** first character, until finding non-blank or reaching first position.
      DO I= LEN(ISTR), 1, -1
       IF ( ISTR(I:I) .NE. ' ' ) GOTO 10
      END DO
   10 LENTRM= I
C
      END
