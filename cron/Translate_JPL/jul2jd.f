      DOUBLE PRECISION FUNCTION JUL2JD( YEAR, MONTH, DAY )
      IMPLICIT NONE
C
C** Declare dummy arguments
      INTEGER   YEAR, MONTH, DAY
C
C------------------------------------------------------------------------------
C JUL2JD - Convert input Julian calendar year, month, day integer set to Julian
C  Day Number. Valid only for Julian calendar input after year -4800.
C
C  NOTE: BC dates should be passed as the negative of the year number + 1.
C   i.e., 5 BC as (-5 + 1) = -4,  1 BC as (-1 + 1) = 0
C
C Inputs:
C  YEAR ... Integer year number
C  MONTH .. Integer month number
C  DAY .... Integer day number
C
C Output:
C  Function value, equivalent Julian astronomical day 
C
C  DATE         Who  Change
C  -----------  ---  ----------------------------------------------------------
C  2013-Jul-15  JDG  Version 1.0
C  2016-Feb-26  JDG  Removed unnecessary variables, clarified comments
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
      INTEGER   A, Y, M
C
C** Assign intermediate values
      A= (14 - MONTH) / 12
      Y=  YEAR + 4800 - A
      M= MONTH + 12*A - 3
C
C** Assign function value for return
      JUL2JD=  DAY + (153*M+2)/5 + 365*Y + Y/4 - 32083.5
C
      END
