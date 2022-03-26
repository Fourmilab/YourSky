      DOUBLE PRECISION FUNCTION GREG2JD( YEAR, MONTH, DAY )
      IMPLICIT NONE
C
C** Declare dummy arguments
      INTEGER   YEAR, MONTH, DAY
C
C------------------------------------------------------------------------------
C GREG2JD - Convert input Gregorian calendar year, month, day integer set to 
C  Julian Day Number.  Valid only for Gregorian calendar input.
C
C Inputs:
C  YEAR ... Integer year number
C  MONTH .. Integer month number
C  DAY .... Integer day number
C
C Output:
C  Function value, equivalent Julian Day Number
C
C** Descriptive notes on constants:
C
C   Constant   Description
C   --------   -----------
C    1721119   The offset from JD 0 to 1BC-Mar-1 start (March 1 in 
C              astronomical year 0). Subtracting gives number of days since
C              that date. March is used as the start of year so that leap 
C              days will be at the end for simplicity.
C     146097   Number of days in four centuries (leap-year cycle)
C       1461   Number of days in four years (short leap-year cycle)
C        153   Scaled linear term coefficient in least-squares fit to 
C               zero-referenced day/month numbers
C 3's and 9's  Used to shift month of start-of-year basis 
C
C  DATE         Who  Change
C  -----------  ---  ----------------------------------------------------------
C  1994-???-??  JDG  Version 1.0
C  2016-Feb-26  JDG  Removed unnecessary variable, enhanced comments
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
      INTEGER   Y, M, C, YA
C
C** Reassign input values to local variables
      Y= YEAR
      M= MONTH
C
C** Shift reference start-of-year from 0-Jan-1 to 0-Mar-1
      IF ( M .GT. 2 )THEN
       M = M - 3
      ELSE
       M = M + 9
       Y = Y - 1
      END IF
C
C** Compute centuries and remainder years
      C= Y / 100
      YA= Y - 100 * C
C
C** Assign function value for return
      GREG2JD= (146097*C)/4 + (1461*YA)/4 + (153*M+2)/5 + DAY + 1721118.5
C
      END
