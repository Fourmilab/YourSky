      LOGICAL FUNCTION LTLEND()
      IMPLICIT NONE
C
C------------------------------------------------------------------------------
C LTLEND - Determine if the executing system stores numbers in little-endian 
C  or big-endian hex byte order.
C
C Inputs: 
C  none
C
C Output (function value):
C  .TRUE. = Little-endian (low-order byte first)
C  .FALSE.= Big-endian (high-order byte first)
C
C NOTES:
C  If compilation problems are encountered, variations commented out in the
C  code below should resolve it (alternatives to INTEGER*1 and EQUIVALENCE)
C
C Modification History:
C
C  DATE         Who  Change
C  -----------  ---  ----------------------------------------------------------
C  2012-Aug-07  JDG  Version 1.0
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
C** Declare local variables. Older compilers that don't support 
C** "INTEGER*1" may support alternatives "LOGICAL*1", or "BYTE"
C     BYTE         I1ARR(2)
C     LOGICAL*1    I1ARR(2)
      INTEGER*1    I1ARR(2)
      INTEGER*2    I2VAL
C
C** f77+: Map 2-byte integer test variable onto 1-byte array of dimension 2
C
C   'equivalence' is deprecated but remains in the 2008 FORTRAN standard 
C   and is efficient here. 'transfer' can alternatively be used (below).
C
      EQUIVALENCE  (I2VAL,I1ARR)
C
      I2VAL= 26901  ! hex value is 6915; x69=105 decimal, x15=21 decimal
C
C** f90+: Map 2-byte integer test variable onto 1-byte array of dimension 2
C     I1ARR= TRANSFER(I2VAL,I1ARR)
C
C** Check high-order byte, assign flag for return
      LTLEND= .FALSE.                       ! Big-endian (default)
      IF ( I1ARR(1) .EQ. 21) LTLEND= .TRUE. ! Little-endian
C
      END
