      SUBROUTINE I2SWAP( N, I2ARR )
      IMPLICIT NONE
C
C** Declare dummy arguments
      INTEGER      N
      INTEGER*2    I2ARR(*)
C
C------------------------------------------------------------------------------
C I2SWAP - Swaps byte-order of input 2-byte integer(s), converting I*2 data 
C  stored in big or little-endian byte order to the opposite byte order.
C
C Inputs: 
C  N ....... Numbers of elements in I2ARR to swap
C  I2ARR ... Array of 2-byte integers (over-written on return) 
C             Can be a single value (non-array)
C
C Output (function value):
C  I2ARR ... Input values with first N elements having had bytes swapped
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
C      BYTE         I1ARR(2), I1TMP
C      LOGICAL*1    I1ARR(2), I1TMP
      INTEGER*1    I1ARR(2), I1TMP
      INTEGER*2    I2
      INTEGER      I
C
C** f77+: Map 2-byte integer variable onto 1-byte array of dimension 2
C
C   'equivalence' is deprecated but remains in the 2008 FORTRAN standard 
C   and is efficient here. 'transfer' can alternatively be used (below).
C
      EQUIVALENCE  (I2,I1ARR)
C
C** Loop through input INTEGER*2 array values
      DO I= 1, N
C
C** f77+: Assign current input value to working swap space
       I2= I2ARR(I)
C
C** f90+: Alternative approach (if equivalencing is not being done)
C      I1ARR= TRANSFER(I2ARR(I),I1ARR) 
C
C** Swap 2 bytes
       I1TMP= I1ARR(1)
       I1ARR(1)= I1ARR(2)
       I1ARR(2)= I1TMP
C
C** f77+: Over-write current input value with byte-swapped version
       I2ARR(I)= I2
C
C** f90+: Alternative approach (if equivalencing is not being done)
C      I2ARR(I)= TRANSFER(I1ARR,I2ARR(I)) 
C
      END DO
C
      END
