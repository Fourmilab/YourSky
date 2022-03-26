      SUBROUTINE R4SWAP( N, R4ARR )
      IMPLICIT NONE
C
C** Declare dummy arguments
      INTEGER      N
      REAL*4       R4ARR(*)
C
C------------------------------------------------------------------------------
C R4SWAP - Swaps byte-order of input 4-byte floating point real(s), converting
C  R*4 data stored in big or little-endian byte order to the opposite byte 
C  order.
C
C Inputs: 
C  N ....... Numbers of elements in R4ARR to swap
C  R4ARR ... Array of 4-byte reals (over-written on return) 
C             Can be a single value (non-array)
C
C Output:
C  R4ARR ... Input values with first N elements having had bytes swapped
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
C      BYTE         I1ARR(4), I1TMP
C      LOGICAL*1    I1ARR(4), I1TMP
      INTEGER*1    I1ARR(4), I1TMP
      REAL*4       R4
      INTEGER      I, J, K
C
C** f77+: Map 4-byte real variable onto 1-byte array of dimension 4
C
C   'equivalence' is deprecated but remains in the 2008 FORTRAN standard 
C   and is efficient here. 'transfer' can alternatively be used (below).
C
      EQUIVALENCE  (R4,I1ARR)
C
C** Loop through input REAL*4 array values
      DO I= 1, N
C
C** f77+: Assign current input value to working swap space
       R4= R4ARR(I)
C
C** f90+: Alternative approach (if equivalencing is not being done)
C      I1ARR= TRANSFER(R4ARR(I),I1ARR) 
C
C** Swap 4-bytes
       DO J= 1,2           ! Upper limit is (# bytes to swap) / 2
        K= 5 - J           ! (# bytes to swap - (J - 1))
        I1TMP= I1ARR(J)
        I1ARR(J)= I1ARR(K)
        I1ARR(K)= I1TMP
       END DO
C
C** f77+: Over-write current input value with byte-swapped version
       R4ARR(I)= R4
C
C** f90+: Alternative approach (if equivalencing is not being done)
C      R4ARR(I)= TRANSFER(I1ARR,R4ARR(I)) 
C
      END DO
C
      END
