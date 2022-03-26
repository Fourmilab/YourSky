      SUBROUTINE UCASE3( CHR )
      IMPLICIT NONE
C
C** Declare dummy argument.
      CHARACTER*1  CHR*(*)
C
C---------------------------------------------------------------------------
C UCASE3 -- Convert an input string to upper-case equivalent.
C
C Input:
C   CHR   Arbitrary length character string.
C
C Output
C   CHR   Original input CHR over-written with uppercase if necessary.
C
C Modification History:
C
C  DATE         Who  Change
C  -----------  ---  -------------------------------------------------------
C  1996-???-??  JDG  Version 1.0 written.
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
C** Declare local variables.
      INTEGER      L, I, J
C
C** Declare external functions
      INTEGER      LENTRM 
C
C** Process string & convert.
C     L= LEN_TRIM(CHR)   ! f95 intrinsic
      L= LENTRM(CHR)
      DO I= 1,L
       J= ICHAR( CHR(I:I) )
       IF ( (J.GE.97) .AND. (J.LE.122) ) CHR(I:I)= CHAR(J-32)
      END DO
C
      END
