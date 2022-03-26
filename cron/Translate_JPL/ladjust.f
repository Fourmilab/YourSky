      SUBROUTINE LADJUST( CHR )
      IMPLICIT NONE
C
C** Declare dummy argument.
      CHARACTER*1  CHR*(*)
C
C---------------------------------------------------------------------------
C LADJUST -- Adjust string to the left, removing leading blanks and filling 
C  trailing blanks.  If input string is blank, no change is made.
C
C  Portable (f77) subroutine version of f95's ADJUSTL() instrinsic function.
C
C Input:
C   CHR ...  Arbitrary length input string
C
C Output
C   CHR ...  Left-adjusted version of input string
C
C Modification History:
C
C  DATE         Who  Change
C  -----------  ---  -------------------------------------------------------
C  2013-Jul-15  JDG  Version 1.0
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
      INTEGER      L
C
C** Declare external functions
      INTEGER      LFTTRM 
C
C** Process string
      L= LFTTRM(CHR)
      IF ( L .LE. LEN(CHR) ) CHR(1:)= CHR(L:)
C
      END
