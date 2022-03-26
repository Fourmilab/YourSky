      SUBROUTINE DXCODLK( IREQ, NUMF, FCODE, FLAB, FLABL, LAB, L )
C
C** Declare dummy arguments
      INTEGER           IREQ, NUMF, FCODE(NUMF), FLABL(NUMF), L
      CHARACTER*8       FLAB(NUMF), LAB
C
C------------------------------------------------------------------------------
C DXCODLK -- DastcomX CODe LooK-up. Given a database integer field-code, and
C  arrays that define the codes, labels, and label lengths, look-up the
C  specified code in the tables and return the field label (name) and label 
C  length.
C
C Inputs:
C  IREQ .... Integer field-code to look up
C  NUMF .... Number of entries in the look-up table
C  FCODE ... Table of monotonically decreasing/increasing integer codes
C             in array of length NUMF. If 'curdef.inc' list is out of 
C             numerical order, search may fail.
C  FLAB .... Table of corresponding field labels in array of length NUMF
C  FLABL ... Table of field label lengths in array of length NUMF
C
C Output (if match is found; if no match, LAB and L are unchanged):
C  LAB ..... Field label/name for IREQ
C  L ....... Character length of field label/name LAB
C
C Called by DXLBL. Should not be called directly by users.
C  
C Modification History:
C
C  DATE         Who   Change
C  -----------  ----  ---------------------------------------------------------
C  2013-Jul-15  JDG   Version 1.0
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
      INTEGER           J, J1, J2, J3
C
C** Initialize search bounds
      J1= 0
      J3= NUMF + 1
C
C** Begin table search loop
 10   IF ( J3-J1 .GT. 1 ) THEN
       J2= (J3 + J1) / 2 
       IF ( (FCODE(NUMF).GT.FCODE(1)) .EQV. (IREQ.GT.FCODE(J2)) ) THEN
        J1= J2
       ELSE
        J3= J2
       END IF
       GOTO 10
      END IF
C
C** Index J is such that FCODE(J) <= IREQ < FCODE(J+1)
      J= J1
C
C** Make assignments if match found. Otherwise leave default values.
C
C   The additional tests allow for J= 0 if IREQ prior to table. If IREQ is
C   after the table, J= NUMF, as it also is when IREQ is last entry in table.
C
      IF ( (J .GE. 1) .AND. (FCODE(J) .EQ. IREQ) ) THEN
       LAB= FLAB(J)
       L= FLABL(J)
      END IF
C
      END
