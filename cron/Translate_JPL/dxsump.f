      SUBROUTINE DXSUMP
      IMPLICIT NONE
C
C------------------------------------------------------------------------------
C DXSUMP -- DastcomX SUMmary Print. Generate a summary of databases currently 
C  open in DXREAD and send to standard output.
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
      INTEGER      IVERS, K, K0, IBND(6), IBIAS(3), ITOT(3)
      LOGICAL      LFLG(2)
      CHARACTER*1  FILNM(2)*256, DBBYT(2), ST*282, CAL(2)*19
      REAL*8       JD(2)
C
C** Declare external functions
      INTEGER      LENTRM
C
C** Retrieve information about currently open database(s)
      CALL DXDAT1( IVERS )
      CALL DXDAT3( LFLG  )
      CALL DXDAT2( FILNM )
      CALL DXDAT4( DBBYT )
      CALL DXDAT5( CAL )
      CALL DXDAT6( JD )
C
C** Summarize opened database(s)
C
C~~ DASTCOM3 or DASTCOM4 (single file w/ comets and asteroids)
      K0= LENTRM( FILNM(1) )
      IF ( LFLG(1) .AND. (IVERS .LT. 5) ) THEN
       ST= 'Loaded asteroid & comet database "'//FILNM(1)(1:K0)//'"'
       WRITE(6,'(/A)') ST(1:LENTRM(ST))
       ST= ' Vers: DASTCOM? (??????-endian)   Date: undefined'
       WRITE(ST(15:15),'(I1)') IVERS
       IF ( DBBYT(1) .EQ. 'l' ) ST(17:31)= '(little-endian)'
       IF ( DBBYT(1) .EQ. 'b' ) ST(17:31)= '   (big-endian)'
       IF ( LENTRM(CAL(1)) .NE. 0 ) THEN
        WRITE(ST(35:),'(A,F14.6,A)')
     &   'Date: '//CAL(1)//'  (JD=',JD(1),')'
       END IF
       WRITE(6,'(A)') ST(1:80)
      END IF
C
C~~ DASTCOMX+ (possibly two files)
      IF ( IVERS .GE. 5 ) THEN
C
C~~ DASTX summary
       IF ( LFLG(1) ) THEN
        ST= 'Loaded ASTEROID database "'//FILNM(1)(1:K0)//'"'
        WRITE(6,'(/A)') ST(1:LENTRM(ST))
        ST= ' Vers: DAST? (??????-endian)      Date: undefined'
        WRITE(ST(12:12),'(I1)') IVERS
        IF ( DBBYT(1) .EQ. 'l' ) ST(14:28)= '(little-endian)'
        IF ( DBBYT(1) .EQ. 'b' ) ST(14:28)= '(   big-endian)'
        WRITE(ST(35:),'(A,F14.6,A)')
     &   'Date: '//CAL(1)//'  (JD=',JD(1),')'
        WRITE(6,'(A)') ST(1:80)
       END IF
C
C~~ DCOMX summary
       IF ( LFLG(2) ) THEN
        K0= LENTRM( FILNM(2) )
        ST= 'Loaded COMET    database "'//FILNM(2)(1:K0)//'"'
        WRITE(6,'(/A)') ST(1:LENTRM(ST))
        ST= ' Vers: DCOM? (??????-endian)      Date: undefined'
        WRITE(ST(12:12),'(I1)') IVERS
        IF ( DBBYT(2) .EQ. 'l' ) ST(14:28)= '(little-endian)'
        IF ( DBBYT(2) .EQ. 'b' ) ST(14:28)= '(   big-endian)'
        WRITE(ST(35:),'(A,F14.6,A)')
     &   'Date: '//CAL(2)//'  (JD=',JD(2),')'
        WRITE(6,'(A)') ST(1:80)
       END IF
      END IF
C
C~~ Summarize available dataset
      CALL DXBND( IBND, IBIAS )
      ITOT(1)= IBND(1)-IBND(4)+1 ! # of numbered asteroid records
      ITOT(2)= IBND(2)-IBND(5)+1 ! # of unnumbered asteroid records
      ITOT(3)= IBND(3)-IBND(6)+1 ! # of comet records
      ST= 'Object_records: XXXXXXXX Object_type   : '//
     &    'Logical_record_span    Total Rec_offset'
      WRITE(ST(17:24),'(I8)') ITOT(1)+ITOT(2)+ITOT(3)
      WRITE(6,'(/A)') ST(1:80)
      K= 26
      ST= ' '
      ST(K:)='Numbered   ast:  XXXXXXXX-XXXXXXXX  XXXXXXXX   XXXXXXXX'
      WRITE(ST(43:),'(I8,A,I8,2X,I8,3X,I8)')
     & IBND(4),'-',IBND(1),ITOT(1),IBIAS(1)
      WRITE(6,'(A)') ST(1:80)
      ST= ' '
      ST(K:)='Unnumbered ast:  XXXXXXXX-XXXXXXXX  XXXXXXXX   XXXXXXXX'
      WRITE(ST(43:),'(I8,A,I8,2X,I8,3X,I8)')
     & IBND(5),'-',IBND(2),ITOT(2),IBIAS(2)
      WRITE(6,'(A)') ST(1:80)
      ST= ' '
      ST(K:)='Comet         :  XXXXXXXX-XXXXXXXX  XXXXXXXX   XXXXXXXX'
      WRITE(ST(43:),'(I8,A,I8,2X,I8,3X,I8)')
     & IBND(6),'-',IBND(3),ITOT(3),IBIAS(3)
      WRITE(6,'(A)') ST(1:80)
C
      END
