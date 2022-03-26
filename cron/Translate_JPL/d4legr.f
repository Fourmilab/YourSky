      SUBROUTINE D4LEGR
     & ( U4,PREC,ZONE,SWAP4,I1P,I2P,I4P,R4P,R8P,CXP,ERRMS,ISTAT )
      IMPLICIT NONE
C
C** Declare  arguments. Older compilers that don't support
C** "INTEGER*1" may support alternatives "LOGICAL*1", or "BYTE"
C      BYTE           I1P(*)
C      LOGICAL*1      I1P(*)
      INTEGER*1      I1P(*)
      INTEGER        U4, PREC, ISTAT
      LOGICAL        ZONE(3), SWAP4
      INTEGER*2      I2P(*)
      INTEGER*4      I4P(*)
      REAL*4         R4P(*)
      REAL*8         R8P(*)
      CHARACTER*(*)  ERRMS
C
C** Include standard array definitions.
      INCLUDE 'dxparms.inc'
C
C     ENTRY D4CASGN( CXP, ZONE, CHOUT )
C     CHARACTER*(*)  CHOUT(*)
      CHARACTER*1    CHOUT*(NUMCH*80)
C
C------------------------------------------------------------------------------
C D4LEGR - DASTCOM4 LEGacy Reader subroutine. Reads a DASTCOM4 physical record 
C  and returns record values slotted into DASTCOMX array assignments, as if 
C  they had been filled from a DASTCOMX read (with gaps for DX data items not 
C  existing in D4).  Also fills in parameter array slots with supplemental
C  quantites derived from the values physically stored or parsed from comment
C  fields.
C
C  Database file is assumed open with correct record size, attached to U4, 
C  and ready for read of valid PREC.
C
C  Called by DXREAD. Users should not directly call this subroutine.
C
C Inputs:
C   U4   .... Unit number for opened database to be read
C   PREC .... Physical record number to read
C   ZONE .... Logical flag array indicating database zone of PREC
C   SWAP4 ... Logical flag to swap byte-order on DASTCOM4
C
C Output:
C   I1P ..... Array for returning INTEGER*1 values
C   I2P ..... Array for returning INTEGER*2 values
C   I4P ..... Array for returning INTEGER*4 values
C   R4P ..... Array for returning REAL*4 values
C   R8P ..... Array for returning REAL*8 values
C   CXP ..... DX character block for returning D4 character parameters
C              Should be of length CHARACTER*226 or larger.
C   ERRMS ... Error message string
C   ISTAT ... Error code
C              0= no error
C             -3= PREC doesn't match record internal NO value (corrupt file?)
C             -5= Empty place-holder record (skip)
C             -6= Invalid LSRC (corrupt record) 
C              x= FORTRAN error code
C------------------------------------------------------------------------------
C D4CASGN -- DASTCOM4 Character ASsiGNment. Entry points takes a standard 
C  DASTCOMX formatted object character-data block variable and returns a 
C  legacy D3READ formatted object character-data block.
C 
C Inputs:
C  CXP ...... Standard character-data block in DASTCOMx format
C  ZONE() ... Logical array indicating type of object
C              ZONE(1)= .TRUE. for IAU-numbered asteroids
C              ZONE(2)= .TRUE. for unnumbered asteroids
C              ZONE(3)= .TRUE. for comets
C             Only one element should be .TRUE., the others .FALSE.
C  
C Outputs: 
C  CHOUT() .. Output character-variable block in legacy D3READ format.
C              Passed in as an empty (space-filled) array that is
C              over-addressed to make assignments.
C 
C Called by DXREAD. Users should not directly call this entry-point.
C
C Modification History:
C
C  DATE         Who  Change
C  -----------  ---  ----------------------------------------------------------
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
C------------------------------------------------------------------------------
C
C** Declare local variables, generally for type or size proxy-conversion 
C   between DASTCOM4 record and DASTCOM5 usage.
      CHARACTER*6    SPTYPE
      CHARACTER*256  FNAM
      REAL*8         PYR, R8T, OBSTRT, OBSTOP
      REAL*4         R4T
      INTEGER*4      UPARM, LSRC, I, RCL, NR
C
C** Original D4 reader data-type and size declarations
C
C      INTEGER        NO, NOBS, UPARM, LSRC
C      REAL*8         EPOCH, CALEPO, RMO, RNODE, OM, RI, E, A, PERJD
C      REAL*8         PERCAL, Q, OBSTRT, OBSTOP, SOLDAT, SRC(55)
C      REAL*8         SIG1, LEAK1, SLEAK1, LEAK5, SLEAK5, PYR
C      REAL           GM, RAD, H, G, BVT, RP, ALBEDO, RMSW, RMSH, MOID
C      REAL           OQP, CEU, A1, A2, A3, DT, M1, M2
C      CHARACTER*1    ASTNAM*18, EQUNOX*4, IREF*8, SPTYPE*6
C      CHARACTER*1    DARC*9, COMNT1*41, COMNT4*80, SBNAM*12, PENAM*6
C      CHARACTER*1    COMNAM*24, COMNT3*49, DESIG*13, EST*6
C
C** Initialize
      ERRMS= ' '
C
C~~ COMET
C
C Original D4 reader comet record-read statement:
C        READ( U, REC=L1, IOSTAT=STAT) 
C     +        NO,COMNAM,EPOCH,CALEPO,EQUNOX,IREF,DESIG,
C     +        RMO,RNODE,OM,RI,E,A,
C     +        PERJD,PERCAL,Q,GM,RAD,A1,A2,
C     +        M1, M2, PYR, DARC, COMNT3, COMNT4,
C     +        SBNAM, PENAM, RMSW, MOID, UPARM,
C     +        OBSTRT, OBSTOP, SOLDAT, A3, DT, EST,
C     +        LSRC, (SRC(I),I=1,55)
C
C    load: R8P(1-11,13,99-153),R4P(3-4,8-11,32-33,37,42),I4P(1,3-4),I2P(1),
C          I1P(7-8),CHOUT(1-3,6,9-14)
C
      IF ( ZONE(3) ) THEN
       READ( U4, REC=PREC, IOSTAT= ISTAT)
C                NO,       COMNAM,  EPOCH, CALEPO,  EQUNOX,
     &       I4P(1), CXP(198:221), R8P(1), R8P(2), CXP(1:4),
C
C                    IREF,        DESIG,
     &       CXP(188:195), CXP(161:173),
C
C               RMO,  RNODE,     OM,     RI,      E,      A,
     &       R8P(3), R8P(4), R8P(5), R8P(6), R8P(7), R8P(8),
C
C              PERJD,  PERCAL,      Q,      GM,    RAD,     A1,     A2,
     &       R8P(10), R8P(11), R8P(9), R4P(32),R4P(33), R4P(8), R4P(9),
C
C                M1,     M2, PYR,       DARC,     COMNT3,      COMNT4,
     &       R4P(3), R4P(4), PYR, CXP(23:31), CXP(32:80), CXP(81:160),
C
C                 SBNAM,     PENAM,    RMSW,    MOID, UPARM,
     &       CXP(11:22), CXP(5:10), R4P(42), R4P(37), UPARM,
C
C             OBSTRT,  OBSTOP,  SOLDAT,      A3,      DT,          EST,
     &        OBSTRT,  OBSTOP, R8P(13), R4P(10), R4P(11), CXP(174:179),
C
C            LSRC, (SRC(I),I= 1,55)
     &       LSRC, (R8P(I),I=99,153)
C
C** Check for error
       IF ( ISTAT .NE. 0 ) THEN
        WRITE( ERRMS, '(A,I8,A,I5)') 'D4LEGR: requested PREC= ',PREC,
     &   ' record failed on read, ISTAT= ',ISTAT
        RETURN
       END IF
C
C** Check LSRC for problem
       IF ( SWAP4 ) CALL I4SWAP( 1, LSRC ) 
       I= -0.5 + SQRT(1.+8.*LSRC) / 2
       IF ( (LSRC .NE. 0) .AND. ((I .LT. 6) .OR. (I .GT. 10)) ) THEN
        ISTAT= -6
        WRITE( ERRMS, '(A,I8,A,I5)') 'D4LEGR: requested PREC= ',PREC,
     &   ' has invalid or corrupt LSRC= ',LSRC
        RETURN
       END IF
C
C** Switch byte-order on the rest if necessary
       IF ( SWAP4 ) THEN
        CALL I4SWAP(     1,     I4P ) ! I4P( 1)
        CALL I4SWAP(     1,   UPARM ) ! Holding variable
        CALL R8SWAP(     1,  OBSTRT ) ! Holding variable
        CALL R8SWAP(     1,  OBSTOP ) ! Holding variable
        CALL R8SWAP(    11,     R8P ) ! R8P( 1-11)
        CALL R8SWAP(     3, R8P(13) ) ! R8P(13-15)
        IF ( LSRC .GT. 0 )
     &   CALL R8SWAP( LSRC, R8P(99) ) ! R8P(99~153), SRC (10 parameter)
        CALL R8SWAP(     1,     PYR ) ! Holding variable
        CALL R4SWAP(     2,  R4P(3) ) ! R4P( 3- 4)
        CALL R4SWAP(     4,  R4P(8) ) ! R4P( 8-11)
        CALL R4SWAP(     2, R4P(32) ) ! R4P(32-33)
        CALL R4SWAP(     1, R4P(37) ) ! R4P(37)
        CALL R4SWAP(     1, R4P(42) ) ! R4P(42)
       END IF
C
C** Check for empty record. Should not exist, but allow for it.
       IF ( (R8P(1).EQ.0.D0) .OR. (R8P(1).EQ.1.D0) .OR.
     &      (R8P(2).EQ.0.D0) .OR. (R8P(9).EQ.0.D0) ) THEN
        ISTAT= -5
        WRITE( ERRMS, '(A,I8,A)') 'D4LEGR: requested PREC= ',PREC,
     &   ' is place-holder record (non-fatal)'
        RETURN
       END IF
C
C** Reassign holding variables for changed-size/type cases
       I4P(3)= INT(OBSTRT) ! Convert from D4-stored REAL*8 to DX I*4
       I4P(4)= INT(OBSTOP) ! Convert from D4-stored REAL*8 to DX I*4
       I2P(1)= INT(PYR)    ! Convert from D4-stored REAL*8 to DX I*2
       I1P(7)= UPARM       ! Convert from D4-stored INTEGER*4 to DX I*1
       I1P(8)= LSRC        ! Convert from D4-stored INTEGER*4 to DX I*1
      ELSE
C
C~~ ASTEROID
C
C Original D4 reader asteroid record-read statement:
C
C         READ( U, REC=L1, IOSTAT=STAT )
C     +       NO,ASTNAM,EPOCH,CALEPO,EQUNOX,IREF,DESIG,
C     +       RMO,RNODE,OM,RI,E,A,
C     +       PERJD,PERCAL,Q,GM,RAD,H,G,BVT,
C     +       RP,ALBEDO,SPTYPE,DARC,NOBS,COMNT1,COMNT2,
C     +       SBNAM,PENAM,RMSW,RMSH,MOID,UPARM,
C     +       OBSTRT,OBSTOP,SOLDAT,
C     +       LSRC,(SRC(I),I=1,21),
C     +       OQP,CEU,SIG1,LEAK1,SLEAK1,LEAK5,SLEAK5
C
C    load D5: R8P(1-11,13,99-143),R4P(1-2,31-33,37-39,42,46),I4P(1-4),
C             I1P(7-8),CHOUT(1-8,11-14)
C
       SPTYPE= ' '
C      READ( U4, REC=PREC, IOSTAT= ISTAT )
       READ( U4, REC=PREC )
C               NO,       ASTNAM,  EPOCH, CALEPO,   EQUNOX,
     &      I4P(1), CXP(194:211), R8P(1), R8P(2), CXP(1:4),
C
C                   IREF,        DESIG, 
     &      CXP(184:191), CXP(163:175),
C
C              RMO,  RNODE,     OM,     RI,      E,      A,
     &      R8P(3), R8P(4), R8P(5), R8P(6), R8P(7), R8P(8),
C
C             PERJD,  PERCAL,      Q,      GM,     RAD,      H,      G,
     &      R8P(10), R8P(11), R8P(9), R4P(32), R4P(33), R4P(1), R4P(2), 
C
C               BVT,
     &      R4P(39),
C
C                RP,  ALBEDO,  SPTYPE,       DARC,   NOBS,     COMNT1,
     &      R4P(31), R4P(38),  SPTYPE, CXP(33:41), I4P(2), CXP(42:82),
C
C                COMNT2,
     &      CXP(83:162),
C
C                SBNAM,     PENAM,    RMSW,    RMSH,    MOID, UPARM,
     &      CXP(11:22), CXP(5:10), R4P(42), R4P(46), R4P(37), UPARM,
C
C            OBSTRT,  OBSTOP,  SOLDAT, LSRC, (SRC(I),I=1,21),
     &       OBSTRT,  OBSTOP, R8P(13), LSRC, (R8P(I),I=99,119),
C
C           OQP, CEU, SIG1, LEAK1, SLEAK1, LEAK5, SLEAK5
     &      R4T, R4T,  R8T,   R8T,    R8T,   R8T,    R8T
C
C** Check for errors
       IF ( ISTAT .NE. 0 ) THEN
        WRITE( ERRMS, '(A,I8,A,I5)') 'D4LEGR: requested PREC= ',PREC,
     &   ' record failed on read, ISTAT= ',ISTAT
        RETURN
       END IF
C
C** Check LSRC for problem
       IF ( SWAP4 ) CALL I4SWAP( 1, LSRC ) 
       I= -0.5 + SQRT(1.+8.*LSRC) / 2
       IF ( (LSRC .NE. 0) .AND. (I .NE. 6) ) THEN
        ISTAT= -6
        WRITE( ERRMS, '(A,I8,A,I5)') 'D4LEGR: requested PREC= ',PREC,
     &   ' has invalid or corrupt LSRC= ',LSRC
        RETURN
       END IF
C
C** Switch byte-order on the rest if necessary
       IF ( SWAP4 ) THEN
        CALL I4SWAP(     2,     I4P ) ! I4P( 1- 2)
        CALL I4SWAP(     1,   UPARM ) ! Holding variable
        CALL R8SWAP(     1,  OBSTRT ) ! Holding variable
        CALL R8SWAP(     1,  OBSTOP ) ! Holding variable
        CALL R8SWAP(    11,     R8P ) ! R8P( 1-11)
        CALL R8SWAP(     3, R8P(13) ) ! R8P(13-15)
        IF ( LSRC .GT. 0 )
     &   CALL R8SWAP( LSRC, R8P(99) ) ! R8P(99~119), SRC (0,6 parameter)
        CALL R4SWAP(     2,  R4P(1) ) ! R4P( 1- 2)
        CALL R4SWAP(     3, R4P(31) ) ! R4P(31-33)
        CALL R4SWAP(     3, R4P(37) ) ! R4P(37-39)
        CALL R4SWAP(     1, R4P(42) ) ! R4P(42)
        CALL R4SWAP(     1, R4P(46) ) ! R4P(46)
       END IF
C
C** Check for empty record. Should not exist, but allow for it.
       IF ( (R8P(1).EQ.0.D0) .OR. (R8P(1).EQ.1.D0) .OR.
     +      (R8P(2).EQ.0.D0) .OR. (R8P(9).EQ.0.D0) ) THEN
        ISTAT= -5
        WRITE( ERRMS, '(A,I8,A)') 'D4LEGR: requested PREC= ',PREC,
     &   ' is place-holder record (non-fatal)'
        RETURN
       END IF
C
C** Reassign holding variables for changed-size/type cases
       CXP(28:32)= SPTYPE(1:5) ! Trim trailing space in D4 record for DX
       I4P(3)= INT(OBSTRT)     ! Convert from D4-stored REAL*8 to DX I*4
       I4P(4)= INT(OBSTOP)     ! Convert from D4-stored REAL*8 to DX I*4
       I1P(7)= UPARM           ! Convert from D4-stored INTEGER*4 to DX I*1 
       I1P(8)= LSRC            ! Convert from D4-stored INTEGER*4 to DX I*1
      END IF
C
C** Fill out arrays with derived parameters and parameters from comment fields
      CALL DXSUPLQ( ZONE,R8P,R4P,I4P,I2P,I1P,CXP,ERRMS,ISTAT )
C
      RETURN
C
C------------------------------------------------------------------------------
C
      ENTRY D4CASGN( CXP, ZONE, CHOUT )
C
      IF ( ZONE(3) ) THEN
       CHOUT(1:24)   = CXP(198:221)   ! COMNAM (POSSIBLY TRUNCATED NAME)
       IF ( CXP(222:222) .NE. ' ' ) CHOUT(24:24)= '+'
       CHOUT(25:37)  = CXP(161:173)   ! DESIG
       IF ( INDEX(CXP(188:190),'MPO') .NE. 0 .AND.
     &           CXP(196:196) .NE. ' ') THEN
        CHOUT(38:45)= 'MP'//CXP(191:196) ! IREF (trim MPO->MP, poss. trunc.)
       ELSE
        CHOUT(38:45)= CXP(188:195)       ! IREF (possible truncation)
       END IF
       CHOUT(46:49)  = CXP(1:4)       ! EQUNOX
C      CHOUT(50:55)  = ' '            ! (blank; SPTYPS placeholder)
       CHOUT(56:64)  = CXP(23:31)     ! DARC
       CHOUT(65:113) = CXP(32:80)     ! COMNT3
       CHOUT(114:193)= CXP(81:160)    ! COMNT2 (DX COMNT4)
       CHOUT(194:205)= CXP(11:22)     ! SBNAM
       CHOUT(206:211)= CXP(5:10)      ! PENAM
       CHOUT(212:217)= CXP(174:179)   ! COMEST (POSSIBLY TRUNCATED LIST)
       IF ( CXP(180:180) .NE. ' ' ) CHOUT(217:217)= '+'
      ELSE
       CHOUT(1:24)   = CXP(194:211)   ! ASTNAM
       CHOUT(25:37)  = CXP(163:175)   ! DESIG
       IF ( INDEX(CXP(184:186),'MPO') .NE. 0 .AND.
     &            CXP(192:192) .NE. ' ') THEN
        CHOUT(38:45)= 'MP'//CXP(187:192) ! IREF (trim MPO->MP, poss. trunc.)
       ELSE
        CHOUT(38:45)= CXP(184:191)       ! IREF (possible truncation)
       END IF
       CHOUT(46:49)  = CXP(1:4)       ! EQUNOX
       CHOUT(50:55)  = CXP(28:32)     ! SPTYPS
       CHOUT(56:64)  = CXP(33:41)     ! DARC
       CHOUT(65:105) = CXP(42:82)     ! COMNT1
       CHOUT(106:185)= CXP(83:162)    ! COMNT2
       CHOUT(186:197)= CXP(11:22)     ! SBNAM
       CHOUT(198:203)= CXP(5:10)      ! PENAM
      END IF
C
      END
