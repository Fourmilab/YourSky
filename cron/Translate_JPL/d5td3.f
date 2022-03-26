      PROGRAM D5tD3
      IMPLICIT NONE
C
C-----------------------------------------------------------------------------
C D5tD3 -- Convert DASTCOM5 database to DASTCOM3 (big & little endian forms). 
C  DASTCOM4 output is also supported (historically used internally at JPL).
C
C Inputs
C  Optional file 'd5td3.inp' is read if present, else drops into command-line
C  input mode. 
C
C Output
C  Requested binary database files
C
C USAGE: 
C  
C  Place commands below in './d5td3.inp' or enter at the 'd5td3>' prompt.
C  Directives are case insensitive, but file-names are not.
C
C     Directive [specification] Meaning
C     ------------------------  -----------------------------------------------
C     DAST5 [input  path/file]  REQUIRED ... D5 asteroid database input file
C     DCOM5 [input  path/file]  REQUIRED ... D5 comet database input file
C     D3LE  [output path/file]  OPTIONAL ... D3 output file (little-endian)
C     D3BE  [output path/file]  OPTIONAL ... D3 output file (big-endian)
C     D3NA  [output path/file]  OPTIONAL ... D3 output (native; big OR little)
C     D4LE  [output path/file]  OPTIONAL ... D4 output file (little-endian)
C     D4BE  [output path/file]  OPTIONAL ... D4 output file (big-endian)
C     D4NA  [output path/file]  OPTIONAL ... D4 output (native; big OR little)
C     "" (carriage return)      Begin converting specified DASTCOM5 files
C     CNVRT                     Begin converting specified DASTCOM5 files
C
C NOTES:
C
C  #1) Both DAST5 and DCOM5 must be specified and exist.
C
C  #2) If no output file is specified (no D3LE, D3BE, or D3NA, etc.), a 
C      native-endian file for the executing platform called DASTCOM3 will 
C      be created in the current working directory, ./DASTCOM3
C
C  #3) If a specified output file exists, it will be overwritten if possible.
C      If over-write is not possible, the conversion will halt. It is
C      recommended files be created in a staging location, then moved to a 
C      final location after this conversion program completes to avoid
C      over-writing an "operational" file.
C
C  #4) Both D3LE and D3BE output can be specified -- in which case two files
C      will be created by the same run, one in each byte-order form.
C
C  #5) If D3NA output is specified, it will replace the corresponding 
C      D3LE or D3BE specification. If both types remain specified at the 
C      start of execution, two files will be created. Otherwise, just the 
C      single file given as input D3NA.
C
C  #6) Each line of input is read until the first blank line is encountered.
C      The blank input will trigger execution based on what has been input 
C      up to that point. If no blank line is encountered, keyboard input 
C      mode will start. A blank "return" will start conversion or additional 
C      set-up commands may be entered manually.
C
C Modification History:
C
C  DATE         Who  Change
C  -----------  ---  -------------------------------------------------------
C  2013-Jul-15  JDG  Version 1.0 (DASTCOM5 conversion)
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
C----------------------------------------------------------------------------
C
C** Declare program variables
      CHARACTER*256     DAST5, DCOM5, D3LE, D3BE, D4LE, D4BE, D3NA, D4NA
      CHARACTER*256     DBNAM, OLIN, LINE
      CHARACTER*1       TYP*1
      INTEGER           U4, U3L, U3B, IBND(6), IBIAS(3), ISTAT, P1, ISUB
      INTEGER           ILIT, IOS, I, J, K, I1, L2, STRT, FNSH 
      INTEGER           IU(4), DCHK, D3, D4, LOOP, IYR, IMN, IDY
      INTEGER*4         I4TMP(3), ITMP
      REAL*4            R4TMP(10)
      LOGICAL           MATCH, XTRA, SPACE, FINP, NONATV
C
C** D4 reader data-type and size declarations
      INTEGER*4         NO(2), NOBS(2), UPARM(2), LSRCS(2)
      REAL*8            EPOCH(2), CALEPO(2), RMO(2), RNODE(2), OM(2) 
      REAL*8            RI(2), E(2), A(2), PERJD(2), PERCAL(2), Q(2)
      REAL*8            OBSTRT(2), OBSTOP(2), SOLDAT(2), SRC(55,2)
      REAL*8            SIG1, LEAK1, SLEAK1, LEAK5, SLEAK5, PYR(2)
      REAL*4            GM(2), RAD(2), H(2), G(2), BVT(2), RP(2)
      REAL*4            ALBEDO(2), RMSW(2), RMSH(2), MOID(2), OQP, CEU
      REAL*4            A1(2), A2(2), A3(2), DT(2), M1(2), M2(2)
      CHARACTER*1       ASTNAM*18, EQUNOX*4, IREF*8, SPTYPE*6
      CHARACTER*1       DARC*9, COMNT1*41, COMNT2*80, SBNAM*12, PENAM*6
      CHARACTER*1       COMNAM*24, COMNT3*49, DESIG*13, ESTL*6
C
C** D5 reader data-type and size declaration. 
      DOUBLE PRECISION  R8OUT(99)
      CHARACTER*1       FNAM(2)*256, CERRMS*340, CHOUT*217
      INTEGER           IR8ORD(99), NR8, ICHORD(1), NCH, LERR 
      INTEGER           IVERS, LREC, PREC, IZONE 
      INTEGER*4         LSRC
      LOGICAL           BUF, WARN
      DATA              NR8, NCH  /     99,       1 /
      DATA              BUF, WARN / .TRUE., .FALSE. /
C
C** Additional D4 header variables
      INTEGER*4         IBIAS1(2), IBIAS2(2) 
      CHARACTER*1       ENDPT*18, BEGINP*18, FILLER*105
      CHARACTER*1       CAL(2)*19
      DOUBLE PRECISION  HEPOCH, HCALEPO, JD(2)
C
C** Declare local parameters.
      INTEGER           RECL3, RECL4
      PARAMETER( RECL3= 395 )   !DASTCOM3 record length
      PARAMETER( RECL4= 823 )   !DASTCOM4 record length
C
C** Declare local parameters 
      INTEGER          NCMD
      PARAMETER( NCMD= 9 )   ! Max. number of D5tD3 program directives
      CHARACTER*5      CMD(NCMD)
      INTEGER          CMDL(NCMD), DLEN(NCMD-1)
C
C** Declare external functions
      DOUBLE PRECISION GREG2JD, JUL2JD
      INTEGER          LENTRM, LFTTRM
      LOGICAL          LTLEND
C
C** Store recognized program directives
      DATA  CMD /
     & 'DAST5', 'DCOM5', 'D3LE', 'D3BE', 'D3NA', 'D4LE', 'D4BE', 'D4NA',
     & 'CNVRT' /
C CMDL|      5,       5,      4,      4,      4,      4,      4,      4,
C    &       5 /
C
      DATA  CMDL /
C CMD |'DAST5', 'DCOM5', 'D3LE', 'D3BE', 'D3NA', 'D4LE', 'D4BE', 'D4NA',
C CMD |'CNVRT' /
     &       5,       5,      4,      4,      4,      4,      4,      4, 
     &       5 /   
C
C** Set default unit numbers for the possible database files
      DATA IU    / 31, 32, 41, 42 /
C
C** Set constant values
      DATA OQP,CEU                        /0.0,0.0/
      DATA SIG1,LEAK1,SLEAK1,LEAK5,SLEAK5 /0.D0,0.D0,0.D0,0.D0,0.D0/
      DATA HEPOCH,HCALEPO                 /0.D0,0.D0/
C
C** Determine if there is a default input file.  The default input file 
C   can specify the DB files without having to drop into keyboard input. 
C   But they can be specified by keyboard input also.
C
C   Input must contain the full path for a DAST5 and DCOM5 set of files.
C   It may optionally contain the output path
C
      FINP= .FALSE.
      OPEN( UNIT=7, FILE='d5td3.inp', STATUS='OLD', IOSTAT= IOS)
      IF ( IOS .EQ. 0 ) FINP= .TRUE.
C
C** Initialize
      DAST5 = ' '
      DCOM5 = ' '
      D3LE  = ' '
      D3BE  = ' '
      D3NA  = ' '
      D4LE  = ' '
      D4BE  = ' '
      D4NA  = ' '
      D3    = 0
      D4    = 0
C
C~~ Determine executing systems native byte-order
      ILIT= 2
      IF ( LTLEND() ) ILIT= 1
C
C** Read character string from standard input, supporting keyboard and 
C   input redirection. Uses f95+ functionality via ADVANCE option. An
C   alternative for Solaris f77 is '(A,$)'.
C
C   Get new user-input line from file or standard input.
 10   LINE= ' '
      IF ( FINP ) THEN
       READ( 7, '(A)', IOSTAT=IOS ) LINE
       IF ( IOS .NE. 0 ) THEN
        CLOSE( 7 )
        FINP= .FALSE.
       END IF
      END IF
      IF ( .NOT. FINP ) THEN
       WRITE( 6, '(A)', ADVANCE='NO') 'd5td3> '
       READ(  5, '(A)', END=20, IOSTAT= ISTAT ) LINE
       IF ( ISTAT .NE. 0 ) THEN
        WRITE( 0, '(A)' ) 'Unrecognized input within line'
        GOTO 10 ! Get new user-input line
       END IF
      END IF
C
C** Test and initially parse user-input line
      L2= LENTRM(LINE)            ! Position of last non-blank character
      IF ( L2 .EQ. 0 ) GOTO 20    ! Blank input line, begin processing
      CALL LADJUST( LINE )        ! Left justify input line
      L2= LENTRM(LINE)            ! New position of last non-blank character
C
C** Process system commands immediately
      IF ( LINE(1:1) .EQ. '!' ) THEN
       CALL SYSTEM( LINE(2:L2) )
       GOTO 10
      END IF
C
C** Continue processing input.
C~~ Convert case of directive (first symbols on line) but preserve original
      OLIN= LINE
      I1= INDEX( LINE, ' ' ) - 1  ! Position of last char. prior to first blank
      CALL UCASE3( LINE(1:I1) )
C
C** Check for explicit execution request
      J= MIN( CMDL(NCMD), I1 )
      IF ( CMD(NCMD)(1:J) .EQ. LINE(1:J) ) GOTO 20 
C
C** Extract and store user-input line's file name
      DBNAM= OLIN(I1+1:)
      CALL LADJUST( DBNAM ) 
C
C** Check for missing data
      K= LENTRM( DBNAM )
      IF ( K .LT. 1 ) THEN
       WRITE(0,'(A)') 'IGNORED (incomplete): '//OLIN(1:L2)
       GOTO 10 ! Get new user-input line
      END IF
C
C** If directive is present, process & make assignments
      MATCH= .FALSE.
      DO I= 1, NCMD-1
       J= MIN( CMDL(I), I1 )
       IF ( CMD(I)(1:J) .EQ. LINE(1:J) ) THEN 
        MATCH= .TRUE.
C
C** Assign file names
        IF ( I .EQ. 1 ) THEN
         DAST5= DBNAM(1:K)
        ELSE IF ( I .EQ. 2 ) THEN
         DCOM5= DBNAM(1:K)
        ELSE IF ( I .EQ. 3 ) THEN
         D3LE = DBNAM(1:K)
        ELSE IF ( I .EQ. 4 ) THEN
         D3BE = DBNAM(1:K)
        ELSE IF ( I .EQ. 6 ) THEN
         D4LE = DBNAM(1:K)
        ELSE IF ( I .EQ. 7 ) THEN
         D4BE = DBNAM(1:K)
C
C~~ If D3-native requested, over-write any relevant prior specification
        ELSE IF ( I .EQ. 5 ) THEN
         D3NA = DBNAM(1:K)
         IF ( ILIT .EQ. 1 ) D3LE= D3NA
         IF ( ILIT .EQ. 2 ) D3BE= D3NA
C
C~~ If D4-native requested, over-write any relevant prior specification
        ELSE IF ( I .EQ. 8 ) THEN
         D4NA= DBNAM(1:K)
         IF ( ILIT .EQ. 1 ) D4LE= D4NA
         IF ( ILIT .EQ. 2 ) D4BE= D4NA
        END IF
       END IF
      END DO
C
C** Inform user if no match to command set
      IF (.NOT. MATCH) WRITE(0,'(A)') 'IGNORED (UNKNOWN): '//OLIN(1:L2)
      GOTO 10 ! Get new user-input line
C
C** Only arrive here if a blank line of input or "CNVRT" directive
 20   CONTINUE
C
C** Find & assign length of each (final) database input/output
      DO I= 1, NCMD-1
       DLEN(I)= 0
      END DO
      DLEN(1)= LENTRM( DAST5 )
      DLEN(2)= LENTRM( DCOM5 )
      DLEN(3)= LENTRM( D3LE  )
      DLEN(4)= LENTRM( D3BE  )
      DLEN(5)= LENTRM( D3NA  )
      DLEN(6)= LENTRM( D4LE  )
      DLEN(7)= LENTRM( D4BE  )
      DLEN(8)= LENTRM( D4NA  )
C
C** Set flag denoting request for a non-native output file
      NONATV= .FALSE.
      IF ( ((ILIT.EQ.1).AND.((DLEN(4)+DLEN(7)).NE.0)) .OR.
     &     ((ILIT.EQ.2).AND.((DLEN(3)+DLEN(6)).NE.0)) ) NONATV= .TRUE.
C
C** Check for required input (or simple exit with no action)
      IF ( DLEN(1) .EQ. 0 ) THEN 
       WRITE(0,'(A)') 'D5tD3: no DAST5 input specification, no action'
       STOP 
      ELSE IF ( DLEN(2) .EQ. 0 ) THEN
       WRITE(0,'(A)') 'D5tD3: no DCOM5 input specification, no action'
       STOP
      END IF
C
C** If no output files are assigned by user, set default output
      DCHK= 0
      DO I= 3, NCMD-1
       DCHK= DCHK + DLEN(I)
      END DO
      IF ( DCHK .EQ. 0 ) THEN
       IF ( ILIT .EQ. 1 ) THEN
        D3LE= './DASTCOM3_LE'
        DLEN(3)= LENTRM(D3LE)
        WRITE(0,'(A)') 
     &   'WARNING: output unspecified, defaulting to '//D3LE(1:DLEN(3))
       END IF
       IF ( ILIT .EQ. 2 ) THEN
        D3BE= './DASTCOM3_BE'
        DLEN(4)= LENTRM(D3BE)
        WRITE(0,'(A)') 
     &   'WARNING: output unspecified, defaulting to '//D3BE(1:DLEN(4))
       END IF
      END IF
C
C** All I/O files have been assigned
C
C** Assign flags
      D3= DLEN(3) + DLEN(4)
      D4= DLEN(6) + DLEN(7)
C
C** Initialize DASTCOM5 source files. Request DASTCOM4 legacy reader
C** return of arrays.
      IR8ORD(1)= -4
      FNAM(1)= DAST5
      FNAM(2)= DCOM5
      CALL DXINI( FNAM, IR8ORD, NR8, ICHORD, NCH, BUF, WARN, ISTAT )
      IF ( ISTAT .NE. 0 ) THEN
       CALL DXERR( CERRMS, LERR ) 
       WRITE( 0, '(A)' ) CERRMS(1:LERR)
       STOP
      END IF
C
C** Verify DASTCOM5 or higher was opened
      CALL DXDAT1( IVERS )
      IF ( IVERS .LT. 5 ) THEN
       WRITE(0,'(A,I1)') 'D5tD3: can only convert DASTCOM5 or higher,'//
     & ' supplied version= ',IVERS
       STOP
      END IF
C
C** Retrieve and check object zonal bounds
      CALL DXBND( IBND, IBIAS )
      I= IBND(1)-IBND(4)+IBND(2)-IBND(5)+IBND(3)-IBND(6)+4
      IF ( I .GT. 999999 ) THEN
       WRITE(0,'(A)') 
     & 'More than 999999 records in DASTCOM5 - conversion not possible.' 
       WRITE(0,'(A)') 'DASTCOM3 and DASTCOM4 formats are obsolete.'
       STOP
      ELSE IF ( I .GT. 989999 ) THEN
       WRITE(0,'(A)') 'WARNING: more than 989999 records in DASTCOM5.'
       WRITE(0,'(A)') 
     &  'DASTCOM3 and DASTCOM4 formats will soon be unavailable.'
       WRITE(0,'(A)') 'Upgrade to use DASTCOM5 as soon as possible.'
      END IF
C
C** Initialize common header data. DASTCOM3/4 header contains only 2 bias
C   parameters. They specify offset of unnumbered asteroid and comet zones.
C
C         physical_record = logical_record - bias
C                    bias = logical_record - physical_record
C
C   DASTCOM5 has 3 bias parameters (numbered, unnumbered, comet), with the
C   comet value being relative to the start of the second file (which has 
C   its own header record).
C
C   DASTCOM3/4:
C   -----------
C     z: |1   |2 |3  |     (zone)
C     l: 12   45689        (logical/user record number)
C       Hnn...uuucc...
C     p:12345678901234     (physical storage record)
C                1
C      (BIAS0= -1)
C       BIAS1= -3
C       BIAS2= -2 
C       
C   DASTCOM5
C   --------
C     z: |1   |2           (zone) 
C     l: 12   456          (logical/user record number)
C       Hnn...uuu
C     p:123456789          (physical storage record) 
C                
C     IBIAS(1)= -1
C     IBIAS(2)= -3
C     IBND(2)= 6
C
C     z:|3   |   
C     l: 89
C       Hcc... 
C     p:123456
C  
C     IBIAS(3)= 6
C
C To compute comet zone bias for the new D3/D4 files (with comets merged in)...
C   
C   The last physical asteroid record, X, in DAST5 (i.e. unnumbered ast) is:
C
C     X= IBND(2) - IBIAS(2)
C
C   ... therefore, the first comet physical record in DASTCOM3/4,"Y", will 
C   be the next physical record:
C
C     Y= X + 1 
C      = IBND(2) - IBIAS(2) + 1
C
C   The first comet logical record from DCOM5 (assumed immediately after 
C   header in physical record 2):
C
C     Z= 2 + IBIAS(3)
C
C   Logical record is the same in DASTCOM3/4 as in DCOM5, so the new comet 
C   zone bias for DASTCOM3/4 is ...
C
C     IBIAS2 = Z - Y 
C            = 2 + IBIAS(3) - (IBND(2) - IBIAS(2) + 1)
C            = 1 + IBIAS(3) - IBND(2) + IBIAS(2)
C
C HISTORICAL NOTE: 
C The old D3READ subroutine internally reverses the signs of the stored bias 
C parameters and computes prec= lrec + bias. This arrives at the same end 
C result. But be aware of this behavior if legacy code using the BIAS array 
C returned by D3INIT is adapted to use the unaltered output of modern DXBND 
C (for which prec= lrec - bias).
C
       CAL(1)= ' '
       CAL(2)= ' '
       ENDPT = ' '
       EQUNOX= ' '
       BEGINP= ' '
       FILLER= ' '
       COMNT2= ' '
       COMNT1= ' '
       WRITE(ENDPT, '(3I6)') IBND(1), IBND(2), IBND(3)
       WRITE(BEGINP,'(3I6)') IBND(4), IBND(5), IBND(6)
C
C** Retrieve date-stamps from DAST5, convert byte-order. 
C
C   Time/date is not part of the D3/D4 header definition, but added here
C   since the D3/D4 header space is not otherwise used.
C
       CALL DXDAT5( CAL )
       CALL DXDAT6( JD ) 
C
       FILLER(87:105)= CAL(1) ! Copy DAST5 date to undefined header space
       JD(2)= JD(1)           ! JD() now both native-endian DAST5 value
       I= 1 
       IF ( ILIT .EQ. 1 ) I= 2
       CALL R8SWAP( 1, JD(I) )
C
C** Store native byte-order form of bias parameters
       IBIAS1(ILIT)= IBIAS(2)
       IBIAS2(ILIT)= 1 + IBIAS(3) - IBND(2) + IBIAS(2)
C
C** Store swapped form of bias parameters
       I= 1
       IF ( ILIT .EQ. 1 ) I= 2
C
       ITMP= IBIAS1(ILIT)
       CALL I4SWAP( 1, ITMP )  
       IBIAS1(I)= ITMP
C
       ITMP= IBIAS2(ILIT)
       CALL I4SWAP( 1, ITMP )  
       IBIAS2(I)= ITMP
C
C** Open requested output files (over-writing if permitted) & write header.
C
C~~ DASTCOM3, little-endian (LE)
      IF ( DLEN(3) .NE. 0 ) THEN
       OPEN( IU(1), FILE=D3LE, STATUS='UNKNOWN', ACCESS='DIRECT',
     &       FORM='UNFORMATTED', RECL=RECL3, IOSTAT=ISTAT )
       CLOSE( IU(1), STATUS='DELETE' )
       OPEN( IU(1), FILE=D3LE, STATUS='NEW', ACCESS='DIRECT',
     &       FORM='UNFORMATTED', RECL=RECL3, IOSTAT=ISTAT )
       IF ( ISTAT .NE. 0 ) THEN
        WRITE(0,'(A)' ) 'D5tD3: IOSTAT error code ',ISTAT,
     &                  ' opening new D3LE '//D3LE(1:DLEN(3))
        STOP
       END IF
C
C~~ Complete and write out DASTCOM3 header (LE)
       FILLER(20:20) = 'A'
       WRITE( IU(1), REC= 1, IOSTAT= ISTAT )
     &   IBIAS1(1),ENDPT,JD(1),hcalepo,equnox,
     &   BEGINP,FILLER,IBIAS2(1),comnt1,comnt2
       IF ( ISTAT .NE. 0 ) THEN
        WRITE(0,'(A,I4)') 
     &   'D5tD3: error writing D3LE header, ISTAT= ',ISTAT
        STOP
       END IF
      END IF
C
C~~ DASTCOM3, big-endian (BE)
      IF ( DLEN(4) .NE. 0 ) THEN
       OPEN( IU(2), FILE=D3BE, STATUS='UNKNOWN', ACCESS='DIRECT',
     &       FORM='UNFORMATTED', RECL=RECL3, IOSTAT=ISTAT )
       CLOSE( IU(2), STATUS='DELETE' )
       OPEN( IU(2), FILE=D3BE, STATUS='NEW', ACCESS='DIRECT',
     &       FORM='UNFORMATTED', RECL=RECL3, IOSTAT=ISTAT )
       IF ( ISTAT .NE. 0 ) THEN
        WRITE(0,'(A)' ) 'D5tD3: IOSTAT error code ',ISTAT,
     &                  ' opening new D3BE '//D3BE(1:DLEN(4))
        STOP
       END IF
C
C~~ Create and write DASTCOM3 header (BE)
       FILLER(20:20)= 'A'
       WRITE( IU(2), REC= 1, IOSTAT= ISTAT )
     &   IBIAS1(2),ENDPT,JD(2),hcalepo,equnox,
     &   BEGINP,FILLER,IBIAS2(2),comnt1,comnt2
       IF ( ISTAT .NE. 0 ) THEN
        WRITE(0,'(A,I4)') 
     &   'D5tD3: error writing D3BE header, ISTAT= ',ISTAT
        STOP
       END IF
      END IF
C
C~~ DASTCOM4, little-endian (LE)
      IF ( DLEN(6) .NE. 0 ) THEN
       OPEN( IU(3), FILE=D4LE, STATUS='UNKNOWN', ACCESS='DIRECT',
     &       FORM='UNFORMATTED', RECL=RECL4, IOSTAT=ISTAT )
       CLOSE( IU(3), STATUS='DELETE' )
       OPEN( IU(3), FILE=D4LE, STATUS='NEW', ACCESS='DIRECT',
     &       FORM='UNFORMATTED', RECL=RECL4, IOSTAT=ISTAT )
       IF ( ISTAT .NE. 0 ) THEN
        WRITE(0,'(A)' ) 'D5tD3: IOSTAT error code ',ISTAT,
     &                  ' opening new D4LE '//D4LE(1:DLEN(6))
        STOP
       END IF
C
C~~ Create and write DASTCOM4 header (LE)
       FILLER(20:20)= 'C'
       WRITE( IU(3), REC= 1, IOSTAT= ISTAT )
     &   IBIAS1(1),ENDPT,JD(1),hcalepo,equnox,
     &   BEGINP,FILLER,IBIAS2(1),comnt1,comnt2
       IF ( ISTAT .NE. 0 ) THEN
        WRITE(0,'(A,I4)') 
     &   'D5tD3: error writing D4LE header, ISTAT= ',ISTAT
        STOP
       END IF
      END IF
C
C~~ DASTCOM4, big-endian (BE)
      IF ( DLEN(7) .NE. 0 ) THEN
       OPEN( IU(4), FILE=D4BE, STATUS='UNKNOWN', ACCESS='DIRECT',
     &       FORM='UNFORMATTED', RECL=RECL4, IOSTAT=ISTAT )
       CLOSE( IU(4), STATUS='DELETE' )
       OPEN( IU(4), FILE=D4BE, STATUS='NEW', ACCESS='DIRECT',
     &       FORM='UNFORMATTED', RECL=RECL4, IOSTAT=ISTAT )
       IF ( ISTAT .NE. 0 ) THEN
        WRITE(0,'(A)' ) 'D5tD3: IOSTAT error code ',ISTAT,
     &                  ' opening new D4BE '//D4BE(1:DLEN(7))
        STOP
       END IF
C
C~~ Create and write DASTCOM4 header (BE)
       FILLER(20:20)= 'C'
       WRITE( IU(4), REC= 1, IOSTAT= ISTAT )
     &   IBIAS1(2),ENDPT,JD(2),hcalepo,equnox,
     &   BEGINP,FILLER,IBIAS2(2),comnt1,comnt2
       IF ( ISTAT .NE. 0 ) THEN
        WRITE(0,'(A,I4)') 
     &   'D5tD3: error writing D4BE header, ISTAT= ',ISTAT
        STOP
       END IF
      END IF
C
C** Cycle through all DASTCOM5 object records and write to new file formats
      K= 0
 30   IF ( K .EQ. 0 ) THEN       ! Numbered asteroids
       K= 1
       STRT= IBND(4)
       FNSH= IBND(1)
      ELSE IF ( K .EQ. 1 ) THEN  ! Unnumbered asteroid
       K= 2
       STRT= IBND(5)
       FNSH= IBND(2)
      ELSE IF ( K .EQ. 2 ) THEN  ! Comets
       K= 3
       STRT= IBND(6)
       FNSH= IBND(3)
      ELSE                       ! End of program here!
       CLOSE( IU(1) )
       CLOSE( IU(2) )
       CLOSE( IU(3) )
       CLOSE( IU(4) )
       CALL DXCLOS
       STOP
      END IF
C
      DO LREC= STRT, FNSH
       CALL DXREAD( LREC, IZONE, LSRC, R8OUT, CHOUT, ISTAT )
       IF ( ISTAT .NE. 0 ) THEN
        WRITE(0,'(A,I8,A,I4)') 
     &   'D5tD3: FATAL read error, DASTCOM5 logical record ',
     &   LREC,', DXREAD ISTAT= ',ISTAT
        STOP
       END IF
       IF ( (IZONE .LT. 1) .OR. (IZONE .GT. 3) ) THEN
        WRITE(0,'(A)') 
     &  'D5tD3: cannot identify DASTCOM5 database zone, IZONE= ',IZONE
        STOP
       END IF
       NO(ILIT)= INT( R8OUT(28) )
       IF ( NO(ILIT) .NE. LREC ) THEN
        WRITE(0,'(A,I8,A,I4)') 
     &   'D5tD3: fatal file structure error, DASTCOM5 logical record ',
     &   LREC,' contains object ',NO(ILIT)
        STOP
       END IF
C
C** Extract elements from return array and prepare to write
C
C~~ First assign byte-order indifferent values common to ast/com records
       EQUNOX= CHOUT(46:49)      
       IREF  = CHOUT(38:45)
       DESIG = CHOUT(25:37)
       DARC  = CHOUT(56:64)
C
       IF ( IZONE .NE. 3 ) THEN   ! Asteroid record 
C
C   D4 asteroid record:
C     +       NO,ASTNAM,EPOCH,CALEPO,EQUNOX,IREF,DESIG,
C     +       RMO,RNODE,OM,RI,E,A,
C     +       PERJD,PERCAL,Q,GM,RAD,H,G,BVT,
C     +       RP,ALBEDO,SPTYPE,DARC,NOBS,COMNT1,COMNT2,
C     +       SBNAM,PENAM,RMSW,RMSH,MOID,UPARM,
C     +       OBSTRT,OBSTOP,SOLDAT,
C     +       LSRC,(SRC(I),I=1,21),
C     +       OQP,CEU,SIG1,LEAK1,SLEAK1,LEAK5,SLEAK5
C
C~~ Assign asteroid-specific byte-order indifferent values
        ASTNAM= ' '//CHOUT(1:17)
        SPTYPE= CHOUT(50:55)
        COMNT1= CHOUT(65:105)
        COMNT2= CHOUT(106:185)
        SBNAM = CHOUT(186:197)
        PENAM = CHOUT(198:203)
C
C~~ Assign native & non-native byte-order arrays according to user-request
        LOOP= 1      
        I= ILIT                  ! Start with native byte-order assignment
C
C~~ Type-convert array values to original database types, assign temporary 
C   storage
C
C   INTEGER*4
        I4TMP(1)= INT( R8OUT(28) )
        I4TMP(2)= INT( R8OUT(27) )
        I4TMP(3)= INT( R8OUT(41) )
C
C   REAL*4
        R4TMP(1)= SNGL( R8OUT(10) )
        R4TMP(2)= SNGL( R8OUT(9)  )
        R4TMP(3)= SNGL( R8OUT(13) )
        R4TMP(4)= SNGL( R8OUT(15) )
        R4TMP(5)= SNGL( R8OUT(12) )
        R4TMP(6)= SNGL( R8OUT(16) )
        R4TMP(7)= SNGL( R8OUT(14) )
        R4TMP(8)= SNGL( R8OUT(38) )
        R4TMP(9)= SNGL( R8OUT(39) )
        R4TMP(10)=SNGL( R8OUT(40) )
C
C~~ Change R8OUT(42) & R8OUT(43) from YYYYMMDD.000000 to JD (over-write).
        IF ( R8OUT(42) .NE. 0 ) THEN
         IYR= R8OUT(42) / 10000  
         IMN= (R8OUT(42)-IYR*10000)/100
         IDY= ((R8OUT(42)-IYR*10000)/100 - IMN) * 100
         IF ( R8OUT(42) .LT. 15821015.D0 ) THEN
          R8OUT(42)= JUL2JD( IYR, IMN, IDY )
         ELSE
          R8OUT(42)= GREG2JD( IYR, IMN, IDY )
         END IF
        END IF
C
        IF ( R8OUT(43) .NE. 0 ) THEN
         IYR= R8OUT(43) / 10000  
         IMN= (R8OUT(43)-IYR*10000)/100
         IDY= ((R8OUT(43)-IYR*10000)/100 - IMN) * 100
         IF ( R8OUT(43) .LT. 15821015.D0 ) THEN
          R8OUT(43)= JUL2JD( IYR, IMN, IDY )
         ELSE
          R8OUT(43)= GREG2JD( IYR, IMN, IDY )
         END IF
        END IF
C
        DO WHILE ( LOOP .LE. 2 )
C
         NO(I)    = I4TMP(1)
C
         EPOCH(I) = R8OUT(7)
         CALEPO(I)= R8OUT(8)
         RMO(I)   = R8OUT(6)
         RNODE(I) = R8OUT(5)
         OM(I)    = R8OUT(4)
         RI(I)    = R8OUT(3)
         E(I)     = R8OUT(2)
         A(I)     = R8OUT(1)
         PERJD(I) = R8OUT(17)
         PERCAL(I)= R8OUT(18)
         Q(I)     = R8OUT(11)
C
         GM(I)    = R4TMP(1)
         RAD(I)   = R4TMP(2)
         H(I)     = R4TMP(3)
         G(I)     = R4TMP(4)
         BVT(I)   = R4TMP(5)
         RP(I)    = R4TMP(6)
         ALBEDO(I)= R4TMP(7)
C
         NOBS(I)  = I4TMP(2)
C
         RMSW(I)  = R4TMP(8)
         RMSH(I)  = R4TMP(9)
         MOID(I)  = R4TMP(10)
C
         UPARM(I) = I4TMP(3)
C
         OBSTRT(I)= R8OUT(42)
         OBSTOP(I)= R8OUT(43)
         SOLDAT(I)= R8OUT(44)
C
C~~ If DAST5 has an extended covariance (> 6 parms), zero out conversion
         IF ( LSRC .LE. 21 ) THEN
          DO J= 1,21
           SRC(J,I)= R8OUT(44+J)
          END DO
         ELSE
          LSRC= 0
          DO J= 1,21
           SRC(J,I)= 0.D0
          END DO
          WRITE(0,'(A,I8,A)') 
     &    'WARNING: covariance skipped due to unsupported parameters,'//
     &    ' object record = ',LREC,', DESIG= '//DESIG
         END IF
C
C~~ Assign non-native byte-order array, if necessary
         LOOP= LOOP + 1
         IF ( LOOP .EQ. 2 ) THEN
          IF ( NONATV ) THEN
           I= 1
           IF ( ILIT .EQ. 1 ) I= 2
           CALL I4SWAP(  3,      I4TMP)  ! Swap byte-order INTEGER*4 data
           CALL R4SWAP( 10,      R4TMP)  ! Swap byte-order REAL*4 data
           CALL R8SWAP(  8, R8OUT( 1) )  ! Swap byte-order R8OUT(01-08)
           CALL R8SWAP(  1, R8OUT(11) )  ! Swap byte-order R8OUT(   11)
           CALL R8SWAP(  2, R8OUT(17) )  ! Swap byte-order R8OUT(17-18)
           CALL R8SWAP(  3, R8OUT(42) )  ! Swap byte-order R8OUT(42-44)
           IF (LSRC.NE.0) CALL R8SWAP(21,R8OUT(45)) ! Swap R8OUT(45-65)
          ELSE
           LOOP= 3 ! non-native byte-order not needed
          END IF
         END IF
        END DO 
C
       ELSE
C
C D4 comet record:
C    +        NO,COMNAM,EPOCH,CALEPO,EQUNOX,IREF,DESIG,
C    +        RMO,RNODE,OM,RI,E,A,
C    +        PERJD,PERCAL,Q,GM,RAD,A1,A2,
C    +        M1, M2, PYR, DARC, COMNT3, COMNT2,
C    +        SBNAM, PENAM, RMSW, MOID, UPARM,
C    +        OBSTRT, OBSTOP, SOLDAT, A3, DT, EST,
C    +        LSRC, (SRC(I),I=1,55)
C
C~~ Assign comet-specific byte-order indifferent values
        COMNAM= ' '//CHOUT(1:23)
        COMNT3= CHOUT(65:113)
        COMNT2= CHOUT(114:193)
        SBNAM = CHOUT(194:205)
        PENAM = CHOUT(206:211)
        ESTL  = CHOUT(212:217)
C
C~~ Assign native & non-native byte-order arrays according to user-request
        LOOP= 1      
        I= ILIT                  ! Start with native byte-order assignment
C
C~~ Type-convert array values to original database types, assign temporary 
C   storage
C
C   INTEGER*4
        I4TMP(1)= INT( R8OUT(28) )
        I4TMP(2)= INT( R8OUT(41) )
C
C   REAL*4
        R4TMP(1)= SNGL( R8OUT(10) )
        R4TMP(2)= SNGL( R8OUT(9)  )
        R4TMP(3)= SNGL( R8OUT(14) )
        R4TMP(4)= SNGL( R8OUT(15) )
        R4TMP(5)= SNGL( R8OUT(12) )
        R4TMP(6)= SNGL( R8OUT(13) )
        R4TMP(7)= SNGL( R8OUT(38) )
        R4TMP(8)= SNGL( R8OUT(40) )
        R4TMP(9)= SNGL( R8OUT(39) )
        R4TMP(10)=SNGL( R8OUT(36) )
C
C~~ Change R8OUT(42) & R8OUT(43) from YYYYMMDD.000000 to JD (over-write)
        IF ( R8OUT(42) .NE. 0 ) THEN
         IYR= R8OUT(42) / 10000  
         IMN= (R8OUT(42)-IYR*10000)/100
         IDY= ((R8OUT(42)-IYR*10000)/100 - IMN) * 100
         IF ( R8OUT(42) .LT. 15821015.D0 ) THEN
          R8OUT(42)= JUL2JD( IYR, IMN, IDY )
         ELSE
          R8OUT(42)= GREG2JD( IYR, IMN, IDY )
         END IF
        END IF
C
        IF ( R8OUT(43) .NE. 0 ) THEN
         IYR= R8OUT(43) / 10000  
         IMN= (R8OUT(43)-IYR*10000)/100
         IDY= ((R8OUT(43)-IYR*10000)/100 - IMN) * 100
         IF ( R8OUT(43) .LT. 15821015.D0 ) THEN
          R8OUT(43)= JUL2JD( IYR, IMN, IDY )
         ELSE
          R8OUT(43)= GREG2JD( IYR, IMN, IDY )
         END IF
        END IF
C
        DO WHILE ( LOOP .LE. 2 )
         NO(I)    = I4TMP(1)
C
         EPOCH(I) = R8OUT(7)
         CALEPO(I)= R8OUT(8)
         RMO(I)   = R8OUT(6)
         RNODE(I) = R8OUT(5)
         OM(I)    = R8OUT(4)
         RI(I)    = R8OUT(3)
         E(I)     = R8OUT(2)
         A(I)     = R8OUT(1)
         PERJD(I) = R8OUT(17)
         PERCAL(I)= R8OUT(18)
         Q(I)     = R8OUT(11)
C
         GM(I)    = R4TMP(1)
         RAD(I)   = R4TMP(2)
         A1(I)    = R4TMP(3)
         A2(I)    = R4TMP(4)
         M1(I)    = R4TMP(5)
         M2(I)    = R4TMP(6)
C
         PYR(I)   = R8OUT(16)
C
         RMSW(I)  = R4TMP(7)
         MOID(I)  = R4TMP(8)
C
         UPARM(I) = I4TMP(2)
C
         OBSTRT(I)= R8OUT(42)
         OBSTOP(I)= R8OUT(43)
         SOLDAT(I)= R8OUT(44)
C
         A3(I)    = R4TMP(9)
         DT(I)    = R4TMP(10)
C
C~~ If DCOM5 has an extended covariance (> 10 parms), zero out conversion
         IF ( LSRC .LE. 55 ) THEN
          DO J= 1,55
           SRC(J,I)= R8OUT(44+J)
          END DO
         ELSE
          LSRC= 0
          DO J= 1,55
           SRC(J,I)= 0.D0
          END DO
          WRITE(0,'(A,I8,A)') 
     &    'WARNING: covariance skipped due to unsupported parameters,'//
     &    ' object record = ',LREC,', DESIG= '//DESIG
         END IF
C
C~~ Assign non-native byte-order array, if necessary
         LOOP= LOOP + 1
         IF ( LOOP .EQ. 2 ) THEN
          IF ( NONATV ) THEN
           I= 1
           IF ( ILIT .EQ. 1 ) I= 2
           CALL I4SWAP(  2,     I4TMP )  ! Swap byte-order INTEGER*4
           CALL R4SWAP( 10,     R4TMP )  ! Swap byte-order REAL*4
           CALL R8SWAP(  8, R8OUT( 1) )  ! Swap byte-order R8OUT(01-08)
           CALL R8SWAP(  1, R8OUT(11) )  ! Swap byte-order R8OUT(   11)
           CALL R8SWAP(  3, R8OUT(16) )  ! Swap byte-order R8OUT(16-18)
           CALL R8SWAP(  3, R8OUT(42) )  ! Swap byte-order R8OUT(42-44)
           IF (LSRC.NE.0) CALL R8SWAP(55,R8OUT(45)) ! Swap R8OUT(45-99)
          ELSE
           LOOP= 3 ! non-native byte-order not needed
          END IF
         END IF
        END DO 
       END IF
C
C** Assign/convert LSRC to byte-order arrays
       LSRCS(ILIT)= LSRC       ! Assign native value
       CALL I4SWAP( 1, LSRC )
       IF ( ILIT .EQ. 1 ) THEN ! Assign byte-swapped value
        LSRCS(2)= LSRC
       ELSE
        LSRCS(1)= LSRC
       END IF
C
C** Values have been extracted from DASTCOM5 and stored in up to two
C   byte-order arrays. Now write out records in requested DASTCOM3/4
C   databases.
C
C~~ Compute output physical record number
       IF ( IZONE .NE. 3 ) THEN
        PREC= LREC - IBIAS(IZONE)  ! no change from DASTCOM5
       ELSE
        PREC= LREC - IBIAS2(ILIT)  ! changed from DCOM5 for merge
       END IF
C       
C~~ DASTCOM3, little-endian (LE)
       IF ( DLEN(3) .NE. 0 ) THEN
        IF ( IZONE .EQ. 3 ) THEN ! DASTCOM3 comet record
         WRITE( IU(1), REC= PREC, IOSTAT= ISTAT ) 
     &        NO(1),COMNAM,EPOCH(1),CALEPO(1),EQUNOX,IREF,DESIG,
     &        RMO(1),RNODE(1),OM(1),RI(1),E(1),A(1),
     &        PERJD(1),PERCAL(1),Q(1),GM(1),RAD(1),A1(1),A2(1),
     &        M1(1), M2(1), PYR(1), DARC, COMNT3, COMNT2,
     &        SBNAM
        ELSE                     ! DASTCOM3 asteroid record
         WRITE( IU(1), REC= PREC, IOSTAT= ISTAT ) 
     &         NO(1),ASTNAM,EPOCH(1),CALEPO(1),EQUNOX,IREF,DESIG,
     &         RMO(1),RNODE(1),OM(1),RI(1),E(1),A(1),
     &         PERJD(1),PERCAL(1),Q(1),GM(1),RAD(1),H(1),G(1),BVT(1),
     &         RP(1),ALBEDO(1),SPTYPE,DARC,NOBS(1),COMNT1,COMNT2,
     &         SBNAM
        END IF
        IF ( ISTAT  .NE. 0 ) THEN
         WRITE(0,'(A,I4)') 
     &    'D5tD3: fatal error writing D3LE record, ISTAT= ',ISTAT
         WRITE(0,'(A,I8,A,I8)') ' LREC= ',LREC,', PREC= ',PREC
         STOP
        END IF
       END IF
C       
C~~ DASTCOM3, big-endian (BE)
       IF ( DLEN(4) .NE. 0 ) THEN
        IF ( IZONE .EQ. 3 ) THEN ! DASTCOM3 comet record
         WRITE( IU(2), REC= PREC, IOSTAT= ISTAT ) 
     &        NO(2),COMNAM,EPOCH(2),CALEPO(2),EQUNOX,IREF,DESIG,
     &        RMO(2),RNODE(2),OM(2),RI(2),E(2),A(2),
     &        PERJD(2),PERCAL(2),Q(2),GM(2),RAD(2),A1(2),A2(2),
     &        M1(2), M2(2), PYR(2), DARC, COMNT3, COMNT2,
     &        SBNAM
        ELSE                     ! DASTCOM3 asteroid record
         WRITE( IU(2), REC= PREC, IOSTAT= ISTAT ) 
     &         NO(2),ASTNAM,EPOCH(2),CALEPO(2),EQUNOX,IREF,DESIG,
     &         RMO(2),RNODE(2),OM(2),RI(2),E(2),A(2),
     &         PERJD(2),PERCAL(2),Q(2),GM(2),RAD(2),H(2),G(2),BVT(2),
     &         RP(2),ALBEDO(2),SPTYPE,DARC,NOBS(2),COMNT1,COMNT2,
     &         SBNAM
        END IF
        IF ( ISTAT  .NE. 0 ) THEN
         WRITE(0,'(A,I4)') 
     &    'D5tD3: fatal error writing D3BE record, ISTAT= ',ISTAT
         WRITE(0,'(A,I8,A,I8)') ' LREC= ',LREC,', PREC= ',PREC
         STOP
        END IF
       END IF
C       
C~~ DASTCOM4, little-endian (LE)
       IF ( DLEN(6) .NE. 0 ) THEN
        IF ( IZONE .EQ. 3 ) THEN ! DASTCOM4 comet record
         WRITE( IU(3), REC= PREC, IOSTAT= ISTAT ) 
     &        NO(1),COMNAM,EPOCH(1),CALEPO(1),EQUNOX,IREF,DESIG,
     &        RMO(1),RNODE(1),OM(1),RI(1),E(1),A(1),
     &        PERJD(1),PERCAL(1),Q(1),GM(1),RAD(1),A1(1),A2(1),
     &        M1(1), M2(1), PYR(1), DARC, COMNT3, COMNT2,
     &        SBNAM, PENAM, RMSW(1), MOID(1), UPARM(1),
     &        OBSTRT(1), OBSTOP(1), SOLDAT(1), A3(1), DT(1), ESTL,
     &        LSRCS(1), (SRC(I,1),I=1,55)
        ELSE                     ! DASTCOM4 asteroid record
         WRITE( IU(3), REC= PREC, IOSTAT= ISTAT ) 
     &        NO(1),ASTNAM,EPOCH(1),CALEPO(1),EQUNOX,IREF,DESIG,
     &        RMO(1),RNODE(1),OM(1),RI(1),E(1),A(1),
     &        PERJD(1),PERCAL(1),Q(1),GM(1),RAD(1),H(1),G(1),BVT(1),
     &        RP(1),ALBEDO(1),SPTYPE,DARC,NOBS(1),COMNT1,COMNT2,
     &        SBNAM,PENAM,RMSW(1),RMSH(1),MOID(1),UPARM(1),
     &        OBSTRT(1),OBSTOP(1),SOLDAT(1),
     &        LSRCS(1),(SRC(I,1),I=1,21),
     &        OQP,CEU,SIG1,LEAK1,SLEAK1,LEAK5,SLEAK5
        END IF
        IF ( ISTAT  .NE. 0 ) THEN
         WRITE(0,'(A,I4)') 
     &    'D5tD3: fatal error writing D4LE record, ISTAT= ',ISTAT
         WRITE(0,'(A,I8,A,I8)') ' LREC= ',LREC,', PREC= ',PREC
         STOP
        END IF
       END IF
C      
C~~ DASTCOM4, big-endian (BE)
       IF ( DLEN(7) .NE. 0 ) THEN
        IF ( IZONE .EQ. 3 ) THEN ! DASTCOM4 comet record
         WRITE( IU(4), REC= PREC, IOSTAT= ISTAT ) 
     &        NO(2),COMNAM,EPOCH(2),CALEPO(2),EQUNOX,IREF,DESIG,
     &        RMO(2),RNODE(2),OM(2),RI(2),E(2),A(2),
     &        PERJD(2),PERCAL(2),Q(2),GM(2),RAD(2),A1(2),A2(2),
     &        M1(2), M2(2), PYR(2), DARC, COMNT3, COMNT2,
     &        SBNAM, PENAM, RMSW(2), MOID(2), UPARM(2),
     &        OBSTRT(2), OBSTOP(2), SOLDAT(2), A3(2), DT(2), ESTL,
     &        LSRCS(2), (SRC(I,2),I=1,55)
        ELSE                     ! DASTCOM4 asteroid record
         WRITE( IU(4), REC= PREC, IOSTAT= ISTAT ) 
     &        NO(2),ASTNAM,EPOCH(2),CALEPO(2),EQUNOX,IREF,DESIG,
     &        RMO(2),RNODE(2),OM(2),RI(2),E(2),A(2),
     &        PERJD(2),PERCAL(2),Q(2),GM(2),RAD(2),H(2),G(2),BVT(2),
     &        RP(2),ALBEDO(2),SPTYPE,DARC,NOBS(2),COMNT1,COMNT2,
     &        SBNAM,PENAM,RMSW(2),RMSH(2),MOID(2),UPARM(2),
     &        OBSTRT(2),OBSTOP(2),SOLDAT(2),
     &        LSRCS(2),(SRC(I,2),I=1,21),
     &        OQP,CEU,SIG1,LEAK1,SLEAK1,LEAK5,SLEAK5
        END IF
        IF ( ISTAT  .NE. 0 ) THEN
         WRITE(0,'(A,I4)') 
     &    'D5tD3: fatal error writing D4BE record, ISTAT= ',ISTAT
         WRITE(0,'(A,I8,A,I8)') ' LREC= ',LREC,', PREC= ',PREC
         STOP
        END IF
       END IF
C
C~~ Get next record in DASTCOM5 zone
      END DO
C
C** Completed database zone. Start on next remaining zone.
      GOTO 30 
C
      END
