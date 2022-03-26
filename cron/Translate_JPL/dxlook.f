      PROGRAM DXLOOK
      IMPLICIT NONE
C
C------------------------------------------------------------------------------
C DXLOOK -- Look up objects in a DASTCOMX database and display record data 
C  based on user directives. Program is intended as a command-line utility 
C  and example to demonstrate database access.
C
C  If an index is loaded and used for look-ups, a UNIX/Linux-like "egrep"
C  command executable must exist in the user's path.
C
C  Assumes standard pre-connected FORTRAN I/O units: 0 is standard error,
C  5 is standard input, 6 is standard output.
C
C Usage:
C
C 'dxlook' is a stand-alone command-line tool to rapidly examine DASTCOM
C database records.  It can be used to examine legacy databases such as
C DASTCOM3 and DASTCOM4 as well.
C
C Type 'dxlook' from the command-line to run, 'help' to list commands,
C then 'x' to exit. Program directives are:
C
C  Directive           Meaning
C  ------------------  -----------------------------------------------------
C  DB     [path/file]  Open specified database(s); DASTCOM 3-5 are supported
C  SUMM                Summarize currently opened databases
C  INDEX  [path/file]  Specify optional ASCII index file for look-ups
C  FIELDS [list]       Set field codes to display (default= -5, all DASTCOM5)
C  PAGER  {path}       Toggle output pager w/executable path (default= none)
C  LABEL  [ON/OFF]     Toggle display of field labels (default= ON) 
C  HELP                Display this list of directives (same as "?")
C  {Object}            Display {object};  record #, name, designation, SPK ID
C  !{X}                Pass {X} to operating system as a command
C  X                   Exit program; same as CLOSE, QUIT, EXIT
C
C 'dxlook' is controlled by input file, keyboard, or combination of the two. 
C 
C It is recommended that set-up information which doesn't change, such as the 
C paths to the latest database files, be placed in an input file called
C "./dxlook.inp".
C      
C When 'dxrook' is run, it will first look for that input file 'dxlook.inp' in
C the run directory, load anything there, then drop into keyboard input, which
C would typically be user-queries of the database.
C          
C For example, set './dxlook.inp' to contain the lines between the '---' marks:
C          
C ---          
C DB    ./dastcom5/dat/dast5_le.dat
C DB    ./dastcom5/dat/dcom5_le.dat
C INDEX ./dastcom5/dat/dastcom.idx
C PAGER /bin/usr/less
C ---  
C
C Now execute 'dxlook'. Type 'summ' at the prompt to summarize the databases
C now open. Type "1" to look-up the object in record #1, "99942" the object in
C record 99942, and so on.
C
C While the default behavior is to display all defined DASTCOM fields, the
C FIELDS directive can be used to request 'dxlook' display only select data.
C For example, the directive ...
C
C   FIELDS 14,11,802,807,808,809,804,805,806
C
C ... requests display of only object name (14), designation (11), calendar
C epoch (802), and orbital elements EC, A, QR, W, OM, IN.
C
C See the './dastcom5/src/curdef.inc' file for a list of all available fields
C and their numeric codes.
C
C Some macros are also defined:
C
C   'FIELDS -3' requests legacy DASTCOM3 fields only,
C   'FIELDS -4' requests legacy DASTCOM4 fields only,
C   'FIELDS -5' restores the default, which is "all DASTCOM5 fields"
C
C Note that numeric data is always displayed first, followe by character data,
C regardless of the order you specify with FIELDS. However, the order specified
C within those two categories (numeric and character) is maintained for display.
C
C If the Unix/Linux/Mac 'egrep' command is in your path (type '!which egrep' in
C 'dxlook' to determine if it can be found), and INDEX is defined, it will be
C possible to look up objects based on their names, SPK IDs, designations, and
C other aliases, including limited regular expressions.
C
C If there are multiple matches from such a search, select the desired object
C using the unique DASTCOM record number on the left-most part of each index
C line in the list of matches.
C
C If 'egrep' is not available in your path, only record numbers may be used
C to do look-ups.
C
C Modification History:
C
C  DATE         Who   Change
C  -----------  ----  ---------------------------------------------------------
C  2013-Jul-15  JDG   Version 1.0
C  2013-Aug-15  JDG   Alter to support look-up of SPK ID integers
C  2016-Mar-05  JDG   Change character output indexing to match recent 
C                      DXCASGN fix.
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
C** Declare small-body database subroutine package dummy-argument variables
C
      INCLUDE 'dxparms.inc'
C
      CHARACTER*1      DBNAM(2)*256, CERRMS*340, CHOUT(NUMCH)*80, CLAB*3
      CHARACTER*1      CXARR(NUMCH)*80, CTMP*25, LABELS(NUMLB)*8
      REAL*8           R8OUT(NUMNS)
      INTEGER          IU(2), LERR, ISTAT, RNR8, RNCH
      INTEGER          IRORD(NUMNS), ICORD(NUMCH)
      INTEGER          IOBJ, IZONE, LSRC
      INTEGER          NLBL, CODES(NUMLB), ILNL(NUMLB)
      LOGICAL          WARN, BUF
C
C** Declare program variables
      CHARACTER*1      LINE*800, OLIN*800, DXNAM*800, PAGER*256
      CHARACTER*1      ST*282, FILNM(2)*256, DBBYT(2), CAL(2)*19
      CHARACTER*1      SYS*256, DXLIN*800
      INTEGER          L1, L2, I1, I, J, K, FC, DXL, UTMP
      INTEGER          FLIST(NUMF), KCH, KR8, K0, IVERS, IOS,ITEST
      INTEGER          IBND(6), IBIAS(3), ITOT(3), DTYP, UO, IPAGE
      INTEGER          IOS1, IOS2, CCNT
      LOGICAL          LABEL, INITED, INDX, EXISTS, LFLG(2), FINP
      LOGICAL          PRTHDR, SUMLOD, LPAGE
C
C** Declare local parameters 
      INTEGER          NCMD
      PARAMETER( NCMD= 12 )   ! Max. number of DXLOOK program directives
      CHARACTER*6      CMD(NCMD)
      INTEGER          CMDL(NCMD)
C
C** Declare external functions
      INTEGER          LENTRM, LFTTRM
C
C** Store program help as internal data
C
      INTEGER          NHELP
      PARAMETER( NHELP= 21   )  ! Number of lines of help text
      CHARACTER*79     HELP(NHELP)
C
      DATA HELP /
     &' ',
     &'If the file "./dxlook" does not exist, commands may be redirected 
     & in through',
     &'standard input. "dxlook" remains in interactive input until an ex
     &it directive:',
     &' ',
     &' Directive           Meaning',
     &' ------------------  --------------------------------------------
     &---------',
     &' DB     [path/file]  Open specified database(s); DASTCOM 3-5 are 
     &supported',
     &' SUMM                Summarize currently opened databases',
     &' INDEX  [path/file]  Specify optional ASCII index file for look-u
     &ps',
     &' FIELDS [list]       Set field codes to display (default= -5, all
     & DASTCOM5)',
     &' PAGER  {path}       Toggle output pager w/executable path (defau
     &lt= none)', 
     &' LABEL  [ON/OFF]     Toggle display of field labels (default= ON)
     &',
     &' HELP                Display this list of directives (same as "?"
     &)',
     &' {Object}            Display {object};  record #, name, designati
     &on, SPK ID',
     &' !{X}                Pass {X} to operating system as a command',
     &' X                   Exit program; same as CLOSE, QUIT, EXIT',
     &' ',
     &'Set-up constants (such as "DB" and "INDEX" database paths, or des
     &ired FIELDS',
     &'output) can be specified once in "./dxlook.inp" and dxlook launch
     &ed thereafter',
     &'to examine records without specifying set-up preferences & databa
     &ses each time.',
     &' ' /
C 
C** Store recognized program directives that set up for object record requests.
      DATA  CMD /
     &  'DB', 'INDEX', 'FIELDS', 'LABEL', 'SUMM', 'CLOSE', 'QUIT', 
C CMDL|    2,       5,        6,       5,      4,       5,      4,     
     &  'EXIT', 'X', 'PAGER', 'HELP', '?' /
C CMDL|      4,   1,       5,      4,   1 /
C
      DATA  CMDL /
C CMD | 'DB', 'INDEX', 'FIELDS', 'LABEL', 'SUMM', 'CLOSE', 'QUIT', 
     &     2,       5,        6,       5,      4,       5,      4,     
C CMD | 'EXIT', 'X', 'PAGER', 'HELP', '?' /
     &       4,   1,       5,      4,   1 /
C
C** Set default unit numbers for the two possible database files
      DATA IU     / 20, 21 /
C
C** Set default scratch directory for optional paging
      CHARACTER*1      SCRDIR*5, TMPFIL*256
      DATA SCRDIR / '/tmp/' /
C
C** Initialize
      SUMLOD= .FALSE.
      PRTHDR= .TRUE.
      INITED= .FALSE.
      WARN= .TRUE.
      LABEL= .TRUE.
      LPAGE= .FALSE.
      INDX= .FALSE.
      PAGER=   ' '
      DBNAM(1)= ' '
      DBNAM(2)= ' '
      CLAB= ' '
      FC= 0
      DTYP= 0
C
C** Initialize default field request list (all fields). Users need only
C   load a database and then specify a record number to get a dump of the 
C   entire record. Values below should be updated if database is expanded
C   (i.e., DXREAD changes).  RNR8 allows for SRC vector.
C
C   RNR8 is equal to RR8D+RR4D+RI4D+RI2D+RI1D+MXSRC-1 (NUMNS)
C   RNCH is equal to NUMCH (see DXREAD)
C
C   Coordinate with DXREAD changes
C
      RNR8= NUMNS   ! Total number of defined numeric array SLOTS
      RNCH= NUMCH   ! Total number of defined character FIELDS
      IRORD(1)= -5  ! Short-cut specification for all DASTCOM5 fields.
C
C** Determine if there is a default input file. The default input file can
C   typically specify the DB and index files before dropping into keyboard
C   input of specific record requests. But objects can be specified there
C   as well.
C
      FINP= .FALSE.
      OPEN( UNIT=7, FILE='dxlook.inp', STATUS='OLD', IOSTAT= IOS)
      IF ( IOS .EQ. 0 ) FINP= .TRUE.
C
C** Read character string from standard input, supporting keyboard and 
C   input redirection. Uses f95+ functionality via ADVANCE option. An
C   alternative for Solaris f77 is '(A,$)'.
C
C   Get new user-input line from file or standard input.
 10   LINE= ' '
      CCNT= 0 ! character array store/output index value
      IF ( FINP ) THEN
       READ( 7, '(A)', IOSTAT=IOS ) LINE
       IF ( IOS .NE. 0 ) THEN
        CLOSE( 7 )
        FINP= .FALSE.
       END IF
      END IF
      IF ( .NOT. FINP ) THEN
C      WRITE( 6, '(A,$)') 'dxlook> '
       WRITE( 6, '(A)', ADVANCE='NO') 'dxlook> '
       READ(  5, '(A)', END=30, IOSTAT= ISTAT ) LINE
       IF ( ISTAT .NE. 0 ) THEN
        WRITE( 0, '(A)' ) 'Unrecognized input within line'
        GOTO 10 ! Get new user-input line
       END IF
      END IF
C
C** Test and initially parse line
      L2= LENTRM(LINE)            ! Position of last non-blank character
      IF ( L2 .EQ. 0 ) GOTO 10    ! Blank input line, get next
      CALL LADJUST( LINE )        ! Left justify input line
C
C** Process system commands immediately
      IF ( LINE(1:1) .EQ. '!' ) THEN
       CALL SYSTEM( LINE(2:) )
       GOTO 10
      END IF
C
C** Continue processing input 
C~~ Convert case of directive (first symbols on line) but preserve original
      OLIN= LINE
      I1= INDEX( LINE, ' ' ) - 1  ! Position of last char. prior to first blank
      CALL UCASE3( LINE(1:I1) )
C
C** Start loop to compare current line against directive list
C==
      DO I= 1,NCMD
       J= MIN( CMDL(I), I1 )
C
C** If directive is present, process it
       IF ( CMD(I)(1:J) .EQ. LINE(1:J) ) THEN 
C
C** #1) DB DIRECTIVE (prepare for call to DXINI)
        IF ( I .EQ. 1 ) THEN
         FC= FC + 1
C
C~~ If this is a third DB file, close/clear ALL prior files and start again.
         IF ( FC .EQ. 3 ) THEN
          CALL DXCLOS
          FC= 1
          DBNAM(1)= ' '
          DBNAM(2)= ' '
C
C~~ If second file, close first and re-open with both unless first was
C   DASTCOM3 or 4 -- in that case, close and reopen only with new file.
         ELSE IF ( FC .EQ. 2 ) THEN
          CALL DXDAT1( IVERS )
          IF ( (IVERS .EQ. 3) .OR. (IVERS .EQ. 4) ) FC= 1
          CALL DXCLOS
         END IF
C
C~~ Assign new DB name & unit number to array for later DXINI initialization
         INITED= .FALSE.
         PRTHDR= .TRUE.
         DBNAM(FC)= OLIN(I1+1:)
         CALL LADJUST( DBNAM(FC) ) 
         IF ( LENTRM( DBNAM(FC) ) .LT. 1 ) THEN
          WRITE(0,'(A)') 'DB directive ignored (missing filename)'
          FC= FC - 1
         END IF
         GOTO 10 ! Get new user-input line
C
C** #2) INDEX DIRECTIVE (verify existence of file)
        ELSE IF ( I .EQ. 2 ) THEN 
         INDX= .FALSE.
         DXNAM= OLIN(I1+1:)
         CALL LADJUST( DXNAM )
         DXL= LENTRM( DXNAM )
         IF ( DXL .GT. 0 ) THEN
          INQUIRE( FILE= DXNAM, EXIST=EXISTS )
          IF ( .NOT. EXISTS ) THEN
           WRITE( 0, '(A)' ) 'Cannot find INDEX "'//DXNAM(1:DXL)//'"'
           DXNAM= ' '
           DXL= 0
          ELSE
           INDX= .TRUE.
          END IF
         ELSE
          WRITE(0,'(A)') 'INDEX directive ignored (missing filename)'
         END IF 
         GOTO 10 ! Get new user-input line
C
C** #3) FIELDS DIRECTIVE (list of fields to show; parse into array(s))
        ELSE IF ( I .EQ. 3 ) THEN 
         CALL DXCLOS     ! Prepare for new initialization by closing
         INITED= .FALSE. ! Reset for new initializtion.
         DO K=1,NUMF     ! Clear user field input array
          FLIST(K)= 0
         END DO
         READ(LINE(I1+1:),*,END=20,IOSTAT=IOS) (FLIST(K),K=1,NUMF)
         IF ( IOS .NE. 0 ) THEN
          WRITE( 0, '(A)' ) 'Unrecognized input within line'
          GOTO 10 ! Get new user-input line
         END IF 
C
C** Initialize counters (also zero out default)
 20      CONTINUE
         KCH= 0  ! counter for requested character fields
         KR8= 0  ! counter for requested numeric fields
         RNCH= 0
         RNR8= 0
C
C** Initialize arrays (if only to zero out default assignment)
         DO K0= 1, NUMNS
          IRORD(K0)= 0
         END DO
         DO K0= 1, NUMCH
          ICORD(K0)= 0
         END DO
C
C~~ Process list of fields
         DO K= 1,NUMF
C
C~~ Skip zero fields
          IF ( FLIST(K) .EQ. 0 ) THEN
           CONTINUE
C
C~~ Check for predefined short-cut list request. Values below should not
C   be changed. For a new database version with DXREAD changes, a new 
C   predefined list should be added without altering prior models.
C
          ELSE IF ( FLIST(K) .LT. 0 ) THEN   ! Request for PREDEFINED list
           IF ( FLIST(K) .EQ. -5 ) THEN      ! DASTCOM5 default ALL
            DTYP= 0
            RNR8= 142
            RNCH=  14
           ELSE IF ( FLIST(K) .EQ. -4 ) THEN ! DASTCOM4 default ALL
            DTYP= -4
            RNR8= 99
            RNCH= 1
           ELSE IF ( FLIST(K) .EQ. -3 ) THEN ! DASTCOM3 default ALL
            DTYP= -3
            RNR8= 37
            RNCH= 1
           ELSE
            WRITE(0,'(A,I4,A)') 
     &       'DXLOOK does not recognize predefined list "',
     &       FLIST(K),'" (ignored)'
            GOTO 10 ! Get new user-input line
           END IF
           IRORD(1)= FLIST(K)
C
C~~ Zero out rest of list, if any (short-cut request was found)
           DO K0= 2, NUMNS
            IRORD(K0)= 0
           END DO
           DO K0= 2, NUMCH
            ICORD(K0)= 0
           END DO
C
C++ Separate non-zero, non-predefined-list value into numerical and 
C++ character field lists.
          ELSE IF ( FLIST(K) .LE. 100 ) THEN
           IF ( KCH .EQ. NUMCH ) THEN
            WRITE(0,'(A,I3)') 
     &       'Too many character fields requested, ignoring ',FLIST(K)
           ELSE
            DTYP= 0                  ! reset flag for pre-defined list
            KCH= KCH + 1
            ICORD(KCH)= FLIST(K)
           END IF
C
C++ Special handling for SRC request ... leave open space in array for
C++ storage of maximal SRC vector.
          ELSE IF ( FLIST(K) .EQ. 899 ) THEN 
           IF ( KR8+MXSRC .GT. NUMNS ) THEN
            WRITE(0,'(A,I3)') 
     &       'Too many numeric fields requested, ignoring ',FLIST(K)
           ELSE
            DTYP= 0                  ! reset flag for pre-defined list
            KR8= KR8 + 1
            IRORD(KR8)= FLIST(K)
            DO K0= 1,MXSRC-1
             KR8= KR8 + 1
             IRORD(KR8)= 0
            END DO 
           END IF
C
C++ Standard numeric field
          ELSE IF ( FLIST(K) .LT. 899 ) THEN
           IF ( KR8 .EQ. NUMF ) THEN
            WRITE(0,'(A,I3)') 
     &       'Too many numeric fields requested, ignoring ',FLIST(K)
           ELSE
            DTYP= 0                  ! reset flag for pre-defined list
            KR8= KR8 + 1
            IRORD(KR8)= FLIST(K)
           END IF
          END IF
         END DO  
C
C** Assign results from field list parsing
         IF ( RNR8 .EQ. 0 ) RNR8= KR8
         IF ( RNCH .EQ. 0 ) RNCH= KCH
         GOTO 10 ! Get new user-input line
C
C** #4) LABEL DIRECTIVE (toggle flag)
        ELSE IF ( I .EQ. 4 ) THEN
         CALL LADJUST( LINE(I1+1:) )
         CLAB= LINE(I1+1:)
         CALL UCASE3( CLAB ) 
         IF ( CLAB .EQ. 'ON ' ) THEN
          LABEL= .TRUE.
         ELSE IF ( CLAB .EQ. 'OFF' ) THEN
          LABEL= .FALSE.
         ELSE 
          WRITE(0,'(A)') 'LABEL directive ignored (invalid option)'
         END IF
         GOTO 10 ! Get new user-input line
C
C** #5) SUMMary directive
        ELSE IF ( I .EQ. 5 ) THEN
         SUMLOD= .TRUE.
C
C** #6-#9) EXIT DIRECTIVE 
        ELSE IF ( (I .GE. 6) .AND. (I .LE. 9) ) THEN
         STOP
C
C** #10) PAGER directive
        ELSE IF ( I .EQ. 10 ) THEN
         CALL LADJUST( LINE(I1+1:) )
         PAGER= LINE(I1+1:)
         IPAGE= LENTRM( PAGER )
         IF ( IPAGE .EQ. 0 ) THEN
C         IF ( PAGE ) WRITE(0,'(A)') 'Paging OFF'
          LPAGE= .FALSE.
          UO= 6
         ELSE
          INQUIRE( FILE= PAGER, EXIST=EXISTS )
          IF ( .NOT. EXISTS ) THEN
           WRITE(0,'(A)') 
     &      'Cannot find PAGER "'//PAGER(1:IPAGE)//'", ignored'
           LPAGE= .FALSE.
           PAGER= ' '
           UO= 6
          ELSE
           LPAGE= .TRUE.
C          WRITE(0,'(A)') 'Paging ON'
          END IF
         END IF
         GOTO 10 ! Get new user-input line
C
C** #11-#12) HELP directive
        ELSE IF ( ( I. GE. 11) .AND. (I .LE. 12) ) THEN
         DO K0= 1, NHELP
          WRITE(6,'(A)') HELP(K0)
         END DO
         GOTO 10 ! Get new user-input line 
        END IF ! End "directive-found-in-line" processing 
       END IF ! End input-line test
      END DO ! Get next possible directive to test for match
C
C** Current line does not match any directive; assume object specification. 
C   Determine record number (including  index look-up).
C
C   Assumes UNIX/Linux-like 'egrep' is in the user's path
C
      READ( OLIN, '(I30)', IOSTAT=IOS ) ITEST
C
C~~  .. not integer, do index look-up on string
 15   IF ( (IOS .NE. 0) .AND. (LINE(1:I1) .NE. 'SUMM') ) THEN
       IF ( .NOT. INDX ) THEN
        WRITE(0,'(A)') 'Unknown input - specify INDEX for look-ups'
        GOTO 10 ! Get new user-input line 
       END IF
       CALL GETUNI2( UTMP )
       CALL GETFLNM( SCRDIR, TMPFIL )
       SYS= '(egrep -i -e "'//OLIN(1:LENTRM(OLIN))//'" '//
     &  DXNAM(1:DXL)//' > '//TMPFIL(1:LENTRM(TMPFIL))//') >& /dev/null'
       CALL SYSTEM( SYS )
C
C~~ Verify egrep output file exists and has at least one line (match)
       OPEN( UNIT= UTMP, FILE= TMPFIL, STATUS= 'OLD', IOSTAT= IOS1 )
       READ( UTMP, '(A)', IOSTAT=IOS2 ) DXLIN 
       IF ( (IOS1+IOS2) .NE. 0 ) THEN
        WRITE(0,'(A)') 
     &   'No index match found for "'//OLIN(1:LENTRM(OLIN))//'"'
        CLOSE( UTMP, STATUS='DELETE' )
        GOTO 10 ! Get new user-input line
       ELSE
C
C~~ Extract record number for the first match
        READ( DXLIN, *, IOSTAT=IOS1 ) IOBJ
        IF ( IOS1 .NE. 0 ) THEN
         WRITE(0,'(A)') 'Cannot parse index search output:'
         WRITE(0,'(A)') '"'//DXLIN(1:LENTRM(DXLIN))//'"'
         CLOSE( UTMP, STATUS='DELETE' )
         GOTO 10 ! Get new user-input line
        END IF
C
C~~ See if there is more than one matched line. If so, display all. 
        READ( UTMP, '(A)', IOSTAT=IOS2 ) LINE
        IF ( IOS2 .EQ. 0 ) THEN 
         IF ( LPAGE ) THEN
          CALL SYSTEM( PAGER(1:IPAGE)//' '//TMPFIL )
          CLOSE( UTMP, STATUS='DELETE' )
          GOTO 10 ! Get new user-input line
         ELSE
          WRITE(6,'(A)') DXLIN(1:LENTRM(DXLIN)) 
          WRITE(6,'(A)') LINE(1:LENTRM(LINE)) 
          READ( UTMP, '(A)', IOSTAT=IOS ) DXLIN
          DO WHILE ( IOS .EQ. 0 ) 
           WRITE(6,'(A)') DXLIN(1:LENTRM(DXLIN)) 
           READ( UTMP, '(A)', IOSTAT=IOS ) DXLIN
          END DO
          CLOSE( UTMP, STATUS='DELETE' )
          GOTO 10 ! Get new user-input line
         END IF
C
C~~ Single match, proceed with extracted IOBJ
        ELSE
         CLOSE( UTMP, STATUS='DELETE' )
        END IF
       END IF
      ELSE
       IOBJ= ITEST
      END IF
C
C** Initialize database with current parameters (DBNAM, IU, IRORD, RNR8, 
C   ICORD, RNCH) if necessary, look up object & display or summarize.
C
      IF ( (.NOT. INITED) .OR. SUMLOD ) THEN
C
C** If summary or initialization, close to prepare for potentially new files
       IF ( INITED .AND. SUMLOD ) CALL DXCLOS
C
C** Ensure there is the minimum information to proceed, else ignore input 
C   line and instead get next user input line.
C
       IF ( (IU(1)     .NE. 0) .AND.   ! Available logical unit for DB 
     &    (((IRORD(1)  .NE. 0) .AND.   ! One or more requested numeric fields
     &      (RNR8      .NE. 0)) .OR.   !  * OR *
     &      (RNCH      .NE. 0))) THEN  ! One or more requested character fields
        CONTINUE
       ELSE
        WRITE(0,'(A)') 
     &   'Request ignored '//
     &   '(not enough specification to proceed): '//OLIN(1:L2)
        GOTO 10 ! Get new user-input line
       END IF
C
C** Initialize database(s) if required
       WARN= PRTHDR
       CALL DXINI( DBNAM,IRORD,RNR8,ICORD,RNCH,BUF,WARN,ISTAT )
       IF ( ISTAT .NE. 0 ) THEN
        CALL DXERR( CERRMS, LERR ) 
        WRITE( 0,'(A)' ) CERRMS(1:LERR)
        GOTO 10 ! Get new user-input line 
       END IF
       INITED= .TRUE.
C
C** Print header only to summarize NEW databases
       IF ( PRTHDR .OR. SUMLOD ) THEN 
        CALL DXSUMP
        PRTHDR= .FALSE.
       ELSE
        WRITE(6,'(A)') 
     &   '[Re-initialized reader with new field list]'
       END IF
      END IF
C
C** If done with summary-only request, go back for next input
      IF ( SUMLOD ) THEN
       SUMLOD= .FALSE.
       GOTO 10 ! Get new user-input line 
      END IF
C
      CALL DXREAD( IOBJ, IZONE, LSRC, R8OUT, CHOUT, ISTAT ) 
C
C** Possible SPK ID integer; try again using index.
      IF ( ISTAT .EQ. -2 ) THEN  
       CALL DXCLOS
       SUMLOD= .FALSE.
       INITED= .FALSE.
       IOS= 1
       GOTO 15
      ELSE IF ( ISTAT .NE. 0 ) THEN
       CALL DXERR( CERRMS, LERR ) 
       WRITE(0,'(A)') CERRMS(1:LERR)
       GOTO 10 ! Get new user-input line
      END IF
C
C~~ Zero out arrays returned from prior requests
      DO I= 1,NUMLB
       LABELS(I)= ' '
       CODES(I)= 0
       ILNL(I)= 0
      END DO
C
C** Set up request for labels for returned fields
      NLBL= 0 
C
C~~ Get field labels for current initialized request
      CALL DXLBL( NLBL, CODES, LABELS, ILNL, ISTAT )
      IF ( ISTAT .NE. 0 ) THEN
       CALL DXERR( CERRMS, LERR ) 
       WRITE(0,'(A)') CERRMS(1:LERR)
      END IF
C
C** Display output
      IF ( LPAGE ) THEN
       CALL GETFLNM( SCRDIR, TMPFIL )
       CALL GETUNI2( UO )
       OPEN( UNIT= UO, FILE=TMPFIL, STATUS= 'NEW', IOSTAT= IOS )
       IF ( IOS .NE. 0 ) THEN
        WRITE(0,'(A,I4,A)') 
     &   'Problem opening paging file "'//TMPFIL(1:LENTRM(TMPFIL))//
     &   '" (IOS= ',IOS,'), continuing with paging OFF'
        UO= 6
        LPAGE= .FALSE.
        PAGER= ' '
       END IF
      ELSE
       UO= 6
      END IF
C
C~~ Display record type
      IF ( LABEL ) THEN
       IF ( IZONE .EQ. 1 ) WRITE(UO,'(A)') 'Numbered asteroid --'
       IF ( IZONE .EQ. 2 ) WRITE(UO,'(A)') 'Unumbered asteroid --'
       IF ( IZONE .EQ. 3 ) WRITE(UO,'(A)') 'Comet --'
       WRITE(UO,'(A)') ' Item Fld Name      Value'
       WRITE(UO,'(A)') '  --- --- -------   -------------------------'
      END IF
C
C~~ Cycle through all labels for previously returned fields
      DO I= 1,NLBL
C
C~~ Display if the particular label is assigned 
       IF ( ILNL(I) .NE. 0 ) THEN
C
C~~ Display numeric fields, checking for & handling field overflows
C   Special handling for proper output of non-grav parameters A1-A3
        IF ( CODES(I) .GE. 100 ) THEN
C
C~~ Floating-point
         IF (CODES(I) .GE. 800 ) THEN      ! Floating point REAL*8
          WRITE(CTMP,'(F25.16)') R8OUT(I)
         ELSE IF (CODES(I) .GE. 400 ) THEN ! Floating point REAL*4
          IF ( (CODES(I) .GE. 408) .AND. (CODES(I) .LE. 410) ) THEN
           WRITE(CTMP,'(1PE15.8)') SNGL(R8OUT(I))
          ELSE
           WRITE(CTMP,'(F15.7)') SNGL(R8OUT(I))
          END IF
C
C~~ INTEGER
         ELSE 
          WRITE(CTMP,'(I12)') INT(R8OUT(I))
         END IF
C
C~~ Look for field overflow, use exponential if present.
         IF ( INDEX(CTMP,'*') .NE. 0 ) WRITE(CTMP,'(E15.8)') R8OUT(I)
         IF ( LABEL ) THEN
          WRITE(UO,'(1X,2I4,A)') I,CODES(I),' '//LABELS(I)//': '//CTMP
         ELSE
          WRITE(UO,'(A)') CTMP
         END IF
C
C~~ Handles SRC entries (REAL*8; code is zero, but label assigned)
        ELSE IF ( CODES(I) .EQ. 0 ) THEN
         WRITE(CTMP,'(F25.16)') R8OUT(I)
         IF ( INDEX(CTMP,'*') .NE. 0 ) WRITE(CTMP,'(E15.8)') R8OUT(I)
         IF ( LABEL ) THEN
          WRITE(UO,'(1X,I4,4X,A)') I,' '//LABELS(I)//': '//CTMP
         ELSE
          WRITE(UO,'(A)') CTMP
         END IF
C
C~~ Handle character fields
        ELSE IF ( CODES(I) .GT. 0 ) THEN
C
C~~ If legacy character-block, extract elements for display
         IF ( DTYP .NE. 0 ) THEN
          CALL DLTXCH( CHOUT, DTYP, IZONE, NUMCH, CXARR )
          IF ( LABEL ) THEN
           WRITE(UO,'(1X,2I4,A)') 
     &      I,CODES(I),' '//LABELS(I)//': '//
     &      CXARR(CODES(I))(1:LENTRM(CXARR(CODES(I))))
          ELSE
           WRITE(UO,'(A)') CXARR(CODES(I))(1:LENTRM(CXARR(CODES(I))))
          END IF
C
C~~ If character values are already in element-based array
         ELSE
C
C~~ Separate count for character field array index, since CHOUT is 
C   returned in the desired output order with no place-holder gaps though 
C   comet and asteroid records have slightly different character fields.
C
          CCNT= CCNT + 1
C
          IF ( LABEL ) THEN
           WRITE(UO,'(1X,2I4,A)') 
     &      I,CODES(I),' '//LABELS(I)//': '//
     &      CHOUT(CCNT)(1:LENTRM(CHOUT(CCNT)))
          ELSE
           WRITE(UO,'(A)') CHOUT(CCNT)(1:LENTRM(CHOUT(CCNT)))
          END IF
         END IF
        END IF
       END IF
      END DO
C
C** Start pager if enabled
      IF ( LPAGE ) THEN
       CLOSE( UO )                    ! Close to force output flush
       CALL SYSTEM( PAGER(1:IPAGE)//' '//TMPFIL ) 
       OPEN( UNIT=UO, FILE=TMPFIL, STATUS='UNKNOWN', IOSTAT=ISTAT)
       CLOSE( UO, STATUS='DELETE' )   ! Deletes scratch file
       UO= 6                          ! restore standard output
      END IF
C
C** Get new user-input line 
      GOTO 10
C
 30   CONTINUE
      END
