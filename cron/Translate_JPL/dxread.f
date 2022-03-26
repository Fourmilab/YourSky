      SUBROUTINE DXREAD( IOBJ, IZONE, LSRC, R8OUT, CHOUT, ISTAT )
      IMPLICIT NONE
C
C** Assign database sizing parameters
      INCLUDE 'dxparms.inc'
C
C** Declare dummy arguments
      INTEGER           IOBJ, IZONE, LSRC, ISTAT
      REAL*8            R8OUT(NUMNS)
      CHARACTER*(*)     CHOUT(*)
C
C     ENTRY DXINI( FNAM, IR8ORD, NR8, ICHORD, NCH, BUF, WARN, ISTAT )
      CHARACTER*256     FNAM(2)
      INTEGER           IR8ORD(NUMNS), ICHORD(NUMCH)
      INTEGER           NR8, NCH
      LOGICAL           WARN, BUF
C
C     ENTRY DXBND( IBND, IBIAS )
      INTEGER*4         IBND(6), IBIAS(3)
C
C     ENTRY DXDAT1( VERS )
      INTEGER           VERS
C
C     ENTRY DXDAT2( FILNM )
      CHARACTER*1       FILNM(2)*256
C
C     ENTRY DXDAT3( LFLG )
      LOGICAL           LFLG(2)
C
C     ENTRY DXDAT4( DBBYT )
      CHARACTER*1       DBBYT(2)
C
C     ENTRY DXDAT5( CAL )
      CHARACTER*1       CAL(2)*19
C
C     ENTRY DXDAT6( JD )
      REAL*8            JD(2)
C
C     ENTRY DXLBL( NLBL, CODES,  LABELS, ILNL, ISTAT )
      INTEGER           NLBL, CODES(*), ILNL(*)
      CHARACTER*8       LABELS(*)
C
C     ENTRY DXERR( CERRMS, LERR )
      CHARACTER*1       CERRMS*340
      INTEGER           LERR
C
C     ENTRY DXCLOS
C
C------------------------------------------------------------------------------
C DXREAD -- DastcomX READ. Return asteroid and comet parameters for one 
C  requested object from JPL DASTCOM5 (or earlier) direct access binary 
C  database files. 
C
C  This subroutine does not include search functionality but returns 
C  one user-specified object at a time. External search software should 
C  provide the look-up record number to pass into this subroutine (IOBJ)
C  for a desired object.
C
C  To support look-ups, a separate plain-text (ASCII) index file contains 
C  object names, designations, SPK ID numbers, and packed MPC designations, 
C  and links them to DASTCOM record numbers (IOBJ). This external index 
C  can be searched separately, the DASTCOM record number determined, and 
C  passed to this subroutine for retrieval.
C 
C  The index file is also bundled in the dastcom5.zip distribution archive,
C  nominally retrievable here:
C
C                ftp://ssd.jpl.nasa.gov/pub/xfr/dastcom5.zip
C
C  To perform a sequential search of the entire database, call DXINI to 
C  initialize. Then call DXBND to retrieve the logical record bounds for 
C  each category of object. Proceed to read each record in sequence, 
C  calling DXREAD as IOBJ is incremented from BND(4) to BND(1) for numbered
C  asterods, BND(5) to BND(2) for unnumbered asteroids, and BND(6) to BND(3)
C  for comets.
C
C  Input:
C   IOBJ ...... The integer-valued look-up/logical record number
C
C  Outputs:
C   IZONE ..... Integer object-category of requested IOBJ
C
C                 1= IAU-numbered asteroid
C                 2= unnumbered asteroid
C                 3= comet
C
C   LSRC ...... Length of IOBJ's SRC vector in database (regardless of whether
C                user requested return of SRC).
C
C   R8OUT() ... An array of requested output numeric parameters, in the order 
C                specified by IR8ORD() in the initializing call to DXINI. The 
C                R8OUT() input array should be dimensioned in the calling 
C                program to be of maximum length NUMNS:
C
C                            REAL*8  R8OUT(NUMNS)
C
C                .. where NUMNS is given in 'dxparms.inc' as the sum of parameters
C                NUMNF + MXSRC - 1 (nominally 88 + 55 - 1 = 142 for DASTCOM5, but 
C                check current parameterization statements). 
C
C                A smaller array, R8OUT(X), can be used if 
C
C                              NR8  <= X <= NUMNS 
C 
C                NR8 is the user-value passed to DXINI
C
C   CHOUT() ... Output character string(s) holding requested character data 
C
C                - If D3READ-DASTCOM3/4 output requested [IR8ORD(1)=-3 or -4]:
C 
C                  CHOUT is a 217-character (byte) or larger block of data  
C                  the user must parse upon return. The calling subroutine 
C                  should dimension:  CHARACTER*217  CHOUT
C                
C                - If DASTCOM5 or higher (including predefined IR8ORD(1)=-5):
C                  CHOUT() is a character array with one requested character
C                  output parameter assigned to each element of the array.
C
C                  The calling program should pass in an 80-character array of 
C                  dimension X:
C
C                            CHARACTER*80  CHOUT(X) 
C
C                  ... where NCH <= X <= NUMCH, and NCH is the value used to 
C                  initialize DXINI (number of requested character parameters)
C                  and NUMCH is the maximum defined package parameter set 
C                  'dxparms.inc'.
C
C   ISTAT ..... Integer status code. If returned as non-zero, optionally call 
C               DXERR to retrieve an error message character string.
C
C                 0= success (no error)
C                -1= Fatal error; subroutine not initialized by DXINI
C                -2= Non-fatal error; non-existent IOBJ record requested
C                -3= Fatal-error; requested IOBJ doesn't match internal NO value
C                     (corrupt record)
C                -4= No database loaded for object type requested
C                -5= Non-fatal error; empty place-holder record [from D3/4LEGR]
C                -6= Fatal-error; invalid LSRC (corrupt record) [from D4LEGR]
C                -7= Hyperbola's semi-major axis, a, is positive [from DXSUPLQ]
C                 X= Compiler run-time/execution integer error code 
C
C  USAGE
C
C   1. Call DXINI entry-point to initialize the reader package prior to the
C       first call to DXREAD
C
C   2. Call DXREAD to retrieve one object
C
C   3. Check ISTAT error code after DXREAD subroutine call, then optionally
C       call DXERR if ISTAT is non-zero to retrieve any error text. Handle
C       any reported error conditions with user-code. 
C
C   4. Repeat from #2, if additional objects are requested from same database
C
C   5. Call DXCLOS entry-point to close a previously initialized database. 
C       This is only necessary prior to opening another database.
C
C  The DXREAD library contains these routines:
C
C    DXREAD  - subroutine; open, read, and release database files
C     DXINI .... entry-point; initialize package & load database
C     DXBND .... entry-point; return loaded database logical record boundaries
C     DXDAT1 ... entry-point; return version number of open database(s)
C     DXDAT2 ... entry-point; return file names of open databases
C     DXDAT3 ... entry-point; return logical flags for open asteroid/comet dbs 
C     DXDAT4 ... entry-point; return flags indicating endianness of open dbs
C     DXDAT5 ... entry-point; return calendar creation date of open databases
C     DXDAT6 ... entry-point; return JD creation date of open databases
C     DXLBL .... entry-point; return name labels for list of field codes
C     DXERR  ... entry-point; return error message from last DXREAD call
C     DXCLOS ... entry-point; cleanly close reader package
C    DXCODLK - subroutine; given field code, look up field label & label length
C    DXSUMP  - subroutine; prints summary of databases open in DXREAD
C
C  Required utility routines:
C
C    LTLEND - logical function (determine host system byte-order)
C    I2SWAP - subroutine; switch byte-order of 2-byte integers
C    I4SWAP - subroutine; switch byte-order of 4-byte integers
C    R4SWAP - subroutine; switch byte-order of 4-byte floating point reals
C    R8SWAP - subroutine; switch byte-order of 8-byte FP double precision
C    LENTRM - integer function (portable string length)
C    LFTTRM - integer function (portable start of non-blank string)
C    GETUNI2- subroutine; finds unused logical unit number
C    LADJUST- subroutine; left-justifies string
C    UCASE3 - subroutine; convert string to upper-case with overwrite of input
C    GETFLNM- subroutine; return unused filename for temporary output
C    GREG2JD- double precision function (convert Gregorian date to Julian day)
C    JUL2JD - double precision function (convert Julian date to Julian day)
C
C  None of the following routines should be called by users:
C
C    D4LEGR  - subroutine; DASTCOM4 legacy reader
C     D4CASGN ..  entry-point; D3READ legacy character assignment
C    D3LEGR  - subroutine; DASTCOM3 legacy reader
C     D3CASGN ..  entry-point; D3READ legacy character assignment
C    DXSUPLQ - subroutine; supplemental quantities
C    DXNASGN - subroutine; numeric output array assignment
C    DXCASGN - subroutine; character output array assignment
C    DLTXCH  - subroutine; parses DASTCOM3/4 char. block to DX char. array
C------------------------------------------------------------------------------
C DXINI --  DastcomX database INItialization. Entry point to initialize the
C  package of routines used to read the JPL small-body databases. Must be 
C  called prior to DXREAD.
C
C  NOTE: Each call to DXINI will initialize and thus lose information from any
C  prior intialization. If loading both DAST and DCOM files, pass both file 
C  names in FNAM() using a single call.
C
C  Inputs:
C
C   FNAM(1..2) Full pathname(s) for database file(s). 
C               If a DASTCOM3 or DASTCOM4 database is to be used, only FNAM(1)
C               is specified. For DASTCOM5, which uses with two separate files 
C               for asteroids and comets, both files can be specified.
C
C   IR8ORD()   Integer array of codes specifying what numeric data to place 
C               in the R8OUT() array returned by DXREAD call. For a list of
C               field codes and their meanings, see 'curdef.inc'.
C
C              Since this is basically a map of the desired output array, when
C              quantity 899 is requested (the SRC vector), there must be enough 
C              zero-filled slots immediately afterward in the requesting IR8ORD()
C              array to allow for vector storage (e.g., MXSRC-1 slots).
C
C              For example, if IR8ORD(8)= 899, IR8ORD(9-62) should be zero,
C              and the next requested quantity (if any) should be specified
C              in IR8ORD(63). Upon return, R8OUT(9-62) will then contain any
C              SRC vector up to length 55, and R8OUT(63) would contain the
C              next requested quantity specified in IR8OUT(63).
C
C              Three short-cut codes are available:
C
C               D3READ DASTCOM3 legacy data-return order (see NR8 setting notes)
C               ----------------------------------------
C                 IR8ORD(1)=-3, ICHORD() is ignored
C
C                 For asteroids, this requests numeric fields in order:
C                  808-803,801-802,433-432,809,439,401,438,402,431,810-811,
C                  (851-858,202-201,0,0,0,0,152,153,0,0,0), and block character
C                  data.
C                 
C                 For comets, this requests numeric fields in order:
C                  808-803,801-802,433-432,809,403-404,408-409,151,810-811,
C                  (851-858,202-201,405-407,156,152-153,0,411,417), and block
C                  character data.
C
C               D3READ DASTCOM4 legacy data-return order (see NR8 setting notes)
C               ----------------------------------------
C                 IR8ORD(1)=-4, ICHORD() is ignored
C
C                 For asteroids, this requests numeric fields in order: 
C                  808-803,801-802,433-432,809,439,401,438,402,431,810-811,
C                  851-858,202-201,0,0,0,0,152-153,0,0,0,442,446,437,107,
C                  203-204,813,899-[899+MXSRC-1], and D3READ block character
C                  data
C
C                 For comets, this requests numeric fields in order:
C                  808-803,801-802,433-432,809,403-404,408-409,151,810-811,
C                  851-858,202-201,405-407,156,152-153,0,411,417,442,410,437,
C                  107,203-204,813,899-[899+MXSRC-1], and D3READ block character
C                  data
C
C               DASTCOM5 -- "all fields" (see NR8 setting notes)
C               ------------------------
C                 IR8ORD(1)=-5, ICHORD() is ignored
C
C                 For both asteroids & comets, this requests numeric fields in
C                 order: 801-813,851-858,899-[899+MXSRC-1],401-448,201-204,
C                 101-108,151-156, and character fields 1-14.
C
C                 NOTE: As of initial release, DASTCOM5 has fields reserved
C                  for future use that may not be immediately populated with 
C                  data. Therefore, the "all fields" output option may return 
C                  many zero or blank-filled fields. These may be filled at 
C                  some later date.
C
C   NR8        Integer number of numeric parameters requested via IR8ORD(),
C              including space for the SRC vector if requested (quantity 899) 
C              (i.e., if an IR8ORD() list contains 899, then 
C
C                      NR8= number_of_parameters_in_list + MXSRC - 1).
C
C              The first NR8 values requested in IR8ORD() will be returned as 
C              output in the first NR8 slots of the R8OUT array passed to 
C              DXREAD, for each record read. 
C
C              NR8 must be <= NUMNS (see 'dxparms.inc').
C
C               If IR8ORD(1)=-3 (D3READ/DASTCOM3 legacy output order), setting 
C                NR8=18 will return the standard D3READ/DASTCOM3 set; NR8=37 
C                will return the extended set, with some derived quantities. 
C                No other values of NR8 are allowed.
C
C               If IR8ORD(1)=-4 (D3READ/DASTCOM4 legacy output order), then 
C                NR8 must be 99 and no other value is allowed.
C
C               If IR8ORD(1)=-5 (all defined DASTCOM5 data fields), then NR8 
C                must be 142 and no other value is allowed. 
C
C   ICHORD()   Integer array of codes specifying what character data to place 
C               in the CHOUT() array returned by DXREAD call. Ignored if
C               DASTCOM3/4/5 output short-cuts are specified (IR8ORD(1)=-3 or
C               IR8ORD(1)=-4 or IR8ORD(1)= -5)
C
C   NCH        Integer number of character parameters requested in ICHORD()
C               The first NCH values requested in ICHORD() will be returned
C               in the CHOUT array passed to DXREAD for a record read. 
C 
C               NOTE #1: If IR8ORD(1)=-3 or I8ORD(1)=-4  (i.e., DASTCOM3 or 
C                DASTCOM4 dataset return in legacy D3READ format), NCH must 
C                be 1 and no other value is allowed. The CHOUT passed in must 
C                contain at least 217 bytes total; bigger is OK.
C
C               NOTE #2: IF IR8ORD(1)=-5 (i.e., all DASTCOM5 fields), NCH must 
C                be 14 and no other value is allowed. 
C
C   BUF        Logical flag. Hook for future use. Currently ignored, but
C                set .FALSE. for now.
C
C   WARN       Logical flag specifying notification preference for database 
C               little/big-endian byte-order translation. There is a small
C               (pretty much negligible) speed penalty for dynamic conversion.
C               Display of the warning message can alert users who might be 
C               able to obtain a native binary format database for better 
C               performance. If users can't affect this (by getting a native
C               database format), message output would be annoying:
C
C               .TRUE. = If database byte-order translation is necessary,
C                         inform user with message display to standard output
C               .FALSE.= If database byte-order translation is necessary,
C                         do it silently without informing user
C
C               Performance tests indicate the conversion speed penalty to be 
C               insignificant and its recommended WARN be set to .FALSE. in
C               general.
C
C  Outputs:
C   ISTAT      Integer status code. Always check return value.
C                0= Success (no error)
C               -1= Failed initialization. 
C                    This must be considered fatal by user. Do not proceed.
C                    The database will not be accessible due to a calling 
C                    program error, system problem, or a damaged database. 
C                    Call DXERR to retrieve specific error message text.
C------------------------------------------------------------------------------
C DXBND -- DastcomX database BouNDaries. Entry-point to return arrays giving
C  logical & physical record-boundary database structure determined during 
C  the last call to DXINI.
C 
C  The relationship between physical database record numbers and the logical 
C  record numbers users pass to DXREAD to retrieve those records is ...
C
C              physical_record = logical_record - bias_for_zone
C
C  Logical ("look-up") record numbers are aliases for the physical record 
C  numbers. They provide flexibility; categories of objects can be physically 
C  moved in the database yet be requested using the same number. This
C  supports database compactness and provides users more consistent access. 
C
C  For example, when orbit knowledge for an unnumbered asteroid orbit is 
C  secured and the object is assigned an IAU number, it is relocated within
C  the database -- from the block of unnumbered asteroids "up" to the block
C  of numbered asteroids. Record aliasing avoids users having to change the 
C  request made for all the other objects that didn't get numbered even 
C  though they were all bumped down a slot in physical record number.
C
C  CONVERSION & HISTORICAL NOTE:  
C  The legacy D3READ subroutine internally reverses the signs of the stored
C  bias parameters and computes prec= lrec + bias. This arrives at the same
C  result. But be aware of this behavior if legacy code using the BIAS array
C  returned by D3INIT is adapted to use the unaltered output of this routine 
C  (for which prec= lrec - bias). The negative of the values returned by 
C  this call would need to be assigned, or the legacy code calculation of 
C  physical record changed.
C
C  Inputs:
c   None
C
C  Output:
C   IBND() ... DASTCOM logical record boundaries. 
C               IBND(1)  = Last numbered asteroid record
C               IBND(2)  = Last unnumbered asteroid record
C               IBND(3)  = Last comet record
C               IBND(4)  = First numbered asteroid record
C               IBND(5)  = First unnumbered asteroid record
C               IBND(6)  = First comet record
C
C   IBIAS() .. Record bias parameters
C               IBIAS(1) = Numbered asteroid record bias
C               IBIAS(2) = Unnumbered asteroid record bias
C               IBIAS(3) = Comet record bias 
C------------------------------------------------------------------------------
C DXDAT1 -- DastcomX database DATa 1. Entry-point to return integer version 
C  number of database(s) currently open from last call to DXINI.
C
C Inputs:
C  None
C
C Output:
C  VERS ........ Database version code:
C                 0= No open database
C                 3= DASTCOM3 is open
C                 4= DASCTOM4 is open
C                 5= DAST and/or DCOM5 is open
C------------------------------------------------------------------------------
C DXDAT2 -- DastcomX database DATa 2. Entry-point to return file names of 
C  database(s) currently open from last call to DXINI.
C
C  Note that the data return-order is fixed and not necessarily the order in 
C  which the database(s) were loaded or presented in the call to DXINI.
C
C Inputs:
C  None
C
C Output:
C  FILNM() ..... Filename of opened database(s)
C                 FILNM(1) is asteroid database
C                 FILNM(2) is comet database
C------------------------------------------------------------------------------
C DXDAT3 -- DastcomX database DATa 3. Entry-point to return logical flags 
C  corresponding to opened database(s) currently open from last call to DXINI.
C
C  Note that the data return-order is fixed, not necessarily the order in 
C  which the database(s) were loaded or presented in the call to DXINI.
C
C Inputs:
C  None
C
C  LFLG() ...... Logical flag for database existence
C                 LFLG(1) is for asteroid database (.TRUE. or .FALSE.)
C                 LFLG(2) is for comet database (.TRUE. or .FALSE.)
C------------------------------------------------------------------------------
C DXDAT4 -- DastcomX database DATa 4. Entry-point to return character flags 
C  indicating byte-order (endian-ness) of database(s) currently open from 
C  last call to DXINI
C
C  Note that the data return-order is fixed, not necessarily the order in 
C  which the database(s) were loaded or presented in the call to DXINI.
C
C Inputs:
C  None
C 
C Output:
C  DBBYT() ..... Character flag indicating byte-order of database; 
C                big-endian ('b') or little-endian ('l')
C                  DBBYT(1) is for asteroid database ('b' or 'l' or ' ')
C                  DBBYT(2) is for comet database ('b' or 'l' or ' ')
C------------------------------------------------------------------------------
C DXDAT5 -- DastcomX database DATa 5. Entry-point to return character calendar 
C  date of creation for corresponding database(s) currently open from last call
C  to DXINI.
C
C  Note that the data return-order is fixed, not necessarily the order in 
C  which the database(s) were loaded or presented in the call to DXINI.
C
C Inputs:
C  None
C 
C Output:
C  CAL() ....... Database creation date, calendar format YYYY-MM-DD_HH:MN:SC
C                 CAL(1) is for asteroid database 
C                 CAL(2) is for comet database
C------------------------------------------------------------------------------
C DXDAT6 -- DastcomX database DATa 6. Entry-point to return Julian day of 
C  creation for corresponding database(s) currently open from last call to 
C  DXINI.
C
C  Note that the data return-order is fixed, not necessarily the order in 
C  which the database(s) were loaded or presented in the call to DXINI.
C
C Inputs:
C  None
C 
C Output:
C  JD() ....... Database creation date, Julian astronomical day
C                 JD(1) is for ast. database
C                 JD(2) is for comet database
C------------------------------------------------------------------------------
C DXLBL -- DastcomX database LaBeLs. Entry-point returns an array of strings
C  with each element containing the name label of the corresponding quantity in
c  the user-input array of quantity code integer(s). 
C
C  Optionally returns data for the list of quantities requested by the last 
C  call to DXREAD (i.e., last record read).
C
C  Inputs:
C   NLBL ..... Number of ID codes in CODES() array [logical length of CODES()]
C               If NLBL > 0:
C                Arrays CODES(), LABELS(), and ILNL() must be dimensioned
C                at least X, where NLBL <= X <= NUMF 
C               If NLBL = 0: 
C                CODES(), LABELS(), and ILNL() for last call to to DXREAD are 
C                returned; if DXINI was initialized with IR8OUT(1)=-3 or -4
C                (D3READ/DASTCOM 3 or 4 legacy output) and DXREAD hasn't been 
C                called since DXINI, no values are returned here since it     
C                isn't known whether legacy asteroid or comet output will occur.
C                However, IR8ORD(1)= -5 (DASTCOM5 "all-fields") will return
C                values given only an DXINI initialization call, as will
C                a user-specified output list (IR8ORD(1...) >= 0)
C   CODES()... Array of integer quantity codes for which to retrieve labels
C
C  Output:
C   NLBL ..... Unchanged if NLBL input > 0
C               Otherwise, over-written with number of entries in predefined
C               list in output CODES(), LABELS(), and INLN() arrays
C   CODES()... Unchanged if NLBL input > 0 
C               If NLBL = 0: CODES() is over-written with the quantities 
C               specified during the last call to DXINI/DXREAD.
C   LABELS().. Name labels for the corresponding quantity codes in CODES()
C   ILNL() ... Integer array giving length of corresponding names in LABELS()
C   ISTAT .... Integer error code
C                -1= NLBL out of range
C                -2= Not initialized by DXINI was needed when input NLBL= 0
C                -3= Legacy D3READ format initialized but no read yet, as
C                     required when NLBL=0 (can't tell if asteroid/comet
C                     record will be read).
C------------------------------------------------------------------------------
C DXERR -- DastcomX ERRor. Entry-point to return a string containing any
C  error message from last call to DXREAD. Call immediately after DXREAD 
C  returns a non-zero ISTAT, if message text is wanted.
C
C  Argument CERRMS should be nominally declared at least length 340 to allow  
C  for maximum 80-character text possibly concatenated with a file name 
C  theoretically up to 256 characters in length.
C
C  Inputs:
C   None
C 
C  Output:
C   CERRMS ... error message text associated with last call to DXREAD.
C   LERR ..... integer giving trimmed, non-blank length of CERRMS string
C               (i.e., the position of the last non-blank character in CERRMS 
C               such that output of CERRMS(1:LERR) will show the entire 
C               message without trailing blanks)
C------------------------------------------------------------------------------
C DXCLOS-- DastcomX database CLoSe. Entry point to close small-body database
C  reader package. Call prior to any second call to DXINI.
C
C  Inputs:
C   None 
C
C  Output:
C   None (releases internal units and flags)
C------------------------------------------------------------------------------
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
C** Declare database record variables
      CHARACTER*1  BEGINP*24, ENDPT*24, CALDATE*19, FTYP
      REAL*8       JDDATE
      INTEGER*4    BIAS(3), BND(6)
C
C** Labeled quantity declarations below are shown for reference, not used.
C** The quantities are instead read into index-addressable arrays.
C
C     CHARACTER*1  ASTNAM*18, COMNAM*29, EQUNOX*4, IREF*10, DESIG*13
C     CHARACTER*1  PENAM*6, SBNAM*12, ASTEST*8, COMEST*14
C     CHARACTER*1  SPTYPT*5, SPTYPS*5
C     CHARACTER*1  DARC*9, COMNT1*41, COMNT2*80, COMNT3*49
C     REAL*4       H, G, M1, M2, K1, K2, PHCOF, A1, A2, A3, DT, S0, TCL
C     REAL*4       R0, ALN, NM, NN, NK, LGK, RHO, AMRAT
C     REAL*4       AJ1, AJ2, ET1, ET2, DTH, ALF, DEL
C     REAL*4       SPHLM3, SPHLM5, RP, GM, RAD, EXTNT1, EXTNT2, EXTNT3
C     REAL*4       MOID, ALBEDO, BVCI, UBCI, IRCI
C     REAL*4       RMSW, RMSU, RMSN, RMSNT, RMSH, RMSMT, RMSMN
C     REAL*8       EPOCH, CALEPO, MA, W, OM, IN, EC, A, TP, TPCAL
C     REAL*8       TPFRAC, QR, OBSFRST, OBSLAST, SOLDAT
C     INTEGER*4    NO, NOBS
C     INTEGER*2    IPYR, NDEL, NDOP, NOBSMT, NOBSMN
C     INTEGER*1    PRELTV, SPHMX3, SPHMX5, JGSEP, TWOBOD, NSATS 
C     INTEGER*1    UPARM, LSRC 
C
C   Older compilers that don't support "INTEGER*1" may support alternatives 
C   "LOGICAL*1", or "BYTE".
C      LOGICAL*1     I1P(MXI1P)
C      BYTE          I1P(MXI1P)
      INTEGER*1     I1P(MXI1P)
      INTEGER*2     I2P(MXI2P)
      INTEGER*4     I4P(MXI4P) 
      REAL*4        R4P(MXR4P)
      REAL*8        R8P(MXR8P)
C
C** Declare additional DASTCOM3/4 header variables (obsolete forms).
C   Lower-case variables are unused place-holders.
      CHARACTER*1   BEGINP6*18, ENDPT6*18, FILL105*105, LBL*1, equnox*4
      REAL*8        calepo
C
C** Declare local variables
      CHARACTER*1   ERRMS*340
      INTEGER       I, J, K, RECL0, PREC, BNDSV(6), BIASV(3)
      INTEGER*2     BYTE2A, BYTE2C
      INTEGER*4     BYTE4
      LOGICAL       EXISTS, OPND, ZONE(3)
      SAVE          ERRMS, ZONE
C
C** Declare variables established by entry point DXINI
      CHARACTER*1   BTYP*13, CAL1*19, CAL2*19, FILA*256, FILB*256
      REAL*8        JD1, JD2
      INTEGER       UAST, UCOM, U3, U4, IVERS, NR8S, NCHS
      INTEGER       IRORD(NUMNS), ICORD(NUMCH), IU(2)
      LOGICAL       SMBINIT, SWAPA, SWAPC, SWAP3, SWAP4
      LOGICAL       D3OUT, D4OUT, D5OUT
      SAVE          BND, BIAS, FTYP, SMBINIT, UAST, UCOM, U3, U4, IVERS
      SAVE          SWAPA, SWAPC, SWAP3, SWAP4
      SAVE          IRORD, ICORD, D3OUT, D4OUT, D5OUT
      SAVE          NR8S, NCHS
      SAVE          CAL1, CAL2, JD1, JD2, BTYP, FILA, FILB
C
C** Declare more local parameters (database physical record sizes)
      INTEGER       RECL5A, RECL5C, RECL3, RECL4, RECL5(2)
      PARAMETER( RECL5A= 835 )  ! DAST5 record size, bytes
      PARAMETER( RECL5C= 976 )  ! DCOM5 record size, bytes
      PARAMETER( RECL3 = 395 )  ! (OLD) DASTCOM3 record length, bytes
      PARAMETER( RECL4 = 823 )  ! (OLD) DASTCOM4 record length, bytes
C
C** Declare external functions
      INTEGER       LENTRM      ! Generic string-length function
      LOGICAL       LTLEND      ! Byte-order determination function
C
C** Assign current database labels and codes
      INCLUDE 'curdef.inc'
C
C** Assign legacy-output fields and their order
      INCLUDE 'legdef.inc'
C
C** Verify successful initialization
      IF ( .NOT. SMBINIT ) THEN
       ISTAT= -1
       ERRMS= 'DXREAD: '//
     & 'Small-body database unavailable, no initialization'
       RETURN
      END IF
C
C** Check IOBJ for quick return cases. If doing a top-to-bottom scan without 
C   regard to zone boundaries, this error could be ignorable (and Cases 2-3
C   shouldn't occur for DASTCOM5+), but let user-processing decide.
C
C     Case 1: user request before or after last object record
C     Case 2: user request between numbered and unnumbered asteroids
C     Case 3: user request between unnumbered asteroids and comets
C
      IF ( (IOBJ .LE. 0) .OR. (IOBJ .GT. BND(3)) .OR.    
     &   ( (IOBJ .GT. BND(1)) .AND. (IOBJ .LT. BND(5)) ) .OR.
     &   ( (IOBJ .GT. BND(2)) .AND. (IOBJ .LT. BND(6)) ) ) THEN
       ISTAT= -2
       WRITE( ERRMS, '(A,I8,A)') 'DXREAD: requested IOBJ= ',IOBJ,
     &  ' is out of bounds, no action taken.' 
       RETURN
      END IF
C
C** Initialize variables. 
      ISTAT= 0
      ERRMS= ' '
      ACHARP= ' '
      CCHARP= ' '
      CXP= ' '
      IZONE= 0
      LSRC= 0
      DO I= 1,3 
       ZONE(I)= .FALSE.
       BIASV(I)= 0
       BNDSV(I)= 0
       BNDSV(I+3)= 0
      END DO
C
C** Clear output arrays
      J= LEN(CHOUT(1))
      IF ( J .EQ. 80 ) THEN ! Character array
       J= NCHS
      ELSE                  ! Character block
       J= 1
      END IF
      DO I= 1,J
       CHOUT(I)= ' '
       R8OUT(I)= 0.D0
      END DO
      DO I= J+1,NR8S
       R8OUT(I)= 0.D0
      END DO
C
C** Clear internal data arrays, MXR8P >= MXR4P,MXI4P,MXI2P,MXI1P
      DO I= 1, MXR8P
       R8P(I)= 0.D0
       IF ( I .LE. MXR4P ) R4P(I)= 0.0
       IF ( I .LE. MXI4P ) I4P(I)= 0
       IF ( I .LE. MXI2P ) I2P(I)= 0
       IF ( I .LE. MXI1P ) I1P(I)= 0
      END DO
C
C** Determine type of object. Assign IZONE integer for return.
      IF ( IOBJ .GE. BND(6) ) THEN
       IZONE= 3   ! Comet
      ELSE IF ( IOBJ .GE. BND(5) ) THEN
       IZONE= 2   ! Unnumbered asteroid
      ELSE IF ( IOBJ .GE. BND(4) ) THEN
       IZONE= 1   ! IAU-numbered asteroid
      END IF
C
      ZONE(IZONE)= .TRUE.
C
C** Assign corresponding physical record
      PREC= IOBJ - BIAS(IZONE)
C
C** Read physical data record from appropriate database
C
C~~ Read DAST5 record
C
C   The indexed-array read used is equivalent to this labeled read ...
C
C      READ(UAST, REC= PREC, IOSTAT= ISTAT)
C     +  NO, NOBS, OBSFRST, OBSLAST, EPOCH, CALEPO, MA, W, OM, IN, EC, 
C     +  A, QR, TP, TPCAL, TPFRAC, SOLDAT, (SRC(I),I=1,45), PRELTV,
C     +  SPHMX3, SPHMX5, JGSEP, TWOBOD, NSATS, UPARM, LSRC, NDEL, NDOP,
C     +  H, G, A1, A2, A3, R0, ALN, NM, NN, NK, LGK, RHO, AMRAT, ALF, 
C     +  DEL, SPHLM3, SPHLM5, RP, GM, RAD, EXTNT1, EXTNT2, EXTNT3, MOID,
C     +  ALBEDO, BVCI, UBCI, IRCI, RMSW, RMSU, RMSN, RMSNT, RMSH,
C     +  EQUNOX, PENAM, SBNAM, SPTYPT, SPTYPS, DARC, COMNT1, COMNT2,
C     +  DESIG, ASTEST, IREF, ASTNAM
C
      IF ( (UAST .NE. 0) .AND. (IZONE .LE. 2) ) THEN
C
       READ(UAST, REC= PREC, IOSTAT= ISTAT) 
     &     (I4P(I),I=1,NI4PA),       ! Sequential,  NO    -> OBSLAST
     &     (R8P(I),I=1,NR8PA),       ! Sequential,  EPOCH -> SOLDAT
     &     (R8P(I),I=99,98+MXSRCA),  ! SRCA
     &     (I1P(I),I=1,NI1PA),       ! Sequential, PRELTV -> LSRC
     &     (I2P(I),I=2,3),           ! NDEL, NDOP
     &     (R4P(I),I=1,2),           ! H, G
     &     (R4P(I),I=8,10),          ! A1, A2, A3
     &     (R4P(I),I=12,16),         ! Non-grav. model R0 -> NK
     &     (R4P(I),I=19,21),         ! LGK, RHO, AMRAT
     &     (R4P(I),I=27,46),         ! ALF -> RMSH
     &     ACHARP
C
       IF ( ISTAT .NE. 0 ) THEN
        WRITE( ERRMS, '(A,I8,A,I5)') 'DXREAD: requested IOBJ= ',IOBJ,
     &   ' record failed on read, ISTAT= ',ISTAT 
        RETURN
       END IF
C
C** Convert byte-order (even prior to error check)
       IF ( SWAPA ) THEN
        CALL I4SWAP(  NI4PA,     I4P )
        CALL R8SWAP(  NR8PA,     R8P )
        CALL I2SWAP(      2,  I2P(2) )
        CALL R4SWAP(      2,  R4P(1) )
        CALL R4SWAP(      3,  R4P(8) )
        CALL R4SWAP(      5, R4P(12) )
        CALL R4SWAP(      3, R4P(19) )
        CALL R4SWAP(     20, R4P(27) )
       END IF
C
C** Check for extended SRC. Disable if in legacy output mode.
C   Otherwise, byte-swap SRC vector if needed.
       IF ( D4OUT .AND. (I1P(8) .GT. 21) ) THEN
        I1P(8)= 0
        DO I= 1,MXSRCA
         R8P(98+I)= 0.D0
        END DO
       ELSE IF ( SWAPA ) THEN
        CALL R8SWAP( MXSRCA, R8P(99) )
       END IF
C
C** Check for record match. Failure means corrupt record or problem with
C** byte-order detection and swapping.
       IF ( I4P(1) .NE. IOBJ ) THEN
        ISTAT= -3
        WRITE( ERRMS, '(A,I8,A,I8)') 'DXREAD: requested IOBJ= ',IOBJ,
     &   ' does not match internal record NO= ',I4P(1)
        RETURN
       END IF
C
C** Fill out arrays with derived parameters and parameters from comment fields
       CXP= ACHARP
       CALL DXSUPLQ( ZONE,R8P,R4P,I4P,I2P,I1P,CXP,ERRMS,ISTAT )
       IF ( ISTAT .NE. 0 ) RETURN
C
C~~ Read DCOM5 record
C
C   The indexed-array read is equivalent to this labeled read ...
C
C      READ(UCOM, REC=PREC, IOSTAT= ISTAT)
C     +  NO, NOBS, OBSFRST, OBSLAST, EPOCH, CALEPO, MA, W, OM, IN, EC, 
C     +  A, QR, TP, TPCAL, TPFRAC, SOLDAT, (SRC(I),I=1,55), PRELTV,
C     +  SPHMX3, SPHMX5, JGSEP, TWOBOD, NSATS, UPARM, LSRC, IPYR, NDEL, 
C     +  NDOP, NOBSMT, NOBSMN, H, G, M1, M2, K1, K2, PHCOF, A1, A2, A3,
C     +  DT, R0, ALN, NM, NN, NK, S0, TCL, RHO, AMRAT, AJ1, AJ2, ET1, ET2,
C     +  DTH, ALF, DEL, SPHLM3, SPHLM5, RP, GM, RAD, EXTNT1, EXTNT2,
C     +  EXTNT3, MOID, ALBEDO, RMSW, RMSU, RMSN, RMSNT, RMSMT, RMSMN, 
C     +  EQUNOX, PENAM, SBNAM, DARC, COMNT3, COMNT2, DESIG, COMEST,
C     +  IREF, COMNAM
C
      ELSE IF ( (UCOM .NE. 0) .AND. ZONE(3) ) THEN
C
       READ(UCOM, REC=PREC, IOSTAT= ISTAT) 
     &     (I4P(I),I=1,NI4PC),       ! Sequential,  NO    -> OBSLAST
     &     (R8P(I),I=1,NR8PC),       ! Sequential,  EPOCH -> SOLDAT
     &     (R8P(I),I=99,98+MXSRCC),  ! SRCC
     &     (I1P(I),I=1,NI1PC),       ! Sequential, PRELTV -> LSRC
     &     (I2P(1),I=1,NI2PC),       ! Sequential,   IPYR -> NOBSMN
     &     (R4P(I),I=1,18),          ! H -> TCL   
     &     (R4P(I),I=20,38),         ! RHO -> ALBEDO
     &     (R4P(I),I=42,45),         ! RMSW -> RMSNT
     &     (R4P(I),I=47,48),         ! RMSMT -> RMSMN
     &     CCHARP
C
       IF ( ISTAT .NE. 0 ) THEN
        WRITE( ERRMS, '(A,I8,A,I5)') 'DXREAD: requested IOBJ= ',IOBJ,
     &   ' record failed on read, ISTAT= ',ISTAT 
        RETURN
       END IF
C
C** Convert byte-order if necessary
       IF ( SWAPC ) THEN
        CALL I4SWAP(  NI4PC,     I4P )
        CALL R8SWAP(  NR8PC,     R8P )
        CALL R8SWAP( MXSRCC, R8P(99) )
        CALL I2SWAP(  NI2PC,     I2P )
        CALL R4SWAP(     38,  R4P(1) )
        CALL R4SWAP(      4, R4P(42) )
        CALL R4SWAP(      2, R4P(47) )
       END IF
C
C** Check for record match
       IF ( I4P(1) .NE. IOBJ ) THEN
        ISTAT= -3
        WRITE( ERRMS, '(A,I8,A,I8)') 'DXREAD: requested IOBJ= ',IOBJ,
     &   ' does not match internal record NO= ',I4P(1)
        RETURN
       END IF
C
C** Fill out arrays with derived parameters and parameters from comment fields
       CXP= CCHARP
       CALL DXSUPLQ( ZONE,R8P,R4P,I4P,I2P,I1P,CXP,ERRMS,ISTAT )
       IF ( ISTAT .NE. 0 ) RETURN
C
C~~ Read DASTCOM4 record. If there is an error, return empty output arrays.
      ELSE IF ( U4 .NE. 0 ) THEN

       CALL D4LEGR
     &  (U4,PREC,ZONE,SWAP4,I1P,I2P,I4P,R4P,R8P,CXP,ERRMS,ISTAT)
       IF ( ISTAT .NE. 0 ) RETURN
C
C** Check for record match. Failure means corrupt record or problem with
C** byte order detection and swapping.
       IF ( I4P(1) .NE. IOBJ ) THEN
        ISTAT= -3
        WRITE( ERRMS, '(A,I8,A,I8)') 'DXREAD: requested IOBJ= ',IOBJ,
     &   ' does not match internal record NO= ',I4P(1)
        RETURN
       END IF
C
C~~ Read DASTCOM3 record. If there is an error, return empty output arrays.
      ELSE IF ( U3 .NE. 0 ) THEN
C
       CALL D3LEGR
     &  (U3,PREC,ZONE,SWAP3,I1P,I2P,I4P,R4P,R8P,CXP,ERRMS,ISTAT)
       IF ( ISTAT .NE. 0 ) RETURN
C
C** Check for record match. Failure means corrupt record or problem with
C** byte order detection and swapping.
       IF ( I4P(1) .NE. IOBJ ) THEN
        ISTAT= -3
        WRITE( ERRMS, '(A,I8,A,I8)') 'DXREAD: requested IOBJ= ',IOBJ,
     &   ' does not match internal record NO= ',I4P(1)
        RETURN
       END IF

      ELSE
C
C~~ If here, then no DASTCOM5 database for the type of object requested
C    was loaded during initialization. For example, an asteroid is being
C    requested but no DAST5 was loaded.
C
       ISTAT= -4
       IF ( ZONE(3) ) THEN 
        WRITE( ERRMS, '(A,I8,A,I5)') 'DXREAD: requested comet IOBJ= ',
     &   IOBJ,' but no comet database was loaded'
       ELSE
        WRITE( ERRMS, '(A,I8,A,I5)') 'DXREAD: requested asteroid IOBJ= '
     &   ,IOBJ,' but no asteroid database was loaded'
       END IF
       RETURN 
C
      END IF
C
C** Assign dummy argument for return
      LSRC= I1P(8)
C
C** Having read the physical record numeric parameters into standard 
C   array slots, assign the output arrays according to user request.
C
C~~ DASTCOM3: Fill legacy output array assignments 
      IF ( D3OUT ) THEN
       DO I= 1,NR8S   
        IF ( ZONE(3) ) THEN    ! Comets
         IRORD(I)= D3NORDC(I)
        ELSE
         IRORD(I)= D3NORDA(I)  ! Asteroids
        END IF
       END DO
       DO I= NR8S+1,NUMNS ! Zero rest of array
        IRORD(I)= 0
       END DO
       CALL DXNASGN( NR8S,IRORD,LSRC,R8P,R4P,I4P,I2P,I1P,R8OUT )
       CALL D3CASGN( CXP, ZONE, CHOUT )
       RETURN
      END IF
C
C~~ DASTCOM4: fill LEGACY output array assignments
      IF ( D4OUT ) THEN
       DO I= 1, NR8S
        IF ( ZONE(3) ) THEN
         IRORD(I)= D4NORDC(I)  ! Comets
        ELSE
         IRORD(I)= D4NORDA(I)  ! Asteroids
        END IF
       END DO
       DO I= NR8S+1,NUMNS ! Zero rest of array
        IRORD(I)= 0
       END DO
       CALL DXNASGN( NR8S,IRORD,LSRC,R8P,R4P,I4P,I2P,I1P,R8OUT )
       CALL D4CASGN( CXP, ZONE, CHOUT )
       RETURN
      END IF
C
C~~ DASTCOM5: fill field output for record
      IF ( D5OUT ) THEN
       IF ( ZONE(3) ) THEN     ! Comets
        NR8S= D5NUMCS 
        NCHS= D5NUMCC
        CALL DXNASGN( NR8S,D5NORDC,LSRC,R8P,R4P,I4P,I2P,I1P,R8OUT )
        CALL DXCASGN( NCHS, D5CORDC, ZONE, CXP, CHOUT )
       ELSE                    ! Asteroids
        NR8S= D5NUMAS
        NCHS= D5NUMAC
        CALL DXNASGN( NR8S,D5NORDA,LSRC,R8P,R4P,I4P,I2P,I1P,R8OUT )
        CALL DXCASGN( NCHS, D5CORDA, ZONE, CXP, CHOUT )
       END IF
       RETURN
      END IF
C
C~~ Fill individual numeric field requests
      IF ( NR8S. GE. 1 )
     & CALL DXNASGN( NR8S,IRORD,LSRC,R8P,R4P,I4P,I2P,I1P,R8OUT )
C
C~~ Fill individual character field requests
      IF ( NCHS .GE. 1 ) CALL DXCASGN( NCHS, ICORD, ZONE, CXP, CHOUT )
C
      RETURN
C
C-------------------------------------------------------------------------------
C
      ENTRY DXINI(FNAM, IR8ORD, NR8, ICHORD, NCH, BUF, WARN, ISTAT)
C
C** Initialize error handling
      ISTAT= -1
      ERRMS= ' '
C
C** Check if database reader package is already initialized
      IF ( SMBINIT ) THEN
       ERRMS= 'DXINI: cannot re-initialize without calling DXCLOS'
       RETURN
      END IF
C
C** Check array-sizing values
      IF ( (NCH .GT. NUMCH) .OR. (NCH .LT. 0) ) THEN
       WRITE(ERRMS,'(A,I2)') 
     &  'DXINI: calling argument NCH out of range, must be 0 to ',NUMCH
       RETURN
      END IF
C
      IF ( (NR8 .GT. NUMNS) .OR. (NR8 .LT. 0) ) THEN
       WRITE(ERRMS,'(A,I3)') 
     &  'DXINI: calling argument NR8 out of range, must be 0 to ',NUMNS
       RETURN
      END IF
C
      IF ( (NR8+NCH) .LE. 0 .AND. (IR8ORD(1) .GE. 0 ) ) THEN
       WRITE(ERRMS,'(A)') 
     &  'DXINI: no output data requested, NR8 & NCH are zero'
       RETURN
      END IF
C
C** If two files are specified, check that they are different names
      IF ( FNAM(1) .EQ. FNAM(2) .AND. (LENTRM(FNAM(1)) .NE. 0) ) THEN
       WRITE(ERRMS,'(A)') 
     &  'DXINI: duplicate database file names provided'
       RETURN
      END IF
C
C** Initialize values to be used by entire subroutine (SAVEd)
      UAST= 0         ! Unit number if DAST5+ asteroid file
      UCOM= 0         ! Unit number if DCOM5+ comet file
      U3= 0           ! Unit number if DASTCOM3 file
      U4= 0           ! Unit number if DASTCOM4 file
      IVERS= 0        ! Database version code 
      NR8S= 0         ! Number of user-requested numeric parameters
      NCHS= 0         ! Number of user-requested character parameters
      BIAS(1)= 0      ! Numbered asteroid bias (phys_rec=log_rec-bias)
      BIAS(2)= 0      ! Unnumbered asteroid bias (phys_rec=log_rec-bias)
      BIAS(3)= 0      ! Comet bias (phys_rec=log_rec-bias)
      ZONE(1)= .FALSE.
      ZONE(2)= .FALSE.
      ZONE(3)= .FALSE.
      CAL1= ' '       ! Asteroid database creation date (calendar)
      CAL2= ' '       ! Comet database creation date (calendar)
      FILA= ' '       ! Asteroid database name
      FILB= ' '       ! Comet database name
      JD1= -9.9D99    ! Asteroid database creation date (Julian day)
      JD2= -9.9D99    ! Asteroid database creation date (Julian day)
      DO I= 1, 6      ! Zone start/stop logical record number boundaries
       BND(I)= 0
      END DO
      DO I= 1, NUMCH  ! Internal output request arrays
       ICORD(I)= 0
       IRORD(I)= 0
      END DO
      DO I= NUMCH+1,NUMNS
       IRORD(I)= 0
      END DO
      SWAPA= .FALSE.  ! Flag to swap byte-order on DAST5+
      SWAPC= .FALSE.  ! Flag to swap byte-order on DCOM5+
      SWAP3= .FALSE.  ! Flag to swap byte order on DASTCOM3
      SWAP4= .FALSE.  ! Flag to swap byte order on DASTCOM4
      D3OUT= .FALSE.  ! Flag to return DASTCOM3-type legacy output
      D4OUT= .FALSE.  ! Flag to return DASTCOM4-type legacy output
      D5OUT= .FALSE.  ! Flag to return DASTCOM5 "all-field" output
      SMBINIT= .FALSE.! Successful initialization flag
C
C** Initialize values internal to entry point (not SAVEd)
      FTYP= ' '       ! Test character (DB type) read from header byte-80
      BEGINP= ' '     ! Character string read from DASTCOM5+
      ENDPT= ' '      ! Character string read from DASTCOM5+
      BEGINP6= ' '    ! Character string read from DASTCOM3/4 header
      ENDPT6= ' '     ! Character string read from DASTCOM3/4 header
      BYTE4= 0        ! Test 4-byte integer read from DASTCOM3/4
      BYTE2A= 0       ! Test 2-byte integer read from DAST5+ header
      BYTE2C= 0       ! Test 2-byte integer read from DCOM5+ header
      BIASV(1)= 0
      BIASV(2)= 0
      BIASV(3)= 0
      DO I= 1,6
       BNDSV(I)= 0
      END DO
      ERRMS= ' '
C
C** Initialize default record sizes
      RECL5(1)= RECL5A
      RECL5(2)= RECL5C
C
C** Determine endian status of executing system
      BTYP= 'big-endian'
      IF ( LTLEND() ) BTYP= 'little-endian'
C
C** Check that at least one database file name is specified, or if
C   too many are specified. 
C
C   For DASTCOM3 or DASTCOM4, only 1 file can be specified
C   For DASTCOM5, 1 OR 2 files can be specified (DAST5 and DCOM5
C    do not both have to be loaded)
C
C   Start a loop that will check and count up to 2 files
C
      I= 0 
 10   CONTINUE
      ISTAT= -1
      IF ( LENTRM(FNAM(I+1)) .EQ. 0 ) THEN
       IF (I .EQ. 0) THEN
        ERRMS= 'DXINI: no database file #1 is specified'
        RETURN
       ELSE 
        GOTO 20         ! Skip file_2 tests if only file_1 is specified
       END IF
      ELSE
       IF ((I .GE. 1) .AND. (U3+U4 .NE. 0)) THEN
        ERRMS= 'DXINI: too many database files are specified'
        RETURN
       ELSE
        I= I + 1        ! Increment file name count
       END IF
      END IF
C
C** Get logical unit number(s). Halts if one cannot be found.
      CALL GETUNI2( IU(I) )
C
C** Check on database file existence
      INQUIRE(FILE=FNAM(I), EXIST=EXISTS)
      IF (.NOT. EXISTS) THEN
       WRITE(ERRMS,'(A)') 'DXINI: cannot find database "'//
     &  FNAM(I)(1:LENTRM(FNAM(I)))//'"'
       RETURN
      END IF
C
C** Test-open current file with a default record size (DAST5 assumed)
      OPEN(UNIT=IU(I), FILE=FNAM(I), STATUS='OLD', ACCESS='DIRECT',
     &     FORM='UNFORMATTED', RECL=RECL5(I), IOSTAT=ISTAT)
C
C** Halt on any problem opening the database
      IF (ISTAT .NE. 0) THEN
       WRITE(ERRMS,'(A,I4,A)') 
     &  'DXINI: error code ISTAT= ',ISTAT,' opening file "'//
     &  FNAM(I)(1:LENTRM(FNAM(I)))//'"'
       RETURN
      END IF
C
C** Read header to find type of file from byte 80 character
      READ(IU(I), REC=1, IOSTAT=ISTAT) 
     & BIAS(2), BEGINP, ENDPT, CALDATE, JDDATE, FTYP, BYTE2A, BIAS(1)
C
C** Halt on any problem reading the header
      IF (ISTAT .NE. 0) THEN
       WRITE(ERRMS,'(A,I4,A)') 
     &  'DXINI: error code ISTAT= ',ISTAT,' reading header'//
     &  ' of file '//FNAM(I)(1:LENTRM(FNAM(I)))
       RETURN
      END IF
C
C** If no type-marker, then treat as DASTCOM3
      IF ( ICHAR(FTYP) .EQ. 0 ) FTYP= 'A'
C
C** Reopen with proper record size if necessary and parse header
C   
C   Goal is to ... 
C
C    A) Leave opened, valid files of known type with proper record 
C        size and known unit numbers,
C
C    B) Set logical boundary arrays and bias parameters describing
C        database organization
C
C    C) Determine endianness of file(s) and any need for conversion,
C        converting header parameters as needed,
C
C++++++++++++++++++++++
C~~ DASTCOM5  (CURRENT)
C++++++++++++++++++++++
C
      IF (FTYP .EQ. '5') THEN ! DASTCOM5
C
C** Assume DAST5. Already opened and read as such. Test further by 
C   parsing header specifics.
       READ(BEGINP, '(3I8)', IOSTAT=ISTAT) BND(4), BND(5), BND(6)
       IF (ISTAT .NE. 0) THEN
        WRITE(ERRMS,'(A,I4,A)') 
     &  'DXINI: error code ISTAT= ',ISTAT,' reading DASTCOM5 BEGINP '
     &  //' of file '//FNAM(I)(1:LENTRM(FNAM(I)))
        RETURN
       END IF
C
C** Test header for consistency with DAST5 type
C
C++++++++
C== DAST5
C++++++++
C
       IF ((BND(6).EQ.0).AND.(BND(4).NE.0).AND.(BND(5).NE.0)) THEN
C
C** DAST5 so far: parse logical end-record values
        READ(ENDPT, '(3I8)', IOSTAT=ISTAT) BND(1), BND(2), BND(3)
        IF (ISTAT .NE. 0) THEN
         WRITE(ERRMS, '(A,I4,A)') 
     &    'DXINI: error code ISTAT= ',ISTAT,' reading asteroid ENDPT '
     &    //' of file '//FNAM(I)(1:LENTRM(FNAM(I)))
         RETURN
        ELSE IF (BND(3) .NE. 0) THEN
         ISTAT= -1
         WRITE(ERRMS,'(A)') 
     &    'DXINI: non-zero BND(3) is inconsistent with '
     &    //'an asteroid database, file '//FNAM(I)(1:LENTRM(FNAM(I)))
         RETURN
        END IF
C
C** Set default return error flag
        ISTAT= -1
C
C** Final DAST5 consistency check
        IF ( (BND(1)-BND(4).LE.0) .OR. (BND(2)-BND(5).LE.0)) THEN
         WRITE(ERRMS,'(A)') 'DXINI: invalid header boundaries in DAST5'
     &   //' file '//FNAM(I)(1:LENTRM(FNAM(I))) 
         RETURN
        END IF
C
C** Check for double load
        IF (UAST .NE. 0) THEN
         WRITE(ERRMS,'(A)') 
     &    'DXINI: two asteroid (DAST5) files were specified'
         RETURN
        END IF
C
C** DAST5 confirmed: check byte-order of asteroid file.
        IF (BYTE2A .NE. 26901) THEN
         CALL I2SWAP(1, BYTE2A)
         IF (BYTE2A .NE. 26901) THEN
          WRITE(ERRMS,'(A)') 'DXINI: corrupted or unreadable file '//
     &     FNAM(I)(1:LENTRM(FNAM(I)))
          RETURN
         ELSE
          IF ( WARN ) THEN
           WRITE(*,'(A)') 'WARNING: non-native binary database '//
     &      FNAM(I)(1:LENTRM(FNAM(I)))
           WRITE(*,'(A)') 'Obtain '//BTYP(1:LENTRM(BTYP))//
     &      ' native format to avoid dynamic conversion speed penalty.'
          END IF
         END IF
         SWAPA= .TRUE. 
         CALL I4SWAP(2, BIAS(2))
        END IF
C
C** Assign asteroid database type code 
        UAST= IU(I)
        CAL1= CALDATE
        JD1 = JDDATE
        FILA= FNAM(I)
C
C** Restore values from last successful DCOM5 open (if any)
        BND(3) = BNDSV(3)
        BND(6) = BNDSV(6)
        BIAS(3)= BIASV(3) 
C
C** Save confirmed DAST5 header values
        BNDSV(1)= BND(1)
        BNDSV(2)= BND(2)
        BNDSV(4)= BND(4)
        BNDSV(5)= BND(5) 
        BIASV(1)= BIAS(1)
        BIASV(2)= BIAS(2)
C
C** DCOM5 database (by default, not yet proven). Open and read with
C   proper record size.
C
C++++++++
C== DCOM5
C++++++++
C
       ELSE IF ((BND(6).NE.0).AND.(BND(4).EQ.0).AND.(BND(5).EQ.0)) THEN
        CLOSE( IU(I) )
        OPEN(UNIT=IU(I), FILE=FNAM(I), STATUS='OLD', ACCESS='DIRECT',
     &       FORM='UNFORMATTED', RECL=RECL5(2), IOSTAT=ISTAT)
C
C** Halt on any problem opening the database. Shouldn't occur since it
C   is a "re-open", but test just in case.
C
        IF (ISTAT .NE. 0) THEN
         WRITE(ERRMS,'(A,I4,A)') 
     &    'DXINI: error code ISTAT= ',ISTAT,' opening file "'//
     &    FNAM(I)(1:LENTRM(FNAM(I)))//'"'
         RETURN
        END IF
C
        READ(IU(I),REC=1,IOSTAT=ISTAT) 
     &   BIAS(3), BEGINP, ENDPT, CALDATE, JDDATE, FTYP, BYTE2C
        IF (ISTAT .NE. 0) THEN
         WRITE(ERRMS,'(A,I4,A)') 
     &    'DXINI: error code ISTAT= ',ISTAT,' reading header of '//
     &     FNAM(I)(1:LENTRM(FNAM(I)))
         RETURN
        END IF
C
C** DCOM5 (assumed): parse logical start-record values
        READ(BEGINP, '(3I8)', IOSTAT=ISTAT) BND(4), BND(5), BND(6)
        IF (ISTAT .NE. 0) THEN
         WRITE(ERRMS,'(A,I4,A)') 
     &   'DXINI: error code ISTAT= ',ISTAT,' reading DCOM5 BEGINP '
     &   //' of file '//FNAM(I)(1:LENTRM(FNAM(I)))
         RETURN
        ELSE IF ((BND(6).EQ.0).OR.(BND(4).NE.0).OR.(BND(5).NE.0)) THEN
         ISTAT= -1
         WRITE(ERRMS,'(A)') 'DXINI: header BEGINP is inconsistent with'
     &    //' a comet database, file '//FNAM(I)(1:LENTRM(FNAM(I)))
         RETURN
        END IF
C
C** DCOM5 so far: parse logical end-record values
        READ(ENDPT, '(3I8)', IOSTAT=ISTAT) BND(1), BND(2), BND(3)
        IF (ISTAT .NE. 0) THEN
         WRITE(ERRMS, '(A,I4,A)') 
     &    'DXINI: error code ISTAT= ',ISTAT,' reading asteroid ENDPT '
     &    //' of file '//FNAM(I)(1:LENTRM(FNAM(I)))
         RETURN
        ELSE IF ((BND(1).NE.0).OR.(BND(2).NE.0).OR.(BND(3).EQ.0)) THEN
         ISTAT= -1
         WRITE(ERRMS,'(A)') 'DXINI: header ENDPT is inconsistent '
     &    //'with a comet database, file '//FNAM(I)(1:LENTRM(FNAM(I)))
         RETURN
        END IF
C
C** Set default return error flag
        ISTAT= -1
C
C** Final DCOM5 consistency check
        IF ((BND(3)-BND(6)) .LE. 0) THEN
         WRITE(ERRMS,'(A)') 'DXINI: invalid header boundaries in DCOM5'
     &   //' file '//FNAM(I)(1:LENTRM(FNAM(I))) 
         RETURN
        END IF
C
C** Check for double load
        IF (UCOM .NE. 0) THEN
         WRITE(ERRMS,'(A)') 
     &    'DXINI: two comet (DCOM5) files were specified'
         RETURN
        END IF
C
C** DCOM5 confirmed: check byte-order of comet file.
        IF (BYTE2C .NE. 26901) THEN
         CALL I2SWAP(1, BYTE2C)
         IF (BYTE2C .NE. 26901) THEN
          WRITE(ERRMS,'(A)') 'DXINI: corrupted or unreadable file '//
     &     FNAM(I)(1:LENTRM(FNAM(I)))
          RETURN
         ELSE
          IF ( WARN ) THEN 
           WRITE(*,'(A)') 'WARNING: non-native binary database '//
     &      FNAM(I)(1:LENTRM(FNAM(I)))
           WRITE(*,'(A)') 'Obtain '//BTYP(1:LENTRM(BTYP))//
     &      ' native format to avoid dynamic conversion speed penalty.'
          END IF
         END IF
         SWAPC= .TRUE. 
         CALL I4SWAP(1, BIAS(3))
        END IF
C
C** Assign comet database information
        UCOM= IU(I)
        CAL2= CALDATE
        JD2 = JDDATE
        FILB= FNAM(I)
C
C** Restore values from last successful DAST5 open (if any)
        BND(1) = BNDSV(1)
        BND(2) = BNDSV(2)
        BND(4) = BNDSV(4)
        BND(5) = BNDSV(5)
        BIAS(1)= BIASV(1)
        BIAS(2)= BIASV(2)
C       
C** Save confirmed DCOM5 header values from this open
        BNDSV(3)= BND(3)
        BNDSV(6)= BND(6) 
        BIASV(3)= BIAS(3)
C
C** File labeled DASTCOM5, but header data is non-conforming
       ELSE
        WRITE(ERRMS,'(A)') 'DXINI: inconsistent BND(4-6) header for '
     &   //' DASTCOM5 labeled file '//FNAM(I)(1:LENTRM(FNAM(I))) 
        RETURN
       END IF
C
C** Assign database type code 
       IVERS= 5

C+++++++++++++++++++++++++++++++++++
C~~ DASTCOM3 and DASTCOM4 (OBSOLETE)
C+++++++++++++++++++++++++++++++++++
C
      ELSE IF ((FTYP .EQ. 'A') .OR. (FTYP .EQ. 'C')) THEN
C
C** Check for redundancy & set up distinctives for DASTCOM3 or DASTCOM4 
       IF (FTYP .EQ. 'A') THEN  ! DASTCOM3
        RECL0= RECL3
        LBL= '3'
       ELSE                     ! DASTCOM4
        RECL0= RECL4
        LBL= '4'
       END IF
C
C** Reopen database with proper record size
       CLOSE( IU(I) )
       OPEN(UNIT=IU(I), FILE=FNAM(I), STATUS='OLD', ACCESS='DIRECT',
     &      FORM='UNFORMATTED', RECL=RECL0, IOSTAT=ISTAT)
C
C** Halt on any problem opening the database
       IF (ISTAT .NE. 0) THEN
        WRITE(ERRMS,'(A,I4,A)') 
     &   'DXINI: error code ISTAT= ',ISTAT,' opening file '//
     &   FNAM(I)(1:LENTRM(FNAM(I)))
        RETURN
       END IF
C
C** Read header (lower-case variables are unused format place-holders)
       READ(IU(I), REC=1, IOSTAT=ISTAT) 
     &  BIAS(2), ENDPT6, JDDATE, calepo, equnox,
     &  BEGINP6, FILL105, BIAS(3)
       IF (ISTAT .NE. 0) THEN
        WRITE(ERRMS,'(A,I4,A)') 
     &   'DXINI: error code ISTAT= ',ISTAT,' reading header'//
     &   ' of file '//FNAM(I)(1:LENTRM(FNAM(I)))
        RETURN
       END IF
C
C** Parse logical start-record values
       READ(BEGINP6, '(3I6)', IOSTAT=ISTAT) BND(4), BND(5), BND(6)
       IF (ISTAT .NE. 0) THEN
        WRITE(ERRMS,'(A,I4,A)') 
     &  'DXINI: error code ISTAT= ',ISTAT,' reading DASTCOM'//
     &  LBL//' BEGINP '//' of file '//FNAM(I)(1:LENTRM(FNAM(I)))
        RETURN
       END IF
C
C** Parse logical end-record values
       READ(ENDPT6, '(3I6)', IOSTAT=ISTAT) BND(1), BND(2), BND(3)
       IF (ISTAT .NE. 0) THEN
        WRITE(ERRMS,'(A,I4,A)') 
     &   'DXINI: error code ISTAT= ',ISTAT,' reading DASTCOM'//
     &   LBL//' ENDPT '//' of file '//FNAM(I)(1:LENTRM(FNAM(I)))
        RETURN
       END IF
C
C** Final consistency check
       IF ( (BND(1)-BND(4).LE.0) .OR. (BND(2)-BND(5).LE.0) .OR.
     &      (BND(3)-BND(6).LE.0) ) THEN
        ISTAT= -1
        WRITE(ERRMS,'(A)') 'DXINI: invalid header boundaries in '//
     &  ' DASTCOM'//LBL//' file '//FNAM(I)(1:LENTRM(FNAM(I))) 
        RETURN
       END IF
C
C** DASTCOM3/4 confirmed: check byte-order of file.
       READ(IU(I), REC=26902, IOSTAT=ISTAT) BYTE4
       IF (ISTAT .NE. 0) THEN
        WRITE(ERRMS,'(A,I4,A)') 
     &   'DXINI: error code ISTAT= ',ISTAT,' reading magic number'//
     &   ' of file '//FNAM(I)(1:LENTRM(FNAM(I)))
        RETURN
       END IF
C
C** Set default return error flag
       ISTAT= -1
C
       IF (BYTE4 .NE. 26901) THEN
        CALL I4SWAP(1, BYTE4)
        IF ( BYTE4 .NE. 26901 ) THEN
         WRITE(ERRMS,'(A)') 'DXINI: corrupted or unreadable file '//
     &    FNAM(I)(1:LENTRM(FNAM(I)))
         RETURN
        ELSE
         IF ( WARN ) THEN
          WRITE(*,'(A)') 'WARNING: non-native binary database '//
     &     FNAM(I)(1:LENTRM(FNAM(I)))
          WRITE(*,'(A)') 'Obtain '//BTYP(1:LENTRM(BTYP))//
     &     ' native format to avoid dynamic conversion speed penalty.'
         END IF
        END IF
        IF (FTYP .EQ. 'A') THEN
         SWAP3= .TRUE. 
        ELSE
         SWAP4= .TRUE.
        END IF
        CALL I4SWAP(2, BIAS(2))
C
       END IF
C
C** Set constant for DASTCOM3 and DASTCOM4 (not being stored in file)
       BIAS(1)= -1
C
C** Save unit number and database integer code
       IF (FTYP .EQ. 'A') THEN
        U3= IU(I)
        IVERS= 3
       ELSE
        U4= IU(I)
        IVERS= 4
       END IF
       FILA= FNAM(I)
       FILB= FNAM(I)
       JD1 = JDDATE 
       CAL1= FILL105(87:105) ! Extract calendar date
C
C++++++++++
C== UNKNOWN
C++++++++++
C
      ELSE
       ISTAT= -1
       WRITE(ERRMS,'(A)') 
     & 'DXINI: unrecognized database type '//FTYP//
     & ' based on header of '//FNAM(I)(1:LENTRM(FNAM(I)))
       RETURN
      END IF
C
C** Set default return error flag
      ISTAT= -1
C
C** Check for incompatible file loading. This can happen here if
C   file #1 is DASTCOM5 and file #2 is DASTCOM3/4. Other situations
C   are filtered out above.
C
      IF ( (UAST+UCOM .NE. 0) .AND. (U3+U4 .NE. 0) ) THEN
       IVERS= 0
       WRITE(ERRMS,'(A)')
     & 'DXINI: cannot load different database types simultaneously'
       RETURN
      END IF
C
C** Process possible second database file (DASTCOM5)
      IF ( I .LT. 2 ) GOTO 10
C
  20  CONTINUE 
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C== Process list of data user wants returned during from reader call
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C** Initialize flags
      D3OUT= .FALSE.
      D4OUT= .FALSE.
      D5OUT= .FALSE.
      ISTAT= -1
C
C** Assign local list of parameters to return
C
C~~ D3READ/DASTCOM3 legacy output quantities and order
      IF ( IR8ORD(1) .EQ. -3 ) THEN
       IF ( (NR8 .NE. 18) .AND. (NR8 .NE. 37) ) THEN
        WRITE(ERRMS,'(A,I3,A)') 'DXINI: calling argument NR8= ',NR8,
     &  ' value not allowed for D3READ/DASTCOM3 output. See doc.'
        RETURN
       END IF
       IF ( NCH .NE. 1 ) THEN
        WRITE(ERRMS,'(A,I3,A)') 'DXINI: calling argument NCH= ',NCH,
     &  ' value not allowed for D3READ/DASTCOM3 output. See doc.'
        RETURN
       END IF
       D3OUT= .TRUE.  ! DASTCOM3 legacy output only (from D3NORDA/C lists)
C
C~~ D3READ/DASTCOM4 legacy output quantities and order
      ELSE IF ( IR8ORD(1) .EQ. -4 ) THEN
       IF ( NR8 .NE. 99 ) THEN
        WRITE(ERRMS,'(A,I3,A)') 'DXINI: calling argument NR8= ',NR8,
     &  ' value not allowed for D3READ/DASTCOM4 output. See doc.'
        RETURN
       END IF
       IF ( NCH .NE. 1 ) THEN
        WRITE(ERRMS,'(A,I3,A)') 'DXINI: calling argument NCH= ',NCH,
     &  ' value not allowed for D3READ/DASTCOM4 output. See doc.'
        RETURN
       END IF
       D4OUT= .TRUE.  ! DASTCOM4 legacy output only (from D4NORDA/C lists)
C
C** DASTCOM5 "all-field" output
      ELSE IF ( IR8ORD(1) .EQ. -5 ) THEN
       IF ( NR8 .NE. 142 ) THEN
        WRITE(ERRMS,'(A,I3,A)') 'DXINI: calling argument NR8= ',NR8,
     &  ' value not allowed for DASTCOM5 all-field output. See doc.'
        RETURN
       END IF
       IF ( NCH .NE. 14 ) THEN
        WRITE(ERRMS,'(A,I3,A)') 'DXINI: calling argument NCH= ',NCH,
     &  ' value not allowed for DASTCOM5 all-field output. See doc.'
        RETURN
       END IF
       D5OUT= .TRUE.  ! DASTCOM5 "all-field" parameters
C
C~~ Reject any other negative value
      ELSE IF ( IR8ORD(1) .LT. 0 ) THEN
       WRITE(ERRMS,'(A,I3,A)') 'DXINI: calling argument IR8ORD(1)= ',
     &  IR8ORD(1),' value not recognized. See documentation.'
       RETURN
C
C~~ DASTCOM5 (user-customized output quantities and order)
      ELSE
C
C   Do basic checks on user's output request, since it is not pre-defined
C
C    - No duplicated, non-zero quantities
C    - Requests are within allocated range for each data-type
C    - Enough space for SRC vector
C 
C   Requests for unavailable or incongruous data are not prevented, just
C   ignored (returned with zero-valued output).
C
C   The internal parameter arrays are temporarily used here to search for
C   duplicates, but are zeroed again in DXREAD before use to store data. 
C
C   Clear intrnl. arrays. Depends on (requires) MXR8P >= MXR4P,MXI4P,MXI2P,MXI1P
C
       DO I= 1, MXR8P
        R8P(I)= 0.D0
        IF ( I .LE. MXR4P ) R4P(I)  = 0.0
        IF ( I .LE. MXI4P ) I4P(I)  = 0
        IF ( I .LE. MXI2P ) I2P(I)  = 0
        IF ( I .LE. MXI1P ) I1P(I)  = 0
        IF ( I .LE. NUMCH ) ICORD(I)= 0
       END DO
C
C~~ Step through user character-data request array
       I= 1
       DO WHILE ( I .LE. NCH )
        IF ( ICHORD(I) .EQ. 0 ) THEN ! place-holder
         CONTINUE 
        ELSE IF ( (ICHORD(I) .GT. NUMCH) .OR. (ICHORD(I) .LT. 0) ) THEN
         WRITE(ERRMS,'(A,I2,A,I3,A,I2)') 
     &   'DXINI: calling argument ICHORD(',I,')= ',ICHORD(I),
     &   ' requests a field # outside the range of 1 to ',NUMCH
         RETURN
        ELSE IF ( ICORD(ICHORD(I)) .NE. 0 ) THEN 
         WRITE(ERRMS,'(A,I2,A,I3,A)') 
     &   'DXINI: calling argument ICHORD(',I,')= ',ICHORD(I),
     &   '; duplicate field requested'
         RETURN
        ELSE
         ICORD(ICHORD(I))= 1  ! mark request,
        END IF
        I= I + 1
       END DO
C
C~~ Step through user-requested numeric data array
       I= 1
       DO WHILE ( I .LE. NR8 )
C
C Place-holder entry
        IF ( IR8ORD(I) .EQ. 0 ) THEN
         CONTINUE 
C
C Check REAL*8 requests
        ELSE IF ( (IR8ORD(I).GT.800) .AND. (IR8ORD(I).LE.800+RR8) ) THEN
C
         IF ( R8P(IR8ORD(I)-800) .NE. 0.D0 ) THEN
          WRITE(ERRMS,'(A,I3,A,I3,A)') 
     &    'DXINI: calling argument IR8ORD(',I,')= ',IR8ORD(I),
     &    '; duplicate field requested'
          RETURN
         ELSE
          R8P(IR8ORD(I)-800)= 1.D0  ! mark request
         END IF
C
C Check for enough SRC vector space. 
C
C If NP is the number of estimated dynamic parameters ...
C
C   LSRC= NP * ( NP + 1 ) / 2
C   NP  = -0.5 + SQRT( 1 + 8*LSRC ) / 2
C
C   DCOM5 can have 10-parameter comet solutions (55 slots)
C   DAST5 can have  9-parameter asteroid solutions (45 slots)
C   DASTCOM4 can have 10-parameter comet solutions (55 slots) and
C     6-parameter asteroid solutions (21 slots), but must allow 
C     for the larger comet record read, so 55.
C   DASTCOM3 provides no covariance data. Request 899 is ignored.
C
         IF ( IR8ORD(I) .EQ. 899 ) THEN
          J= 1 
          DO WHILE ( (IR8ORD(I+J) .EQ. 0) .AND. (I+J .LE. NR8) )  
           J= J + 1
          END DO
          I= I + J - 1 ! Adjust index to last slot found to be zero
          K= 0         ! Initialize required maximum number of slots fail flag
          IF ( (U4+UCOM .NE. 0) .AND. (J .LT. MXSRC) ) THEN ! 10-param. comet
           K= MXSRC
          ELSE IF ( (UAST .NE. 0) .AND. (J .LT. 45) ) THEN  !  9-param. ast.
           K= 45
          END IF
          IF ( K .NE. 0 ) THEN
           WRITE(ERRMS,'(A,I3,A,I2,A,I2)') 
     &     'DXINI: quantity 899 requested in IR8ORD(',I,'), but '//
     &     'insufficient space in array follows; need ',K,
     &     ' empty slots, find ',J
           RETURN
          END IF
         END IF
C
C Check REAL*4 requests
        ELSE IF ((IR8ORD(I).GT.400) .AND. (IR8ORD(I).LE.400+RR4D)) THEN 
         IF ( R4P(IR8ORD(I)-400) .NE. 0.0 ) THEN
          WRITE(ERRMS,'(A,I3,A,I3,A)')
     &    'DXINI: calling argument IR8ORD(',I,')= ',IR8ORD(I),
     &    ' duplicates an already requested quantity'
          RETURN
         ELSE
          R4P(IR8ORD(I)-400)= 1.0  ! mark request
         END IF
C
C Check INTEGER*4 requests
        ELSE IF ((IR8ORD(I).GT.200) .AND. (IR8ORD(I).LE.200+RI4D)) THEN
         IF ( I4P(IR8ORD(I)-200) .NE. 0 ) THEN
          WRITE(ERRMS,'(A,I3,A,I3,A)')
     &    'DXINI: calling argument IR8ORD(',I,')= ',IR8ORD(I),
     &    ' duplicates an already requested quantity'
          RETURN
         ELSE
          I4P(IR8ORD(I)-200)= 1  ! mark request
         END IF
C
C Check INTEGER*2 requests
        ELSE IF ((IR8ORD(I).GT.150) .AND. (IR8ORD(I).LE.150+RI2D)) THEN
         IF ( I2P(IR8ORD(I)-150) .NE. 0 ) THEN
          WRITE(ERRMS,'(A,I3,A,I3,A)')
     &    'DXINI: calling argument IR8ORD(',I,')= ',IR8ORD(I),
     &    ' duplicates an already requested quantity'
          RETURN
         ELSE
          I2P(IR8ORD(I)-150)= 1  ! mark request
         END IF
C
C Check INTEGER*1 requests
        ELSE IF ((IR8ORD(I).GT.100) .AND. (IR8ORD(I).LE.100+RI1D)) THEN
         IF ( I1P(IR8ORD(I)-100) .NE. 0 ) THEN
          WRITE(ERRMS,'(A,I3,A,I3,A)')
     &    'DXINI: calling argument IR8ORD(',I,')= ',IR8ORD(I),
     &    ' duplicates an already requested quantity'
          RETURN
         ELSE
          I1P(IR8ORD(I)-100)= 1  ! mark request
         END IF
        ELSE 
C
C Unknown numeric request
         WRITE(ERRMS,'(A,I3,A,I4,A)') 
     &   'DXINI: calling argument IR8ORD(',I,')= ',IR8ORD(I),
     &   ' is an undefined quantity'
         RETURN
        END IF
C
C~~ Increment to next item in numeric request array
        I= I + 1
       END DO
      END IF
C
C** With user requested output now filtered, assign IR8ORD() and ICHORD() 
C   to internal IRORD() and ICORD() (over-writing their prior use as flags
C   to detect duplicate requestrs).
C
C~~ D3 and D4 legacy output are assigned in DXREAD on the fly, once it 
C   is known whether the record is an asteroid or comet. D5 "all-fields"
C   is also predefined.
C
      IF ( D3OUT .OR. D4OUT .OR. D5OUT ) THEN
       CONTINUE
C
      ELSE
       IF ( NR8 .GE. 1 ) THEN
        DO I= 1,NR8
         IRORD(I)= IR8ORD(I)
        END DO
       END IF
       IF ( NCH .GE. 1 ) THEN
        DO I= 1,NUMCH
         ICORD(I)= ICHORD(I)
        END DO
       END IF
      END IF
C
C** Clear error flag, mark as successful initialization. 
      ISTAT= 0
      ERRMS= ' '
      NR8S= NR8
      NCHS= NCH
      SMBINIT= .TRUE.
      RETURN
C
C------------------------------------------------------------------------------
C
      ENTRY DXBND( IBND, IBIAS ) 
      DO I= 1,6
       IBND(I)= 0      
       IF ( SMBINIT ) IBND(I)= BND(I) 
      END DO
      DO I= 1,3
       IBIAS(I)= 0      
       IF ( SMBINIT ) IBIAS(I)= BIAS(I) 
      END DO
      RETURN
C
C------------------------------------------------------------------------------
C
      ENTRY DXDAT1( VERS )
      VERS= 0
      IF ( SMBINIT ) VERS= IVERS 
      RETURN
C
C------------------------------------------------------------------------------
C
      ENTRY DXDAT2( FILNM )
      FILNM(1)= ' '
      FILNM(2)= ' '
      IF ( SMBINIT ) THEN
       FILNM(1)= FILA
       FILNM(2)= FILB
      END IF
      RETURN
C
C------------------------------------------------------------------------------
C
      ENTRY DXDAT3( LFLG )
C
C** Initialize default return
      LFLG(1)= .FALSE.
      LFLG(2)= .FALSE.
C
C** Assign values
      IF ( SMBINIT ) THEN
C
C~~ DASTCOM3
       IF ( IVERS .EQ. 3 ) THEN
        LFLG(1)= .TRUE. ! asteroid database
        LFLG(2)= .TRUE. ! comet database
C
C~~ DASTCOM4
       ELSE IF ( IVERS .EQ. 4 ) THEN
        LFLG(1)= .TRUE. ! asteroid database
        LFLG(2)= .TRUE. ! comet database
C
C~~ DASTCOM5+
       ELSE
        IF ( UAST .NE. 0 ) LFLG(1)= .TRUE. ! asteroid database
        IF ( UCOM .NE. 0 ) LFLG(2)= .TRUE. ! comet database
       END IF
      END IF
      RETURN
C
C------------------------------------------------------------------------------
C
      ENTRY DXDAT4( DBBYT )
C
C** Initialize default return
      DBBYT(1)= ' '
      DBBYT(2)= ' '
C
C** Assign values
      IF ( SMBINIT ) THEN
C
C~~ DASTCOM3
       IF ( IVERS .EQ. 3 ) THEN
        DBBYT(1)= 'b'
        IF ( (BTYP(1:1) .EQ. 'b' .AND. SWAP3) .OR.
     &       (BTYP(1:1) .EQ. 'l' .AND. (.NOT. SWAP3)) ) DBBYT(1)= 'l'
        DBBYT(2)= DBBYT(1)
C
C~~ DASTCOM4
       ELSE IF ( IVERS .EQ. 4 ) THEN
        DBBYT(1)= 'b'
        IF ( (BTYP(1:1) .EQ. 'b' .AND. SWAP4) .OR.
     &       (BTYP(1:1) .EQ. 'l' .AND. (.NOT. SWAP4)) ) DBBYT(1)= 'l'
        DBBYT(2)= DBBYT(1)
       ELSE
C
C~~ DAST5+
        IF ( UAST .NE. 0 ) THEN ! asteroid database
         DBBYT(1)= 'b'
         IF ( (BTYP(1:1) .EQ. 'b' .AND. SWAPA) .OR.
     &        (BTYP(1:1) .EQ. 'l' .AND. (.NOT. SWAPA)) ) DBBYT(1)= 'l'
        END IF
C
C~~ DCOM5+
        IF ( UCOM .NE. 0 ) THEN ! comet database
         DBBYT(2)= 'b'
         IF ( (BTYP(1:1) .EQ. 'b' .AND. SWAPC) .OR.
     &        (BTYP(1:1) .EQ. 'l' .AND. (.NOT. SWAPC)) ) DBBYT(2)= 'l'
        END IF
       END IF
      END IF
      RETURN
C
C------------------------------------------------------------------------------
C
      ENTRY DXDAT5( CAL )
C
C** Initialize
      CAL(1)= ' '
      CAL(2)= ' '
      IF ( SMBINIT ) THEN
       IF ( IVERS .LT. 5 ) THEN
        CAL(1)= CAL1
        CAL(2)= CAL2
       ELSE
        IF ( UAST .NE. 0 ) CAL(1)= CAL1 ! asteroid database
        IF ( UCOM .NE. 0 ) CAL(2)= CAL2 ! comet database
       END IF
      END IF
      RETURN
C
C------------------------------------------------------------------------------
C
      ENTRY DXDAT6( JD )
C
C** Initialize
      JD(1)= -9.9D99
      JD(2)= -9.9D99
      IF ( SMBINIT ) THEN
       IF ( IVERS .LT. 5 ) THEN
        JD(1)= JD1
       ELSE
        IF ( UAST .NE. 0 ) JD(1)= JD1 ! asteroid database
        IF ( UCOM .NE. 0 ) JD(2)= JD2 ! comet database
       END IF
      END IF
      RETURN
C
C------------------------------------------------------------------------------
C
      ENTRY DXLBL ( NLBL, CODES, LABELS, ILNL, ISTAT )
C
C** Initialize
      ISTAT= 0
      ERRMS= ' '
C
C** Basic check on input dimensions
      IF ( (NLBL .LT. 0) .OR. (NLBL .GT. NUMF) ) THEN
       ISTAT= -1
       WRITE( ERRMS, '(A,I3)')
     &  'DXLBL: input NLBL must be in range 0 to ',NUMF
       RETURN
      END IF
C
C** Process predefined lists
C
      IF ( NLBL .EQ. 0 ) THEN
C
C~~ No action taken if reader package not initialized
       IF ( .NOT. SMBINIT ) THEN
        ISTAT= -2
        ERRMS= 'DXLBL: package not initialized by DXINI as '//
     &         'required for input NLBL= 0'
        RETURN
       END IF
C
C~~ No action taken if initialized for legacy D3READ/DASTCOM3/4 but no 
C   read yet (i.e., don't know whether comet or asteroid fields were loaded)
C
       IF ( (D3OUT.OR.D4OUT).AND.
     &      (ZONE(1).NEQV.ZONE(2).EQV.ZONE(3)) ) THEN
        ISTAT= -3
        ERRMS= 'DXLBL: initialized for legacy D3READ but no prior '//
     &         'DXREAD call, as required for input NLBL= 0'
        RETURN
       END IF
C
C~~ Copy pre-defined lists to working/return array
C
C   Legacy D3READ/DASTCOM3 output
C
       IF ( D3OUT ) THEN      ! Numerics
        DO I= 1, NR8S         ! (user-selectable as 18 or 37)
         IF ( ZONE(3) ) THEN   ! Comet
          CODES(I)= D3NORDC(I)
         ELSE
          CODES(I)= D3NORDA(I) ! Asteroid
         END IF
        END DO
        IF ( ZONE(3) ) THEN   ! Character
         DO I= 1, D3NCC        ! Comet
          CODES(NR8S+I)= D3CORDC(I)
         END DO
         NLBL= NR8S + D3NCC 
        ELSE                   ! Asteroid
         DO I= 1, D3NAC
          CODES(NR8S+I)= D3CORDA(I)
         END DO
         NLBL= NR8S + D3NAC 
        END IF
        J= 0                    !(Max. SRC, none)
C
C   Legacy D3READ/DASTCOM4 output
C
       ELSE IF ( D4OUT ) THEN  ! Numerics
        IF ( ZONE(3) ) THEN     ! Comet
         DO I= 1,D4NUMCS 
          CODES(I)= D4NORDC(I)
         END DO
         K= D4NUMCS
         J= 55                  !(Max. SRC, 10 parameter)                   
        ELSE
         DO I= 1,D4NUMAS        ! Asteroid
          CODES(I)= D4NORDA(I)
         END DO
         K= D4NUMAS
         J= 21                  !(Max. SRC,  6 parameter)                   
        END IF
        DO I= 1, D4NC          ! Character
         IF ( ZONE(3) ) THEN    ! Comet
          CODES(K+I)= D4CORDC(I)
         ELSE                   ! Asteroid
          CODES(K+I)= D4CORDA(I)
         END IF
        END DO
        NLBL= K + D4NC
C
C   DASTCOM5 all-field request. Trim the SRC() list as
C   appropriate for object type.
C
       ELSE IF ( D5OUT ) THEN
        IF ( ZONE(3) ) THEN   ! Comet
         J= MXSRCC            ! Comet max. SRC, 10 parameter
         DO I= 1,D5NUMCS
          CODES(I)= D5NORDC(I)
         END DO
         DO I= 1,D5NUMCC
          CODES(D5NUMCS+I)= D5CORDC(I)
         END DO
         NLBL= D5NUMCS + D5NUMCC
        ELSE                  ! Asteroid
         J= MXSRCA            ! Asteroid max. SRC, 9 parameter
         DO I= 1,D5NUMAS
          CODES(I)= D5NORDA(I)
         END DO
         DO I= 1,D5NUMAC
          CODES(D5NUMAS+I)= D5CORDA(I)
         END DO
         NLBL= D5NUMAS + D5NUMAC
        END IF
C
C   Prior selective field list
C
       ELSE
        J= MXSRCA                ! Ast. max. SRC, 9 parameter
        IF ( ZONE(3) ) J= MXSRCC ! Comet max. SRC, 10 parameter
        IF ( NR8S .GT. 0 ) THEN
         DO I= 1,NUMNF
          CODES(I)= IRORD(I)
         END DO
        END IF
        IF ( NCHS .GT. 0 ) THEN 
         DO I= 1,NUMCH
          CODES(NR8S+I)= ICORD(I)
         END DO
        END IF
        NLBL= NR8S + NCHS

       END IF
      END IF
C
C** At this point, initialization has been checked, NLBL and CODES()
C   have been filled from various sources. Problem in standard form. 
C
C~~ Look-up each code in input list
      K= 0
      DO I= 1, NLBL
       LABELS(I)= '-UNDEF-'
       ILNL(I)= 7
C
C   Take different actions based on specific CODES():
       IF ( CODES(I) .GT. 0 ) THEN
C
C~~ .. If SRC code, turn on SRC slot counter. If not, turn it off.
C
C   Use J variable (set above) to truncate SRC list as appropriate for 
C   MAXSRC for specific record type
C
        IF ( CODES(I) .EQ. 899 ) THEN
         K= 1
         LABELS(I)= 'SRC(01)'
         ILNL(I)= 7
        ELSE
         K= 0
         CALL DXCODLK(CODES(I),NUMF,FCODE,FLAB,FLABL,LABELS(I),ILNL(I))
        END IF
C
C~~ .. placeholder code
       ELSE IF ( CODES(I) .EQ. 0 ) THEN
        IF ( (K .GE. 1) .AND. (K .LT. J) ) THEN
         K= K + 1
         WRITE( LABELS(I), '(A,I2.2,A)' ) 'SRC(',K,')'
         ILNL(I)= 7
        ELSE
         LABELS(I)= ' '
         ILNL(I)= 0
        END IF
C       
C~~ .. invalid field codes (skip)
       ELSE
        CONTINUE                        
       END IF
      END DO
C
      RETURN
C
C------------------------------------------------------------------------------
C
      ENTRY DXERR( CERRMS, LERR )
      CERRMS= ERRMS(1:MIN(LEN(ERRMS),LEN(CERRMS)) )
      LERR= LENTRM( CERRMS ) 
      RETURN
C
C------------------------------------------------------------------------------
C
      ENTRY DXCLOS
C
C** Close any open files and reset database set-up variables
      IF (UAST .NE. 0) CLOSE(UAST)
      IF (UCOM .NE. 0) CLOSE(UCOM)
      IF (U3   .NE. 0) CLOSE(U3  )
      IF (U4   .NE. 0) CLOSE(U4  )
      UAST= 0
      UCOM= 0
      U3= 0
      U4= 0
      IVERS= 0
      NR8S= 0
      NCHS= 0
      DO I= 1,3
       BIAS(I)= 0
       ZONE(I)= .FALSE.
       BND(I)= 0
       BND(I+3)= 0
       BNDSV(I)= 0
       BNDSV(I+3)= 0
      END DO
      CAL1= ' '
      CAL2= ' '
      FILA= ' '
      FILB= ' '
      JD1= -9.9D99
      JD2= -9.9D99
      DO I= 1, NUMNS
       IRORD(I)= 0
      END DO
      DO I= 1, NUMCH
       ICORD(I)= 0
      END DO
      SWAPA= .FALSE.
      SWAPC= .FALSE.
      SWAP3= .FALSE.
      SWAP4= .FALSE.
      D3OUT= .FALSE.
      D4OUT= .FALSE.
      D5OUT= .FALSE.
C
      SMBINIT= .FALSE.
C
      END
