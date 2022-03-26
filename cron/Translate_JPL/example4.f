      PROGRAM EXAMPLE4
C
C** Open legacy DASTCOM3, return data in legacy subroutine D3READ order
C
C** Parameterize array dimensions for convenience. Might instead use
C   "INCLUDE dxparms.inc" and package NUMNS and NUMCH parameters for
C   better future-proofing
C
      INTEGER      NUMNS0, NUMCH0
      PARAMETER( NUMNS0= 142) ! Max.# of numeric fields to be retrieved
      PARAMETER( NUMCH0=  14) ! Max.# of character fields to be retrieved
C
C** Declare necessary variables for DXINI
      CHARACTER*1  DBNAM(2)*256
      INTEGER      IR8ORD(NUMNS0), ICHORD(NUMCH0), NR8, NCH, ISTAT
      LOGICAL      BUF, WARN
C
C** Declare necessary variables for DXREAD
      CHARACTER*1  CHOUT5(NUMCH0)*80, CHOUT3*217, CERRMS*340
      INTEGER      IOBJ, IZONE, LSRC, LERR
      REAL*8       R8OUT(NUMNS0)
C
C** Declare local variables
      INTEGER      I
C
C** Initialization settings
      DATA         DBNAM    /
     &              '../dat/DASTCOM3',   ! Legacy database
     &              '               ' /  ! Database #2
      DATA         IR8ORD   / -3,141*0 / ! Macro specifying only DASTCOM3 fields
      DATA         ICHORD   /     14*0 / ! Requested character fields
      DATA         NR8,NCH  /  18,  1  / ! Max. # num. & char. fields to get
      DATA         BUF,WARN / .FALSE., .FALSE. / ! Normal values
C
C** Initialize
      CALL DXINI( DBNAM, IR8ORD, NR8, ICHORD, NCH, BUF, WARN, ISTAT )
C
C** Check initialization return status
      IF ( ISTAT .NE. 0 ) THEN
       PRINT *,'Error on DXINI(), ISTAT= ', ISTAT
       CALL DXERR( CERRMS, LERR )
       PRINT *,CERRMS(1:LERR)
       STOP
      ELSE
       PRINT *,'Nominal initialization'
      END IF

C** Set IOBJ for the desired objects' logical record & retrieve data
      IOBJ= 4
      CALL DXREAD( IOBJ, IZONE, LSRC, R8OUT, CHOUT3, ISTAT )
C
C** Check return status and display data
      IF ( ISTAT .NE. 0 ) THEN
       PRINT *,'Error on DXREAD(), ISTAT= ', ISTAT
       CALL DXERR( CERRMS, LERR )
       PRINT *,CERRMS(1:LERR)
       STOP
      ELSE
       PRINT *,'IZONE= ',IZONE     ! Database zone of record
       PRINT *,'LSRC = ',LSRC      ! Length of SRC vector
       DO I= 1, NR8                ! Objects' numeric data, ordered by IR8ORD()
        PRINT *,' R8OUT(',I,')= ',R8OUT(I)
       END DO
       IF ( NCH .GT. 1 ) THEN      ! DASTCOM5 character array
        DO I= 1, NCH               ! Objects' character data, order by ICHORD()
         PRINT *,' CHOUT5(',I,')= ',CHOUT5(I)(1:LENTRM(CHOUT5(I)))
        END DO
       ELSE
        PRINT *,' CHOUT3= ',CHOUT3 ! Legacy character block
       END IF
      END IF
      END
