      PROGRAM EXAMPLE2
C
C** Open DASTCOM5, return only DASTCOM3 information (subset)       
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
C** Initialization settings for old DASTCOM3 *asteroid* record contents:
C        READ( U, REC=PREC, IOSTAT= STAT )
C    &      NO,ASTNAM,EPOCH,CALEPO,EQUNOX,IREF,DESIG,
C    &      RMO,RNODE,OM,RI,E,A,
C    &      PERJD,PERCAL,Q,GM,RAD,H,G,BVT,
C    &      RP,ALBEDO,SPTYPE,DARC,NOBS,COMNT1,COMNT2,
C    &      SBNAM
C
      DATA         DBNAM    /
     &              '../dat/dast5_le.dat',  ! Database #1
     &              '../dat/dcom5_le.dat' / ! Database #2
      DATA         IR8ORD   /            ! Request DASTCOM3 asteroid num. fields
     &              201,801,802,803,804,805,806,807,808,810,
     &              811,809,432,433,401,402,439,431,438, 123*0  /
      DATA         ICHORD   /            ! Request DASTCOM3 asteroid char fields
     &              14,1,13,11,4,6,7,8,3,5*0 /
      DATA         NR8,NCH  /  19,  9  / ! Max. # num. & char. fields to get
      DATA         BUF,WARN / .FALSE., .FALSE. / ! Normal values
C
C   If a comet were requested instead, the read would have been ...
C
C       READ( U, REC=L1, IOSTAT=STAT) 
C    &        NO,COMNAM,EPOCH,CALEPO,EQUNOX,IREF,DESIG,
C    &        RMO,RNODE,OM,RI,E,A,
C    &        PERJD,PERCAL,Q,GM,RAD,A1,A2,
C    &        M1,M2,PYR,DARC,COMNT3,COMNT2,
C    &        SBNAM
C
C  In that cometary case, IR8ORD and ICHORD would be
C     DATA         IR8ORD   /            ! Request DASTCOM3 comet num. fields
C    &              201,801,802,803,804,805,806,807,808,810,
C    &              811,809,432,433,408,409,403,402,151, 123*0  /
C     DATA         ICHORD   /            ! Request DASTCOM3 comet char fields
C    &               14,1,13,11,6,9,10,3,6*0 /
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
      CALL DXREAD( IOBJ, IZONE, LSRC, R8OUT, CHOUT5, ISTAT )
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
