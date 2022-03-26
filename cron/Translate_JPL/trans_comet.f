      PROGRAM TRNCOM
C
C** Open DASTCOM5 database(s) and return all information in a record
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
     &              '../dat/dast5_le.dat',  ! Database #1
     &              '../dat/dcom5_le.dat' / ! Database #2
      DATA         IR8ORD   / -5,141*0 / ! Macro specifying all DASTCOM5 fields
      DATA         ICHORD   /     14*0 / ! Requested character fields
      DATA         NR8,NCH  / 142, 14  / ! Max. # num. & char. fields to get
      DATA         BUF,WARN / .FALSE., .FALSE. / ! Normal values
      
C     Database bounds

      INTEGER IDBND(6), IDBIAS(6)
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
C       PRINT *,'Nominal initialization'
      END IF
      
      call dxbnd(IDBND, IDBIAS)
      
      if (.false.) then
          do i = 1,6
              print *, 'IDBND(', i, ') = ', idbnd(i)
          end do

          do i = 1,3
              print *, 'IDBIAS(', i, ') = ', idbias(i)
          end do
      end if

      write(*, 201)
 201  format (
     1  'Num            ', 1x,
     2  'Name                                   ', 1x,
     3  '  Epoch', 1x,
     4  '     q      ', 1x,
     5  '     e     ', 1x,
     6  '   i     ', 1x,
     7  '   w     ', 1x,
     8  '  Node   ', 1x,
     9  '      Tp       ', 1x,
     A  'Ref'
     B )
      write(*, 202)
 202  format (
     1  '---------------', 1x,
     2  '---------------------------------------', 1x,
     3  '-------', 1x,
     4  '--------------', 1x,
     5  '-----------', 1x,
     6  3('---------', 1x),
     7  '---------------', 1x,
     8  '------------'
     9 )

 101  format (a16,x,a38,1x,I7,1x,f13.8,1x,f11.8,1x,f9.5,1x,f9.5,
     1        1x,f9.5,1x,f15.5,1x,a16)

C** Set IOBJ for the desired objects' logical record & retrieve data
      do IOBJ= idbnd(6),10000000
          CALL DXREAD( IOBJ, IZONE, LSRC, R8OUT, CHOUT5, ISTAT )
          if (ISTAT .eq. -2) then
              STOP
          end if
          if (ISTAT .ne. 0) then
              print *,'Object: ', IOBJ
              PRINT *,'Error on DXREAD(), .ISTAT= ', ISTAT
              CALL DXERR( CERRMS, LERR )
              PRINT *,CERRMS(1:LERR)
              STOP
          end if
C** Check return status and display data
C        print *, 'Iobj ', iobj, ' Izone ', izone
        if (izone .eq. 3) then
      IF ( ISTAT .NE. 0 ) THEN
       PRINT *,'Error on DXREAD(), ISTAT= ', ISTAT
       CALL DXERR( CERRMS, LERR )
       PRINT *,CERRMS(1:LERR)
       STOP
      ELSE
        if (.false.) then
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
        end if
      END IF
              imjd = r8out(5)-2400000.5D0
              write (*, 101) chout5(7), chout5(10), imjd, r8out(13),
     1          r8out(11),r8out(10), r8out(8), r8out(9), r8out(15),
     2          chout5(9)
        end if
      end do
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
