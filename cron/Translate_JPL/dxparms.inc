C DXPARMS.INC (FORTRAN)
C   Parameterize size declarations for DASTCOMX external and internal arrays. 
C
C Users may wish to include this parameter file in application programs that
C use DXREAD to ensure compatibility. Future versions of the database could
C then be more easily accomodated by including the new version of this file 
C and recompiling/linking all the other programs.
C
C "Internal arrays" include R8P, R4P, I4P, I2P, I1P and CXP. "External arrays" 
C include dummy arguments IR8ORD, R8OUT and CHOUT. 
C
C Internal arrays are used when reading from a database record. Data actually 
C requested by users is then copied to external arrays for return to the user.
C
C Since SRC is stored at the end of the internal REAL*8 array R8P, starting
C in slot #99, and derived quantities start in the middle, the "in-between"
C slots are effectively allocated space although they are not defined in
C the sense of being associated with a current database field. 
C
C Hence a distinction is made between defined/reserved, actual/maximum below. 
C If a new REAL*8 field is added to a future database version, RR8D can 
C increment (up to RR8), but RR8 won't change.
C
C The numbers below should never be changed by a user; they define the current
C database record structure.
C
      INTEGER    RR8, RR8D, RR4D, RI4D, RI2D, RI1D, NUMCH, MXSRCA
      INTEGER    MXSRCC, MXSRC, NUMNF, NUMNS, NUMF, NUMLB
C
      PARAMETER( RR8   = 99 )               ! No. reserved 8-byte REAL arr.slots
C
      PARAMETER( RR8D  = 22 )               ! No. DEFINED 8-byte REAL fields
      PARAMETER( RR4D  = 48 )               ! No. DEFINED 4-byte REAL fields
      PARAMETER( RI4D  =  4 )               ! No. DEFINED 4-byte INTEGER fields
      PARAMETER( RI2D  =  6 )               ! No. DEFINED 2-byte INTEGER fields
      PARAMETER( RI1D  =  8 )               ! No. DEFINED 1-byte INTEGER fields
      PARAMETER( NUMCH = 14 )               ! No. DEFINED character fields
      PARAMETER( MXSRCA= 45)                ! Maximum asteroid SRC vector length
      PARAMETER( MXSRCC= 55)                ! Maximum comet SRC vector length
C
      PARAMETER( MXSRC= MAX(MXSRCA,MXSRCC)) ! Maximum length of SRC vector
      PARAMETER( NUMNF= RR8D+RR4D+RI4D+RI2D+RI1D)! No. of defined numeric fields
      PARAMETER( NUMNS= NUMNF + MXSRC - 1)  ! No. of defined numeric array slots
      PARAMETER( NUMF = NUMNF + NUMCH    )  ! No. of defined fields
      PARAMETER( NUMLB= NUMNS + NUMCH    )  ! Max. # of labels
C
C** Parameterize DASTCOM5 physical rec. array data-type sizes for READ statement
      INTEGER       NI1PA, NI2PA, NI4PA, NR4PA, NR8PA
C
C~~ DAST5 physical ASTEROID records
      PARAMETER( NI1PA=  8 )                ! No. of INTEGER*1 fields
      PARAMETER( NI2PA=  2 )                ! No. of INTEGER*2 fields
      PARAMETER( NI4PA=  4 )                ! No. of INTEGER*4 fields
      PARAMETER( NR4PA= 33 )                ! No. of REAL*4 fields 
      PARAMETER( NR8PA= 13 )                ! No. of REAL*8 fields (excl. SRC)
C
C~~ DCOM5 physical COMET records
      INTEGER       NI1PC, NI2PC, NI4PC, NR4PC, NR8PC
      PARAMETER( NI1PC=  8 )                ! No. of INTEGER*1 fields
      PARAMETER( NI2PC=  5 )                ! No. of INTEGER*2 fields
      PARAMETER( NI4PC=  4 )                ! No. of INTEGER*4 fields
      PARAMETER( NR4PC= 43 )                ! No. of REAL*4 fields
      PARAMETER( NR8PC= 13 )                ! No. of REAL*8 fields (excl. SRC)
C
C** Parameterize internal array sizes; maximum number/extent of each data type.
      INTEGER       MXR8P, MXR4P, MXI4P, MXI2P, MXI1P
      PARAMETER( MXR8P = RR8+MXSRC-1 )      ! Max. no. of REAL*8 slots
      PARAMETER( MXR4P = RR4D )             ! Max. no. of REAL*4 slots
      PARAMETER( MXI4P = RI4D )             ! Max. no. of INTEGER*4 slots
      PARAMETER( MXI2P = RI2D )             ! Max. no. of INTEGER*2 slots
      PARAMETER( MXI1P = RI1D )             ! Max. no. of INTEGER*1 slots
C
C** Declare lengths of DASTCOM5 character data blocks for asteroids and comets.
C    CXP is used internally to pass data from legacy database file formats and
C    should be dimensioned as the maximum of ACHARP, CCHARP (or larger is OK).
C
      CHARACTER*211 ACHARP
      CHARACTER*226 CCHARP, CXP
C
C------------------------------------------------------------------------------
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
