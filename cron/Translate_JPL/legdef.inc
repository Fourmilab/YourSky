C LEGDEF.INC (FORTRAN)
C  Assignments should not be altered; they are based on prior-database
C  definitions.
C
C** D3READ legacy compatibility:
C
C   D3READ was a prior subroutine in limited distribution from 2001-2012 that
C   read both DASTCOM3 and DASTCOM4 format databases, returning data arrays 
C   in one of two fixed formats, depending on whether a comet or asteroid 
C   record request.
C
C   Since this behavior may be built into existing user software, this 
C   subroutine provides the option of returning the same data in the same order 
C   as D3READ did, as well as offering user-specified field return, order, and 
C   expanded DASTCOM5+ content.
C
C   The correspondences below are therefore established to permit optional
C   output-mimicking of the prior D3READ subroutine. They are used only if
C   DXINI is initialized with IR8ORD(1)= -3 or -4 or -5.
C
C   (1) For D3READ-compatible, DASTCOM3 data return analogous to XTRA= .FALSE.,
C   ignore everything after slot 18 (i.e., call DXINI with IR8ORD(1)=-3 and
C   NR8= 18)
C
C   (2) For D3READ-compatible, DASTCOM3 data return analogous to XTRA= .TRUE.,
C   call DXINI with IR8ORD(1)=-3 and NR8= 37.
C
C   (3) For D3READ-compatible, DASTCOM4 data return, call DXINI with
C   IR8ORD(1)=-4 and NR8=99.
C
C   (4) An additional output list is set up below to return all DASTCOM5
C   quantities in the pre-defined order indicated. Call DXINI with
C   IR8ORD(1)=-5 and NR8=142.
C
C   For a list defining what the quantities are, see 'curdef.inc'.
C
C~~ DASTCOM3 / ASTEROID  (output model from legacy D3READ subroutine)
C    Notes:
C      (1) D3NORDA & D3CORDA and parameters should never be changed
C      (2) 'NSCT' slot was reserved but never implemented (placeholder here)
C
      INTEGER      D3NUMS
      PARAMETER( D3NUMS= 37 ) ! Number of quantities in DASTCOM3 return output
      INTEGER      D3NORDA(D3NUMS), D3NORDC(D3NUMS)
      INTEGER      D3NAC, D3NCC
      PARAMETER( D3NAC= 9) ! Number of character fields in D3 ast. return
      PARAMETER( D3NCC= 8) ! Number of character fields in D3 comet return
      INTEGER      D3CORDA(D3NAC), D3CORDC(D3NCC)
C
      DATA D3NORDA /
C         A,   E,  RI,  OM, RNODE, RMO, EPOCH, CALEPO, RAD,  GM,   Q,
     &  808, 807, 806, 805,   804, 803,   801,    802, 433, 432, 809,
C       BVT,   H, ALBEDO,   G,  RP, PERJD, PERCAL, ADIST, PER, ANGMOM,
     &  439, 401,    438, 402, 431,   810,    811,   851, 852,    853,
C         N, DAN, DDN,   L,   B, NOBS,  NO, 0, 0, 0, 0, NDEL,
     &  854, 855, 856, 857, 858,  202, 201, 0, 0, 0, 0,  152,
C      NDOP, 'NSCT', 0, 0
     &  153,      0, 0, 0 /
C
      DATA D3CORDA /
C      NAME, DESIG, IREF, EQUNOX, SPTYPT, DARC, COMNT1, COMNT2, SBNAM /
     &   14,    11,   13,      1,      4,    6,      7,      8,     3 / 
C
C~~ DASTCOM3 / COMET  (output model from legacy D3READ subroutine)
C    Notes:
C     (1) D3NORDC & D3CORDC should never be changed.
C     (2) No A3 parameter;
C     (3) 'NSCT' slot was reserved but never implemented (placeholder here);
C
      DATA D3NORDC /
C        A,   E,  RI,  OM, RNODE, RMO, EPOCH, CALEPO, RAD,  GM,   Q,
     & 808, 807, 806, 805,   804, 803,   801,    802, 433, 432, 809,
C       M1,  M2,  A1,  A2, PYR, PERJD, PERCAL, ADIST, PER, ANGMOM,
     & 403, 404, 408, 409, 151,   810,    811,   851, 852,    853,
C        N, DAN, DDN,   L,   B, NOBS,  NO,  K1,  K2, PHCOF, COMNUM, NDEL,
     & 854, 855, 856, 857, 858,  202, 201, 405, 406,   407,    156, 152,
C     NDOP, 'NSCT',  DT,  S0
     & 153,      0, 411, 417 /
C
      DATA D3CORDC /
C      NAME, DESIG, IREF, EQUNOX, DARC, COMNT3, COMNT2, SBNAM /
     &   14,    11,   13,      1,    6,      9,      8,     3 / 
C
C~~ DASTCOM4 / ASTEROID (output model from legacy D3READ subroutine)
C    Notes:
C     (1) D4NORDA, D4NUMAS, D4NC, D4CORDA should never be changed
C     (2) OQP, CEU, SIG1, LEAK1, SLEAK1, LEAK5, SLEAK5 were reserved but
C         not implemented in DASTCOM4 and are not carried forward here
C     (3) 'NSCT' slot was reserved but never implemented (placeholder here)
C
      INTEGER      D4NUMAS
      PARAMETER( D4NUMAS= 99 ) !No. of numeric slots in D3READ/DASTCOM4 return
      INTEGER      D4NORDA(D4NUMAS), D4NC
      PARAMETER( D4NC= 10) ! Number of character fields in D4 ast./comet return
      INTEGER      D4CORDA(D4NC)
C
      DATA D4NORDA /
C         A,   E,  RI,  OM, RNODE, RMO, EPOCH, CALEPO, RAD,  GM,   Q,
     &  808, 807, 806, 805,   804, 803,   801,    802, 433, 432, 809,
C
C       BVT,   H, ALBEDO,   G,  RP, PERJD, PERCAL, ADIST, PER, ANGMOM,
     &  439, 401,    438, 402, 431,   810,    811,   851, 852,    853,
C
C         N, DAN, DDN,   L,   B, NOBS,  NO,   0,   0,   0,   0, NDEL,
     &  854, 855, 856, 857, 858,  202, 201,   0,   0,   0,   0,  152,
C
C      NDOP, 'NSCT',   0,   0, RMSW, RMSH, MOID, UPARM, OBSTRT, OBSTOP,
     &  153,      0,   0,   0,  442,  446,  437,   107,    203,   204,
C
C      SOLDAT, SRC(01-21)
     &  813,   899, 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
C
C      .. placeholders to fill out array from 66-99.
     &  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
     &  0,0,0,0,0,0,0 /
C
      DATA D4CORDA /
C      NAME, DESIG, IREF, EQUNOX,SPTYPS,DARC,COMNT1,COMNT2,SBNAM,PENAM /
     &   14,    11,   13,      1,     5,   6,     7,     8,    3,    2 / 
C
C~~ DASTCOM4 / COMET (output model from legacy D3READ subroutine)
C    Notes:
C      (1) D4NUMCS, D4NORDC & D4CORDC should never be changed
C      (2) 'NSCT' slot was reserved but never implemented (placeholder here)
C
      INTEGER      D4NUMCS
      PARAMETER( D4NUMCS= 99 ) !No. of numeric slots in D3READ/DASTCOM4 return
      INTEGER      D4NORDC(D4NUMCS), D4CORDC(D4NC)
C
      DATA D4NORDC /
C        A,   E,  RI,  OM, RNODE, RMO, EPOCH, CALEPO, RAD,  GM,   Q,
     & 808, 807, 806, 805,   804, 803,   801,    802, 433, 432, 809,
C
C       M1,  M2,  A1,  A2, PYR, PERJD, PERCAL, ADIST, PER, ANGMOM,
     & 403, 404, 408, 409, 151,   810,    811,   851, 852,    853,
C
C        N, DAN, DDN,   L,   B, NOBS,  NO,  K1,  K2, PHCOF,COMNUM,NDEL,
     & 854, 855, 856, 857, 858,  202, 201, 405, 406,   407,   156, 152,
C
C     NDOP, 'NSCT',  DT,  S0, RMSW,  A3, MOID, UPARM, OBSTRT, OBSTOP,
     & 153,      0, 411, 417,  442, 410,  437,   107,    203,    204,
C
C     SOLDAT, SRC(01-55)
     & 813,   899,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
C
C     SRC_vector placeholders (54)
     & 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0 /
C
      DATA D4CORDC /
C      NAME, DESIG, IREF, EQUNOX, DARC,COMNT3,COMNT2,SBNAM,PENAM,ESTL /
     &   14,    11,   13,      1,    6,     9,     8,    3,    2,  12 / 
C
C~~ DAST5 / asteroid record field output
C    Notes:
C      (1) D5NORDA, D5CORDA, D5NUMAS, and D5NUMAC should never be changed
C
      INTEGER      D5NUMAS, D5NUMAC
      PARAMETER( D5NUMAS= 114 ) ! No. slots in DAST5 numerics output
      PARAMETER( D5NUMAC= 12  ) ! No. slots in DAST5 character output
      INTEGER      D5NORDA(D5NUMAS), D5CORDA(D5NUMAC)
C
C~~ DAST5 numeric slots (in same order as physical record) 
      DATA D5NORDA /
C         NO, NOBS, OBSFRST, OBSLAST,
     &   201,  202,     203,     204,
C
C      EPOCH, CALEPO,  MA,   W,  OM,  IN,  EC,  A,  QR,    TP, TPCAL,
     &   801,    802, 803, 804, 805, 806, 807, 808, 809,  810,   811,
C
C       TPFRAC, SOLDAT, ADIST, PER, ANGMOM,   N, DAN, DDN,   L,
     &     812,    813,   851, 852,    853, 854, 855, 856, 857,
C
C         B,  SRC,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
     &  858,  899,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
C
C      SRC_vector placeholders (44)
     & 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
C
C     PRELTV, SPHMX3, SPHMX5, JGSEP, TWOBOD, NSATS, UPARM, LSRC,
     &   101,    102,    103,   104,    105,   106,   107,  108,
C
C       NDEL, NDOP,
     &   152,  153, 
C
C        H,   G,  A1,  A2,  A3,  DT,  R0,
     & 401, 402, 408, 409, 410, 411, 412,
C
C      ALN,  NM,  NN,  NK,  LGK, RHO, AMRAT, 
     & 413, 414, 415, 416,  419, 420,   421,
C
C      ALF, DEL, SPHLM3, SPHLM5,  RP,  GM, RAD, EXTNT1,
     & 427, 428,    429,    430, 431, 432, 433,    434,
C
C     EXTNT2, EXTNT3, MOID, ALBEDO, BVCI, UBCI, IRCI, RMSW, RMSU,
     &   435,    436,  437,    438,  439,  440,  441,  442,  443,
C
C       RMSN, RMSNT, RMSH /
     &   444,   445,  446 / 
C
C~~ DAST5 character fields
      DATA D5CORDA /
C      EQUNOX, PENAM, SBNAM, SPTYPT, SPTYPS, DARC, COMNT1, COMNT2,
     &      1,     2,     3,      4,      5,    6,      7,      8,
C       DESIG, ESTL, IREF, NAME
     &     11,   12,   13,   14 /
C
C~~ DCOM5 / comet record field output
C    Notes:
C      (1) D5NORDC, D5CORDC, D5NUMCS, and D5NUMCC should never be changed
C
      INTEGER      D5NUMCS, D5NUMCC
      PARAMETER( D5NUMCS= 137 ) ! No. slots in DCOM5 numerics output
      PARAMETER( D5NUMCC= 10  ) ! No. slots in DCOM5 character output
      INTEGER      D5NORDC(D5NUMCS), D5CORDC(D5NUMCC)
C
C~~ DCOM5 numeric slots (in same order as physical record) 
      DATA D5NORDC /
C         NO, NOBS, OBSFRST, OBSLAST,
     &   201,  202,     203,     204,
C
C      EPOCH, CALEPO,  MA,   W,  OM,  IN,  EC,  A,  QR,    TP, TPCAL,
     &   801,    802, 803, 804, 805, 806, 807, 808, 809,  810,   811,
C
C       TPFRAC, SOLDAT, ADIST, PER, ANGMOM,   N, DAN, DDN,   L,
     &     812,    813,   851, 852,    853, 854, 855, 856, 857,
C
C         B,  SRC,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
     &  858,  899,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
C
C      SRC_vector placeholders (54)
     & 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
C
C     PRELTV, SPHMX3, SPHMX5, JGSEP, TWOBOD, NSATS, UPARM, LSRC,
     &   101,    102,    103,   104,    105,   106,   107,  108,
C
C     IPYR,   NDEL, NDOP, NOBSMT, NOBSMN, COMNUM
     &   151,  152,  153,    154,    155,    156, 
C
C        H,   G,  M1,  M2,  K1,  K2, PHCOF,  A1,  A2,  A3,  DT,  R0,
     & 401, 402, 403, 404, 405, 406,   407, 408, 409, 410, 411, 412,
C
C      ALN,  NM,  NN,  NK,  S0, TCL,  RHO, AMRAT, AJ1, AJ2, ET1,
     & 413, 414, 415, 416, 417, 418,  420,   421, 422, 423, 424,
C
C      ET2, DTH, ALF, DEL, SPHLM3, SPHLM5,  RP,  GM, RAD, EXTNT1,
     & 425, 426, 427, 428,    429,    430, 431, 432, 433,    434,
C
C     EXTNT2, EXTNT3, MOID, ALBEDO,  RMSW, RMSU,
     &   435,    436,  437,    438,   442,  443,
C
C       RMSN, RMSNT,  RMSMT, RMSMN /
     &   444,   445,    447,   448 / 
C
C~~ DCOM5 character fields
      DATA D5CORDC /
C      EQUNOX, PENAM, SBNAM,  DARC, 
     &      1,     2,     3,     6,
C      COMNT3, COMNT4, DESIG, ESTL, IREF, NAME
     &      9,     10,    11,   12,   13,   14 /
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
