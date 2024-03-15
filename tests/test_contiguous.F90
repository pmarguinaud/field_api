! (C) Copyright 2022- ECMWF.
! (C) Copyright 2022- Meteo-France.
!
! This software is licensed under the terms of the Apache Licence Version 2.0
! which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
! In applying this licence, ECMWF does not waive the privileges and immunities
! granted to it by virtue of its status as an intergovernmental organisation
! nor does it submit to any jurisdiction.

PROGRAM TEST_CONTIGUOUS

USE FIELD_MODULE
USE FIELD_FACTORY_MODULE
USE FIELD_ACCESS_MODULE
USE PARKIND1
USE FIELD_ABORT_MODULE
IMPLICIT NONE

CLASS(FIELD_2RB), POINTER :: YLF => NULL()

REAL (KIND=JPRB) :: ZDATA (2077,0:170)
REAL (KIND=JPRB), POINTER :: ZDEV (:,:)

INTEGER (KIND=JPIM) :: ISTR, ILEN

CALL FIELD_NEW (YLF, DATA=ZDATA (:, 18:34), PERSISTENT=.TRUE., LBOUNDS=[0,1])

ZDEV  => GET_DEVICE_DATA_RDONLY (YLF)

PRINT *, " LBOUND (ZDEV) = ", LBOUND (ZDEV)
PRINT *, " UBOUND (ZDEV) = ", UBOUND (ZDEV)

ISTR = (LOC (ZDEV (1,1)) - LOC (ZDEV (1,0)))/KIND (ZDEV)
ILEN = (UBOUND (ZDEV, 1) - LBOUND (ZDEV, 1)) + 1

PRINT *, " ISTR = ", ISTR
PRINT *, " ILEN = ", ILEN

IF (ILEN /= ISTR) THEN
  PRINT *, " ZDEV IS NOT CONTIGUOUS "
ELSE
  PRINT *, " ZDEV **IS** CONTIGUOUS "
ENDIF

!$acc serial present (ZDEV)

CALL TOTO (ZDEV)

!$acc end serial

CONTAINS

SUBROUTINE TOTO (P) 

!$acc routine (TOTO) seq

REAL*8 :: P (2077,0:16)

END SUBROUTINE

END
