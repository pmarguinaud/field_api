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
USE FIELD_ARRAY_MODULE
IMPLICIT NONE

TYPE (FIELD_3RB_ARRAY) :: YLA

REAL (KIND=JPRB) :: ZDATA (2077,170,5)
REAL (KIND=JPRB), POINTER :: ZDEV (:,:,:)

INTEGER (KIND=JPIM) :: ISTR, ILEN

CALL YLA%INIT (ZDATA)

ZDEV  => GET_DEVICE_DATA_RDONLY (YLA%F_P)

PRINT *, " LBOUND (ZDEV) = ", LBOUND (ZDEV)
PRINT *, " UBOUND (ZDEV) = ", UBOUND (ZDEV)

ISTR = (LOC (ZDEV (1,2,1)) - LOC (ZDEV (1,1,1)))/KIND (ZDEV)
ILEN = (UBOUND (ZDEV, 1) - LBOUND (ZDEV, 1)) + 1

PRINT *, " ISTR = ", ISTR
PRINT *, " ILEN = ", ILEN

IF (ILEN /= ISTR) THEN
  PRINT *, " ZDEV IS NOT CONTIGUOUS "
ELSE
  PRINT *, " ZDEV **IS** CONTIGUOUS "
ENDIF

!$acc serial present (ZDEV)

CALL TOTO2 (ZDEV (:,:,1))

!$acc end serial

PRINT *, " TOTO2 OK"

!$acc serial present (ZDEV)

!CALL TOTO3 (ZDEV (:,:,:))
CALL TOTO3 (ZDEV)

!$acc end serial

PRINT *, " TOTO3 OK"

!$acc serial present (ZDEV)

CALL TOTO4 (ZDEV (:,11:20,:))

!$acc end serial

PRINT *, " TOTO4 OK"


CONTAINS

SUBROUTINE TOTO2 (P) 

!$acc routine (TOTO2) seq

REAL*8 :: P (2077,170)

END SUBROUTINE

SUBROUTINE TOTO3 (P) 

!$acc routine (TOTO3) seq

REAL*8 :: P (2077,170,5)

END SUBROUTINE

SUBROUTINE TOTO4 (P) 

!$acc routine (TOTO4) seq

REAL*8 :: P (2077,10,5)

END SUBROUTINE

END
