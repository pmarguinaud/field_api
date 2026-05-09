! (C) Copyright 2022- ECMWF.
! (C) Copyright 2022- Meteo-France.
!
! This software is licensed under the terms of the Apache Licence Version 2.0
! which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
! In applying this licence, ECMWF does not waive the privileges and immunities
! granted to it by virtue of its status as an intergovernmental organisation
! nor does it submit to any jurisdiction.

PROGRAM TEST_OWNER_LASTDIM

USE FIELD_MODULE
USE FIELD_FACTORY_MODULE
USE FIELD_ACCESS_MODULE
USE FIELD_ABORT_MODULE
USE FIELD_SHUFFLE_MODULE
USE PARKIND1

IMPLICIT NONE

CLASS (FIELD_3RB), POINTER :: YLF => NULL()

REAL (KIND=JPRB), POINTER :: ZDATA (:,:,:)
REAL (KIND=JPRB), POINTER :: ZVIEW (:,:)

REAL (KIND=JPRB), ALLOCATABLE :: ZDATA0 (:,:,:)

TYPE (FIELD_SHUFFLE) :: FGS

INTEGER :: JBLK

ALLOCATE (ZDATA0 (10, 11, 0))

CALL FIELD_NEW (YLF, DATA=ZDATA0)

ZDATA => GET_HOST_DATA_RDWR (YLF)

PRINT *, " LBOUND (ZDATA0) = ", LBOUND (ZDATA0)
PRINT *, " UBOUND (ZDATA0) = ", UBOUND (ZDATA0)

PRINT *, " LBOUND (ZDATA) = ", LBOUND (ZDATA)
PRINT *, " UBOUND (ZDATA) = ", UBOUND (ZDATA)

IF (ANY (LBOUND (ZDATA0) /= LBOUND (ZDATA))) THEN
  STOP 1
ENDIF

IF (ANY (UBOUND (ZDATA0) /= UBOUND (ZDATA))) THEN
  STOP 1
ENDIF

CALL FIELD_DELETE (YLF)

END PROGRAM TEST_OWNER_LASTDIM
