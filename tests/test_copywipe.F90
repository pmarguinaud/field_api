! (C) Copyright 2022- ECMWF.
! (C) Copyright 2022- Meteo-France.
!
! This software is licensed under the terms of the Apache Licence Version 2.0
! which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
! In applying this licence, ECMWF does not waive the privileges and immunities
! granted to it by virtue of its status as an intergovernmental organisation
! nor does it submit to any jurisdiction.

PROGRAM TEST_FIELD1D

USE FIELD_MODULE
USE FIELD_FACTORY_MODULE
USE FIELD_ACCESS_MODULE
USE FIELD_UTIL_MODULE
USE PARKIND1
USE FIELD_ABORT_MODULE

#ifdef _OPENACC
USE OPENACC, ONLY : ACC_IS_PRESENT
#endif

IMPLICIT NONE

INTEGER, PARAMETER :: NPROMA = 10, NGPBLKS = 4

CLASS(FIELD_2RB), POINTER :: YLF => NULL()
REAL(KIND=JPRB), POINTER :: Z (:,:)

INTEGER :: JLON, JBLK

CALL FIELD_NEW (YLF, UBOUNDS=[NPROMA, NGPBLKS])

Z => GET_HOST_DATA_RDWR (YLF)

DO JBLK = 1, NGPBLKS
  DO JLON = 1, NPROMA
    Z (JLON, JBLK) = (JLON-1) + JBLK * NPROMA
  ENDDO
ENDDO

CALL COPY (YLF)

#ifdef _OPENACC
IF (.NOT. ACC_IS_PRESENT (YLF, SIZEOF (YLF))) THEN
  CALL FIELD_ABORT ('YLF NOT PRESENT')
ENDIF
#endif

Z => GET_DEVICE_DATA_RDWR (YLF)

!$acc serial present (YLF, Z)
IF (SIZE (YLF%DEVPTR) /= SIZE (Z)) STOP "SIZE MISMATCH"
IF (LOC (Z (1, 1)) /= LOC (YLF%DEVPTR (1,1))) STOP "POINTER MISMATCH"
!$acc end serial

CALL WIPE (YLF)

#ifdef _OPENACC
IF (ACC_IS_PRESENT (YLF, SIZEOF (YLF))) THEN
  CALL FIELD_ABORT ('YLF IS PRESENT')
ENDIF
#endif

CALL FIELD_DELETE (YLF)

END PROGRAM 
