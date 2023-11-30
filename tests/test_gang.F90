! (C) Copyright 2022- ECMWF.
! (C) Copyright 2022- Meteo-France.
!
! This software is licensed under the terms of the Apache Licence Version 2.0
! which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
! In applying this licence, ECMWF does not waive the privileges and immunities
! granted to it by virtue of its status as an intergovernmental organisation
! nor does it submit to any jurisdiction.

PROGRAM TEST_GANG

USE FIELD_4RD_GANG_MODULE
USE PARKIND1
IMPLICIT NONE

CLASS(FIELD_4RD), POINTER :: YLF4

INTEGER (KIND=JPIM), PARAMETER :: NPROMA = 32, NFLEVG = 15, NDIM = 3, NGPBLKS = 10
LOGICAL, PARAMETER :: LLPERSISTENT = .TRUE.

INTEGER (KIND=JPIM), PARAMETER :: ILB (4) = [1,0,1,1], IUB (4) = [NPROMA, NFLEVG, NDIM, NGPBLKS]

TYPE(FIELD_4RD_GANG_OWNER), POINTER :: FIELD_GANG

ALLOCATE (FIELD_GANG)

YLF4 => FIELD_GANG

CALL YLF4%SET_STATUS (0)

END
