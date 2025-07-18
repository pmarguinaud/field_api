! (C) Copyright 2022- ECMWF.
! (C) Copyright 2022- Meteo-France.
!
! This software is licensed under the terms of the Apache Licence Version 2.0
! which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
! In applying this licence, ECMWF does not waive the privileges and immunities
! granted to it by virtue of its status as an intergovernmental organisation
! nor does it submit to any jurisdiction.

PROGRAM FIELD_VIEW

        USE FIELD_MODULE
        USE FIELD_FACTORY_MODULE
        USE PARKIND1
        USE FIELD_ABORT_MODULE
        USE FIELD_ACCESS_MODULE, ONLY: GET_HOST_DATA_RDWR, GET_DEVICE_DATA_RDWR, GET_HOST_DATA_RDONLY,GET_DEVICE_DATA_RDONLY
        IMPLICIT NONE
        CLASS(FIELD_3RB), POINTER :: FIELD_A => NULL()
        REAL(KIND=JPRB), ALLOCATABLE :: A(:,:,:)
        REAL(KIND=JPRB), ALLOCATABLE :: B(:,:,:)

        REAL(KIND=JPRB), POINTER :: ZZ3 (:,:,:)
        REAL(KIND=JPRB), POINTER :: ZZ2 (:,:)
        TYPE (FIELD_3RB_VIEW),ALLOCATABLE :: VIEWS_A(:)

        INTEGER :: JLEV,KLEV,JINIT
        LOGICAL :: LDACC, A2B

        KLEV = 5
        ALLOCATE(A(10,10,KLEV))
        ALLOCATE(B(10,10,KLEV))
        ALLOCATE(VIEWS_A(KLEV))
        !$ACC ENTER DATA CREATE(A,B)

        DO  JINIT = 1, 4

            CALL FIELD_NEW(FIELD_A, DATA=A)

            LDACC = .FALSE.
            A2B = .FALSE.
            IF (JINIT > 2) LDACC = .TRUE.
            IF (MOD(JINIT,2) == 0) A2B = .TRUE.

            IF (LDACC) THEN
              !$acc kernels present (A,B)
              IF (A2B) THEN
                A(:,:,:) = JINIT
                B(:,:,:) = -1
              ELSE
                A(:,:,:) = -1
                B(:,:,:) = JINIT
              ENDIF
              !$acc end kernels
            ELSE
              IF (A2B) THEN
                A(:,:,:) = JINIT
                B(:,:,:) = -1
              ELSE
                A(:,:,:) = -1
                B(:,:,:) = JINIT
              ENDIF
            ENDIF

            IF (A2B) THEN
              IF (LDACC) THEN
                  ZZ3 => GET_DEVICE_DATA_RDONLY(FIELD_A)
                ELSE
                  ZZ3 => GET_HOST_DATA_RDONLY(FIELD_A)
              ENDIF
            ELSE
              IF (LDACC) THEN
                ZZ3 => GET_DEVICE_DATA_RDWR(FIELD_A)
              ELSE
                ZZ3 => GET_HOST_DATA_RDWR(FIELD_A)
              ENDIF
            ENDIF

          DO JLEV = 1,KLEV
!!!         !$acc data present(zz3(:,jlev,:)) if(LDACC)
!!!         !$ACC HOST_DATA USE_DEVICE (ZZ3 (:, JLEV, :)) IF (LDACC)
            VIEWS_A(JLEV)%P => ZZ3 (:, :, JLEV)
!!!         !$ACC END HOST_DATA
!!!         !$acc end data
          ENDDO

          DO JLEV = 1,KLEV
            ZZ2=>VIEWS_A(JLEV)%P
            IF (LDACC) THEN
              !$acc kernels present (B, ZZ2)

              IF (A2B) THEN
                B(:,:,JLEV) = ZZ2(:,:)
              ELSE
                ZZ2(:,:) = B(:,:,JLEV)
              ENDIF
              !$acc end kernels
              
            ELSE
              IF (A2B) THEN
                B(:,:,JLEV) = ZZ2(:,:)
              ELSE
                ZZ2(:,:) = B(:,:,JLEV)
              ENDIF
            ENDIF
          ENDDO

          WRITE(*,*) "JINIT", JINIT
          WRITE(*,*) "LDACC", LDACC
          WRITE(*,*) "A2B",A2B

          IF (LDACC) THEN
            ! Synchronize on host before testing values
            ZZ3 => GET_HOST_DATA_RDONLY (FIELD_A)
            !$acc update host (B)
          ENDIF
      
          IF ( .NOT. ALL(A == JINIT))THEN
            WRITE(*,*) "A Not correct", JINIT
            CALL FIELD_ABORT ("ERROR")
          END IF
          IF ( .NOT. ALL(B == JINIT))THEN
            WRITE(*,*) "B Not correct", JINIT
            CALL FIELD_ABORT ("ERROR")
          END IF

        ENDDO

      !$ACC EXIT DATA DELETE(A,B)

      DEALLOCATE(A)
      DEALLOCATE(B)
      DEALLOCATE(VIEWS_A)

      CALL FIELD_DELETE(FIELD_A)

    END PROGRAM FIELD_VIEW
