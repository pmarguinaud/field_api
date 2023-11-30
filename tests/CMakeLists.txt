# (C) Copyright 2022- ECMWF.
# (C) Copyright 2022- Meteo-France.
#
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
# In applying this licence, ECMWF does not waive the privileges and immunities
# granted to it by virtue of its status as an intergovernmental organisation
# nor does it submit to any jurisdiction.

set(LIBNAME_PREC ${LIBNAME}_${DEFAULT_PRECISION})

## Unit tests
list(APPEND TEST_FILES
        test_gang.F90
	)

#Place-holder for failing tests
set(FAILING_TEST_FILES
	)

#These tests will call abor1
set(ABOR1_TEST_FILES
	)

foreach(TEST_FILE ${TEST_FILES})
	get_filename_component(TEST_NAME ${TEST_FILE} NAME_WE)
    ecbuild_add_test(
        TARGET ${TEST_NAME}.x
        SOURCES ${TEST_FILE}
        LIBS
	${LIBNAME_PREC}
	parkind_${DEFAULT_PRECISION}
           fiat
           OpenMP::OpenMP_Fortran
        LINKER_LANGUAGE Fortran
    )

    set_target_properties(${TEST_NAME}.x
        PROPERTIES Fortran_MODULE_DIRECTORY ${CMAKE_BINARY_DIR}/include/tests
    )
endforeach()

foreach(FAILING_TEST_FILE ${FAILING_TEST_FILES})
	get_filename_component(FAILING_TEST_NAME ${FAILING_TEST_FILE} NAME_WE)
	add_executable(${FAILING_TEST_NAME}.x ${FAILING_TEST_FILE})
	target_link_libraries(${FAILING_TEST_NAME}.x ${LIBNAME_PREC} parkind_${DEFAULT_PRECISION} fiat OpenMP::OpenMP_Fortran)
	set_target_properties(${FAILING_TEST_NAME}.x PROPERTIES LINKER_LANGUAGE Fortran)
	add_test(NAME ${FAILING_TEST_NAME} COMMAND ${FAILING_TEST_NAME}.x)
	set_property(TEST ${FAILING_TEST_NAME} PROPERTY WILL_FAIL TRUE)
endforeach()

foreach(ABOR1_TEST_FILE ${ABOR1_TEST_FILES})
	get_filename_component(ABOR1_TEST_NAME ${ABOR1_TEST_FILE} NAME_WE)
	add_executable(${ABOR1_TEST_NAME}.x ${ABOR1_TEST_FILE})
	target_link_libraries(${ABOR1_TEST_NAME}.x ${LIBNAME_PREC} parkind_${DEFAULT_PRECISION} fiat OpenMP::OpenMP_Fortran)
	set_target_properties(${ABOR1_TEST_NAME}.x PROPERTIES LINKER_LANGUAGE Fortran)
	add_test(NAME ${ABOR1_TEST_NAME} COMMAND ${CMAKE_CURRENT_SOURCE_DIR}/abor1catcher.sh ${ABOR1_TEST_NAME}.x)
endforeach()

