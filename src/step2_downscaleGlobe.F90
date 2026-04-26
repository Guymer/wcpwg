PROGRAM main
    ! Import standard modules ...
    USE ISO_FORTRAN_ENV

    ! Import my modules ...
    USE mod_safe,           ONLY:   sub_allocate_array,                         &
                                    sub_load_array_from_BIN,                    &
                                    sub_save_array_as_BIN,                      &
                                    sub_shrink_array

    IMPLICIT NONE

    ! Declare parameters ...
    CHARACTER(len = 256)                                                        :: dName = "../data/globe"
    INTEGER(kind = INT64), PARAMETER                                            :: nx = 43200_INT64
    INTEGER(kind = INT64), PARAMETER                                            :: ny = 21600_INT64
    INTEGER(kind = INT64), PARAMETER                                            :: res = 1_INT64

    ! Declare variables ...
    CHARACTER(len = 256)                                                        :: fName
    LOGICAL                                                                     :: fExist
    INTEGER(kind = INT64)                                                       :: iShrinkScale
    INTEGER(kind = INT64)                                                       :: shrinkScale
    REAL(kind = REAL32), ALLOCATABLE, DIMENSION(:, :)                           :: elev
    REAL(kind = REAL32), ALLOCATABLE, DIMENSION(:, :)                           :: elevScaled

    ! **************************************************************************

    ! Check size ...
    IF(MOD(nx, 2_INT64) /= 0_INT64)THEN
        WRITE(fmt = '("ERROR: ", a, ".")', unit = ERROR_UNIT) '"nx" is not even'
        FLUSH(unit = ERROR_UNIT)
        STOP
    END IF
    IF(MOD(ny, 2_INT64) /= 0_INT64)THEN
        WRITE(fmt = '("ERROR: ", a, ".")', unit = ERROR_UNIT) '"ny" is not even'
        FLUSH(unit = ERROR_UNIT)
        STOP
    END IF

    ! Allocate array and populate it ...
    CALL sub_allocate_array(                                                    &
        elev,                                                                   &
        "elev",                                                                 &
        nx,                                                                     &
        ny,                                                                     &
        .FALSE._INT8                                                            &
    )
    CALL sub_load_array_from_BIN(                                               &
        elev,                                                                   &
        TRIM(dName) // ".bin"                                                   &
    )                                                                           ! [m]

    ! **************************************************************************

    ! Loop over possible shrink scales ...
    DO iShrinkScale = 1_INT64, 10_INT64
        ! Determine shrink scale ...
        shrinkScale = 2_INT64 ** iShrinkScale

        ! Skip this shrink scale if it is not an integer division of both axes
        ! of the array ...
        IF(MODULO(nx, shrinkScale) /= 0_INT64)THEN
            CYCLE
        END IF
        IF(MODULO(ny, shrinkScale) /= 0_INT64)THEN
            CYCLE
        END IF

        ! Create file name ...
        WRITE(                                                                  &
            fName,                                                              &
            fmt = '(a, "/scale=", i2.2, "km.bin")'                              &
        ) TRIM(dName), res * shrinkScale

        ! Skip if the output file exists ...
        INQUIRE(                                                                &
            exist = fExist,                                                     &
             file = TRIM(fName)                                                 &
        )
        IF(fExist)THEN
            WRITE(                                                              &
                 fmt = '("Skipping for resolution of ", i2, "km (the output already exists).")',    &
                unit = OUTPUT_UNIT                                              &
            ) res * shrinkScale
            FLUSH(unit = OUTPUT_UNIT)
            CYCLE
        END IF

        WRITE(                                                                  &
            fmt = '("Processing """, a, """ ...")',                             &
            unit = OUTPUT_UNIT                                                  &
        ) TRIM(fName)

        ! Shrink array, save it and clean up ...
        CALL sub_shrink_array(                                                  &
                     nx = nx,                                                   &
                     ny = ny,                                                   &
                    arr = elev,                                                 &
            shrinkScale = shrinkScale,                                          &
            shrunkenArr = elevScaled                                            &
        )                                                                       ! [m]
        CALL sub_save_array_as_BIN(                                             &
            elevScaled,                                                         &
            TRIM(fName)                                                         &
        )
        DEALLOCATE(elevScaled)
    END DO

    ! **************************************************************************

    ! Clean up ...
    DEALLOCATE(elev)
END PROGRAM main
