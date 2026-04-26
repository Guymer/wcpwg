PROGRAM main
    ! Import standard modules ...
    USE ISO_FORTRAN_ENV

    ! Import my modules ...
    USE mod_funcs
    USE mod_safe,           ONLY:   sub_allocate_array,                         &
                                    sub_flood_array,                            &
                                    sub_load_array_from_BIN

    IMPLICIT NONE

    ! Declare parameters ...
    INTEGER(kind = INT64), PARAMETER                                            :: nx = 43200_INT64
    INTEGER(kind = INT64), PARAMETER                                            :: ny = 21600_INT64
    INTEGER(kind = INT64), PARAMETER                                            :: tileScale = 32_INT64

    ! Declare variables ...
    CHARACTER(len = 256)                                                        :: bName
    CHARACTER(len = 256)                                                        :: cName
    CHARACTER(len = 256)                                                        :: dName1
    CHARACTER(len = 256)                                                        :: dName2
    LOGICAL(kind = INT8), ALLOCATABLE, DIMENSION(:, :)                          :: mask
    INTEGER(kind = INT64)                                                       :: iIter
    INTEGER(kind = INT64)                                                       :: iShrinkScale
    INTEGER(kind = INT64)                                                       :: shrinkScale
    INTEGER(kind = INT64), ALLOCATABLE, DIMENSION(:)                            :: tot
    REAL(kind = REAL32), ALLOCATABLE, DIMENSION(:, :)                           :: elev

    ! Declare FORTRAN variables ...
    CHARACTER(len = 256)                                                        :: errMsg
    INTEGER(kind = INT32)                                                       :: errNum
    INTEGER(kind = INT32)                                                       :: fUnit

    ! **************************************************************************

    ! Create output directory name ...
    WRITE(                                                                      &
        dName1,                                                                 &
        fmt = '("../output/tileScale=", i2.2, "km")'                            &
    ) tileScale

    ! Ensure that the output directory exists ...
    CALL EXECUTE_COMMAND_LINE(                                                  &
        "mkdir -p " // TRIM(dName1),                                            &
          cmdmsg = errMsg,                                                      &
        exitstat = errNum                                                       &
    )
    IF(errNum /= 0_INT32)THEN
        WRITE(fmt = '("ERROR: ", a, ". ERRMSG = ", a, ". ERRNUM = ", i3, ".")', unit = ERROR_UNIT) "Failed to make output directory", TRIM(errMsg), errNum
        FLUSH(unit = ERROR_UNIT)
        STOP
    END IF

    ! **************************************************************************

    ! Allocate array and populate it ...
    CALL sub_allocate_array(                                                    &
        elev,                                                                   &
        "elev",                                                                 &
        nx,                                                                     &
        ny,                                                                     &
        .TRUE._INT8                                                             &
    )
    CALL sub_load_array_from_BIN(                                               &
        elev,                                                                   &
        "../data/globe.bin"                                                     &
    )                                                                           ! [m]

    ! Allocate array and initialize it to not allow pregnant women to go
    ! anywhere ...
    CALL sub_allocate_array(                                                    &
        mask,                                                                   &
        "mask",                                                                 &
        nx,                                                                     &
        ny,                                                                     &
        .TRUE._INT8                                                             &
    )
    mask = .FALSE._INT8

    ! Allow pregnant women to go to the top-left corner and flood the world ...
    mask(1, 1) = .TRUE._INT8
    CALL sub_flood_array(                                                       &
               nx = nx,                                                         &
               ny = ny,                                                         &
             elev = elev,                                                       &
         seaLevel = 2500.0e0_REAL32,                                            &
          flooded = mask,                                                       &
        tileScale = tileScale,                                                  &
              tot = tot                                                         &
    )

    ! **************************************************************************

    ! Create CSV file name ...
    WRITE(                                                                      &
        cName,                                                                  &
        fmt = '(a, ".csv")'                                                     &
    ) TRIM(dName1)

    ! Open CSV ...
    OPEN(                                                                       &
         action = "write",                                                      &
           file = TRIM(cName),                                                  &
           form = "formatted",                                                  &
          iomsg = errMsg,                                                       &
         iostat = errNum,                                                       &
        newunit = fUnit,                                                        &
         status = "replace"                                                     &
    )
    IF(errNum /= 0_INT32)THEN
        WRITE(fmt = '("ERROR: ", a, ". ERRMSG = ", a, ". ERRNUM = ", i3, ".")', unit = ERROR_UNIT) "Failed to open CSV", TRIM(errMsg), errNum
        FLUSH(unit = ERROR_UNIT)
        STOP
    END IF

    ! Write header ...
    WRITE(                                                                      &
         fmt = '(a)',                                                           &
        unit = fUnit                                                            &
    ) "iteration,number of pixels allowed [#]"
    FLUSH(unit = fUnit)

    ! Loop over all the possible iterations ...
    DO iIter = LBOUND(tot, dim = 1, kind = INT64), UBOUND(tot, dim = 1, kind = INT64)
        ! Stop looping if this iteration was not populated ...
        IF(tot(iIter) == 0_INT64)THEN
            EXIT
        END IF

        ! Print progress ...
        WRITE(                                                                  &
             fmt = '("Writing progress for iteration ", i4, " ...")',           &
            unit = OUTPUT_UNIT                                                  &
        ) iIter
        FLUSH(unit = OUTPUT_UNIT)

        ! Write progress ...
        WRITE(                                                                  &
             fmt = '(i3, ",", i9)',                                             &
            unit = fUnit                                                        &
        ) iIter, tot(iIter)
        FLUSH(unit = fUnit)
    END DO

    ! Close CSV ...
    CLOSE(unit = fUnit)

    ! **************************************************************************

    ! Print progress ...
    WRITE(                                                                      &
         fmt = '("Saving final answer ...")',                                   &
        unit = OUTPUT_UNIT                                                      &
    )
    FLUSH(unit = OUTPUT_UNIT)

    ! Loop over possible shrink scales ...
    DO iShrinkScale = 0_INT64, 10_INT64
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

        ! **********************************************************************

        ! Create output directory name ...
        WRITE(                                                                  &
            dName2,                                                             &
            fmt = '(a, "/scale=", i2.2, "km")'                                  &
        ) TRIM(dName1), shrinkScale

        ! Ensure that the output directory exists ...
        CALL EXECUTE_COMMAND_LINE(                                              &
            "mkdir -p " // TRIM(dName2),                                        &
              cmdmsg = errMsg,                                                  &
            exitstat = errNum                                                   &
        )
        IF(errNum /= 0_INT32)THEN
            WRITE(fmt = '("ERROR: ", a, ". ERRMSG = ", a, ". ERRNUM = ", i3, ".")', unit = ERROR_UNIT) "Failed to make output directory", TRIM(errMsg), errNum
            FLUSH(unit = ERROR_UNIT)
            STOP
        END IF

        ! **********************************************************************

        ! Create BIN file name ...
        WRITE(                                                                  &
            bName,                                                              &
            fmt = '(a, "/iIter=", i4.4, ".bin")'                                &
        ) TRIM(dName2), iIter - 1_INT64

        ! Save shrunk final mask ...
        CALL saveShrunkMask(                                                    &
                     nx = nx,                                                   &
                     ny = ny,                                                   &
                   mask = mask,                                                 &
            shrinkScale = shrinkScale,                                          &
                  bName = TRIM(bName)                                           &
        )
    END DO

    ! Clean up ...
    DEALLOCATE(elev)
    DEALLOCATE(mask)
    DEALLOCATE(tot)
END PROGRAM main
