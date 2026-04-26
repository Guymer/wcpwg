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
    CHARACTER(len = 256)                                                        :: bname
    LOGICAL(kind = INT8), ALLOCATABLE, DIMENSION(:, :)                          :: mask
    INTEGER(kind = INT16), ALLOCATABLE, DIMENSION(:, :)                         :: elev
    INTEGER(kind = INT64)                                                       :: iIter
    INTEGER(kind = INT64), ALLOCATABLE, DIMENSION(:)                            :: tot

    ! Declare FORTRAN variables ...
    CHARACTER(len = 256)                                                        :: errmsg
    INTEGER(kind = INT32)                                                       :: errnum
    INTEGER(kind = INT32)                                                       :: funit

    ! Check scale ...
    IF(MOD(nx, tileScale) /= 0_INT64)THEN
        WRITE(fmt = '("ERROR: ", a, ".")', unit = ERROR_UNIT) '"nx" is not an integer multiple of "tileScale"'
        FLUSH(unit = ERROR_UNIT)
        STOP
    END IF
    IF(MOD(ny, tileScale) /= 0_INT64)THEN
        WRITE(fmt = '("ERROR: ", a, ".")', unit = ERROR_UNIT) '"ny" is not an integer multiple of "tileScale"'
        FLUSH(unit = ERROR_UNIT)
        STOP
    END IF

    ! Ensure that the output directory exists ...
    CALL EXECUTE_COMMAND_LINE(                                                  &
        "mkdir -p ../createMask1output",                                        &
          cmdmsg = errmsg,                                                      &
        exitstat = errnum                                                       &
    )
    IF(errnum /= 0_INT32)THEN
        WRITE(fmt = '("ERROR: ", a, ". ERRMSG = ", a, ". ERRNUM = ", i3, ".")', unit = ERROR_UNIT) "Failed to make output directory", TRIM(errmsg), errnum
        FLUSH(unit = ERROR_UNIT)
        STOP
    END IF

    ! Allocate (1.74 GiB) array and populate it ...
    CALL sub_allocate_array(elev, "elev", nx, ny, .TRUE._INT8)
    CALL sub_load_array_from_BIN(elev, "../all10g.bin")                         ! [m]

    ! Allocate (889.89 MiB) array and initialize it to not allow pregnant women
    ! to go anywhere ...
    CALL sub_allocate_array(mask, "mask", nx, ny, .TRUE._INT8)
    mask = .FALSE._INT8

    ! Allow pregnant women to go to the top-left corner and flood the world
    ! (without using tiling) ...
    mask(1, 1) = .TRUE._INT8
    CALL sub_flood_array(                                                       &
               nx = nx,                                                         &
               ny = ny,                                                         &
             elev = elev,                                                       &
         seaLevel = 2500_INT16,                                                 &
          flooded = mask,                                                       &
        tileScale = 1_INT64,                                                    &
              tot = tot                                                         &
    )

    ! Open CSV ...
    OPEN(                                                                       &
         action = "write",                                                      &
           file = "../createMask1.csv",                                         &
           form = "formatted",                                                  &
          iomsg = errmsg,                                                       &
         iostat = errnum,                                                       &
        newunit = funit,                                                        &
         status = "replace"                                                     &
    )
    IF(errnum /= 0_INT32)THEN
        WRITE(fmt = '("ERROR: ", a, ". ERRMSG = ", a, ". ERRNUM = ", i3, ".")', unit = ERROR_UNIT) "Failed to open CSV", TRIM(errmsg), errnum
        FLUSH(unit = ERROR_UNIT)
        STOP
    END IF

    ! Write header ...
    WRITE(fmt = '(a)', unit = funit) "iteration,pixels allowed"
    FLUSH(unit = funit)

    ! Loop over all the iterations ...
    DO iIter = LBOUND(tot, dim = 1, kind = INT64), UBOUND(tot, dim = 1, kind = INT64)
        ! Stop looping if this iteration was not populated ...
        IF(tot(iIter) == 0_INT64)THEN
            EXIT
        END IF

        ! Print progress ...
        WRITE(fmt = '("Saving convergence for iteration ", i4, " ...")', unit = OUTPUT_UNIT) iIter
        FLUSH(unit = OUTPUT_UNIT)

        ! Create file name ...
        WRITE(bname, '("../createMask1output/mask", i4.4, "_scale=", i2.2, "km.bin")') iIter, tileScale

        ! Write progress ...
        WRITE(fmt = '(i3, ",", i9)', unit = funit) iIter, tot(iIter)
        FLUSH(unit = funit)
    END DO

    ! Close CSV ...
    CLOSE(unit = funit)

    ! Print progress ...
    WRITE(fmt = '("Saving final answer ...")', unit = OUTPUT_UNIT)
    FLUSH(unit = OUTPUT_UNIT)

    ! Save shrunk final mask ...
    CALL saveShrunkMask(                                                        &
               nx = nx,                                                         &
               ny = ny,                                                         &
             mask = mask,                                                       &
        tileScale = tileScale,                                                  &
            bname = TRIM(bname)                                                 &
    )

    ! Clean up ...
    DEALLOCATE(elev)
    DEALLOCATE(mask)
    DEALLOCATE(tot)
END PROGRAM main
