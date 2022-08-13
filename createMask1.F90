PROGRAM main
    ! Import modules ...
    USE ISO_FORTRAN_ENV
    USE mod_funcs
    USE mod_safe,           ONLY:   sub_allocate_array,                         &
                                    sub_load_array_from_BIN

    IMPLICIT NONE

    ! Declare parameters ...
    INTEGER(kind = INT64), PARAMETER                                            :: nmax = 200_INT64
    INTEGER(kind = INT64), PARAMETER                                            :: nx = 43200_INT64
    INTEGER(kind = INT64), PARAMETER                                            :: ny = 21600_INT64
    INTEGER(kind = INT64), PARAMETER                                            :: scale = 20_INT64

    ! Declare variables ...
    CHARACTER(len = 24)                                                         :: bname
    CHARACTER(len = 24)                                                         :: iname
    LOGICAL(kind = INT8), ALLOCATABLE, DIMENSION(:, :)                          :: mask
    INTEGER(kind = INT16), ALLOCATABLE, DIMENSION(:, :)                         :: elev
    INTEGER(kind = INT64)                                                       :: i
    INTEGER(kind = INT64)                                                       :: oldtot
    INTEGER(kind = INT64)                                                       :: newtot

    ! Declare FORTRAN variables ...
    CHARACTER(len = 256)                                                        :: errmsg
    INTEGER(kind = INT32)                                                       :: errnum
    INTEGER(kind = INT32)                                                       :: funit

    ! Check scale ...
    IF(MOD(nx, scale) /= 0_INT64)THEN
        WRITE(fmt = '("ERROR: ", a, ".")', unit = ERROR_UNIT) '"nx" is not an integer multiple of "scale"'
        FLUSH(unit = ERROR_UNIT)
        STOP
    END IF
    IF(MOD(ny, scale) /= 0_INT64)THEN
        WRITE(fmt = '("ERROR: ", a, ".")', unit = ERROR_UNIT) '"ny" is not an integer multiple of "scale"'
        FLUSH(unit = ERROR_UNIT)
        STOP
    END IF

    ! Allocate (1.74 GiB) array and populate it ...
    CALL sub_allocate_array(elev, "elev", nx, ny, .TRUE._INT8)
    CALL sub_load_array_from_BIN(elev, "all10g.bin")                            ! [m]

    ! Allocate (889.89 MiB) array and initialize it to not allow pregnant women
    ! to go anywhere ...
    CALL sub_allocate_array(mask, "mask", nx, ny, .TRUE._INT8)
    mask = .FALSE._INT8

    ! Allow pregnant women to go to the top-left corner ...
    mask(1, 1) = .TRUE._INT8

    ! Open CSV ...
    OPEN(action = "write", file = "createMask1.csv", form = "formatted", iomsg = errmsg, iostat = errnum, newunit = funit, status = "replace")
    IF(errnum /= 0_INT32)THEN
        WRITE(fmt = '("ERROR: ", a, ". ERRMSG = ", a, ". ERRNUM = ", i3, ".")', unit = ERROR_UNIT) "Failed to open BIN", TRIM(errmsg), errnum
        FLUSH(unit = ERROR_UNIT)
        STOP
    END IF

    ! Write header ...
    WRITE(fmt = '(a)', unit = funit) "iteration,pixels allowed"
    FLUSH(unit = funit)

    ! Start ~infinite loop ...
    DO i = 1_INT64, nmax
        ! Print progress ...
        WRITE(fmt = '("Calculating step ", i4, " of (up to) ", i4, " ...")', unit = OUTPUT_UNIT) i, nmax
        FLUSH(unit = OUTPUT_UNIT)

        ! Create file names ...
        WRITE(bname, '("createMask1_mask", i4.4, ".bin")') i
        WRITE(iname, '("createMask1_mask", i4.4, ".ppm")') i

        ! Find initial total ...
        oldtot = COUNT(mask, kind = INT64)

        ! Increment mask ...
        CALL incrementMask(nx, ny, elev, mask, 1_INT64, nx, 1_INT64, ny)

        ! Find new total ...
        newtot = COUNT(mask, kind = INT64)

        ! Write progress ...
        WRITE(fmt = '(i3, ",", i9)', unit = funit) i, newtot
        FLUSH(unit = funit)

        ! Stop looping once no changes have been made ...
        IF(newtot == oldtot)THEN
            EXIT
        END IF
    END DO

    ! Close CSV ...
    CLOSE(unit = funit)

    ! Print progress ...
    WRITE(fmt = '("Saving final answer ...")', unit = OUTPUT_UNIT)
    FLUSH(unit = OUTPUT_UNIT)

    ! Save shrunk final mask ...
    CALL saveShrunkMask(nx, ny, mask, scale, bname, iname)

    ! Clean up ...
    DEALLOCATE(elev)
    DEALLOCATE(mask)
END PROGRAM main
