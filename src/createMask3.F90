PROGRAM main
    ! Import modules ...
    USE ISO_FORTRAN_ENV
    USE mod_funcs
    USE mod_safe,           ONLY:   sub_allocate_array,                         &
                                    sub_load_array_from_BIN,                    &
                                    sub_save_array_as_BIN,                      &
                                    sub_save_array_as_PBM

    IMPLICIT NONE

    ! Declare parameters ...
    INTEGER(kind = INT64), PARAMETER                                            :: nmax = 200_INT64
    INTEGER(kind = INT64), PARAMETER                                            :: nmax_thread = 1000_INT64
    INTEGER(kind = INT64), PARAMETER                                            :: nx = 43200_INT64
    INTEGER(kind = INT64), PARAMETER                                            :: ny = 21600_INT64
    INTEGER(kind = INT64), PARAMETER                                            :: scale = 20_INT64

    ! Declare variables ...
    CHARACTER(len = 33)                                                         :: bname
    CHARACTER(len = 33)                                                         :: iname
    LOGICAL(kind = INT8), ALLOCATABLE, DIMENSION(:, :)                          :: mask
    INTEGER(kind = INT16), ALLOCATABLE, DIMENSION(:, :)                         :: elev
    INTEGER(kind = INT64)                                                       :: i
    INTEGER(kind = INT64)                                                       :: i_thread
    INTEGER(kind = INT64)                                                       :: ix
    INTEGER(kind = INT64)                                                       :: ixlo
    INTEGER(kind = INT64)                                                       :: ixhi
    INTEGER(kind = INT64)                                                       :: iy
    INTEGER(kind = INT64)                                                       :: iylo
    INTEGER(kind = INT64)                                                       :: iyhi
    INTEGER(kind = INT64)                                                       :: newtot
    INTEGER(kind = INT64)                                                       :: newtot_thread
    INTEGER(kind = INT64)                                                       :: oldtot
    INTEGER(kind = INT64)                                                       :: oldtot_thread

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

    ! Ensure that the output directory exists ...
    CALL EXECUTE_COMMAND_LINE("mkdir -p ../createMask3output", CMDMSG = errmsg, EXITSTAT = errnum)
    IF(errnum /= 0_INT32)THEN
        WRITE(fmt = '("ERROR: ", a, ". ERRMSG = ", a, ". ERRNUM = ", i3, ".")', unit = ERROR_UNIT) "Failed to make output directory", TRIM(errmsg), errnum
        FLUSH(unit = ERROR_UNIT)
        STOP
    END IF

    ! Allocate (1.74 GiB) array and populate it ...
    CALL sub_allocate_array(elev, "elev", nx, ny, .TRUE._INT8)
    CALL sub_load_array_from_BIN(elev, "../all10g.bin")                         ! [m]

    ! Allocate (889.89 MiB) array ...
    CALL sub_allocate_array(mask, "mask", nx, ny, .TRUE._INT8)

    ! Initialize mask to allow pregnant women to go anywhere <= 2,500m ASL ...
    mask = elev <= 2500_INT16

    ! Print progress ...
    WRITE(fmt = '("Saving initial answer ...")', unit = OUTPUT_UNIT)
    FLUSH(unit = OUTPUT_UNIT)

    ! Save initial mask ...
    CALL sub_save_array_as_BIN(mask, "../createMask3output/before.bin")
    CALL sub_save_array_as_PBM(nx, ny, mask, "../createMask3output/before.pbm")

    ! Initialize mask to not allow pregnant women to go anywhere except the
    ! top-left corner ...
    mask = .FALSE._INT8
    mask(1, 1) = .TRUE._INT8

    ! Open CSV ...
    OPEN(action = "write", file = "../createMask3.csv", form = "formatted", iomsg = errmsg, iostat = errnum, newunit = funit, status = "replace")
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
        WRITE(bname, '("../createMask3output/mask", i4.4, ".bin")') i
        WRITE(iname, '("../createMask3output/mask", i4.4, ".ppm")') i

        ! Find initial total ...
        oldtot = COUNT(mask, kind = INT64)

        ! Increment mask (of the world) ...
        CALL incrementMask(nx, ny, elev, mask, 1_INT64, nx, 1_INT64, ny)

        !$omp parallel                                                          &
        !$omp default(none)                                                     &
        !$omp private(i_thread)                                                 &
        !$omp private(ix)                                                       &
        !$omp private(ixlo)                                                     &
        !$omp private(ixhi)                                                     &
        !$omp private(iy)                                                       &
        !$omp private(iylo)                                                     &
        !$omp private(iyhi)                                                     &
        !$omp private(oldtot_thread)                                            &
        !$omp private(newtot_thread)                                            &
        !$omp shared(elev)                                                      &
        !$omp shared(mask)
            !$omp do                                                            &
            !$omp schedule(dynamic)
                ! Loop over x-axis tiles ...
                DO ix = 1_INT64, nx / scale
                    ! Find the extent of the tile ...
                    ixlo = (ix - 1_INT64) * scale + 1_INT64
                    ixhi =  ix            * scale

                    ! Loop over y-axis tiles ...
                    DO iy = 1_INT64, ny / scale
                        ! Find the extent of the tile ...
                        iylo = (iy - 1_INT64) * scale + 1_INT64
                        iyhi =  iy            * scale

                        ! Start ~infinite loop ...
                        DO i_thread = 1_INT64, nmax_thread
                            ! Find initial total ...
                            oldtot_thread = COUNT(mask(ixlo:ixhi, iylo:iyhi), kind = INT64)

                            ! Stop looping once no changes can be made ...
                            IF(oldtot_thread == 0_INT64 .OR. oldtot_thread == scale ** 2)THEN
                                EXIT
                            END IF

                            ! Increment mask (of the tile) ...
                            CALL incrementMask(nx, ny, elev, mask, ixlo, ixhi, iylo, iyhi)

                            ! Find new total ...
                            newtot_thread = COUNT(mask(ixlo:ixhi, iylo:iyhi), kind = INT64)

                            ! Stop looping once no changes have been made ...
                            IF(newtot_thread == oldtot_thread)THEN
                                EXIT
                            END IF
                        END DO
                    END DO
                END DO
            !$omp end do
        !$omp end parallel

        ! Find new total ...
        newtot = COUNT(mask, kind = INT64)

        ! Write progress ...
        WRITE(fmt = '(i3, ",", i9)', unit = funit) i, newtot
        FLUSH(unit = funit)

        ! Save shrunk mask ...
        CALL saveShrunkMask(nx, ny, mask, scale, bname, iname)

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

    ! Save final mask ...
    CALL sub_save_array_as_BIN(mask, "../createMask3output/after.bin")
    CALL sub_save_array_as_PBM(nx, ny, mask, "../createMask3output/after.pbm")

    ! Clean up ...
    DEALLOCATE(elev)
    DEALLOCATE(mask)
END PROGRAM main
