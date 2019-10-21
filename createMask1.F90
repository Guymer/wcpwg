PROGRAM main
    ! Import modules ...
    USE ISO_FORTRAN_ENV
    USE mod_safe,           ONLY:   sub_allocate_array,                         &
                                    sub_load_array_from_BIN

    IMPLICIT NONE

    ! Declare parameters ...
    INTEGER(kind = INT64), PARAMETER                                            :: nmax = 100_INT64
    INTEGER(kind = INT64), PARAMETER                                            :: nx = 43200_INT64
    INTEGER(kind = INT64), PARAMETER                                            :: ny = 21600_INT64
    INTEGER(kind = INT64), PARAMETER                                            :: scale = 10_INT64

    ! Declare variables ...
    CHARACTER(len = 12)                                                         :: bname
    CHARACTER(len = 12)                                                         :: iname
    LOGICAL(kind = INT8), ALLOCATABLE, DIMENSION(:, :)                          :: mask
    INTEGER(kind = INT16), ALLOCATABLE, DIMENSION(:, :)                         :: elev
    INTEGER(kind = INT64)                                                       :: i
    INTEGER(kind = INT64)                                                       :: ix
    INTEGER(kind = INT64)                                                       :: ix1
    INTEGER(kind = INT64)                                                       :: ix2
    INTEGER(kind = INT64)                                                       :: iy
    INTEGER(kind = INT64)                                                       :: iy1
    INTEGER(kind = INT64)                                                       :: iy2
    INTEGER(kind = INT64)                                                       :: n

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
    WRITE(fmt = '(a)', unit = funit) "iteration,number added"
    FLUSH(unit = funit)

    ! Start ~infinite loop ...
    DO i = 1_INT64, nmax
        ! Create file names ...
        WRITE(bname, '("mask", i4.4, ".bin")') i
        WRITE(iname, '("mask", i4.4, ".ppm")') i

        ! Initialize counter ...
        n = 0_INT64                                                             ! [#]

        ! Loop over x-axis ...
        DO ix = 1_INT64, nx
            ! Find the limits of the border around this pixel ...
            ix1 = MAX(ix - 1_INT64, 1_INT64)
            ix2 = MIN(ix + 1_INT64,      nx)

            ! Loop over y-axis ...
            DO iy = 1_INT64, ny
                ! Find the limits of the border around this pixel ...
                iy1 = MAX(iy - 1_INT64, 1_INT64)
                iy2 = MIN(iy + 1_INT64,      ny)

                ! Check that this pixel has not already been allowed ...
                IF(.NOT. mask(ix, iy))THEN
                    ! Check that this pixel is <= 2,500m ASL ...
                    IF(elev(ix, iy) <= 2500_INT16)THEN
                        ! Check that this pixel is accessible ...
                        IF(ANY(mask(ix1:ix2, iy1:iy2)))THEN
                            ! Allow pregnant women to go here and increment
                            ! counter ...
                            mask(ix, iy) = .TRUE._INT8
                            n = n + 1_INT64                                     ! [#]
                        END IF
                    END IF
                END IF
            END DO
        END DO

        ! Write progress ...
        WRITE(fmt = '(i3, ",", i9)', unit = funit) i, n
        FLUSH(unit = funit)

        ! Stop looping once no changes have been made ...
        IF(n == 0_INT64)THEN
            EXIT
        END IF
    END DO

    ! Close CSV ...
    CLOSE(unit = funit)

    ! Clean up ...
    DEALLOCATE(elev)
    DEALLOCATE(mask)
END PROGRAM main
