PROGRAM main
    ! Import modules ...
    USE ISO_FORTRAN_ENV
    USE mod_safe,           ONLY:   sub_allocate_array,                         &
                                    sub_load_array_from_BIN,                    &
                                    sub_save_array_as_BIN

    IMPLICIT NONE

    ! Declare parameters ...
    INTEGER(kind = INT64), PARAMETER                                            :: nmax = 100_INT64
    INTEGER(kind = INT64), PARAMETER                                            :: nx = 43200_INT64
    INTEGER(kind = INT64), PARAMETER                                            :: ny = 21600_INT64
    INTEGER(kind = INT64), PARAMETER                                            :: scale = 10_INT64

    ! Declare variables ...
    CHARACTER(len = 12)                                                         :: bname
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
    REAL(kind = REAL32), ALLOCATABLE, DIMENSION(:, :)                           :: shrunkMask

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

    ! Allocate (889.89 MiB) array and initialize it ...
    CALL sub_allocate_array(mask, "mask", nx, ny, .TRUE._INT8)
    mask = .FALSE._INT8

    ! Allocate (35.60 MiB) array ...
    CALL sub_allocate_array(shrunkMask, "shrunkMask", nx / scale, ny / scale, .TRUE._INT8)

    ! Allow pregnant women to go to the top-left corner ...
    mask(1, 1) = .TRUE._INT8

    ! Start ~infinite loop ...
    DO i = 1_INT64, nmax
        ! Create file name ...
        WRITE(bname, '("mask", i4.4, ".bin")') i

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

        ! Print summary ...
        WRITE(fmt = '(i9, "px out of ", i9, "px are allowed (", i9, "px were added this iteration)")', unit = OUTPUT_UNIT) COUNT(mask), SIZE(mask), n
        FLUSH(unit = OUTPUT_UNIT)

        ! Loop over x-axis ...
        DO ix = 1_INT64, nx / scale
            ! Find the extent of the window ...
            ix1 = (ix - 1_INT64) * scale + 1_INT64
            ix2 =  ix            * scale

            ! Loop over y-axis ...
            DO iy = 1_INT64, ny / scale
                ! Find the extent of the window ...
                iy1 = (iy - 1_INT64) * scale + 1_INT64
                iy2 =  iy            * scale

                ! Find average mask ...
                shrunkMask(ix, iy) = REAL(COUNT(mask(ix1:ix2, iy1:iy2)), kind = REAL32) / REAL(scale * scale, kind = REAL32)
            END DO
        END DO

        ! Save shrunk mask ...
        CALL sub_save_array_as_BIN(shrunkMask, bname)

        ! Stop looping once no changes have been made ...
        IF(n == 0_INT64)THEN
            EXIT
        END IF
    END DO

    ! Clean up ...
    DEALLOCATE(elev)
    DEALLOCATE(mask)
    DEALLOCATE(shrunkMask)
END PROGRAM main
