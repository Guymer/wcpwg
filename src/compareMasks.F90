PROGRAM main
    ! Import modules ...
    USE ISO_FORTRAN_ENV
    USE mod_safe,           ONLY:   sub_allocate_array,                         &
                                    sub_load_array_from_BIN,                    &
                                    sub_save_array_as_PBM,                      &
                                    sub_save_array_as_PPM

    IMPLICIT NONE

    ! Declare parameters ...
    INTEGER(kind = INT64), PARAMETER                                            :: nx = 43200_INT64
    INTEGER(kind = INT64), PARAMETER                                            :: ny = 21600_INT64

    ! Declare variables ...
    LOGICAL(kind = INT8), ALLOCATABLE, DIMENSION(:, :)                          :: mask1
    LOGICAL(kind = INT8), ALLOCATABLE, DIMENSION(:, :)                          :: mask2
    LOGICAL(kind = INT8), ALLOCATABLE, DIMENSION(:, :)                          :: mask3
    INTEGER(kind = INT16), ALLOCATABLE, DIMENSION(:, :)                         :: flags
    INTEGER(kind = INT64)                                                       :: ix
    INTEGER(kind = INT64)                                                       :: iy

    ! Declare FORTRAN variables ...
    CHARACTER(len = 256)                                                        :: errmsg
    INTEGER(kind = INT32)                                                       :: errnum

    ! Ensure that the output directory exists ...
    CALL EXECUTE_COMMAND_LINE("mkdir -p ../compareMasksOutput", CMDMSG = errmsg, EXITSTAT = errnum)
    IF(errnum /= 0_INT32)THEN
        WRITE(fmt = '("ERROR: ", a, ". ERRMSG = ", a, ". ERRNUM = ", i3, ".")', unit = ERROR_UNIT) "Failed to make output directory", TRIM(errmsg), errnum
        FLUSH(unit = ERROR_UNIT)
        STOP
    END IF

    ! Allocate one (1.74 GiB) array and three (889.89 MiB) arrays ...
    CALL sub_allocate_array(flags, "flags", nx, ny, .TRUE._INT8)
    CALL sub_allocate_array(mask1, "mask1", nx, ny, .TRUE._INT8)
    CALL sub_allocate_array(mask2, "mask2", nx, ny, .TRUE._INT8)
    CALL sub_allocate_array(mask3, "mask3", nx, ny, .TRUE._INT8)

    ! Populate arrays ...
    CALL sub_load_array_from_BIN(mask1, "../createMask3output/before.bin")
    CALL sub_load_array_from_BIN(mask2, "../createMask3output/after.bin")

    ! Print summary ...
    WRITE(fmt = '(f9.6, "% of the world is <= 2,500m ASL")', unit = OUTPUT_UNIT) 100.0e0_REAL64 * REAL(COUNT(mask1, kind = INT64), kind = REAL64) / REAL(nx * ny, kind = REAL64)
    WRITE(fmt = '(f9.6, "% of the world is accessible")', unit = OUTPUT_UNIT) 100.0e0_REAL64 * REAL(COUNT(mask2, kind = INT64), kind = REAL64) / REAL(nx * ny, kind = REAL64)
    FLUSH(unit = OUTPUT_UNIT)

    ! Find out which pixels are <= 2,500m ASL but not accessible ...
    DO ix = 1_INT64, nx
        DO iy = 1_INT64, ny
            IF(mask1(ix, iy))THEN
                IF(mask2(ix, iy))THEN
                    flags(ix, iy) = 255_INT16
                    mask3(ix, iy) = .FALSE._INT8
                ELSE
                    flags(ix, iy) = 127_INT16
                    mask3(ix, iy) = .TRUE._INT8
                END IF
            ELSE
                flags(ix, iy) = 0_INT16
                mask3(ix, iy) = .FALSE._INT8
            END IF
        END DO
    END DO

    ! Clean up ...
    DEALLOCATE(mask1)
    DEALLOCATE(mask2)

    ! Save difference in masks ...
    CALL sub_save_array_as_PBM(mask3, "../compareMasksOutput/diff.pbm")

    ! Clean up ...
    DEALLOCATE(mask3)

    ! Save flags ...
    CALL sub_save_array_as_PPM(flags, "../compareMasksOutput/flags.ppm", "r2o2g")

    ! Clean up ...
    DEALLOCATE(flags)
END PROGRAM
