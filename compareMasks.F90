PROGRAM main
    ! Import modules ...
    USE ISO_FORTRAN_ENV
    USE mod_safe,           ONLY:   sub_allocate_array,                         &
                                    sub_load_array_from_BIN,                    &
                                    sub_save_array_as_BIN,                      &
                                    sub_save_array_as_PBM

    IMPLICIT NONE

    ! Declare parameters ...
    INTEGER(kind = INT64), PARAMETER                                            :: nx = 43200_INT64
    INTEGER(kind = INT64), PARAMETER                                            :: ny = 21600_INT64

    ! Declare variables ...
    LOGICAL(kind = INT8), ALLOCATABLE, DIMENSION(:, :)                          :: mask1
    LOGICAL(kind = INT8), ALLOCATABLE, DIMENSION(:, :)                          :: mask2
    LOGICAL(kind = INT8), ALLOCATABLE, DIMENSION(:, :)                          :: mask3

    ! Allocate three (889.89 MiB) arrays and populate them ...
    CALL sub_allocate_array(mask1, "mask1", nx, ny, .TRUE._INT8)
    CALL sub_allocate_array(mask2, "mask2", nx, ny, .TRUE._INT8)
    CALL sub_allocate_array(mask3, "mask3", nx, ny, .TRUE._INT8)
    CALL sub_load_array_from_BIN(mask1, "createMask3_before.bin")
    CALL sub_load_array_from_BIN(mask2, "createMask3_after.bin")

    ! Print summary ...
    WRITE(fmt = '(f9.6, "% of the world is <= 2,500m ASL")', unit = OUTPUT_UNIT) 100.0e0_REAL64 * REAL(COUNT(mask1, kind = INT64), kind = REAL64) / REAL(nx * ny, kind = REAL64)
    WRITE(fmt = '(f9.6, "% of the world is accessible")', unit = OUTPUT_UNIT) 100.0e0_REAL64 * REAL(COUNT(mask2, kind = INT64), kind = REAL64) / REAL(nx * ny, kind = REAL64)
    FLUSH(unit = OUTPUT_UNIT)

    ! Find out which pixels are <= 2,500m ASL but not accessible ...
    mask3 = mask1 .AND. .NOT. mask2

    ! Clean up ...
    DEALLOCATE(mask1)
    DEALLOCATE(mask2)

    ! Save difference in masks ...
    CALL sub_save_array_as_BIN(mask3, "createMask3_diff.bin")
    CALL sub_save_array_as_PBM(mask3, "createMask3_diff.pbm")

    ! Clean up ...
    DEALLOCATE(mask3)
END PROGRAM
