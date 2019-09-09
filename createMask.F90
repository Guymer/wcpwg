PROGRAM main
    ! Import modules ...
    USE ISO_FORTRAN_ENV
    USE mod_safe,           ONLY:   sub_allocate_array,                         &
                                    sub_load_array_from_BIN

    IMPLICIT NONE

    ! Declare parameters ...
    INTEGER(kind = INT64), PARAMETER                                            :: nx = 43200_INT64
    INTEGER(kind = INT64), PARAMETER                                            :: ny = 21600_INT64

    ! Declare variables ...
    LOGICAL(kind = INT8), ALLOCATABLE, DIMENSION(:, :)                          :: mask
    INTEGER(kind = INT16), ALLOCATABLE, DIMENSION(:, :)                         :: elev

    ! Allocate array and populate it ...
    CALL sub_allocate_array(elev, "elev", nx, ny, .FALSE._INT8)
    CALL sub_load_array_from_BIN(elev, "all10g.bin")                            ! [m]

    ! Allocate array and initialize it ...
    CALL sub_allocate_array(mask, "mask", nx, ny, .FALSE._INT8)
    mask = .FALSE._INT8

    ! Allow pregnant women to go to the top-left corner, i.e., (1,1) ...
    mask(1, 1) = .TRUE._INT8

    WRITE(*, *) elev(1, 1)

    ! Clean up ...
    DEALLOCATE(elev)
    DEALLOCATE(mask)
END PROGRAM main
