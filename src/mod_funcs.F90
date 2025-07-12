MODULE mod_funcs
    CONTAINS

    PURE SUBROUTINE incrementMask(nx, ny, elev, mask, ixlo, ixhi, iylo, iyhi)
        ! Import standard modules ...
        USE ISO_FORTRAN_ENV

        IMPLICIT NONE

        ! Declare inputs/outputs ...
        INTEGER(kind = INT64), INTENT(in)                                       :: nx
        INTEGER(kind = INT64), INTENT(in)                                       :: ny
        INTEGER(kind = INT16), DIMENSION(nx, ny), INTENT(inout)                 :: elev
        LOGICAL(kind = INT8), DIMENSION(nx, ny), INTENT(inout)                  :: mask
        INTEGER(kind = INT64), INTENT(in)                                       :: ixlo
        INTEGER(kind = INT64), INTENT(in)                                       :: ixhi
        INTEGER(kind = INT64), INTENT(in)                                       :: iylo
        INTEGER(kind = INT64), INTENT(in)                                       :: iyhi

        ! Declare variables ...
        INTEGER(kind = INT64)                                                   :: ix
        INTEGER(kind = INT64)                                                   :: ix1
        INTEGER(kind = INT64)                                                   :: ix2
        INTEGER(kind = INT64)                                                   :: iy
        INTEGER(kind = INT64)                                                   :: iy1
        INTEGER(kind = INT64)                                                   :: iy2

        ! Loop over x-axis ...
        DO ix = ixlo, ixhi
            ! Find the limits of the border around this pixel ...
            ix1 = MAX(ix - 1_INT64, ixlo)
            ix2 = MIN(ix + 1_INT64, ixhi)

            ! Loop over y-axis ...
            DO iy = iylo, iyhi
                ! Find the limits of the border around this pixel ...
                iy1 = MAX(iy - 1_INT64, iylo)
                iy2 = MIN(iy + 1_INT64, iyhi)

                ! Check that this pixel has not already been allowed ...
                IF(.NOT. mask(ix, iy))THEN
                    ! Check that this pixel is <= 2,500m ASL ...
                    IF(elev(ix, iy) <= 2500_INT16)THEN
                        ! Check that this pixel is accessible ...
                        IF(ANY(mask(ix1:ix2, iy1:iy2)))THEN
                            ! Allow pregnant women to go here ...
                            mask(ix, iy) = .TRUE._INT8
                        END IF
                    END IF
                END IF
            END DO
        END DO
    END SUBROUTINE incrementMask

    SUBROUTINE saveShrunkMask(nx, ny, mask, tileScale, bname)
        ! Import standard modules ...
        USE ISO_FORTRAN_ENV

        ! Import my modules ...
        USE mod_safe,       ONLY:   sub_save_array_as_BIN,                      &
                                    sub_shrink_array

        IMPLICIT NONE

        ! Declare inputs/outputs ...
        INTEGER(kind = INT64), INTENT(in)                                       :: nx
        INTEGER(kind = INT64), INTENT(in)                                       :: ny
        LOGICAL(kind = INT8), DIMENSION(nx, ny), INTENT(in)                     :: mask
        INTEGER(kind = INT64), INTENT(in)                                       :: tileScale
        CHARACTER(len = *), INTENT(in)                                          :: bname

        ! Declare variables ...
        REAL(kind = REAL32), ALLOCATABLE, DIMENSION(:, :)                       :: shrunkMask

        ! Shrink the logical array down to a real array ..
        CALL sub_shrink_array(                                                  &
                     nx = nx,                                                   &
                     ny = ny,                                                   &
                    arr = mask,                                                 &
            shrinkScale = tileScale,                                            &
            shrunkenArr = shrunkMask                                            &
        )

        ! Save shrunk mask ...
        CALL sub_save_array_as_BIN(shrunkMask, TRIM(bname))

        ! Clean up ...
        DEALLOCATE(shrunkMask)
    END SUBROUTINE saveShrunkMask
END MODULE mod_funcs
