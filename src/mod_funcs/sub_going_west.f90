PURE SUBROUTINE sub_going_west(nx, ny, ixOld, iyOld, elev, z, ixNew, iyNew)
    USE ISO_FORTRAN_ENV

    IMPLICIT NONE

    ! Declare inputs ...
    INTEGER(kind = INT64), INTENT(in)                                           :: nx
    INTEGER(kind = INT64), INTENT(in)                                           :: ny
    INTEGER(kind = INT64), INTENT(in)                                           :: ixOld
    INTEGER(kind = INT64), INTENT(in)                                           :: iyOld
    REAL(kind = REAL32), DIMENSION(0:nx + 1, 0:ny + 1), INTENT(in)              :: elev
    REAL(kind = REAL32), INTENT(in)                                             :: z
    INTEGER(kind = INT64), INTENT(out)                                          :: ixNew
    INTEGER(kind = INT64), INTENT(out)                                          :: iyNew

    ! Check if we can go South ...
    IF(elev(ixOld - 1_INT64, iyOld - 1_INT64) >= z .AND. elev(ixOld - 1_INT64, iyOld) >= z)THEN
        CALL sub_go_south(ixOld, iyOld, ixNew, iyNew)
        RETURN
    END IF

    ! Check if we can go West ...
    IF(elev(ixOld - 1_INT64, iyOld - 1_INT64) >= z)THEN
        CALL sub_go_west(ixOld, iyOld, ixNew, iyNew)
        RETURN
    END IF

    ! Assume that we can go North ...
    CALL sub_go_north(ixOld, iyOld, ixNew, iyNew)
END SUBROUTINE sub_going_west
