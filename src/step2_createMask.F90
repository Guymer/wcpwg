PROGRAM main
    ! NOTE: For documentation see:
    !         * https://support.hdfgroup.org/documentation/hdf5/latest/group___f_h5.html
    !         * https://support.hdfgroup.org/documentation/hdf5/latest/group___f_h5_a.html
    !         * https://support.hdfgroup.org/documentation/hdf5/latest/group___f_h5_d.html
    !         * https://support.hdfgroup.org/documentation/hdf5/latest/group___f_h5_f.html
    !         * https://support.hdfgroup.org/documentation/hdf5/latest/group___f_h5_g.html
    !         * https://support.hdfgroup.org/documentation/hdf5/latest/group___f_h5_s.html
    !         * https://support.hdfgroup.org/documentation/hdf5/latest/group___f_h5_t.html

    ! Import standard modules ...
    USE ISO_C_BINDING,      ONLY:   C_LOC
    USE ISO_FORTRAN_ENV,    ONLY:   ERROR_UNIT,                                 &
                                    INT8,                                       &
                                    INT16,                                      &
                                    INT32,                                      &
                                    INT64,                                      &
                                    OUTPUT_UNIT,                                &
                                    REAL32,                                     &
                                    REAL64

    ! Import special modules ...
    ! HACK: I would love to use "ONLY" in these module imports to only import
    !       the functions, parameters, subroutines and variables that I actually
    !       use. Unfortunately, the H5 library puts these in different modules
    !       in different versions of the H5 library - so if I am strict on my
    !       MacBook Pro and it all works then it will fail on GitHub CI because
    !       GitHub has a different version of the H5 library installed.
    USE H5LIB
    USE H5A
    USE H5D
    USE H5F
    USE H5G
    USE H5S
    USE H5T

    ! Import my modules ...
    USE mod_funcs
    USE mod_safe,           ONLY:   sub_allocate_array,                         &
                                    sub_border_array,                           &
                                    sub_flood_array,                            &
                                    sub_load_array_from_BIN,                    &
                                    sub_save_array_as_BIN,                      &
                                    sub_save_array_as_PGM,                      &
                                    sub_shrink_array

    IMPLICIT NONE

    ! Declare parameters ...
    INTEGER(kind = INT64), PARAMETER                                            :: nx = 43200_INT64
    INTEGER(kind = INT64), PARAMETER                                            :: ny = 21600_INT64
    INTEGER(kind = INT64), PARAMETER                                            :: ringMax = 1048576_INT64
    INTEGER(kind = INT64), PARAMETER                                            :: stepMax = 1048576_INT64
    INTEGER(kind = INT64), PARAMETER                                            :: tileScale = 32_INT64
    REAL(kind = REAL64), PARAMETER                                              :: binSizeMax = 32.0e0_REAL64
    REAL(kind = REAL64), PARAMETER                                              :: hdfSizeMax = 1024.0e0_REAL64
    REAL(kind = REAL64), PARAMETER                                              :: pgmSizeMax = 32.0e0_REAL64

    ! Declare variables ...
    CHARACTER(len = 256)                                                        :: dName1
    CHARACTER(len = 256)                                                        :: dName2
    CHARACTER(len = 256)                                                        :: fNameBIN
    CHARACTER(len = 256)                                                        :: fNameCSV
    CHARACTER(len = 256)                                                        :: fNameHDF
    CHARACTER(len = 256)                                                        :: fNamePGM
    LOGICAL(kind = INT8), ALLOCATABLE, DIMENSION(:, :)                          :: mask1
    LOGICAL(kind = INT8), ALLOCATABLE, DIMENSION(:, :)                          :: mask2
    LOGICAL(kind = INT8), ALLOCATABLE, DIMENSION(:, :)                          :: mask3
    INTEGER(kind = INT8), ALLOCATABLE, DIMENSION(:, :)                          :: used
    INTEGER(kind = INT64), TARGET                                               :: iRing
    INTEGER(kind = INT64)                                                       :: iIter
    INTEGER(kind = INT64)                                                       :: iShrinkScale
    INTEGER(kind = INT64)                                                       :: iStep
    INTEGER(kind = INT64)                                                       :: ixNew
    INTEGER(kind = INT64)                                                       :: ixOld
    INTEGER(kind = INT64)                                                       :: ixScaled
    INTEGER(kind = INT64)                                                       :: iyNew
    INTEGER(kind = INT64)                                                       :: iyOld
    INTEGER(kind = INT64)                                                       :: iyScaled
    INTEGER(kind = INT64)                                                       :: nxScaled
    INTEGER(kind = INT64)                                                       :: nyScaled
    INTEGER(kind = INT64)                                                       :: shrinkScale
    INTEGER(kind = INT64), ALLOCATABLE, DIMENSION(:)                            :: tot
    REAL(kind = REAL32)                                                         :: z
    REAL(kind = REAL32), ALLOCATABLE, DIMENSION(:, :)                           :: elev
    REAL(kind = REAL32), ALLOCATABLE, DIMENSION(:, :)                           :: shrunkMask
    REAL(kind = REAL32), ALLOCATABLE, DIMENSION(:, :)                           :: shrunkMaskRaw
    REAL(kind = REAL64)                                                         :: mega
    REAL(kind = REAL64), ALLOCATABLE, DIMENSION(:), TARGET                      :: lats
    REAL(kind = REAL64), ALLOCATABLE, DIMENSION(:), TARGET                      :: lons
    REAL(kind = REAL64), ALLOCATABLE, DIMENSION(:)                              :: x
    REAL(kind = REAL64), ALLOCATABLE, DIMENSION(:)                              :: y

    ! Declare FORTRAN variables ...
    CHARACTER(len = 256)                                                        :: errMsg
    LOGICAL(kind = INT8)                                                        :: fExist
    INTEGER(kind = INT32)                                                       :: errNum
    INTEGER(kind = INT32)                                                       :: fUnit

    ! Declare HDF5 variables ...
    CHARACTER(len = 256)                                                        :: groupName
    INTEGER(kind = HID_T)                                                       :: aUnit
    INTEGER(kind = HID_T)                                                       :: dUnit
    INTEGER(kind = HID_T)                                                       :: gUnit
    INTEGER(kind = HID_T)                                                       :: hUnit
    INTEGER(kind = HID_T)                                                       :: sUnit

    ! NOTE: The arrays go:
    !       ( 1, 1) ... (nx, 1)
    !         ...         ...
    !       ( 1,ny) ... (nx,ny)

    ! **************************************************************************

    ! Open HDF5 interface ...
    CALL H5OPEN_F(                                                              &
        error = errNum                                                          &
    )
    IF(errNum /= 0_INT32)THEN
        WRITE(                                                                  &
             fmt = '("ERROR: ", a, ". ERRNUM = ", i3, ".")',                    &
            unit = ERROR_UNIT                                                   &
        ) "H5OPEN_F() failed", errNum
        FLUSH(unit = ERROR_UNIT)
        STOP
    END IF

    ! **************************************************************************

    ! Create output directory name ...
    WRITE(                                                                      &
        dName1,                                                                 &
        fmt = '("../output/tileScale=", i2.2, "km")'                            &
    ) tileScale

    ! Ensure that the output directory exists ...
    CALL EXECUTE_COMMAND_LINE(                                                  &
        "mkdir -p " // TRIM(dName1),                                            &
          cmdmsg = errMsg,                                                      &
        exitstat = errNum                                                       &
    )
    IF(errNum /= 0_INT32)THEN
        WRITE(fmt = '("ERROR: ", a, ". ERRMSG = ", a, ". ERRNUM = ", i3, ".")', unit = ERROR_UNIT) "Failed to make output directory", TRIM(errMsg), errNum
        FLUSH(unit = ERROR_UNIT)
        STOP
    END IF

    ! **************************************************************************

    ! Print progress ...
    WRITE(                                                                      &
         fmt = '("Loading GLOBE dataset ...")',                                 &
        unit = OUTPUT_UNIT                                                      &
    )
    FLUSH(unit = OUTPUT_UNIT)

    ! Allocate array and populate it ...
    CALL sub_allocate_array(                                                    &
        elev,                                                                   &
        "elev",                                                                 &
        nx,                                                                     &
        ny,                                                                     &
        .FALSE._INT8                                                            &
    )
    CALL sub_load_array_from_BIN(                                               &
        elev,                                                                   &
        "../data/globe.bin"                                                     &
    )                                                                           ! [m]

    ! **************************************************************************
    ! **************************************************************************
    ! **************************************************************************

    ! Print progress ...
    WRITE(                                                                      &
         fmt = '("Masking the world below 2,500 m ASL ...")',                   &
        unit = OUTPUT_UNIT                                                      &
    )
    FLUSH(unit = OUTPUT_UNIT)

    ! Allocate array and set it as all pixels below 2,500 m ASL ...
    CALL sub_allocate_array(                                                    &
        mask1,                                                                  &
        "mask1",                                                                &
        nx,                                                                     &
        ny,                                                                     &
        .FALSE._INT8                                                            &
    )
    mask1 = elev < 2500.0e0_REAL32

    ! Loop over possible shrink scales ...
    DO iShrinkScale = 0_INT64, 10_INT64
        ! Determine shrink scale ...
        shrinkScale = 2_INT64 ** iShrinkScale

        ! Skip this shrink scale if it is not an integer division of both axes
        ! of the array ...
        IF(MODULO(nx, shrinkScale) /= 0_INT64)THEN
            CYCLE
        END IF
        IF(MODULO(ny, shrinkScale) /= 0_INT64)THEN
            CYCLE
        END IF

        ! Create output directory name ...
        WRITE(                                                                  &
            dName2,                                                             &
            fmt = '(a, "/scale=", i2.2, "km")'                                  &
        ) TRIM(dName1), shrinkScale

        ! Ensure that the output directory exists ...
        CALL EXECUTE_COMMAND_LINE(                                              &
            "mkdir -p " // TRIM(dName2),                                        &
              cmdmsg = errMsg,                                                  &
            exitstat = errNum                                                   &
        )
        IF(errNum /= 0_INT32)THEN
            WRITE(fmt = '("ERROR: ", a, ". ERRMSG = ", a, ". ERRNUM = ", i3, ".")', unit = ERROR_UNIT) "Failed to make output directory", TRIM(errMsg), errNum
            FLUSH(unit = ERROR_UNIT)
            STOP
        END IF

        ! Create file name ...
        fNameBIN = TRIM(dName2) // "/below2500m.bin"

        ! Skip if the output file exists ...
        INQUIRE(                                                                &
            exist = fExist,                                                     &
             file = TRIM(fNameBIN)                                              &
        )
        IF(fExist)THEN
            WRITE(                                                              &
                 fmt = '("Skipping saving """, a, """ (it already exists).")',  &
                unit = OUTPUT_UNIT                                              &
            ) TRIM(fNameBIN)
            FLUSH(unit = OUTPUT_UNIT)
            CYCLE
        END IF

        ! Create short-hands ...
        nxScaled = nx / shrinkScale                                             ! [px]
        nyScaled = ny / shrinkScale                                             ! [px]

        ! Find out how many mega-pixels there are at this shrink scale ...
        mega = REAL(nxScaled * nyScaled, kind = REAL64) / 1.0e6_REAL64          ! [Mpx]

        ! Skip this shrink scale if the BIN output would be too big ...
        IF(mega > binSizeMax)THEN
            WRITE(                                                              &
                 fmt = '("Skipping saving """, a, """ (it would be too big).")',&
                unit = OUTPUT_UNIT                                              &
            ) TRIM(fNameBIN)
            FLUSH(unit = OUTPUT_UNIT)
            CYCLE
        END IF

        ! **********************************************************************

        ! Print progress ...
        WRITE(                                                                  &
             fmt = '("Saving """, a, """ ...")',                                &
            unit = OUTPUT_UNIT                                                  &
        ) TRIM(fNameBIN)
        FLUSH(unit = OUTPUT_UNIT)

        ! Shrink the logical array down to a real array ..
        CALL sub_shrink_array(                                                  &
                     nx = nx,                                                   &
                     ny = ny,                                                   &
                    arr = mask1,                                                &
            shrinkScale = shrinkScale,                                          &
            shrunkenArr = shrunkMask                                            &
        )

        ! Save shrunk mask ...
        CALL sub_save_array_as_BIN(                                             &
            shrunkMask,                                                         &
            TRIM(fNameBIN)                                                      &
        )

        ! Clean up ...
        DEALLOCATE(shrunkMask)
    END DO

    ! **************************************************************************
    ! **************************************************************************
    ! **************************************************************************

    ! Print progress ...
    WRITE(                                                                      &
         fmt = '("Flooding the world to 2,500 m ASL ...")',                     &
        unit = OUTPUT_UNIT                                                      &
    )
    FLUSH(unit = OUTPUT_UNIT)

    ! Allocate array and initialize it to not allow pregnant women to go
    ! anywhere ...
    CALL sub_allocate_array(                                                    &
        mask2,                                                                  &
        "mask2",                                                                &
        nx,                                                                     &
        ny,                                                                     &
        .TRUE._INT8                                                             &
    )
    mask2 = .FALSE._INT8

    ! Allow pregnant women to go to the top-left corner and flood the world ...
    mask2(1, 1) = .TRUE._INT8
    CALL sub_flood_array(                                                       &
               nx = nx,                                                         &
               ny = ny,                                                         &
             elev = elev,                                                       &
         seaLevel = 2500.0e0_REAL32,                                            &
          flooded = mask2,                                                      &
        tileScale = tileScale,                                                  &
              tot = tot                                                         &
    )

    ! Clean up ...
    DEALLOCATE(elev)

    ! Loop over possible shrink scales ...
    DO iShrinkScale = 0_INT64, 10_INT64
        ! Determine shrink scale ...
        shrinkScale = 2_INT64 ** iShrinkScale

        ! Skip this shrink scale if it is not an integer division of both axes
        ! of the array ...
        IF(MODULO(nx, shrinkScale) /= 0_INT64)THEN
            CYCLE
        END IF
        IF(MODULO(ny, shrinkScale) /= 0_INT64)THEN
            CYCLE
        END IF

        ! Create output directory name ...
        WRITE(                                                                  &
            dName2,                                                             &
            fmt = '(a, "/scale=", i2.2, "km")'                                  &
        ) TRIM(dName1), shrinkScale

        ! Ensure that the output directory exists ...
        CALL EXECUTE_COMMAND_LINE(                                              &
            "mkdir -p " // TRIM(dName2),                                        &
              cmdmsg = errMsg,                                                  &
            exitstat = errNum                                                   &
        )
        IF(errNum /= 0_INT32)THEN
            WRITE(fmt = '("ERROR: ", a, ". ERRMSG = ", a, ". ERRNUM = ", i3, ".")', unit = ERROR_UNIT) "Failed to make output directory", TRIM(errMsg), errNum
            FLUSH(unit = ERROR_UNIT)
            STOP
        END IF

        ! Create file name ...
        fNameBIN = TRIM(dName2) // "/flooded.bin"

        ! Skip if the output file exists ...
        INQUIRE(                                                                &
            exist = fExist,                                                     &
             file = TRIM(fNameBIN)                                              &
        )
        IF(fExist)THEN
            WRITE(                                                              &
                 fmt = '("Skipping saving """, a, """ (it already exists).")',  &
                unit = OUTPUT_UNIT                                              &
            ) TRIM(fNameBIN)
            FLUSH(unit = OUTPUT_UNIT)
            CYCLE
        END IF

        ! Create short-hands ...
        nxScaled = nx / shrinkScale                                             ! [px]
        nyScaled = ny / shrinkScale                                             ! [px]

        ! Find out how many mega-pixels there are at this shrink scale ...
        mega = REAL(nxScaled * nyScaled, kind = REAL64) / 1.0e6_REAL64          ! [Mpx]

        ! Skip this shrink scale if the BIN output would be too big ...
        IF(mega > binSizeMax)THEN
            WRITE(                                                              &
                 fmt = '("Skipping saving """, a, """ (it would be too big).")',&
                unit = OUTPUT_UNIT                                              &
            ) TRIM(fNameBIN)
            FLUSH(unit = OUTPUT_UNIT)
            CYCLE
        END IF

        ! **********************************************************************

        ! Print progress ...
        WRITE(                                                                  &
             fmt = '("Saving """, a, """ ...")',                                &
            unit = OUTPUT_UNIT                                                  &
        ) TRIM(fNameBIN)
        FLUSH(unit = OUTPUT_UNIT)

        ! Shrink the logical array down to a real array ..
        CALL sub_shrink_array(                                                  &
                     nx = nx,                                                   &
                     ny = ny,                                                   &
                    arr = mask2,                                                &
            shrinkScale = shrinkScale,                                          &
            shrunkenArr = shrunkMask                                            &
        )

        ! Save shrunk mask ...
        CALL sub_save_array_as_BIN(                                             &
            shrunkMask,                                                         &
            TRIM(fNameBIN)                                                      &
        )

        ! Clean up ...
        DEALLOCATE(shrunkMask)
    END DO

    ! **************************************************************************
    ! **************************************************************************
    ! **************************************************************************

    ! Print progress ...
    WRITE(                                                                      &
         fmt = '("Finding inaccessible places ...")',                           &
        unit = OUTPUT_UNIT                                                      &
    )
    FLUSH(unit = OUTPUT_UNIT)

    ! Set a new mask to be all places below 2,500 m ASL but which are not
    ! flooded ...
    mask3 = mask1 .AND. (.NOT. mask2)

    ! Clean up ...
    DEALLOCATE(mask1)
    DEALLOCATE(mask2)

    ! Loop over possible shrink scales ...
    DO iShrinkScale = 0_INT64, 10_INT64
        ! Determine shrink scale ...
        shrinkScale = 2_INT64 ** iShrinkScale

        ! Skip this shrink scale if it is not an integer division of both axes
        ! of the array ...
        IF(MODULO(nx, shrinkScale) /= 0_INT64)THEN
            CYCLE
        END IF
        IF(MODULO(ny, shrinkScale) /= 0_INT64)THEN
            CYCLE
        END IF

        ! Create output directory name ...
        WRITE(                                                                  &
            dName2,                                                             &
            fmt = '(a, "/scale=", i2.2, "km")'                                  &
        ) TRIM(dName1), shrinkScale

        ! Ensure that the output directory exists ...
        CALL EXECUTE_COMMAND_LINE(                                              &
            "mkdir -p " // TRIM(dName2),                                        &
              cmdmsg = errMsg,                                                  &
            exitstat = errNum                                                   &
        )
        IF(errNum /= 0_INT32)THEN
            WRITE(fmt = '("ERROR: ", a, ". ERRMSG = ", a, ". ERRNUM = ", i3, ".")', unit = ERROR_UNIT) "Failed to make output directory", TRIM(errMsg), errNum
            FLUSH(unit = ERROR_UNIT)
            STOP
        END IF

        ! Create file name ...
        fNameBIN = TRIM(dName2) // "/inaccessible.bin"

        ! Skip if the output file exists ...
        INQUIRE(                                                                &
            exist = fExist,                                                     &
             file = TRIM(fNameBIN)                                              &
        )
        IF(fExist)THEN
            WRITE(                                                              &
                 fmt = '("Skipping saving """, a, """ (it already exists).")',  &
                unit = OUTPUT_UNIT                                              &
            ) TRIM(fNameBIN)
            FLUSH(unit = OUTPUT_UNIT)
            CYCLE
        END IF

        ! Create short-hands ...
        nxScaled = nx / shrinkScale                                             ! [px]
        nyScaled = ny / shrinkScale                                             ! [px]

        ! Find out how many mega-pixels there are at this shrink scale ...
        mega = REAL(nxScaled * nyScaled, kind = REAL64) / 1.0e6_REAL64          ! [Mpx]

        ! Skip this shrink scale if the BIN output would be too big ...
        IF(mega > binSizeMax)THEN
            WRITE(                                                              &
                 fmt = '("Skipping saving """, a, """ (it would be too big).")',&
                unit = OUTPUT_UNIT                                              &
            ) TRIM(fNameBIN)
            FLUSH(unit = OUTPUT_UNIT)
            CYCLE
        END IF

        ! **********************************************************************

        ! Print progress ...
        WRITE(                                                                  &
             fmt = '("Saving """, a, """ ...")',                                &
            unit = OUTPUT_UNIT                                                  &
        ) TRIM(fNameBIN)
        FLUSH(unit = OUTPUT_UNIT)

        ! Shrink the logical array down to a real array ..
        CALL sub_shrink_array(                                                  &
                     nx = nx,                                                   &
                     ny = ny,                                                   &
                    arr = mask3,                                                &
            shrinkScale = shrinkScale,                                          &
            shrunkenArr = shrunkMask                                            &
        )

        ! Save shrunk mask ...
        CALL sub_save_array_as_BIN(                                             &
            shrunkMask,                                                         &
            TRIM(fNameBIN)                                                      &
        )

        ! Clean up ...
        DEALLOCATE(shrunkMask)
    END DO

    ! **************************************************************************
    ! **************************************************************************
    ! **************************************************************************

    ! Create CSV file name ...
    fNameCSV = TRIM(dName1) // ".csv"

    ! Print progress ...
    WRITE(                                                                      &
         fmt = '("Saving """, a, """ ...")',                                    &
        unit = OUTPUT_UNIT                                                      &
    ) TRIM(fNameCSV)
    FLUSH(unit = OUTPUT_UNIT)

    ! Open CSV ...
    OPEN(                                                                       &
         action = "write",                                                      &
           file = TRIM(fNameCSV),                                               &
           form = "formatted",                                                  &
          iomsg = errMsg,                                                       &
         iostat = errNum,                                                       &
        newunit = fUnit,                                                        &
         status = "replace"                                                     &
    )
    IF(errNum /= 0_INT32)THEN
        WRITE(fmt = '("ERROR: ", a, ". ERRMSG = ", a, ". ERRNUM = ", i3, ".")', unit = ERROR_UNIT) "Failed to open CSV", TRIM(errMsg), errNum
        FLUSH(unit = ERROR_UNIT)
        STOP
    END IF

    ! Write header ...
    WRITE(                                                                      &
         fmt = '(a)',                                                           &
        unit = fUnit                                                            &
    ) "iteration,number of pixels allowed [#]"
    FLUSH(unit = fUnit)

    ! Loop over all the possible iterations ...
    DO iIter = LBOUND(tot, dim = 1, kind = INT64), UBOUND(tot, dim = 1, kind = INT64)
        ! Stop looping if this iteration was not populated ...
        IF(tot(iIter) == 0_INT64)THEN
            EXIT
        END IF

        ! Print progress ...
        WRITE(                                                                  &
             fmt = '("Writing progress for iteration ", i4, " ...")',           &
            unit = OUTPUT_UNIT                                                  &
        ) iIter
        FLUSH(unit = OUTPUT_UNIT)

        ! Write progress ...
        WRITE(                                                                  &
             fmt = '(i3, ",", i9)',                                             &
            unit = fUnit                                                        &
        ) iIter, tot(iIter)
        FLUSH(unit = fUnit)
    END DO

    ! Close CSV ...
    CLOSE(unit = fUnit)

    ! Clean up ...
    DEALLOCATE(tot)

    ! **************************************************************************
    ! **************************************************************************
    ! **************************************************************************

    ! Loop over possible shrink scales ...
    DO iShrinkScale = 0_INT64, 10_INT64
        ! Determine shrink scale ...
        shrinkScale = 2_INT64 ** iShrinkScale

        ! Skip this shrink scale if it is not an integer division of both axes
        ! of the array ...
        IF(MODULO(nx, shrinkScale) /= 0_INT64)THEN
            CYCLE
        END IF
        IF(MODULO(ny, shrinkScale) /= 0_INT64)THEN
            CYCLE
        END IF

        ! Create short-hands ...
        nxScaled = nx / shrinkScale                                             ! [px]
        nyScaled = ny / shrinkScale                                             ! [px]

        ! Find out how many mega-pixels there are at this shrink scale ...
        mega = REAL(nxScaled * nyScaled, kind = REAL64) / 1.0e6_REAL64          ! [Mpx]

        ! Skip this shrink scale if the output would be too big ...
        IF(mega > hdfSizeMax)THEN
            CYCLE
        END IF

        ! **********************************************************************

        ! Create output directory name ...
        WRITE(                                                                  &
            dName2,                                                             &
            fmt = '(a, "/scale=", i2.2, "km")'                                  &
        ) TRIM(dName1), shrinkScale

        ! Ensure that the output directory exists ...
        CALL EXECUTE_COMMAND_LINE(                                              &
            "mkdir -p " // TRIM(dName2),                                        &
              cmdmsg = errMsg,                                                  &
            exitstat = errNum                                                   &
        )
        IF(errNum /= 0_INT32)THEN
            WRITE(fmt = '("ERROR: ", a, ". ERRMSG = ", a, ". ERRNUM = ", i3, ".")', unit = ERROR_UNIT) "Failed to make output directory", TRIM(errMsg), errNum
            FLUSH(unit = ERROR_UNIT)
            STOP
        END IF

        ! Create file names ...
        fNameHDF = TRIM(dName2) // "/inaccessible.h5"
        fNamePGM = TRIM(dName2) // "/inaccessible.pgm"

        ! Skip if the output file exists ...
        INQUIRE(                                                                &
            exist = fExist,                                                     &
             file = TRIM(fNameHDF)                                              &
        )
        IF(fExist)THEN
            CYCLE
        END IF

        ! **********************************************************************

        ! Shrink the logical array down to a real array ..
        CALL sub_shrink_array(                                                  &
                     nx = nx,                                                   &
                     ny = ny,                                                   &
                    arr = mask3,                                                &
            shrinkScale = shrinkScale,                                          &
            shrunkenArr = shrunkMaskRaw                                         &
        )

        ! **********************************************************************

        ! Allocate arrays ...
        CALL sub_allocate_array(                                                &
            lats,                                                               &
            "lats",                                                             &
            stepMax,                                                            &
            .FALSE._INT8                                                        &
        )
        CALL sub_allocate_array(                                                &
            lons,                                                               &
            "lons",                                                             &
            stepMax,                                                            &
            .FALSE._INT8                                                        &
        )

        ! Allocate array and populate it ...
        CALL sub_allocate_array(                                                &
            x,                                                                  &
            "x",                                                                &
            nxScaled + 1_INT64,                                                 &
            .FALSE._INT8                                                        &
        )
        DO ixScaled = 0_INT64, nxScaled
            x(ixScaled + 1_INT64) = 360.0e0_REAL64 * (REAL(ixScaled, kind = REAL64) - 0.5e0_REAL64 * REAL(nxScaled, kind = REAL64)) / REAL(nxScaled, kind = REAL64) ! [°]
        END DO

        ! Allocate array and populate it ...
        CALL sub_allocate_array(                                                &
            y,                                                                  &
            "y",                                                                &
            nyScaled + 1_INT64,                                                 &
            .FALSE._INT8                                                        &
        )
        DO iyScaled = 0_INT64, nyScaled
            y(iyScaled + 1_INT64) = 180.0e0_REAL64 * (0.5e0_REAL64 * REAL(nyScaled, kind = REAL64) - REAL(iyScaled, kind = REAL64)) / REAL(nyScaled, kind = REAL64) ! [°]
        END DO

        ! Create a copy which has a border around it and clean up ...
        CALL sub_border_array(                                                  &
            nxScaled,                                                           &
            nyScaled,                                                           &
            shrunkMaskRaw,                                                      &
            shrunkMask,                                                         &
            "shrunkMask",                                                       &
            .FALSE._INT8                                                        &
        )
        DEALLOCATE(shrunkMaskRaw)

        ! Create short-hand ...
        z = 0.5e0_REAL32

        ! Create HDF5 file ...
        CALL H5FCREATE_F(                                                       &
            access_flags = H5F_ACC_TRUNC_F,                                     &
                 file_id = hUnit,                                               &
                  hdferr = errNum,                                              &
                    name = TRIM(fNameHDF)                                       &
        )
        IF(errNum /= 0_INT32)THEN
            WRITE(                                                              &
                 fmt = '("ERROR: ", a, ". ERRNUM = ", i3, ".")',                &
                unit = ERROR_UNIT                                               &
            ) "H5FCREATE_F() failed", errNum
            FLUSH(unit = ERROR_UNIT)
            STOP
        END IF

        ! Allocate array and initialize it to say that no pixels have been used
        ! so far ...
        CALL sub_allocate_array(                                                &
            used,                                                               &
            "used",                                                             &
            nxScaled,                                                           &
            nyScaled,                                                           &
            .FALSE._INT8                                                        &
        )
        used = 127_INT8

        ! Initialize counter ...
        iRing = 0_INT64                                                         ! [#]

        ! Loop over x-axis ...
        DO ixScaled = 1_INT64, nxScaled
            ! Loop over y-axis ...
            DO iyScaled = 1_INT64, nyScaled
                ! Skip this pixel if it is too low ...
                IF(shrunkMask(ixScaled, iyScaled) < z)THEN
                    CYCLE
                END IF

                ! Skip this pixel if it has been used in a previous LinearRing ...
                IF(used(ixScaled, iyScaled) == 0_INT8)THEN
                    CYCLE
                END IF

                ! Skip this pixel if it is not a local top-left corner ...
                IF(shrunkMask(ixScaled - 1_INT64, iyScaled) >= z)THEN
                    CYCLE
                END IF
                IF(shrunkMask(ixScaled - 1_INT64, iyScaled - 1_INT64) >= z)THEN
                    CYCLE
                END IF
                IF(shrunkMask(ixScaled, iyScaled - 1_INT64) >= z)THEN
                    CYCLE
                END IF

                ! Create HDF5 group ...
                WRITE(                                                          &
                    groupName,                                                  &
                    fmt = '("ring=", i6.6)'                                     &
                ) iRing
                CALL H5GCREATE_F(                                               &
                    grp_id = gUnit,                                             &
                    hdferr = errNum,                                            &
                    loc_id = hUnit,                                             &
                      name = TRIM(groupName)                                    &
                )
                IF(errNum /= 0_INT32)THEN
                    WRITE(                                                      &
                         fmt = '("ERROR: ", a, ". ERRNUM = ", i3, ".")',        &
                        unit = ERROR_UNIT                                       &
                    ) "H5GCREATE_F() failed", errNum
                    FLUSH(unit = ERROR_UNIT)
                    STOP
                END IF

                ! **************************************************************

                ! Initialize counter and arrays ...
                iStep = 0_INT64                                                 ! [#]
                lons = 0.0_REAL64                                               ! [°]
                lats = 0.0_REAL64                                               ! [°]

                ! Set initial location, mark the pixel as being used, increment
                ! counter and populate arrays ...
                ixOld = ixScaled                                                ! [px]
                iyOld = iyScaled                                                ! [px]
                used(ixOld, iyOld) = 0_INT8
                iStep = iStep + 1_INT64                                         ! [#]
                lons(iStep) = x(ixOld)                                          ! [°]
                lats(iStep) = y(iyOld)                                          ! [°]

                ! Go eastwards along the northern edge of this pixel, increment
                ! counter and populate arrays ...
                CALL sub_go_east(ixOld, iyOld, ixNew, iyNew)
                iStep = iStep + 1_INT64                                         ! [#]
                lons(iStep) = x(ixNew)                                          ! [°]
                lats(iStep) = y(iyNew)                                          ! [°]

                ! Start infinite loop ...
                DO
                    ! Crash if too many steps have been made ...
                    IF(iStep >= stepMax)THEN
                        WRITE(                                                  &
                             fmt = '("ERROR: ", a, ".")',                       &
                            unit = ERROR_UNIT                                   &
                        ) "exceeded stepMax"
                        FLUSH(unit = ERROR_UNIT)
                        STOP
                    END IF

                    ! Stop looping if we are back at the start ...
                    IF(ixNew == ixScaled .AND. iyNew == iyScaled)THEN
                        ! Create HDF5 dataspace ...
                        CALL H5SCREATE_SIMPLE_F(                                &
                                dims = (/ INT(iStep, kind = HSIZE_T) /),        &
                              hdferr = errNum,                                  &
                                rank = 1,                                       &
                            space_id = sUnit                                    &
                        )
                        IF(errNum /= 0_INT32)THEN
                            WRITE(                                              &
                                 fmt = '("ERROR: ", a, ". ERRNUM = ", i3, ".")',&
                                unit = ERROR_UNIT                               &
                            ) "H5SCREATE_SIMPLE_F() failed", errNum
                            FLUSH(unit = ERROR_UNIT)
                            STOP
                        END IF

                        ! Create HDF5 dataset ...
                        CALL H5DCREATE_F(                                       &
                             dset_id = dUnit,                                   &
                              hdferr = errNum,                                  &
                              loc_id = gUnit,                                   &
                                name = "lats",                                  &
                            space_id = sUnit,                                   &
                             type_id = H5KIND_TO_TYPE(                          &
                                 flag = H5_REAL_KIND,                           &
                                ikind = REAL64                                  &
                            )                                                   &
                        )
                        IF(errNum /= 0_INT32)THEN
                            WRITE(                                              &
                                 fmt = '("ERROR: ", a, ". ERRNUM = ", i3, ".")',&
                                unit = ERROR_UNIT                               &
                            ) "H5DCREATE_F() failed", errNum
                            FLUSH(unit = ERROR_UNIT)
                            STOP
                        END IF

                        ! Write HDF5 dataset ...
                        CALL H5DWRITE_F(                                        &
                                    buf = C_LOC(                                &
                                lats(1_INT64)                                   &
                            ),                                                  &
                                dset_id = dUnit,                                &
                                 hdferr = errNum,                               &
                            mem_type_id = H5KIND_TO_TYPE(                       &
                                 flag = H5_REAL_KIND,                           &
                                ikind = REAL64                                  &
                            )                                                   &
                        )
                        IF(errNum /= 0_INT32)THEN
                            WRITE(                                              &
                                 fmt = '("ERROR: ", a, ". ERRNUM = ", i3, ".")',&
                                unit = ERROR_UNIT                               &
                            ) "H5DWRITE_F() failed", errNum
                            FLUSH(unit = ERROR_UNIT)
                            STOP
                        END IF

                        ! Close HDF5 dataset ...
                        CALL H5DCLOSE_F(                                        &
                            dset_id = dUnit,                                    &
                             hdferr = errNum                                    &
                        )
                        IF(errNum /= 0_INT32)THEN
                            WRITE(                                              &
                                 fmt = '("ERROR: ", a, ". ERRNUM = ", i3, ".")',&
                                unit = ERROR_UNIT                               &
                            ) "H5DCLOSE_F() failed", errNum
                            FLUSH(unit = ERROR_UNIT)
                            STOP
                        END IF

                        ! Create HDF5 dataset ...
                        CALL H5DCREATE_F(                                       &
                             dset_id = dUnit,                                   &
                              hdferr = errNum,                                  &
                              loc_id = gUnit,                                   &
                                name = "lons",                                  &
                            space_id = sUnit,                                   &
                             type_id = H5KIND_TO_TYPE(                          &
                                 flag = H5_REAL_KIND,                           &
                                ikind = REAL64                                  &
                            )                                                   &
                        )
                        IF(errNum /= 0_INT32)THEN
                            WRITE(                                              &
                                 fmt = '("ERROR: ", a, ". ERRNUM = ", i3, ".")',&
                                unit = ERROR_UNIT                               &
                            ) "H5DCREATE_F() failed", errNum
                            FLUSH(unit = ERROR_UNIT)
                            STOP
                        END IF

                        ! Write HDF5 dataset ...
                        CALL H5DWRITE_F(                                        &
                                    buf = C_LOC(                                &
                                lons(1_INT64)                                   &
                            ),                                                  &
                                dset_id = dUnit,                                &
                                 hdferr = errNum,                               &
                            mem_type_id = H5KIND_TO_TYPE(                       &
                                 flag = H5_REAL_KIND,                           &
                                ikind = REAL64                                  &
                            )                                                   &
                        )
                        IF(errNum /= 0_INT32)THEN
                            WRITE(                                              &
                                 fmt = '("ERROR: ", a, ". ERRNUM = ", i3, ".")',&
                                unit = ERROR_UNIT                               &
                            ) "H5DWRITE_F() failed", errNum
                            FLUSH(unit = ERROR_UNIT)
                            STOP
                        END IF

                        ! Close HDF5 dataset ...
                        CALL H5DCLOSE_F(                                        &
                            dset_id = dUnit,                                    &
                             hdferr = errNum                                    &
                        )
                        IF(errNum /= 0_INT32)THEN
                            WRITE(                                              &
                                 fmt = '("ERROR: ", a, ". ERRNUM = ", i3, ".")',&
                                unit = ERROR_UNIT                               &
                            ) "H5DCLOSE_F() failed", errNum
                            FLUSH(unit = ERROR_UNIT)
                            STOP
                        END IF

                        ! Close HDF5 dataspace ...
                        CALL H5SCLOSE_F(                                        &
                              hdferr = errNum,                                  &
                            space_id = sUnit                                    &
                        )
                        IF(errNum /= 0_INT32)THEN
                            WRITE(                                              &
                                 fmt = '("ERROR: ", a, ". ERRNUM = ", i3, ".")',&
                                unit = ERROR_UNIT                               &
                            ) "H5SCLOSE_F() failed", errNum
                            FLUSH(unit = ERROR_UNIT)
                            STOP
                        END IF

                        EXIT
                    END IF

                    ! Check if we went north ...
                    IF(ixNew == ixOld .AND. iyNew == iyOld - 1_INT64)THEN
                        ! Move location and mark the pixel as being used ...
                        ixOld = ixNew                                           ! [px]
                        iyOld = iyNew                                           ! [px]
                        used(ixOld, iyOld) = 0_INT8

                        ! Go northwards, increment counter and populate arrays ...
                        CALL sub_going_north(nxScaled, nyScaled, ixOld, iyOld, shrunkMask, z, ixNew, iyNew)
                        iStep = iStep + 1_INT64                                 ! [#]
                        lons(iStep) = x(ixNew)                                  ! [°]
                        lats(iStep) = y(iyNew)                                  ! [°]
                        CYCLE
                    END IF

                    ! Check if we went east ...
                    IF(ixNew == ixOld + 1_INT64 .AND. iyNew == iyOld)THEN
                        ! Move location and mark the pixel as being used ...
                        ixOld = ixNew                                           ! [px]
                        iyOld = iyNew                                           ! [px]
                        used(ixOld - 1_INT64, iyOld) = 0_INT8

                        ! Go eastwards, increment counter and populate arrays ...
                        CALL sub_going_east(nxScaled, nyScaled, ixOld, iyOld, shrunkMask, z, ixNew, iyNew)
                        iStep = iStep + 1_INT64                                 ! [#]
                        lons(iStep) = x(ixNew)                                  ! [°]
                        lats(iStep) = y(iyNew)                                  ! [°]
                        CYCLE
                    END IF

                    ! Check if we went south ...
                    IF(ixNew == ixOld .AND. iyNew == iyOld + 1_INT64)THEN
                        ! Move location and mark the pixel as being used ...
                        ixOld = ixNew                                           ! [px]
                        iyOld = iyNew                                           ! [px]
                        used(ixOld - 1_INT64, iyOld - 1_INT64) = 0_INT8

                        ! Go southwards, increment counter and populate arrays ...
                        CALL sub_going_south(nxScaled, nyScaled, ixOld, iyOld, shrunkMask, z, ixNew, iyNew)
                        iStep = iStep + 1_INT64                                 ! [#]
                        lons(iStep) = x(ixNew)                                  ! [°]
                        lats(iStep) = y(iyNew)                                  ! [°]
                        CYCLE
                    END IF

                    ! Check if we went west ...
                    IF(ixNew == ixOld - 1_INT64 .AND. iyNew == iyOld)THEN
                        ! Move location and mark the pixel as being used ...
                        ixOld = ixNew                                           ! [px]
                        iyOld = iyNew                                           ! [px]
                        used(ixOld, iyOld - 1_INT64) = 0_INT8

                        ! Go westwards, increment counter and populate arrays ...
                        CALL sub_going_west(nxScaled, nyScaled, ixOld, iyOld, shrunkMask, z, ixNew, iyNew)
                        iStep = iStep + 1_INT64                                 ! [#]
                        lons(iStep) = x(ixNew)                                  ! [°]
                        lats(iStep) = y(iyNew)                                  ! [°]
                        CYCLE
                    END IF

                    ! Catch errors ...
                    WRITE(                                                      &
                         fmt = '("ERROR: ", a, ".")',                           &
                        unit = ERROR_UNIT                                       &
                    ) "did not go N/S/E/W"
                    FLUSH(unit = ERROR_UNIT)
                    STOP
                END DO

                ! **************************************************************

                ! Close HDF5 group ...
                CALL H5GCLOSE_F(                                                &
                    grp_id = gUnit,                                             &
                    hdferr = errNum                                             &
                )
                IF(errNum /= 0_INT32)THEN
                    WRITE(                                                      &
                         fmt = '("ERROR: ", a, ". ERRNUM = ", i3, ".")',        &
                        unit = ERROR_UNIT                                       &
                    ) "H5GCLOSE_F() failed", errNum
                    FLUSH(unit = ERROR_UNIT)
                    STOP
                END IF

                ! Increment counter and crash if too many rings have been made ...
                iRing = iRing + 1_INT64                                         ! [#]
                IF(iRing >= ringMax)THEN
                    WRITE(                                                      &
                         fmt = '("ERROR: ", a, ".")',                           &
                        unit = ERROR_UNIT                                       &
                    ) "exceeded ringMax"
                    FLUSH(unit = ERROR_UNIT)
                    STOP
                END IF
            END DO
        END DO

        ! Check if this shrink scale is small enough for responsible PGM output ...
        IF(mega < pgmSizeMax)THEN
            ! Save mask ...
            ! NOTE: "used" is a 8-bit integer, so saving it as a PGM is only a
            !       couple of bytes larger than saving it as a raw BIN.
            !       Therefore, I will make an exception to my normal programming
            !       style and I will write it out as a PGM directly rather than
            !       as a raw BIN. This means that the Python script to convert
            !       it to a PNG has to use "PIL.Image.open()" rather than
            !       "numpy.fromfile()".
            ! TODO: Will I make an exception though?
            CALL sub_save_array_as_PGM(                                         &
                nxScaled,                                                       &
                nyScaled,                                                       &
                used,                                                           &
                TRIM(fNamePGM)                                                  &
            )
        END IF

        ! Clean up ...
        DEALLOCATE(used)

        ! Create HDF5 dataspace ...
        CALL H5SCREATE_F(                                                       &
            classtype = H5S_SCALAR_F,                                           &
               hdferr = errNum,                                                 &
             space_id = sUnit                                                   &
        )
        IF(errNum /= 0_INT32)THEN
            WRITE(                                                              &
                 fmt = '("ERROR: ", a, ". ERRNUM = ", i3, ".")',                &
                unit = ERROR_UNIT                                               &
            ) "H5SCREATE_F() failed", errNum
            FLUSH(unit = ERROR_UNIT)
            STOP
        END IF

        ! Create HDF5 attribute ...
        CALL H5ACREATE_F(                                                       &
             attr_id = aUnit,                                                   &
              hdferr = errNum,                                                  &
              loc_id = hUnit,                                                   &
                name = "nRings",                                                &
            space_id = sUnit,                                                   &
             type_id = H5KIND_TO_TYPE(                                          &
                 flag = H5_INTEGER_KIND,                                        &
                ikind = INT64                                                   &
            )                                                                   &
        )
        IF(errNum /= 0_INT32)THEN
            WRITE(                                                              &
                 fmt = '("ERROR: ", a, ". ERRNUM = ", i3, ".")',                &
                unit = ERROR_UNIT                                               &
            ) "H5ACREATE_F() failed", errNum
            FLUSH(unit = ERROR_UNIT)
            STOP
        END IF

        ! Write HDF5 attribute ...
        CALL H5AWRITE_F(                                                        &
                   buf = C_LOC(                                                 &
                iRing                                                           &
            ),                                                                  &
               attr_id = aUnit,                                                 &
                hdferr = errNum,                                                &
            memtype_id = H5KIND_TO_TYPE(                                        &
                 flag = H5_INTEGER_KIND,                                        &
                ikind = INT64                                                   &
            )                                                                   &
        )
        IF(errNum /= 0_INT32)THEN
            WRITE(                                                              &
                 fmt = '("ERROR: ", a, ". ERRNUM = ", i3, ".")',                &
                unit = ERROR_UNIT                                               &
            ) "H5AWRITE_F() failed", errNum
            FLUSH(unit = ERROR_UNIT)
            STOP
        END IF

        ! Close HDF5 attribute ...
        CALL H5ACLOSE_F(                                                        &
            attr_id = aUnit,                                                    &
             hdferr = errNum                                                    &
        )
        IF(errNum /= 0_INT32)THEN
            WRITE(                                                              &
                 fmt = '("ERROR: ", a, ". ERRNUM = ", i3, ".")',                &
                unit = ERROR_UNIT                                               &
            ) "H5ACLOSE_F() failed", errNum
            FLUSH(unit = ERROR_UNIT)
            STOP
        END IF

        ! Close HDF5 dataspace ...
        CALL H5SCLOSE_F(                                                        &
              hdferr = errNum,                                                  &
            space_id = sUnit                                                    &
        )
        IF(errNum /= 0_INT32)THEN
            WRITE(                                                              &
                 fmt = '("ERROR: ", a, ". ERRNUM = ", i3, ".")',                &
                unit = ERROR_UNIT                                               &
            ) "H5SCLOSE_F() failed", errNum
            FLUSH(unit = ERROR_UNIT)
            STOP
        END IF

        ! Close HDF5 file ...
        CALL H5FCLOSE_F(                                                        &
            file_id = hUnit,                                                    &
             hdferr = errNum                                                    &
        )
        IF(errNum /= 0_INT32)THEN
            WRITE(                                                              &
                 fmt = '("ERROR: ", a, ". ERRNUM = ", i3, ".")',                &
                unit = ERROR_UNIT                                               &
            ) "H5FCLOSE_F() failed", errNum
            FLUSH(unit = ERROR_UNIT)
            STOP
        END IF

        ! Clean up ...
        DEALLOCATE(shrunkMask)
        DEALLOCATE(lats)
        DEALLOCATE(lons)
        DEALLOCATE(x)
        DEALLOCATE(y)

        ! **********************************************************************
        ! **********************************************************************
        ! **********************************************************************
    END DO

    ! Clean up ...
    DEALLOCATE(mask3)

    ! **************************************************************************

    ! Close HDF5 interface ...
    CALL H5CLOSE_F(                                                             &
        error = errNum                                                          &
    )
    IF(errNum /= 0_INT32)THEN
        WRITE(                                                                  &
             fmt = '("ERROR: ", a, ". ERRNUM = ", i3, ".")',                    &
            unit = ERROR_UNIT                                                   &
        ) "H5CLOSE_F() failed", errNum
        FLUSH(unit = ERROR_UNIT)
        STOP
    END IF
END PROGRAM main
