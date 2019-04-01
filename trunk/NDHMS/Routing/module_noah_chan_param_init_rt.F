
!  Author(s)/Contact(s):
!  Abstract:
!  History Log:
! 
!  Usage:
!  Parameters: <Specify typical arguments passed>
!  Input Files:
!        <list file names and briefly describe the data they include>
!  Output Files:
!        <list file names and briefly describe the information they include>
! 
!  Condition codes:
!        <list exit condition or error codes returned >
!        If appropriate, descriptive troubleshooting instructions or
!        likely causes for failures could be mentioned here with the
!        appropriate error code
! 
!  User controllable options: <if applicable>

MODULE module_noah_chan_param_init_rt


CONTAINS
!
!-----------------------------------------------------------------
  !subroutine CHAN_PARM_INIT (BOTWID, HLINK_INIT, CHAN_SS, CHMann)
  SUBROUTINE CHAN_PARM_INIT (BOTWID, HLINK_INIT, CHAN_SS, CHMann, TOPWID, TOPWIDCC, NCC)
!-----------------------------------------------------------------

    IMPLICIT NONE

    integer :: IINDEX, CHANCATS
    integer :: ORDER, IUNIT
    integer, PARAMETER :: NCHANTYPES=50 
    real,dimension(NCHANTYPES), intent(out)              :: BOTWID,HLINK_INIT,CHAN_SS,CHMann
    real,dimension(NCHANTYPES), intent(out), optional    :: TOPWID, TOPWIDCC, NCC
    character(LEN=11) :: DATATYPE
    character(len=127) :: field_names

!-----SPECIFY CHANNEL RELATED CHARACTERISTICS :
!             ORDER: Strahler Stream Order
!            BOTWID: Channel Bottom Width (meters)
!            TOPWID: Channel Top Width before becoming compound (meters)
!          TOPWIDCC: Channel Top Width after becoming compound (meters)
!            NCC   : mannings n of compound component
!        HLINK_INIT: Initial depth of flow in channel (meters)
!           CHAN_SS: Channel side slope (assuming trapezoidal channel geom)
!            CHMann: Channel Manning's N roughness coefficient 


!-----READ IN CHANNEL PROPERTIES FROM CHANPARM.TBL :
    IUNIT = 23
    OPEN(IUNIT, &
#ifndef NCEP_WCOSS
    FILE='CHANPARM.TBL', &
#endif
    FORM='FORMATTED',STATUS='OLD')
    READ (IUNIT,*)
    READ (IUNIT,2000,END=2002) DATATYPE
#ifdef HYDRO_D
    PRINT *, DATATYPE
#endif
    READ (IUNIT,*) CHANCATS, IINDEX, field_names
2000 FORMAT (A11)

    !-----Read in Channel Parameters as functions of stream order...

    IF (DATATYPE == 'StreamOrder') THEN
#ifdef HYDRO_D
       PRINT *, 'CHANNEL DATA SOURCE TYPE = ', DATATYPE, ' FOUND', &
            CHANCATS, ' CATEGORIES'
#endif
        do ORDER = 1, CHANCATS
        if (index(trim(field_names), 'n_CC') > 0) then
            ! assumption: if file has 'n_CC' in the field names, it's a Compound Channel file. This could be improved.
            if (present(TOPWID)) then
                read (IUNIT, *) IINDEX, BOTWID(ORDER), TOPWID(ORDER), HLINK_INIT(ORDER), CHAN_SS(ORDER),  &
                                CHMann(ORDER), TOPWIDCC(ORDER), NCC(ORDER)
            else
                error stop "Compound Channel params table supplied but compound_channel == .FALSE."
            end if
        else
            if (present(TOPWID)) then
                error stop "compound_channel == .TRUE. but Compound Channel params table not supplied"
            else
                read (IUNIT, *) IINDEX, BOTWID(ORDER), HLINK_INIT(ORDER), CHAN_SS(ORDER), CHMann(ORDER)
            end if
        end if

#ifdef HYDRO_D
          if (present(topwid)) then
              PRINT *, IINDEX, BOTWID(ORDER), TOPWID(ORDER),        &
                       HLINK_INIT(ORDER), CHAN_SS(ORDER),           &
                       CHMann(ORDER), TOPWIDCC(ORDER), NCC(ORDER)
          else
              PRINT *, IINDEX, BOTWID(ORDER), HLINK_INIT(ORDER), CHAN_SS(ORDER), CHMann(ORDER)
          end if
#endif
       ENDDO
    ENDIF


    !-----Read in Channel Parameters as functions of ???other method??? (TBC)...


2002 CONTINUE

    CLOSE (IUNIT)
  END SUBROUTINE CHAN_PARM_INIT



#ifdef MPP_LAND
  subroutine mpp_CHAN_PARM_INIT (BOTWID, HLINK_INIT, CHAN_SS, CHMann, TOPWID, TOPWIDCC, NCC)
    use module_mpp_land, only : my_id, IO_id, mpp_land_bcast_int1, &
            mpp_land_bcast_real, mpp_land_bcast_int, mpp_land_bcast_real1
    implicit none
    integer :: IINDEX, CHANCATS
    integer :: ORDER
    integer, PARAMETER :: NCHANTYPES = 50
    real, dimension(NCHANTYPES), intent(out) :: BOTWID, HLINK_INIT, CHAN_SS, CHMann
    real, dimension(NCHANTYPES), intent(out), optional :: TOPWID, TOPWIDCC, NCC  ! compound components
    character(LEN = 11) :: DATATYPE

    if (my_id.eq.io_id) then
        if (present(topwid)) then
            call CHAN_PARM_INIT(BOTWID, HLINK_INIT, CHAN_SS, CHMann, TOPWID, TOPWIDCC, NCC)
        else
            call CHAN_PARM_INIT(BOTWID, HLINK_INIT, CHAN_SS, CHMann)
        end if
    end if

    call mpp_land_bcast_real(NCHANTYPES, BOTWID)
    call mpp_land_bcast_real(NCHANTYPES, HLINK_INIT)
    call mpp_land_bcast_real(NCHANTYPES, CHAN_SS)
    call mpp_land_bcast_real(NCHANTYPES, CHMann)
    if (present(topwid)) then
        call mpp_land_bcast_real(NCHANTYPES, TOPWID)
        call mpp_land_bcast_real(NCHANTYPES, TOPWIDCC)
        call mpp_land_bcast_real(NCHANTYPES, NCC)
    end if

    return
  end subroutine mpp_CHAN_PARM_INIT
#endif
!-----------------------------------------------------------------
!-----------------------------------------------------------------


END MODULE module_Noah_chan_param_init_rt
