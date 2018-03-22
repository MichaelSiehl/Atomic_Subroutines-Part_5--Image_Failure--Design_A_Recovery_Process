! https://github.com/MichaelSiehl/Atomic_Subroutines-Part_5--Image_Failure--Design_A_Recovery_Process

module OOOGglob_Globals

  implicit none
  public
  ! Application Main Directory Path:
  character(kind=1,len=240) :: OOOGglob_chrAppPath = ""
  !
  ! upper bound declarations for the static components of
  ! derived type coarrays:
  integer, parameter :: OOOGglob_TeamManagers_UpperBound = 5 ! max number of TeamManagers
  integer, parameter :: OOOGglob_TeamMembers_UpperBound = 10 ! max number of TeamMembers per team
  integer, parameter :: OOOGglob_NumImages_UpperBound = 100 ! max number of images
  !
  !********************************************************
  !********************************************************
  !
  ! constants for additional remote data transfer channels
  ! (through additioanl array elements)
  ! with customized EventWait/EventPost synchronization procedures:
  ! (these are used with the coarray's type definition)
  integer(kind=4), parameter :: OOOGglob_intDataTransferChannels_small = 6
  integer(kind=4), parameter :: OOOGglob_intDataTransferChannels_medium = 10
  integer(kind=4), parameter :: OOOGglob_DataTransferChannels_large = 20
  !
!  !********************************************************
!  !********************************************************
!  !
!  ! default time delay in seconds for EventWaitScalar:
!  integer(kind=4), parameter :: OOOGglob_DefaultTimeDelayForEventWaitScalar = 0.0
!  !
  !********************************************************
  !
  ! time limit constants for automatic abort of the
  ! customized EventWait/EventPost synchronization procedures:
  ! (time limit in seconds)
  real(kind=4), parameter :: OOOGglob_reaTimeLimitInSec_small = 0.9 ! 0.9 works well with ifort
                                                                    ! (0.8 may already lead to rare failures)
                                                                    ! 0.7 works with gfortran/OpenCoarrays
  real(kind=4), parameter :: OOOGglob_reaTimeLimitInSec_medium = 5.0
  real(kind=4), parameter :: OOOGglob_reaTimeLimitInSec_large = 10.0
  !
  !********************************************************
  !********************************************************
  !
  ! time delay constants for circular synchronization with the
  ! customized EventWait synchronization procedure:
  ! (time delay in seconds)
  real(kind=4), parameter :: OOOGglob_reaCircSyncTimeDelayInSec_small = 0.001 ! 0.001 works well with ifort
                                                                              ! (gfortran/OpenCoarrays may work better
                                                                              !  without any circular sync at all)
  !
  !********************************************************
  !********************************************************
  ! Stack Trace:
  character(kind=1,len=35), dimension (1:50) :: OOOGglob_chrStackTrace
  integer(kind=4) :: OOOGglob_intStackTraceCounter = 0
  !
  !********************************************************
  !********************************************************
  ! kind values:
  integer, parameter :: OOOGglob_kint = 4 ! kind for integer
  integer, parameter :: OOOGglob_krea = 4 ! kind for real
  integer, parameter :: OOOGglob_kcom = 4 ! kind for COMPLEX
  integer, parameter :: OOOGglob_klog = 4 ! kind for logical
  integer, parameter :: OOOGglob_kcha = 1 ! kind for character, 1=ASCII
  integer, parameter :: OOOGglob_Len40 = 40 ! len for character
  integer, parameter :: OOOGglob_Len200 = 200 ! len for character
  integer, parameter :: OOOGglob_Len240 = 240 ! len for character
  integer, parameter :: OOOGglob_Len2400 = 2400 ! len for character
  !
  !**********************************************************
  ! Input/Output-constants
  ! file UNITs:
  integer(kind=4), parameter :: OOOGglob_FileUnitA = 1
  integer(kind=4), parameter :: OOOGglob_FileUnitB = 2
  ! for Tracing und Error-Handling:
  integer(kind=4), parameter :: OOOGglob_TracingFileUnit = 3
  integer(kind=4), parameter :: OOOGglob_StackTraceFileUnit = 4
  integer(kind=4), parameter :: OOOGglob_ErrorLogFileUnit = 7 ! 5 is keyboard and 6 is screen
  !
  !**********************************************************
  ! Error-Handling
  character(kind=1,len=40) :: OOOGglob_chrCurrentProcedure = "Main"
  character(kind=1,len=40) :: OOOGglob_chrCallingProcedure = "User"
  character(kind=1,len=40) :: OOOGglob_chrReturningProcedure = ""
  character(kind=1,len=40) :: OOOGglob_chrTempCallingProcedur = ""

  !**********************************************************
  ! Error-Handling
  real(kind=4) :: OOOGglob_reaNaN
  real(kind=4) :: OOOGglob_reaPositiveInfinity
  real(kind=4) :: OOOGglob_reaNegativeInfinity

  !**********************************************************
  ! Error-Handling
  integer(kind=4), parameter :: OOOGglob_warning = 1
  integer(kind=4), parameter :: OOOGglob_error = 2
  integer(kind=4), parameter :: OOOGglob_NoErrNumber = 0
  !

!**********************************************************
contains

! Public-Subroutines:
!__________________________________________________________

!***********************************************************
!
subroutine OOOGglob_subSetProcedures (chrCurrentProcedure)
  ! Error-Handling and Tracing
  character(kind=OOOGglob_kcha,len=*), intent(in) :: chrCurrentProcedure
  ! refresh the Stack Trace:
!  OOOGglob_intStackTraceCounter = OOOGglob_intStackTraceCounter + 1
!  OOOGglob_chrStackTrace (OOOGglob_intStackTraceCounter) = chrCurrentProcedure
!  OOOGglob_chrCurrentProcedure = OOOGglob_chrStackTrace (OOOGglob_intStackTraceCounter)
!  OOOGglob_chrCallingProcedure = OOOGglob_chrStackTrace (OOOGglob_intStackTraceCounter - 1)
!  OOOGglob_chrReturningProcedure = ''
  !
end subroutine OOOGglob_subSetProcedures
!_____________
!
subroutine OOOGglob_subResetProcedures
  ! Error-Handling and Tracing
  ! refresh the Stack Trace:
!  OOOGglob_intStackTraceCounter = OOOGglob_intStackTraceCounter - 1
!  OOOGglob_chrCurrentProcedure = OOOGglob_chrStackTrace (OOOGglob_intStackTraceCounter)
!  OOOGglob_chrReturningProcedure = OOOGglob_chrStackTrace (OOOGglob_intStackTraceCounter + 1)
!  if (OOOGglob_intStackTraceCounter > 1) then
!    OOOGglob_chrCallingProcedure = OOOGglob_chrStackTrace (OOOGglob_intStackTraceCounter - 1)
!  end if
  !
end subroutine OOOGglob_subResetProcedures
!__________________________________________________________
!
!************************************************************

!**********************************************************
!**********************************************************
!**********************************************************

end module OOOGglob_Globals
