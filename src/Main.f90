! https://github.com/MichaelSiehl/Atomic_Subroutines-Part_5--Image_Failure--Design_A_Recovery_Process
!
program Main
  ! a simple test-case to show
  ! how to repair the corrupted runtime of a faulty F2008 coarray program
  ! for atomic subroutines
  ! (example using a simple fast running parallel algorithm:
  ! restore segment ordering among coarray images)
  !
  use OOOGglob_Globals
  use OOOEerro_admError
  use OOOPimsc_admImageStatus_CA
  implicit none
  !
  integer(OOOGglob_kint) :: intCount1, intCount2 !, i
  integer(OOOGglob_kint) :: intNumberOfRemoteImages
  integer(OOOGglob_kint), dimension (1:5) :: intA_RemoteImageNumbers ! please compile and run this coarray
                                                                     ! program with 6 coarray images
  logical(OOOGglob_klog) :: logSynchronizationFailure = .false.
  integer(OOOGglob_kint) :: intControlImageNumber
  integer(OOOGglob_kint) :: intNumberOfSuccessfulRemoteImages
  integer(OOOGglob_kint), dimension (1:5) :: intA_TheSuccessfulRemoteImageNumbers
  !
  !******************************************************************
  !** on all images: create unordered execution segments ************
  !******************************************************************
  ! create user-defined (unordered) execution segments among the images:
  do intCount1 = 1, num_images()
    if (this_image() == intCount1) then
      do intCount2 = 1, (6-this_image())
        call OOOPimsc_subSyncMemory (OOOPimscImageStatus_CA_1) ! execute sync memory to
                                                               ! enter a new execution segment
      end do
    end if
  end do
  !
  sync all
  !
  !*************************************************************************
  !** on image 1: control the segment order synchronization (restoring)  ***
  !**             among the involved remote images                       ***
  !*************************************************************************
  !
  if (this_image() == 1) then
    !
    intNumberOfRemoteImages = 5
    intA_RemoteImageNumbers = (/2,3,4,5,6/) ! the involved remote image numbers
    !
    ! ********************************************************************
    ! --- A ---
    ! this starts the segment synchronization control routine on image 1:
    call OOOPimsc_ControlSegmentSynchronization_CA (OOOPimscImageStatus_CA_1, intNumberOfRemoteImages, &
                intA_RemoteImageNumbers(1:intNumberOfRemoteImages), logSynchronizationFailure, &
                intNumberOfSuccessfulRemoteImages, intA_TheSuccessfulRemoteImageNumbers, &
                intDataTransferChannel = 1)
    ! ********************************************************************
    ! --- B ---
    ! sync failure handling:
    if (logSynchronizationFailure) then ! sync failure did occur, repeat the sync:
      !
      write(*,*) 'ControlSegmentSync A failed on image', this_image()
      write(*,*) 'ControlSegmentSync A: NumberOfSuccessfulRemoteImages:', intNumberOfSuccessfulRemoteImages
      write(*,*) 'ControlSegmentSync A: TheSuccessfulRemoteImageNumbers:', intA_TheSuccessfulRemoteImageNumbers
      !
      ! repeat the parallel algorithm after failure, this time through another data transfer channel
      ! (using a distinct derived type coarray array component's array index, indicated by
      ! the optional intDataTransferChannel argument set to value 2) and by applying
      ! a circular synchronization (required with ifort, works also with gfortran/OpenCoarrays)
      intNumberOfRemoteImages = intNumberOfSuccessfulRemoteImages ! use only the successful remote images
      intA_RemoteImageNumbers(1:intNumberOfRemoteImages) = intA_TheSuccessfulRemoteImageNumbers(1:intNumberOfRemoteImages)
      call OOOPimsc_ControlSegmentSynchronization_CA (OOOPimscImageStatus_CA_1, intNumberOfRemoteImages, &
                intA_RemoteImageNumbers(1:intNumberOfRemoteImages), logSynchronizationFailure, &
                intNumberOfSuccessfulRemoteImages, intA_TheSuccessfulRemoteImageNumbers, &
                logActivateCircularSynchronization = .true., & ! this time as circular sync
                intDataTransferChannel = 2) ! and through another data transfer channel
      ! signal if the sync failure handling did fail/succeed:
      write(*,*) 'logSynchronizationFailure (TRUE/FALSE) B on image', logSynchronizationFailure, this_image()
      write(*,*) 'ControlSegmentSync B: NumberOfSuccessfulRemoteImages:', intNumberOfSuccessfulRemoteImages
      write(*,*) 'ControlSegmentSync B: TheSuccessfulRemoteImageNumbers:', intA_TheSuccessfulRemoteImageNumbers
    else ! no sync failure did occur
      write(*,*) 'No ControlSegmentSync A failure on image', this_image()
    end if
    !
    !*********************************************************************
!    ! --- C ---
!    ! repeat the segment synchronization control routine on image 1:
!    call OOOPimsc_ControlSegmentSynchronization_CA (OOOPimscImageStatus_CA_1, intNumberOfRemoteImages, &
!                intA_RemoteImageNumbers(1:intNumberOfRemoteImages), logSynchronizationFailure, &
!                intNumberOfSuccessfulRemoteImages, intA_TheSuccessfulRemoteImageNumbers, &
!                intDataTransferChannel = 3) ! another data transfer channel is required to avoid faulty behaviour
!                                            ! after a sync failure
!    !***********************
!    ! sync failure handling:
!    if (logSynchronizationFailure) then ! sync failure did occur:
!      write(*,*)'ControlSegmentSync C failed on image', this_image()
!      write(*,*) 'ControlSegmentSync C: NumberOfSuccessfulRemoteImages:', intNumberOfSuccessfulRemoteImages
!      write(*,*) 'ControlSegmentSync C: TheSuccessfulRemoteImageNumbers:', intA_TheSuccessfulRemoteImageNumbers
!    else ! no sync failure did occur
!      write(*,*) 'No ControlSegmentSync C failure on image', this_image()
!    end if
  !
  end if
  !
  !
  !**************************************************************
  !** on other images: do the segment synchronization ***********
  !**************************************************************
  !
  if (this_image() > 3) then ! restore the segment ordering on the involved images
    !
    intControlImageNumber = 1 ! image 1 does execute the counterpart ControlSegmentSynchronization
                              ! routine as the other part of this example parallel algorithm
    !
    ! ********************************************************************
    ! --- A ---
    ! start the segment synchronization on the executing image:
    call OOOPimsc_ExecuteSegmentSynchronization_CA (OOOPimscImageStatus_CA_1, intControlImageNumber, &
                logSynchronizationFailure, intDataTransferChannel = 1)
    !
    ! ********************************************************************
    ! --- B ---
    ! sync failure handling:
    if (logSynchronizationFailure) then ! sync failure did occur, repeat the sync:
      write(*,*)'ExecuteSegmentSynchronization A failed on image', this_image()
      ! repeat the parallel algorithm after failure, this time through another data transfer channel
      ! (using a distinct derived type coarray array component's array index, indicated by
      ! the optional intDataTransferChannel argument set to value 2) and by applying
      ! a circular synchronization (required with ifort, works also with gfortran/OpenCoarrays)
      call OOOPimsc_ExecuteSegmentSynchronization_CA (OOOPimscImageStatus_CA_1, intControlImageNumber, &
                logSynchronizationFailure, logActivateCircularSynchronization = .true., &
                intDataTransferChannel = 2)
      !
      if (logSynchronizationFailure) then ! check if the sync failure handling did also fail
        write(*,*)'ExecuteSegmentSynchronization B failed on image', this_image()
      else ! no failure
        write(*,*)'ExecuteSegmentSynchronization B successful on image', this_image()
      end if
    end if
    !
    ! ********************************************************************
    !
    ! create newly user-defined (unordered) execution segments among the images:
    do intCount1 = 1, num_images()
     if (this_image() == intCount1) then
       do intCount2 = 1, this_image()
         call OOOPimsc_subSyncMemory (OOOPimscImageStatus_CA_1) ! execute sync memory to
                                                                ! enter a new execution segment
       end do
     end if
    end do
    !
    ! ********************************************************************
!    ! --- C ---
!    ! repeat the segment synchronization on the executing image:
!    call OOOPimsc_ExecuteSegmentSynchronization_CA (OOOPimscImageStatus_CA_1, intControlImageNumber, &
!                                                logSynchronizationFailure, intDataTransferChannel = 3)
!                                ! another data transfer channel is required to avoid faulty behaviour
!                                ! after a sync failure
!    !
!    ! sync failure handling:
!    if (logSynchronizationFailure) then
!      write(*,*)'ExecuteSegmentSynchronization C failed on image', this_image()
!    else
!      write(*,*)'ExecuteSegmentSynchronization C successful on image', this_image()
!    end if
    !
  end if
    !
    !******************************************************************
    !
  write (*,*) 'execution finsished on image ', this_image()
    !
end program Main
