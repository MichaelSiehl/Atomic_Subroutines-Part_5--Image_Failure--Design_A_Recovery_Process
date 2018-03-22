# Atomic_Subroutines-Part_5--Image_Failure--Design_A_Recovery_Process
Fortran 2008 coarray programming with unordered execution segments (user-defined ordering) and customized synchronization procedures - Atomic Subroutines - Part 5: How to cope with unreliable data transfers at low-level PGAS programming - Image failure: Design a recovery process.

# Be aware:
The example program here uses coding tricks that are surely undefined by the Fortran 2008 language. (More precisely, the runtime behavior of atomic subroutines after an image failure might be undefined). Thus, the coding tricks shown here may or may not work with a specific processor / hardware / algorithm and shouldn't be used with any production codes. The only reason for still uploading this is that the example code could be used to show that data transfer failures through atomic subroutines after (simulated) image failure are due to timing failures: The example code uses a 'circular synchronization trick' to recover from such 'timing failures' between coarray images.

# Overview
This GitHub repository contains a relatively simple Fortran 2008 coarray program that demonstrates the basic design of a recovery process after an image failure. Such a recovery process is described for Fortran 2018 in chapter 3.2 of the paper 'The new features of Fortran 2018' by John Reid, ISO/IEC JTC1/SC22/WG5 N2145 (https://isotc.iso.org/livelink/livelink?func=ll&objId=19441669&objAction=Open&viewType=1).<br />

Currently (February 2018) the implementations do not already offer full support for the Fortran 2018 newly added coarray-related features (image failure and teams). Thus, the example program here does adhere to Fortran 2008 syntax. The recovery solution of this example program is nothing I would recommend for real-world programming or production codes. Rather, it shows that the newly added coarray-related language features of the upcoming Fortran 2018 standard do perfectly match with our real-world requirements for PGAS / coarray programming.<br />

# How it works – A simple test case in Main.f90
The Main.f90 source code file contains a simple test case that is briefly explained here. Further detailed description and codes are given in the sections further below.<br />

After creating unordered execution segments on all coarray images (just by executing varying numbers of calls to a SYNC MEMORY statement internally), the code in Main.f90 does restore the segment ordering on the coarray images greater than 1 through calls to a 'ExecuteSegmentSynchronization' procedure. Image 1 does execute the required counterpart procedure to control the segment restoring (synchronization) 'ControlSegmentSynchronization'. These both procedures do comprise our simple test parallel algorithm. The usual case is that the segment ordering is getting restored successfully on images 2-6.<br />
See the following output from running the test case in Main.f90 successfully:
```fortran
 No ControlSegmentSync A failure on image           1
```

--- A ---<br />
But now, to simulate an image failure with Fortran 2008, we do not execute any code on coarray images 2 and 3. As a consequence the parallel algorithm does fail and execution of the both procedures does abort. (This is because on image 1 the 'ControlSegmentSynchronization' procedure does try to synchronize with the 'ExecuteSegmentSynchronization' procedure executed on coarray images 2-6, but only coarray images 4-6 do actually execute this procedure).<br />
See the following output from running the test case in Main.f90:
```fortran
 ControlSegmentSync A failed on image           1 
 ControlSegmentSync A: NumberOfSuccessfulRemoteImages:           3 
 ControlSegmentSync A: TheSuccessfulRemoteImageNumbers:           4           6 
           5           0           0 
```

--- B ---<br />
So far, the remote data transfer through calls to the atomic subroutines was not corrupted. But then, after the simulated failure on coarray images 2 and 3, any follow-up remote data transfer through the atomic subroutines is very likely to be corrupted. (The Fortran language may leave the behavior of calls to atomic subroutines after an image failure as 'undefined'). To show this we do repeat the parallel algorithm after a failure with only those coarray images that did not fail previously. See the following output from doing that:<br />
```fortran
 logSynchronizationFailure B (TRUE/FALSE), on image: T           1 
 ControlSegmentSync B: NumberOfSuccessfulRemoteImages:           0 
 ControlSegmentSync B: TheSuccessfulRemoteImageNumbers:           0           0 
           0           0           0 
```

To recover from such corrupted remote data transfer channels after an image failure (using Fortran 2008 means), the 'ControlSegmentSynchronization' and 'ExecuteSegmentSynchronization' procedures do have two optional arguments to set: 'logActivateCircularSynchronization = .true.' and 'intDataTransferChannel = 2'. (With Fortran 2018 I would use coarray teams instead). The following output comes from a successful recovery process (this follow-up execution of the parallel algorithm does only use those coarray images that were successful previously):<br />
```fortran
 ExecuteSegmentSynchronization B successful on image           4 
 ExecuteSegmentSynchronization B successful on image           5 
 ExecuteSegmentSynchronization B successful on image           6 
 logSynchronizationFailure B (TRUE/FALSE), on image: F           1 
 ControlSegmentSync B: NumberOfSuccessfulRemoteImages:           3 
 ControlSegmentSync B: TheSuccessfulRemoteImageNumbers:           4           5 
           6           0           0 
```

More detailed explanations are given in the sections below.<br />

# The OOOPimsc_admImageStatus_CA.f90 source code file
The OOOPimsc_admImageStatus_CA.f90 source code file contains all the required parallel logic codes, mainly:<br />
- Two procedures that implement customized EventPost / EventWait synchronizations (OOOPimscEventPostScalar_intImageActivityFlag99_CA , OOOPimscEventWaitScalar_intImageActivityFlag99_CA).<br />
These do offer synchronization diagnostics (to detect synchronization failures of any kind), an abort timer (i.e. a time limit for the synchronization process), and a circular synchronization feature that is required to recover from a corrupted data transfer through atomic subroutines after a (simulated) synchronization failure. (This last feature is outside the Fortran language specification, the behavior of atomic subroutines after image failure may be undefined. We do only use it because it was required with ifort, and nothing else from the Fortran 2008 language could help to recover after image failure. It is strongly recommended to use Fortran 2018 features instead, when available.)<br />
The features of the customized EventPost / EventWait procedures are described in more detail here:<br />
https://github.com/MichaelSiehl/Atomic_Subroutines-Part_4--How_To_Cope_With_Unreliable_Data_Transfers<br />
https://github.com/MichaelSiehl/Atomic_Subroutines-Part_4a--Implementing_A_Circular_Synchronization<br />
https://github.com/MichaelSiehl/Atomic_Subroutines-Part_4b--Implementing_A_Synchronization_With_Abort_Timer<br />

