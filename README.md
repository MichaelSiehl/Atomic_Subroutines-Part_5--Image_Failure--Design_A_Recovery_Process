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

- Two parallel logic procedures (OOOPimsc_ControlSegmentSynchronization_CA, OOOPimsc_ExecuteSegmentSynchronization_CA) that allow to restore the segment ordering among coarray images. This is a simple and fast running parallel algorithm example that requires synchronization at multiple times. For the synchronizations, the procedures make use of the above customized EventPost / EventWait procedures.<br />
The (1) ControlSegmentSynchronization and  (2) ExecuteSegmentSynchronization procedures are getting executed on distinct coarray images: (1) on coarray image 1 and (2) on coarray images 2-6 with the test case (in main.f90). It is important to note, that these procedures do synchronize multiple times through the customized Events procedures, but with always changing calls to the customized EventPost / EventWait procedures resp. This allows our parallel algorithm to abort on both sides (i.e. in both procedures) through the customized EventWait's abort timer after a synchronization failure occurs. This is also the key feature of our recovery process after image failure: To immediately abort the parallel algorithm execution and thus, to allow for a soon restart of the parallel algorithms with different or, in the case of the test case here, with fewer coarray images, excluding the failed image(s).<br />

# The main.f90 test case
The main.f90 source code file contains a simple test case that is further explained here.<br />

Fortran 2018 introduces the FAIL IMAGE statement to allow for failure testing. The Fortran 2008 example program here does not execute any code on a coarray image to simulate a 'failed image' instead. In such a case, the runtime behavior with atomic subroutines may be undefined. With current processors, it appears that the remote data transfer channels get corrupted after such a simulated image failure. (More precisely: it appears as if the timing of the remote data transfer gets corrupted after a failure). The solution to recover the 'corrupted remote data transfer channel timing' for use with atomic subroutines then, was to use a combination of two synchronization 'tricks':<br />
Firstly, simply switch the (corrupted) remote data transfer channel directly after a synchronization failure to a different remote data transfer channel. It appears that there are several options to do that: 1. by using another declared (derived type) coarray, 2. by using another derived type coarray component, and 3. by using another array index of a derived type coarray array component. This last option is the most flexible and easy-to-use option, we only must extend a (derived type) array (component) with an additional dimension and use the newly available array index to switch the remote data transfer channel just using simple Fortran array syntax. (As an aside: the original, now corrupted, data transfer channel does recover automatically with current processor under certain circumstances).<br />
Secondly, and required with ifort, apply a 'circular synchronization' with both, the customized EventPost and customized EventWait procedures. This is explained in some detail here: https://github.com/MichaelSiehl/Atomic_Subroutines-Part_4a--Implementing_A_Circular_Synchronization .<br />

Nevertheless, it is not recommended to rely on such programming tricks for any production codes. Instead we will use Fortran 2018 techniques as soon as they are available.<br />

# Adjusting the timer
The OOOGglob_Globals.f90 source code file contains two parameter definitions (constants) that allow to adjust the timer of the example program: OOOGglob_reaTimeLimitInSec_small and OOOGglob_reaCircSyncTimeDelayInSec_small can be used to adjust for execution with another processor / system / algorithm. That may or may not work (undefined runtime behavior).
