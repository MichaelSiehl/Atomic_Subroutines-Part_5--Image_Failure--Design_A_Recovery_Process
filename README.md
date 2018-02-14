# Atomic_Subroutines-Part_5--Image_Failure--Design_A_Recovery_Process
Fortran 2008 coarray programming with unordered execution segments (user-defined ordering) and customized synchronization procedures - Atomic Subroutines - Part 5: How to cope with unreliable data transfers at low-level PGAS programming - Image failure: Design a recovery process.

# Overview
This GitHub repository contains a relatively simple Fortran 2008 coarray program that demonstrates the basic design of a recovery process after an image failure. Such a recovery process is described for Fortran 2018 in chapter 3.2 of the paper 'The new features of Fortran 2018' by John Reid, ISO/IEC JTC1/SC22/WG5 N2145 (https://isotc.iso.org/livelink/livelink?func=ll&objId=19441669&objAction=Open&viewType=1).<br />

Currently (February 2018) the implementations do not already offer full support for the Fortran 2018 newly added coarray-related features (image failure and teams). Thus, the example program here does adhere to Fortran 2008 syntax. The recovery solution of this example program is nothing I would recommend for real-world programming or production codes. Rather, it shows that the newly added coarray-related language features of the upcoming Fortran 2018 standard do perfectly match with our real-world requirements for PGAS / coarray programming.<br />
