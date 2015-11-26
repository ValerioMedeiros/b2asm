# Introduction #

For the moment being, the source code is composed of B projects that have been produced using the B4free toolkit through the Click'n'Prove interface.

# Details #

The source repository is composed of the usual three subdirectories recommended in the SVN guidelines (branches, tags and trunk) plus the wiki pages. For the moment being, only the trunk and wiki directories are populated.

The trunk directory contains the following subdirectories:
  * The directory **developments** contains B4free projects that are being developed to investigate the b2asm approach. This directory is divided into two sub-directories:
    1. The directory **platforms** contains B4free projects where computational platforms (i.e. microcontrollers) have been modelled in B.
    1. The directory **software** contains B4free projects of software development targeting one of the modelled platforms.
  * The directory **papers** contains B4free projects that have been used to write research papers on the b2asm approach. So far, there are two such projects. One is _UFRN-DIMAp-2008-101-RT_ and contains a B model of the Random Access Machine, or RAM, (a computation model similar to that of micro-processors and micro-controllers), as well as four small examples that illustrate how the different programming constructs may be mapped to instructions of the RAM. This project was important to come up with a general pattern for assembly level models. The second project is _semish2008_, where the ideas of the former project are used to develop an assembly implementation of a simple reactive system (a traffic light) on an existing micro-controller platform, i.e. Microchip's PIC16C432.