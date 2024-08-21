       IDENTIFICATION DIVISION.
       PROGRAM-ID. ZTPCALLD.

      ******************************************************************
      * Broadcom Test4z System Under Test (SUT) example.               *
      * Copyright (c) 2024 Broadcom. All Rights Reserved.              *
      *                                                                *
      * This program, ZTPCALLD, is called by other programs, e.g.,     *
      * ZTTPROGR. ZTPCALLD simply maps the input (single letter A-Z)   *
      * into an output (corresponding animal name).                    *
      *                                                                *
      * This program is used to demonstrate several unit test          *
      * implementations:                                               *
      *                                                                *
      * == Unit test suite ZTTSTUBP uses the _StubProgram              *
      *    API to intercept these calls, log parameters, and           *
      *    substitute its own results as part of unit testing.         *
      *                                                                *
      * == Unit test suite ZTTMOCKP uses the _MockProgram API to       *
      *    simulate invocations of this program.                       *
      *                                                                *
      * == Unit test suite ZTTSPYPG uses the _SPYPROGRAM API to        *
      *    monitor invocations of this program.                        *
      *                                                                *
      * == Unit test ZTTPARMP uses the _GetParm API to control the     *
      *    unit tests run against this program.                        *
      ******************************************************************

       DATA DIVISION.
       WORKING-STORAGE SECTION.

       01  WS-ALPHA-MAP.
           05 FILLER PIC X(10) VALUE 'AARDVARK'.
           05 FILLER PIC X(10) VALUE 'BABOON'.
           05 FILLER PIC X(10) VALUE 'CAMEL'.
           05 FILLER PIC X(10) VALUE 'DEER'.
           05 FILLER PIC X(10) VALUE 'EAGLE'.
           05 FILLER PIC X(10) VALUE 'FALCON'.
           05 FILLER PIC X(10) VALUE 'GAZELLE'.
           05 FILLER PIC X(10) VALUE 'HAMSTER'.
           05 FILLER PIC X(10) VALUE 'IGUANA'.
           05 FILLER PIC X(10) VALUE 'JACKAL'.
           05 FILLER PIC X(10) VALUE 'KANGAROO'.
           05 FILLER PIC X(10) VALUE 'LEMUR'.
           05 FILLER PIC X(10) VALUE 'MACAW'.
           05 FILLER PIC X(10) VALUE 'NEWT'.
           05 FILLER PIC X(10) VALUE 'OCTOPUS'.
           05 FILLER PIC X(10) VALUE 'PANTHER'.
           05 FILLER PIC X(10) VALUE 'QUAIL'.
           05 FILLER PIC X(10) VALUE 'RABBIT'.
           05 FILLER PIC X(10) VALUE 'SCORPION'.
           05 FILLER PIC X(10) VALUE 'TIGER'.
           05 FILLER PIC X(10) VALUE 'URCHIN'.
           05 FILLER PIC X(10) VALUE 'VOLE'.
           05 FILLER PIC X(10) VALUE 'WALRUS'.
           05 FILLER PIC X(10) VALUE 'XENOPS'.
           05 FILLER PIC X(10) VALUE 'YAK'.
           05 FILLER PIC X(10) VALUE 'ZEBRA'.

       01  WS-ALPHA-MAP-REDEF REDEFINES WS-ALPHA-MAP.
           05 WS-ALPHA-RECORD OCCURS 26 TIMES.
               10 FILLER PIC X(10).

       LINKAGE SECTION.

       01  LS-INPUT-LETTER       PIC X(1).
       01  LS-OUTPUT-ANIMAL-NAME PIC X(10).

       PROCEDURE DIVISION USING LS-INPUT-LETTER, LS-OUTPUT-ANIMAL-NAME.

           MOVE ALL '?' TO LS-OUTPUT-ANIMAL-NAME

           PERFORM VARYING TALLY FROM 1 BY 1 UNTIL TALLY > 26
               IF LS-INPUT-LETTER = WS-ALPHA-RECORD(TALLY)(1:1)
                   MOVE WS-ALPHA-RECORD(TALLY) TO LS-OUTPUT-ANIMAL-NAME
                   GOBACK
               END-IF
           END-PERFORM

           GOBACK.
