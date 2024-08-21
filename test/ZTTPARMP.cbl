       PROCESS PGMN(LM),NODYNAM
       IDENTIFICATION DIVISION.
       PROGRAM-ID. 'ZTTPARMP' RECURSIVE.

      ******************************************************************
      * Broadcom Test4z Unit Test Suite (UT) example.                  *
      * Copyright (c) 2024 Broadcom. All Rights Reserved.              *
      *                                                                *
      * This test example shows how to get the user-defined parameter  *
      * located in the ZLOPTS file. These parameters can be defined    *
      * and used by the user in any way that is appropriate. In this   *
      * case, the example looks for two parameters:                    *
      *                                                                *
      * 1. PARM(TEST=ALL) meaning run all tests                        *
      * 2. PARM(TEST=MIN) meaning run the minimum required tests.      *
      *                                                                *
      * For example, using the Test4z CLI, enter this command          *
      * to run the minimal test:                                       *
      *                                                                *
      *     t4z test/ZTTPARMP --zlopts "PARM(TEST=MIN)"                *
      *                                                                *
      * To run the more thorough test, enter:                          *
      *                                                                *
      *     t4z test/ZTTPARMP --zlopts "PARM(TEST=ALL)"                *
      *                                                                *
      * If no PARM(...) in ZLOPTS is provided, TEST=MIN is assumed.    *
      *                                                                *
      * NB: "PROCESS PGMN(LM),NODYNAM" above is required for unit      *
      *     tests to enable long entry point names and locating entry  *
      *     points.                                                    *
      ******************************************************************

       DATA DIVISION.
       WORKING-STORAGE SECTION.

      ******************************************************************
      * Copy in the required control blocks from Test4z. The           *
      * WORKING-STORAGE control blocks start with ZWS_ and include     *
      * an "I" interface sub-block, e.g.:                              *
      *                                                                *
      * ZWS_GETPARM -> I_GETPARM IN ZWS_GETPARM                        *
      *                                                                *
      * The "I" interface sub-block should be initialized to           *
      * LOW-VALUES prior to calling ZTESTUT. For Test4z API control    *
      * blocks in the LINKAGE SECTION, look for the prefix ZLS_.       *
      ******************************************************************

       COPY ZTESTWS.

      ******************************************************************
      * Define any working-storage items that we need.                 *
      ******************************************************************
       01  WS-ZPARM-GET-MYOPTION.
           COPY ZPARM.

       01  WS-INPUT-LETTER        PIC X(1).
       01  WS-OUTPUT-ANIMAL-NAME  PIC X(10).
       01  WS-RUN-TEST-EVERYTHING PIC 9(2) VALUE 0.
       01  WS-RUN-TEST-MINIMUM    PIC 9(2) VALUE 0.
       01  WS-INPUT-LETTERS       PIC X(27) VALUE 
           'ABCDEFGHIJKLMNOPQRSTUVWXYZ?'.
       01  I                      PIC 9(2).

       01  WS-RUN-ZTDCALLD-ARGS.
           05 WS-INPUT-LETTER-ARG USAGE POINTER.
           05 WS-OUTPUT-ANIMAL-NAME-ARG USAGE POINTER.

       LINKAGE SECTION.

      ******************************************************************
      * Define any linkage section items that we need.                 *
      ******************************************************************
       01  LS-GET-PARM-MYOPTION PIC X(100).

       PROCEDURE DIVISION.

      ******************************************************************
      * Get the passed parameter data.                                 *
      ******************************************************************
           MOVE LOW-VALUES TO I_GETPARM IN ZWS_GETPARM
           CALL ZTESTUT USING ZWS_GETPARM, WS-ZPARM-GET-MYOPTION
           SET ADDRESS OF LS-GET-PARM-MYOPTION
                TO PTR IN WS-ZPARM-GET-MYOPTION

      ******************************************************************
      * Register a set of tests to be run based on the passed parm     *
      * data.                                                          *
      ******************************************************************
           IF SIZ IN WS-ZPARM-GET-MYOPTION > 5
                SET ADDRESS OF LS-GET-PARM-MYOPTION
                     TO PTR IN WS-ZPARM-GET-MYOPTION
                INSPECT LS-GET-PARM-MYOPTION
                          (1:SIZ IN WS-ZPARM-GET-MYOPTION)
                     TALLYING WS-RUN-TEST-EVERYTHING FOR ALL 'TEST=ALL'
                INSPECT LS-GET-PARM-MYOPTION
                          (1:SIZ IN WS-ZPARM-GET-MYOPTION)
                     TALLYING WS-RUN-TEST-MINIMUM FOR ALL 'TEST=MIN'
           END-IF

           IF WS-RUN-TEST-EVERYTHING = 0 AND WS-RUN-TEST-MINIMUM = 0
                MOVE LOW-VALUES TO I_MESSAGE IN ZWS_MESSAGE
                MOVE 'ZTTPARMP No parameters found, default to TEST=MIN'
                     TO MESSAGETEXT IN ZWS_MESSAGE
                CALL ZTESTUT USING ZWS_MESSAGE
           END-IF

      *-----------------------------------------------------------------
      * Technically, both parameters TEST=ALL and TEST=MIN can be
      * specified, but the former takes precedence.
      *-----------------------------------------------------------------
           IF WS-RUN-TEST-EVERYTHING > 0
              MOVE LOW-VALUES TO I_TEST
              SET TESTFUNCTION IN ZWS_TEST TO ENTRY 'tryHarderTest'
              MOVE 'Unit test 1: Better test of ZTPCALLD'
                   TO TESTNAME IN ZWS_TEST
              CALL ZTESTUT USING ZWS_TEST
           ELSE
              MOVE LOW-VALUES TO I_TEST
              SET TESTFUNCTION IN ZWS_TEST TO ENTRY 'trySniffTest'
              MOVE 'Unit test 2: Sniff test of ZTPCALLD'
                   TO TESTNAME IN ZWS_TEST
              CALL ZTESTUT USING ZWS_TEST
           END-IF

           GOBACK.

      ******************************************************************
      * UNIT TEST 1: A very short test of ZTPCALLD.                    *
      ******************************************************************
           ENTRY 'trySniffTest'.

           MOVE 'A' TO WS-INPUT-LETTER

      *-----------------------------------------------------------------
      * Test4z _RunFunction combines the _PrepareModule 
      * and _GetFunction API calls and executes the program directly.
      *
      * If parameters are required, as they are for ZTPCALLD,
      * they're provided as a block in the ARGUMENTLIST parameter.
      * These arguments are passed directly to the called program
      * without modification or dereferencing.
      *-----------------------------------------------------------------
           MOVE LOW-VALUES TO I_RUNFUNCTION
           MOVE 'ZTPCALLD' TO MODULENAME IN ZWS_RUNFUNCTION
           MOVE 'ZTPCALLD' TO FUNCTIONNAME IN ZWS_RUNFUNCTION

           SET WS-INPUT-LETTER-ARG IN WS-RUN-ZTDCALLD-ARGS
                TO ADDRESS OF WS-INPUT-LETTER
           SET WS-OUTPUT-ANIMAL-NAME-ARG IN WS-RUN-ZTDCALLD-ARGS
                TO ADDRESS OF WS-OUTPUT-ANIMAL-NAME
           SET ARGUMENTLIST IN ZWS_RUNFUNCTION
                TO ADDRESS OF WS-RUN-ZTDCALLD-ARGS

           CALL ZTESTUT USING ZWS_RUNFUNCTION

      *-----------------------------------------------------------------
      * Input was "A", did it return "AARDVARK"? If not, fail the test.
      *-----------------------------------------------------------------
           IF WS-OUTPUT-ANIMAL-NAME NOT = 'AARDVARK'
                MOVE LOW-VALUES TO I_FAIL IN ZWS_FAIL
                MOVE SPACES TO FAILMESSAGE IN ZWS_FAIL
                MOVE 'ZTTPARMP sniff test for ZTPCALLD'
                     TO FAILMESSAGE IN ZWS_FAIL

                DISPLAY FAILMESSAGE IN ZWS_FAIL
                DISPLAY 'Expected=AARDVARK for ' WS-INPUT-LETTER
                DISPLAY '     Got=' WS-OUTPUT-ANIMAL-NAME

                CALL ZTESTUT USING ZWS_FAIL
           ELSE
                DISPLAY 'ZTTPARMP PASS for sniff test'
           END-IF

           GOBACK.

      ******************************************************************
      * UNIT TEST 2: A more thorough test of ZTPCALLD.                 *
      ******************************************************************
           ENTRY 'tryHarderTest'.

           PERFORM VARYING I FROM 1 BY 1
                     UNTIL I > LENGTH OF WS-INPUT-LETTERS
                MOVE WS-INPUT-LETTERS(I:1) TO WS-INPUT-LETTER
                MOVE SPACES TO WS-OUTPUT-ANIMAL-NAME

      *-----------------------------------------------------------------
      * Test4z requires NODYNAM because the entry point names of the
      * unit test (i.e., this program) must be statically defined
      * so they are addressable by the runtime. However, the called 
      * called program's entry point (ZTPCALLD) can be invoked
      * dynamically by adding the compiler directive below.
      *-----------------------------------------------------------------
                >>CALLINTERFACE DYNAMIC
                CALL 'ZTPCALLD' USING 
                     WS-INPUT-LETTER, WS-OUTPUT-ANIMAL-NAME
                >>CALLINTERFACE STATIC

      *-----------------------------------------------------------------
      * Of course, this is a superficial test since it only tests
      * the first letter. It really should have a comparison table
      * of expected results, but this was omitted to keep this
      * example short(er)
      *-----------------------------------------------------------------
                IF WS-OUTPUT-ANIMAL-NAME(1:1) NOT = WS-INPUT-LETTER OR
                          WS-OUTPUT-ANIMAL-NAME(2:9) = SPACES
                     MOVE LOW-VALUES TO I_FAIL IN ZWS_FAIL
                     MOVE SPACES TO FAILMESSAGE IN ZWS_FAIL
                     MOVE 'ZTTPARMP everything test for ZTPCALLD'
                          TO FAILMESSAGE IN ZWS_FAIL
      
                     DISPLAY FAILMESSAGE IN ZWS_FAIL
                     DISPLAY 'Expected=' WS-INPUT-LETTER
                     DISPLAY '     Got=' WS-OUTPUT-ANIMAL-NAME(1:1)

                     CALL ZTESTUT USING ZWS_FAIL
                ELSE
                     DISPLAY 'ZTTPARMP PASS for everything test'
                END-IF

           END-PERFORM

           GOBACK.
