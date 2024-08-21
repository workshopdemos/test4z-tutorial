       PROCESS PGMN(LM),NODYNAM
       IDENTIFICATION DIVISION.
       PROGRAM-ID. 'ZTTSECTN' RECURSIVE.

      ******************************************************************
      * Broadcom Test4z Unit Test Suite (UT) example.                  *
      * Copyright (c) 2024 Broadcom. All Rights Reserved.              *
      *                                                                *
      * This unit test example shows how to:                           *
      *                                                                *
      * == Run a unit test that isolates a single section from the     *
      *    rest of the user program and only executes it.              *
      *                                                                *
      * == Establish a section "spy" that can be notified when the     *
      *    given section is PERFORMed.                                 *
      *                                                                *
      * == Retrieve system-under-test (SUT) variables for display      *
      *    or validation during a spy callback.                        *
      *                                                                *
      * == Assert that a required section/pargraph was indeed executed *
      *    the expected number of times.                               *
      *                                                                *
      * IMPORTANT: Many COBOL programs assume specific section(s) have *
      * been performed to establish the expected working storage       *
      * variable state. By accessing and running an SUT's section      *
      * directly in a unit test, care should be exercised to confirm   *
      * the appropriate program state has been established, otherwise  *
      * unexpected behavior including abends may occur.                *
      *                                                                *
      * NB: "PROCESS PGMN(LM),NODYNAM" above is required for unit      *
      *     test to enable long entry point names and locating entry   *
      *     points.                                                    *
      ******************************************************************

       DATA DIVISION.
       WORKING-STORAGE SECTION.

      ******************************************************************
      * Copy in the required control blocks from Test4z. The           *
      * WORKING-STORAGE control blocks start with ZWS_ and include     *
      * an "I" interface sub-block, e.g.:                              *
      *                                                                *
      * ZWS_SPYSECTION -> I_SPYSECTION IN ZWS_SPYSECTION               *
      *                                                                *
      * The "I" interface sub-block should be initialized to           *
      * LOW-VALUES prior to calling ZTESTUT. For Test4z API control    *
      * blocks in the LINKAGE SECTION, look for the prefix ZLS_.       *
      ******************************************************************
           COPY ZTESTWS.

      ******************************************************************
      * Define any working-storage items that we need.                 *
      ******************************************************************
       01  WS-FUNCTION-INIT-WATCH   USAGE FUNCTION-POINTER.
       01  WS-RUN-PROGRAM           USAGE FUNCTION-POINTER.

       01  WS-DATE-FROM-SUT.
           05 SUT-YEAR        PIC 9(4).
           05 SUT-MONTH       PIC 9(2).
           05 SUT-DAY         PIC 9(2).
           05 SUT-HOUR        PIC 9(2).
           05 SUT-MINUTE      PIC 9(2).
           05 FILLER          PIC X(9).

       01  WS-ZSPSECT-INIT-DATE-SPY.
           COPY ZSPSECT.

       01  WS-ZSPSECT-INCR-COUNTER-SPY.
           COPY ZSPSECT.

       LINKAGE SECTION.

      ******************************************************************
      * Copy in our required control blocks from Test4z.               *
      ******************************************************************
       COPY ZTESTLS.

      ******************************************************************
      * Define any linkage section items that we need.                 *
      ******************************************************************
       01  LS-CURRENT-DATE PIC X(21).

       PROCEDURE DIVISION.

      ******************************************************************
      * Register the tests to be run once the program is prepared.     *
      ******************************************************************
           MOVE LOW-VALUES TO I_TEST
           SET TESTFUNCTION IN ZWS_TEST
                TO ENTRY 'sectionExecutionIsolationTest'
           MOVE 'Unit test 1: Isolated section execution test'
                TO TESTNAME IN ZWS_TEST
           CALL ZTESTUT USING ZWS_TEST

           MOVE LOW-VALUES TO I_TEST
           SET TESTFUNCTION IN ZWS_TEST
                TO ENTRY 'sectionSpyTest'
           MOVE 'Unit test 2: Section spy validating invocation count'
                TO TESTNAME IN ZWS_TEST
           CALL ZTESTUT USING ZWS_TEST

           GOBACK.

      ******************************************************************
      * UNIT TEST 1: This test will only execute a specific section    *
      *              in the ZTPWATCH program and include a callback.   *
      ******************************************************************
           ENTRY 'sectionExecutionIsolationTest'.

      *-----------------------------------------------------------------
      * Load and prepare the application program ZTPWATCH for use.
      *-----------------------------------------------------------------
           MOVE LOW-VALUES TO I_PREPAREMODULE
           MOVE 'ZTPWATCH' TO MODULENAME IN ZWS_PREPAREMODULE
           CALL ZTESTUT USING ZWS_PREPAREMODULE

      *-----------------------------------------------------------------
      * The ZTPWATCH program includes paragraph 100-INIT-WATCH
      * to establish the working storage state, e.g., retrieve the
      * current date. This unit test will run ZTPWATCH's
      * initialization directly and confirm the date was also
      * initialized by its corresponding section, 110-INIT-DATE.
      *
      * The Test4z _PrepareSection API helps with two test patterns:
      *
      * 1. Confirming that prerequisite code is executed as expected.
      *
      * 2. Testing and validation of subroutine-like paragraph
      *    that accept input/output working storage variables.
      *
      * With the combination of _PrepareSection/_GetVariable APIs, the
      * unit test can validate inputs and confirm outputs for (2). For
      * pattern (1) as in this example code, the _PrepareSection and
      * _AssertCalledSection APIs are used.
      *
      * NB: The invocation of the section via _PrepareSection
      *     can only be done once, i.e., once
      *     "CALL WS-FUNCTION-INIT-WATCH" returns, the SUT has ended.
      *-----------------------------------------------------------------
           MOVE LOW-VALUES TO I_PREPARESECTION
           MOVE 'ZTPWATCH' TO MODULENAME IN ZWS_PREPARESECTION
           MOVE 'ZTPWATCH' TO FUNCTIONNAME IN ZWS_PREPARESECTION
           MOVE '100-INIT-WATCH'
                TO SECTIONNAME IN ZWS_PREPARESECTION
           CALL ZTESTUT USING ZWS_PREPARESECTION, 
                WS-FUNCTION-INIT-WATCH

      *-----------------------------------------------------------------
      * Spy on the COBOL section in the user program - we want to
      * confirm that the overall initialization in ZTPWATCH is
      * indeed initializing the current date in its working storage
      * via a prerequisite section of code, 110-INIT-DATE.
      *
      * NB: The callback below is optional. It can be used to check
      *     more details, e.g., how many times it was called, what
      *     was the program state of the SUT at the time, etc.
      *-----------------------------------------------------------------
           MOVE LOW-VALUES TO I_SPYSECTION
           MOVE 'ZTPWATCH' TO MODULENAME IN ZWS_SPYSECTION
           MOVE 'ZTPWATCH' TO FUNCTIONNAME IN ZWS_SPYSECTION
           MOVE '110-INIT-DATE'
                TO SECTIONNAME IN ZWS_SPYSECTION
           SET CALLBACK IN ZWS_SPYSECTION
                TO ENTRY 'sectionSpyWatchDate'
           CALL ZTESTUT USING ZWS_SPYSECTION,
                SECTIONSPYOBJECT IN WS-ZSPSECT-INIT-DATE-SPY

      *-----------------------------------------------------------------
      * Run ZTPWATCH's initialization.
      *-----------------------------------------------------------------
           CALL WS-FUNCTION-INIT-WATCH

      *-----------------------------------------------------------------
      * Did 110-INITIALIZE-DATE actually run? If not, fail the test.
      *-----------------------------------------------------------------
           MOVE LOW-VALUES TO I_ASSERTCALLEDSECTION
           SET SPYOBJECT IN ZWS_ASSERTCALLEDSECTION TO
               ADDRESS OF SECTIONSPYOBJECT IN WS-ZSPSECT-INIT-DATE-SPY
           CALL ZTESTUT USING ZWS_ASSERTCALLEDSECTION

      *-----------------------------------------------------------------
      * Did 110-INITIALIZE-DATE run more than once? It's not
      * supposed to! If it did run multiple times, fail the test.
      *-----------------------------------------------------------------
           IF EXECUTIONCOUNT IN WS-ZSPSECT-INIT-DATE-SPY NOT = 1
                MOVE LOW-VALUES TO I_FAIL IN ZWS_FAIL
                MOVE SPACES TO FAILMESSAGE IN ZWS_FAIL
                STRING 'ZTTSECTN unexpected # invocations of '
                     SECTIONNAME IN WS-ZSPSECT-INIT-DATE-SPY
                     DELIMITED BY SIZE
                     INTO FAILMESSAGE IN ZWS_FAIL

                DISPLAY FAILMESSAGE IN ZWS_FAIL
                DISPLAY 'Expected=1'
                DISPLAY '     Got=' 
                     EXECUTIONCOUNT IN WS-ZSPSECT-INIT-DATE-SPY
                CALL ZTESTUT USING ZWS_FAIL
           ELSE
                DISPLAY 'ZTTSECTN PASS: paragraph 110-INIT-DATE'
                     ' in ZTPWATCH was called only once'
           END-IF

           GOBACK.

      ******************************************************************
      * UNIT TEST 2: This test will spy on a section in ZTPWATCH       *
      *              without a callback.                               *
      ******************************************************************
           ENTRY 'sectionSpyTest'.

      *-----------------------------------------------------------------
      * Load and prepare the application program ZTPWATCH for use.
      *-----------------------------------------------------------------
           MOVE LOW-VALUES TO I_PREPAREMODULE
           MOVE 'ZTPWATCH' TO MODULENAME IN ZWS_PREPAREMODULE
           CALL ZTESTUT USING ZWS_PREPAREMODULE

      *-----------------------------------------------------------------
      * Spy on the COBOL section in the user program - we want to
      * confirm that paragraph 210-INCREMENT-COUNTER in ZTPWATCH
      * is called the correct number of times.
      *-----------------------------------------------------------------
           MOVE LOW-VALUES TO I_SPYSECTION
           MOVE 'ZTPWATCH' TO MODULENAME IN ZWS_SPYSECTION
           MOVE 'ZTPWATCH' TO FUNCTIONNAME IN ZWS_SPYSECTION
           MOVE '210-INCREMENT-COUNTER'
                TO SECTIONNAME IN ZWS_SPYSECTION
           CALL ZTESTUT USING ZWS_SPYSECTION,
                SECTIONSPYOBJECT IN WS-ZSPSECT-INCR-COUNTER-SPY

      *-----------------------------------------------------------------
      * Get the entry point of the ZTPWATCH routine in load module
      * ZTPWATCH and run it.
      *-----------------------------------------------------------------
           MOVE LOW-VALUES TO I_GETFUNCTION
           MOVE 'ZTPWATCH' TO MODULENAME IN ZWS_GETFUNCTION
           MOVE 'ZTPWATCH' TO FUNCTIONNAME IN ZWS_GETFUNCTION
           CALL ZTESTUT USING ZWS_GETFUNCTION, WS-RUN-PROGRAM

           CALL WS-RUN-PROGRAM

      *-----------------------------------------------------------------
      * The SUT has ended.
      *
      * Verify the section 210-INCREMENT-COUNTER was call the expected
      * number of times for this testcase
      *-----------------------------------------------------------------

           IF EXECUTIONCOUNT IN WS-ZSPSECT-INCR-COUNTER-SPY NOT = 5
                MOVE LOW-VALUES TO I_FAIL IN ZWS_FAIL
                MOVE SPACES TO FAILMESSAGE IN ZWS_FAIL
                STRING 'ZTTSECTN unexpected # invocations of '
                     SECTIONNAME IN WS-ZSPSECT-INCR-COUNTER-SPY
                     DELIMITED BY SIZE
                     INTO FAILMESSAGE IN ZWS_FAIL

                DISPLAY FAILMESSAGE IN ZWS_FAIL
                DISPLAY 'Expected=5'
                DISPLAY '     Got=' 
                     EXECUTIONCOUNT IN WS-ZSPSECT-INCR-COUNTER-SPY
                CALL ZTESTUT USING ZWS_FAIL
           ELSE
                DISPLAY 'ZTTSECTN PASS: paragraph 210-INCREMENT-COUNTER'
                     ' in ZTPWATCH was called multiple times'
           END-IF

           GOBACK.

      ******************************************************************
      * CALLBACK: Provide the callback routine entry point for         *
      *           the COBOL section.                                   *
      ******************************************************************
           ENTRY 'sectionSpyWatchDate' USING ZLS_SECTION_CALL.

           DISPLAY 'ZTTSECTN section='
                SECTIONNAME IN ZLS_SECTION_CALL ' called '
                EXECUTIONCOUNT IN ZLS_SECTION_CALL ' times'

      *-----------------------------------------------------------------
      * IMPORTANT: The callback is invoked _after_ the section is
      *            executed. Be sure your validations of expected
      *            state take this into account.
      *-----------------------------------------------------------------
           MOVE LOW-VALUES TO I_GETVARIABLE
           MOVE 'WS-CURRENT-DATE' 
                TO VARIABLENAME IN ZWS_GETVARIABLE
           CALL ZTESTUT USING ZWS_GETVARIABLE, 
                ADDRESS OF LS-CURRENT-DATE

           MOVE LS-CURRENT-DATE TO WS-DATE-FROM-SUT
           DISPLAY 'ZTTSECTN variable WS-CURRENT-DATE from ZTPWATCH='
               SUT-YEAR '-' SUT-MONTH '-' SUT-DAY ' at '
               SUT-HOUR ':' SUT-MINUTE

           GOBACK.
           