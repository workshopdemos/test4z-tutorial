       PROCESS PGMN(LM),NODYNAM
       IDENTIFICATION DIVISION.
       PROGRAM-ID. 'ZTTMOCKP' RECURSIVE.
       
      ******************************************************************
      * Broadcom Test4z Unit Test Suite (UT) example.                  *
      * Copyright (c) 2024 Broadcom. All Rights Reserved.              *
      *                                                                *
      * This test example shows how to run a unit test on a program    *
      * (ZTPPROGR) that calls another program (ZTPCALLD) that is to be *
      * stubbed out.                                                   *
      *                                                                *
      * Whereas the ZTTSTUBP example unit test provided a callback     *
      * usingthe _StubProgram API, this unit test uses a recording of  *
      * the program responses. That is, the argument data is replaced  *
      * using the previously recorded data captured in ZLDATA/ZLNEXT   *
      * as a JSON file.                                                *
      *                                                                *
      * Both unit tests, ZTTCALLD and ZTTMOCKP, produce the same       *
      * results. Which approach you choose-- programmatic or           *
      * recording --depends on the specifics of your test:             *
      *                                                                *
      * == If the goal is isolating the called program's results to    *
      *    predetermined responses, a recording may be better.         *
      *                                                                *
      * == If the goal is producing called program results that        *
      *    are not easily recorded or forcing "unhappy paths",         *
      *    stubbing may be better.                                     *
      *                                                                *
      * An aside, this unit test's recording was produced by ZTTCALLD  *
      * using the CLI option "--zlnext", demonstrating how one         *
      * approach can also feed the other.                              *
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
      * ZWS_MOCKPROGRAM -> I_MOCKPROGRAM IN ZWS_MOCKPROGRAM            *
      *                                                                *
      * The "I" interface sub-block should be initialized to           *
      * LOW-VALUES prior to calling ZTESTUT. For Test4z API control    *
      * blocks in the LINKAGE SECTION, look for the prefix ZLS_.       *
      ******************************************************************
       COPY ZTESTWS.

      ******************************************************************
      * Define any working-storage items that we need.                 *
      ******************************************************************
       01  WS-ZDATA-ZTPCALLD-RECORDING.
           COPY ZDATA.
       01  WS-ZPGRM-ZTPCALLD-MOCK.
           COPY ZPGRM.
       77  WS-RETURN-CODE-SUT PIC S9(4) USAGE BINARY.

       LINKAGE SECTION.

       PROCEDURE DIVISION.

      ******************************************************************
      * Register a unit test to run.                                   *
      ******************************************************************
           MOVE LOW-VALUES TO I_TEST
           SET TESTFUNCTION IN ZWS_TEST 
                TO ENTRY 'mockedAnimalProgramTest'
           MOVE 'Mocked called program ZTPCALLD'
                TO TESTNAME IN ZWS_TEST
           CALL ZTESTUT USING ZWS_TEST

      *-----------------------------------------------------------------
      * Once all of the tests have been registered, return back to
      * Test4z to start processing.
      *-----------------------------------------------------------------
           GOBACK.

      ******************************************************************
      * UNIT TEST: This test will execute a user program that calls    *
      *            another program. This called program is not to be   *
      *            executed but instead replaced by our previously     *
      *            recorded data. The argument comparison will occur   *
      *            when the subprogram is about to be called.          *
      ******************************************************************
           ENTRY 'mockedAnimalProgramTest'.

      *-----------------------------------------------------------------
      * Create a loaded data object for a test program. The data comes
      * from a previous recording and is stored in a partitioned
      * dataset member named 'ZTPPROGR'. This recorded data is in JSON
      * format. 
      *-----------------------------------------------------------------
           MOVE LOW-VALUES TO I_LOADDATA
           MOVE 'ZTPPROGR' TO MEMBERNAME IN ZWS_LOADDATA
           CALL ZTESTUT USING ZWS_LOADDATA, 
                LOADOBJECT IN WS-ZDATA-ZTPCALLD-RECORDING

      *-----------------------------------------------------------------
      * Define that program ZTPCALLD that is to be prevented from being
      * called in the original load module, but instead to be mocked
      * with recorded data.
      *-----------------------------------------------------------------
           MOVE LOW-VALUES TO I_MOCKPROGRAM
           MOVE 'ZTPPROGR' TO MODULENAME IN ZWS_MOCKPROGRAM
           MOVE 'ZTPCALLD' TO FUNCTIONNAME IN ZWS_MOCKPROGRAM
           SET LOADOBJECT IN ZWS_MOCKPROGRAM 
                TO LOADOBJECT IN WS-ZDATA-ZTPCALLD-RECORDING
           CALL ZTESTUT USING ZWS_MOCKPROGRAM,
                PROGRAMOBJECT IN WS-ZPGRM-ZTPCALLD-MOCK

      *-----------------------------------------------------------------
      * Load and prepare the user application program ZTPPROGR for
      * use. Get the function address and call the function. This can
      * be done all in one call to _RunFunction or as individual calls
      * to _PrepareModule, _GetFunction and then call the function.
      *-----------------------------------------------------------------
           MOVE LOW-VALUES TO I_RUNFUNCTION
           MOVE 'ZTPPROGR' TO MODULENAME IN ZWS_RUNFUNCTION
           MOVE 'ZTPPROGR' TO FUNCTIONNAME IN ZWS_RUNFUNCTION
           CALL ZTESTUT USING ZWS_RUNFUNCTION

      *-----------------------------------------------------------------
      * The SUT has ended.
      *
      * Assert the expected return code (RC=4 means invalid data found,
      * which ZTPPROGR intentionally provides as a test).
      *-----------------------------------------------------------------
           MOVE RETURN-CODE TO WS-RETURN-CODE-SUT

           IF WS-RETURN-CODE-SUT NOT = 4
                DISPLAY 'ZTTMOCK expected RC=4, got RC='
                     WS-RETURN-CODE-SUT

                MOVE LOW-VALUES TO I_FAIL IN ZWS_FAIL
                MOVE 'ZTTMOCKP unexpected return code'
                     TO FAILMESSAGE IN ZWS_FAIL
                CALL ZTESTUT USING ZWS_FAIL
           END-IF

           GOBACK.
           