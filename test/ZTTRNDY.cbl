       PROCESS PGMN(LM),NODYNAM
       IDENTIFICATION DIVISION.
       PROGRAM-ID. 'ZTTRNDY' RECURSIVE.

      ******************************************************************
      * Broadcom Test4z Unit Test Suite (UT) example.                  *
      * Copyright (c) 2024 Broadcom. All Rights Reserved.              *
      *                                                                *
      * TRENDY was inspired by a simplified performance utility used   *
      * to measure program performance. It monitors the health of      *
      * systems in various regions over extended periods, up to 30     *
      * days, as well as moment-to-moment.                             *
      *                                                                *
      * This unit test suite is a Test4z validation of the             *
      * proper operation of TRENDY. That is, it uses relatively        *
      * small amounts of input data (200-500 records) and simple       *
      * validation of the output, such as confirming TRENDY's status   *
      * report totals match the expected values.                       *
      *                                                                *
      * Below is the summary of the Test4z APIs demonstrated by        *
      * this example unit test suite.                                  *
      *                                                                *
      * 1). APIs for starting a unit test:                             *
      *                                                                *
      * == _Test - register a unit test                                *
      * == _PrepareModule - prepare a SUT for execution                *
      * == _GetFunction - retrieve a SUT execution entry point         *
      *                                                                *
      * 2). APIs for setting up input and output datasets:
      *                                                                *
      * == _AddRecord - add records to previously created mock file    *
      * == _MockKSDS - create mock (UT created) KSDS file              *
      * == _MockQSAM - create mock (UT created) QSAM file              *
      *                                                                *
      *    NB: As part of validation, TRENDY's output QSAM mocks are   *
      *        queried to confirm the actual and expected results      *
      *        match. That is, the written records are captured by the *
      *        mock and are available for later interrogation.         *
      *                                                                *
      * 3). APIs for retrieving previously recorded data:              *
      *                                                                *
      * == _CreateHarness - load recording and create associated mocks *
      * == _GetFileObject - retrieve mocked file from test harness     *
      * == _LoadData - load recording for passing to mocked file       *
      *                                                                *
      * 4). APIs for interrogating SUT execution:                      *
      *                                                                *
      * == _GetVariable - retrieve SUT variable for modification       *
      * == _SetBreakpoint - register callback on section/paragraph     *
      * == _SpyKSDS - track KSDS file operations                       *
      * == _SpyQSAM - track QSAM file operations                       *
      * == _SpySection - track section execution w/optional callback   *
      * == _SpyVariable - track SUT variable value for validation      *
      *                                                                *
      * 5). APIs for affecting runtime state changes:                  *
      *                                                                *
      * == _StubProgram - emulate a called program with a UT callback  *
      * == _WatchVariable - observe and optionally change variables    *
      *                                                                *
      * 6). APIs for reporting the unit test results:                  *
      *                                                                *
      * == _AssertCalledOnceSection - fails if section wasn't executed *
      * == _AssertNoErrorKSDS - fails unit test if error was detected  *
      * == _AssertNoErrorQSAM - fails unit test if error was detected  *
      * == _Fail - unconditionally ends the unit test                  *
      *                                                                *
      * 7). APIs for helpful messages and debug:                       *
      *                                                                *
      * == _Message - add messsage to ZLMSG UT log                     *
      * == _PrintFile - output file content via DISPLAY                *
      *                                                                *
      * NB: "PROCESS PGMN(LM),NODYNAM" above is required for unit      *
      *     tests to enable long entry point names and locating entry  *
      *     points.                                                    *
      *                                                                *
      *     For the complete list of Test4z APIs and helpful           *
      *     reminders when writing a unit test suite, see the Test4z   *
      *     COBOL API document (PDF). The APIs are also documented     *
      *     in the copybook ZTESTWSn.cpy where "n" is the document     *
      *     revision number (1, 2, 3...).                              *
      ******************************************************************

       DATA DIVISION.
       WORKING-STORAGE SECTION.

      ******************************************************************
      * Copy in the required control blocks from Test4z. The           *
      * WORKING-STORAGE control blocks start with ZWS_ and include     *
      * an "I" interface sub-block, e.g.:                              *
      *                                                                *
      * ZWS_MESSAGE -> I_MESSAGE IN ZWS_MESSAGE                        *
      *                                                                *
      * The "I" interface sub-block should be initialized to           *
      * LOW-VALUES prior to calling ZTESTUT. For Test4z API control    *
      * blocks in the LINKAGE SECTION, look for the prefix ZLS_.       *
      ******************************************************************

       COPY ZTESTWS.

      ******************************************************************
      * For those APIs with return values, the copybook block contains *
      * an output field (e.g., ZDATA -> LOADOBJECT). The copybooks     *
      * only define one control block and they start at the 03         *
      * level, so you can define your own 01 level variable.           *
      ******************************************************************

       01  WS-ZHRNESS-DATA.
           COPY ZHRNESS.

       01  WS-ZHRNOPT-OPTIONS.
           02 HO-HARNESS-OPTION OCCURS 2 TIMES.
           COPY ZHRNOPT.

       01  WS-ZKSDS-HEALTH-LOG.
           COPY ZKSDS.

       01  WS-ZDATA-LOADDATA.
           COPY ZDATA.

       01  WS-ZQSAM-EXEC-LOG.
           COPY ZQSAM.

       01  WS-ZQSAM-HEALTH-TREND.
           COPY ZQSAM.

       01  WS-ZQSAM-STATS-REPORT.
           COPY ZQSAM.

       01  WS-ZSPVAR-STATS-TOTALS.
           COPY ZSPVAR.

       01  WS-ZSPQSAM-EXEC-LOG-SPY.
           COPY ZSPQSAM.

       01  WS-ZSPKSDS-HEALTH-LOG-SPY.
           COPY ZSPKSDS.

       01  WS-ZSPQSAM-HEALTH-TREND-SPY.
           COPY ZSPQSAM.

       01  WS-ZSPQSAM-REPORT-SPY.
           COPY ZSPQSAM.

       01  WS-ZSPQSAM-REPORT-ERROR-SPY.
           COPY ZSPQSAM.

       01  WS-ZSPSECT-END-PROGRAM-SPY.
           COPY ZSPSECT.

       01  WS-ZPARM-GET-MYOPTION.
           COPY ZPARM.

      *-----------------------------------------------------------------
      * See Test4z _GETFUNCTION for details.
      *-----------------------------------------------------------------
       01  WS-RUN-PROGRAM USAGE FUNCTION-POINTER.

      ******************************************************************
      * Copy in TRENDY's records.                                      *
      ******************************************************************
           COPY ZTPTRREC REPLACING
                   ==:ELR:== BY ==ELR==
                   ==:HLR:== BY ==HLR==
                   ==:HTT:== BY ==HTT==
                   ==:HTR:== BY ==HTR==
                   ==:RPT:== BY ==RPT==.

      ******************************************************************
      * TRENDY control variables.                                      *
      ******************************************************************
       77  WS-DEBUG-MODE                  PIC 9 VALUE 0.
           88 WS-DEBUG-ON                 VALUE 1.
           88 WS-DEBUG-OFF                VALUE 0.
           88 WS-DEBUG-MSG-ON-FAIL        VALUE 2.
       77  WS-FAIL-FLAG                   PIC 9 VALUE 1.
           88 WS-STOP-ON-FAIL             VALUE 1.
           88 WS-CONTINUE-ON-FAIL         VALUE 0.
       77  WS-FORCE-FILE-ERROR-FLAG       PIC 9 VALUE 0.
           88 WS-FORCE-STATS-REPORT-ERROR VALUE 1.
           88 WS-NO-FORCED-FILE-ERRORS    VALUE 0.
       77  WS-RETURN-CODE-SUT             PIC S9(4) USAGE BINARY.
       77  WS-HEALTH-RATING-CALLBACK-CNT  PIC 9(6) VALUE 0.
       77  WS-FAIL-MESSAGE                PIC X(80).
       77  I                              PIC 9(6) VALUE 1.
       77  WS-PARM-SEARCH                 PIC 9(2).

      *-----------------------------------------------------------------
      * Use a fixed date for filling in mocked data records having
      * dates (the hours/minutes/seconds are not used). That way,
      * we can compare expected and actual records precisely.
      *-----------------------------------------------------------------
       01  WS-TEST-DATE.
           05 TD-YEAR       PIC 9(4) VALUE 2024.
           05 TD-MONTH      PIC 9(2) VALUE 05.
           05 TD-DAY        PIC 9(2) VALUE 26.

      *-----------------------------------------------------------------
      * Validation record for RPT-STATS-REPORT-TOTALS that is
      * confirmed via a Spy Variable. See these paragraphs for
      * details:
      *
      * 1. 420-VERIFY-SPY-VARIABLE-STATS
      * 2. 430-VERIFY-STATS-GETVARIABLE
      *
      * These demonstrate two different ways of validating SUT
      * variable results.
      *-----------------------------------------------------------------
       01  CHECK-STATS-REPORT-TOTALS.
           05 CS-START-INTERVAL            PIC X(16).
           05 CS-END-INTERVAL              PIC X(16).
           05 CS-DATA.
                10 CS-VALID-EXEC-LOGS      PIC 9(6).
                10 CS-HEALTH-LOG-UPDATES   PIC 9(6).
                10 CS-HEALTH-LOG-ADDITIONS PIC 9(6).
                10 CS-HEALTH-TREND-COUNT   PIC 9(6).
                10 CS-ERROR-EXEC-LOGS      PIC 9(6).
                10 CS-INVALID-EXEC-LOGS    PIC 9(6).
       01  CS-MESSAGE-TEXT                 PIC X(80).

       77  CS-EXPECTED-RETURN-CODE         PIC S9(4) COMP-4.

      *-----------------------------------------------------------------
      * Working variable for keeping track of changed field
      * RPT-INVALID-EXEC-LOGS within the RPT-STATS-REPORT-TOTALS
      * record. See the callback 'watchStatsReportTotals' for details.
      *-----------------------------------------------------------------
       01  WS-INVALID-EXEC-LOGS-LAST       PIC 9(6) VALUE 0.

      *-----------------------------------------------------------------
      * Validation record for HTR-HEALTH-TREND-RECORDS for each
      * unit test that is confirmed via the QSAM mock. See
      * paragraph 440-VERIFY-HEALTH-TREND-COMP for details.
      *-----------------------------------------------------------------
       01  CHECK-HRT-GENERATED.
           05 CHT-HEALTH-TREND-RECORD PIC X(36)
                VALUE '0000-00-00 01:00 04 05 09 000059 060'.
           05 CHT-HEALTH-TREND-RECORD PIC X(36)
                VALUE '0000-00-00 02:00 04 05 09 000059 060'.
           05 CHT-HEALTH-TREND-RECORD PIC X(36)
                VALUE '0000-00-00 03:00 04 05 09 000059 060'.

       01  CHECK-HRT-GENERATED-STUB.
           05 CHT-HEALTH-TREND-RECORD PIC X(36)
                VALUE '0000-00-00 01:00 05 05 05 000059 060'.
           05 CHT-HEALTH-TREND-RECORD PIC X(36)
                VALUE '0000-00-00 02:00 03 03 03 000059 060'.
           05 CHT-HEALTH-TREND-RECORD PIC X(36)
                VALUE '0000-00-00 03:00 01 01 01 000059 060'.

       01  CHECK-HRT-COMPARE-RECORDS.
           05  CHECK-HRT-COMPARE-RECORD OCCURS 3 TIMES.
                10 CHT-YEAR   PIC 9(4).
                10 FILLER     PIC X.
                10 CHT-MONTH  PIC 9(2).
                10 FILLER     PIC X.
                10 CHT-DAY    PIC 9(2).
                10 FILLER     PIC X.
                10 CHT-RECORD PIC X(25).

       77  CHECK-HRT-COMPARE-CNT      PIC 9 VALUE 3.
       77  CHECK-HRT-GENERATED-FLAG   PIC 9 VALUE 0.
           88 RUNNING-UNIT-TEST-1     VALUE 0.
           88 CALLING-UNIT-TEST-1     VALUE 1.

      *-----------------------------------------------------------------
      * The recorded data is used for several APIs (_LoadData,
      * _CreateHarness, and _CreateHarness with options). They
      * should all produce the same output.
      *-----------------------------------------------------------------
       01  CHECK-HRT-RECORDED.
           05 CHT-HEALTH-TREND-RECORD PIC X(36)
                VALUE '2024-05-26 10:00 06 06 08 000093 060'.
           05 CHT-HEALTH-TREND-RECORD PIC X(36)
                VALUE '2024-05-26 11:00 06 03 10 000098 060'.
           05 CHT-HEALTH-TREND-RECORD PIC X(36)
                VALUE '2024-05-26 12:00 06 04 10 000101 060'.
           05 CHT-HEALTH-TREND-RECORD PIC X(36)
                VALUE '2024-05-26 13:00 07 05 10 000113 060'.

       01  CHECK-HTR-REDEF REDEFINES CHECK-HRT-RECORDED.
           05 CHECK-HRT-RECORDED-RECORD OCCURS 4 TIMES.
                10 CHT-RECORD PIC X(36).

       77  CHECK-HRT-RECORDED-CNT PIC 9 VALUE 1.

      *-----------------------------------------------------------------
      * This is a sanity check of the number of expected writes
      * to the HEALTH-LOG for a given unit test. See the callback
      * 'spyHealthTrendCallback' for details.
      *-----------------------------------------------------------------
       77  CHECK-SPY-TREND-HEALTH-WRITE PIC 9(6).
       77  CHECK-SPY-TREND-HEALTH-REWRITE PIC 9(6).

       LINKAGE SECTION.

      ******************************************************************
      * Copy in our required control blocks from Test4z.               *
      ******************************************************************
       COPY ZTESTLS.

      *-----------------------------------------------------------------
      * Callback parameter for KSDS spy.
      *-----------------------------------------------------------------
       01  LS-ZSPKSDS-HEALTH-LOG-CALLBACK.
           COPY ZSPKSDS.

       01  LS-ZSPQSAM-REPORT-CALLBACK.
           COPY ZSPQSAM.

       01  LS-ZQSAM-HEALTH-TREND-FILE.
           COPY ZQSAM.

      *-----------------------------------------------------------------
      * See forceIOErrorSpyCallback for details.
      *-----------------------------------------------------------------
       01  LS-IFBLOCK-STATUSCODE PIC X(2).
       01  LS-IFBLOCK-CONDCODE   PIC X.

      *-----------------------------------------------------------------
      * See _GetParm invocation used to control "UNIT TEST 5".
      *-----------------------------------------------------------------
       01  LS-GET-MYOPTION PIC X(100).

      *-----------------------------------------------------------------
      * Pointer to actual file I/O status code during spied callback.
      *-----------------------------------------------------------------
       01  LS-SPY-STATUS-CODE PIC X(2).

      *-----------------------------------------------------------------
      * TRENDY's records as pointers.
      *-----------------------------------------------------------------
           COPY ZTPTRREC REPLACING
                   ==:ELR:== BY ==LSR==
                   ==:HLR:== BY ==LLR==
                   ==:HTT:== BY ==LTT==
                   ==:HTR:== BY ==LTR==
                   ==:RPT:== BY ==LPT==.

      *-----------------------------------------------------------------
      * This record is for validating the actual and expected
      * records for the HEALTH-TREND file. For more details, see:
      *
      * 1. 440-VERIFY-HEALTH-TREND-COMP
      * 2. 450-VERIFY-HEALTH-TREND-RECD
      *
      * This variable is used to traverse the captured records from
      * the mocked QSAM HEALTH-TREND file during post-execution
      * validation.
      *-----------------------------------------------------------------
       01  LS-QSAM-HEALTH-TREND-RECORDS.
           05 LS-QSAM-HEALTH-TREND-RECORD OCCURS UNBOUNDED DEPENDING
                     ON SIZE_ IN RECORDS_ IN WS-ZQSAM-HEALTH-TREND.
                10 FILLER PIC X(80).

       PROCEDURE DIVISION.

      ******************************************************************
      * These are basic "gray" and "black box" unit tests that provide *
      * input data and confirm the output data looks reasonable by     *
      * checking the statistics report. This check will catch most     *
      * cases of invalid results, but is not exhaustive.               *
      *                                                                *
      * Register the unit tests that comprise this unit test suite;    *
      * Test4z will call these entry points in sequence once the       *
      * SUT module is prepared and run.                                *
      ******************************************************************

           DISPLAY '==> ZTTRNDY start'
           PERFORM 100-REGISTER-UNIT-TESTS
           DISPLAY '==> ZTTRNDY end'

           GOBACK.

      ******************************************************************
      * UNIT TEST 0: Run TRENDY without any Test4z instrumention.      *
      ******************************************************************
           ENTRY 'simpleTestNoSpies'.

           DISPLAY 'ZTTRNDY start unit test 0: simpleTestNoSpies'

      *-----------------------------------------------------------------
      * Mock all the input and output files;
      * EXEC-LOG is created by this unit test.
      *-----------------------------------------------------------------
           PERFORM 200-MOCK-EXEC-LOG-GENERATED
           PERFORM 240-MOCK-HEALTH-LOG
           PERFORM 250-MOCK-HEALTH-TREND
           PERFORM 260-MOCK-STATUS-REPORT

      *-----------------------------------------------------------------
      * Prepare TRENDY and run.
      *
      * NB: Unlike the other unit tests, this one intentionally has
      *     no validations via spies, breakpoints, or other Test4z
      *     instrumentation. This shows the SUT "normal" output in
      *     SYSOUT without any of the validation commentary.
      *-----------------------------------------------------------------
           PERFORM 300-PREPARE-TRENDY
           PERFORM 400-RUN-TRENDY

           DISPLAY 'ZTTRNDY end unit test 0: simpleTestNoSpies'

           GOBACK.

      ******************************************************************
      * UNIT TEST 1: Test with synthetic performance data.             *
      ******************************************************************
           ENTRY 'testGeneratedData'.

           SET WS-STOP-ON-FAIL TO TRUE

      *-----------------------------------------------------------------
      * This unit test is reused by other unit tests with minor
      * variations. So as not to confuse, don't echo the start of this
      * unit test if it's being reused by another.
      *-----------------------------------------------------------------
           IF RUNNING-UNIT-TEST-1
                DISPLAY 'ZTTRNDY unit test 1:  testGeneratedData'
           END-IF

      *-----------------------------------------------------------------
      * Mock all the input and output files;
      * EXEC-LOG is created by this unit test.
      *-----------------------------------------------------------------
           PERFORM 200-MOCK-EXEC-LOG-GENERATED
           PERFORM 240-MOCK-HEALTH-LOG
           PERFORM 250-MOCK-HEALTH-TREND
           PERFORM 260-MOCK-STATUS-REPORT

      *-----------------------------------------------------------------
      * Prepare TRENDY, instrument it, and run.
      *-----------------------------------------------------------------
           PERFORM 300-PREPARE-TRENDY
           PERFORM 310-SPY-VARIABLE-STATS
           PERFORM 320-SPY-QSAM-FILE-OPS
           PERFORM 330-SPY-KSDS-FILE-OPS
           PERFORM 340-SPY-BASIC-EXECUTION
           PERFORM 350-WATCH-STATS-REPORT-TOTALS
           PERFORM 360-BREAKPOINT-STATS-REPORT

      *-----------------------------------------------------------------
      * Optionally force an error (see testUnhappyIO unit test).
      *-----------------------------------------------------------------
           IF WS-FORCE-STATS-REPORT-ERROR
                MOVE 8 TO CS-EXPECTED-RETURN-CODE
                SET WS-CONTINUE-ON-FAIL TO TRUE
                PERFORM 270-FORCE-ERROR-STATS-REPORT
           END-IF

           PERFORM 400-RUN-TRENDY

      *-----------------------------------------------------------------
      * TRENDY is finished, so double-check its work. For the
      * forced I/O "unhappy path", skip the data validation since
      * the expected result is no data processed.
      *-----------------------------------------------------------------
           IF NOT WS-FORCE-STATS-REPORT-ERROR
                PERFORM 410-ASSERT-BASIC-EXECUTION
                PERFORM 420-VERIFY-SPY-VARIABLE-STATS
                PERFORM 440-VERIFY-HEALTH-TREND-COMP
           END-IF
           PERFORM 460-VERIFY-RETURN-CODE

      *-----------------------------------------------------------------
      * Print the expected results.
      *-----------------------------------------------------------------
           PERFORM 630-PRINT-MOCKED-STATS-REPORT

      *-----------------------------------------------------------------
      * For debug purposes, optionally echo the input/output files.
      *-----------------------------------------------------------------
           PERFORM 600-PRINT-MOCKED-EXEC-LOG
           PERFORM 610-PRINT-MOCKED-HEALTH-LOG
           PERFORM 620-PRINT-MOCKED-HEALTH-TREND

      *-----------------------------------------------------------------
      * This unit test is reused by other unit tests with minor
      * variations. So as not to confuse, don't echo the end of this
      * unit test if it's being reused by another.
      *-----------------------------------------------------------------
           IF RUNNING-UNIT-TEST-1
                DISPLAY 'ZTTRNDY end unit test 1: testGeneratedData'
           END-IF

           GOBACK.

      ******************************************************************
      * UNIT TEST 2: Test with loaded data instead of mocked objects   *
      *              from harness.                                     *
      ******************************************************************
           ENTRY 'testLoadData'.

           DISPLAY 'ZTTRNDY unit test 2: testLoadData'
           SET WS-STOP-ON-FAIL TO TRUE
           
      *-----------------------------------------------------------------
      * Mock all the input and output files except EXEC-LOG; it
      * will be loaded from a previous recording with the _LoadData API.
      *-----------------------------------------------------------------
           PERFORM 230-MOCK-EXEC-LOG-LOADDATA
           PERFORM 240-MOCK-HEALTH-LOG
           PERFORM 250-MOCK-HEALTH-TREND
           PERFORM 260-MOCK-STATUS-REPORT

      *-----------------------------------------------------------------
      * Prepare TRENDY, instrument it, and run.
      *-----------------------------------------------------------------
           PERFORM 300-PREPARE-TRENDY
           PERFORM 310-SPY-VARIABLE-STATS
           PERFORM 320-SPY-QSAM-FILE-OPS
           PERFORM 330-SPY-KSDS-FILE-OPS
           PERFORM 340-SPY-BASIC-EXECUTION
           PERFORM 350-WATCH-STATS-REPORT-TOTALS
           PERFORM 360-BREAKPOINT-STATS-REPORT
           PERFORM 400-RUN-TRENDY

      *-----------------------------------------------------------------
      * TRENDY is finished, so double-check its work.
      *-----------------------------------------------------------------
           PERFORM 410-ASSERT-BASIC-EXECUTION
           PERFORM 420-VERIFY-SPY-VARIABLE-STATS
           PERFORM 450-VERIFY-HEALTH-TREND-RECD
           PERFORM 460-VERIFY-RETURN-CODE

      *-----------------------------------------------------------------
      * Print the expected results.
      *-----------------------------------------------------------------
           PERFORM 630-PRINT-MOCKED-STATS-REPORT

      *-----------------------------------------------------------------
      * For debug purposes, optionally echo the input/output files.
      *-----------------------------------------------------------------
           PERFORM 600-PRINT-MOCKED-EXEC-LOG
           PERFORM 610-PRINT-MOCKED-HEALTH-LOG
           PERFORM 620-PRINT-MOCKED-HEALTH-TREND

           DISPLAY 'ZTTRNDY end unit test 2: testLoadData'

           GOBACK.

      ******************************************************************
      * UNIT TEST 3: Test with realistic performance data from         *
      *              recorded data with test harness.                  *
      ******************************************************************
           ENTRY 'testAllHarnessData'.

           DISPLAY 'ZTTRNDY unit test 3: testAllHarnessData'
           SET WS-STOP-ON-FAIL TO TRUE

      *-----------------------------------------------------------------
      * Let the test harness mock all the input and output files;
      * all the data is from a previous "live" recording with the
      * _CreateHarness API.
      *-----------------------------------------------------------------
           PERFORM 240-CREATE-HARNESS-ALL

      *-----------------------------------------------------------------
      * Set the expected results for 430-VERIFY-STATS-GETVARIABLE.
      *-----------------------------------------------------------------
           INITIALIZE CHECK-STATS-REPORT-TOTALS
           MOVE SPACES TO CS-MESSAGE-TEXT
           MOVE 'testAllHarnessData' TO CS-MESSAGE-TEXT
           MOVE 100 TO CS-VALID-EXEC-LOGS
           MOVE 93 TO CS-HEALTH-LOG-UPDATES
           MOVE 7 TO CS-HEALTH-LOG-ADDITIONS
           MOVE 1 TO CS-HEALTH-TREND-COUNT
           MOVE 2 TO CS-ERROR-EXEC-LOGS
           MOVE 0 TO CS-INVALID-EXEC-LOGS

      *-----------------------------------------------------------------
      * Set the start/end intervals that are expected; they're
      * rounded to the log record times.
      *-----------------------------------------------------------------
           MOVE '2024-05-26 10:30' TO CS-START-INTERVAL
           MOVE '2024-05-26 11:05' TO CS-END-INTERVAL

      *-----------------------------------------------------------------
      * No errors or warnings are expected.
      *-----------------------------------------------------------------
           MOVE 0 TO CS-EXPECTED-RETURN-CODE

      *-----------------------------------------------------------------
      * Prepare TRENDY, instrument it, and run.
      *-----------------------------------------------------------------
           PERFORM 300-PREPARE-TRENDY
           PERFORM 310-SPY-VARIABLE-STATS
           PERFORM 320-SPY-QSAM-FILE-OPS
           PERFORM 330-SPY-KSDS-FILE-OPS
           PERFORM 340-SPY-BASIC-EXECUTION
           PERFORM 350-WATCH-STATS-REPORT-TOTALS
           PERFORM 360-BREAKPOINT-STATS-REPORT
           PERFORM 400-RUN-TRENDY

      *-----------------------------------------------------------------
      * TRENDY is finished, so double-check its work.
      *-----------------------------------------------------------------
           PERFORM 410-ASSERT-BASIC-EXECUTION
           PERFORM 420-VERIFY-SPY-VARIABLE-STATS
           PERFORM 450-VERIFY-HEALTH-TREND-RECD
           PERFORM 460-VERIFY-RETURN-CODE

      *-----------------------------------------------------------------
      * Print the expected results.
      *-----------------------------------------------------------------
           PERFORM 630-PRINT-MOCKED-STATS-REPORT

      *-----------------------------------------------------------------
      * For debug purposes, optionally echo the input/output files.
      *-----------------------------------------------------------------
           PERFORM 600-PRINT-MOCKED-EXEC-LOG
           PERFORM 610-PRINT-MOCKED-HEALTH-LOG
           PERFORM 620-PRINT-MOCKED-HEALTH-TREND

           DISPLAY 'ZTTRNDY end unit test 3: testAllHarnessData'

           GOBACK.

      ******************************************************************
      * UNIT TEST 4: Test with some pre-recorded data  and the rest    *
      *              being mocked (empty) data.                        *
      ******************************************************************
           ENTRY 'testPartialHarnessData'.

           DISPLAY 'ZTTRNDY unit test 4: testPartialHarnessData'
           SET WS-STOP-ON-FAIL TO TRUE

      *-----------------------------------------------------------------
      * Let the test harness mock all the input and output files;
      * all the data is from a previous recording.
      *-----------------------------------------------------------------
           PERFORM 250-CREATE-HARNESS-PARTIAL

      *-----------------------------------------------------------------
      * Since the HEALTH-LOG and STATUS-REPORT were excluded from
      * the harness, they will need to be mocked. Create empty
      * mocks of them.
      *-----------------------------------------------------------------
           PERFORM 240-MOCK-HEALTH-LOG
           PERFORM 260-MOCK-STATUS-REPORT

      *-----------------------------------------------------------------
      * Set the expected results for 430-VERIFY-STATS-GETVARIABLE
      *-----------------------------------------------------------------
           SET WS-STOP-ON-FAIL TO TRUE
           INITIALIZE CHECK-STATS-REPORT-TOTALS
           MOVE SPACES TO CS-MESSAGE-TEXT
           MOVE 'testPartialHarnessData' TO CS-MESSAGE-TEXT
           MOVE 100 TO CS-VALID-EXEC-LOGS
           MOVE 93 TO CS-HEALTH-LOG-UPDATES
           MOVE 7 TO CS-HEALTH-LOG-ADDITIONS
           MOVE 1 TO CS-HEALTH-TREND-COUNT
           MOVE 2 TO CS-ERROR-EXEC-LOGS
           MOVE 0 TO CS-INVALID-EXEC-LOGS

      *-----------------------------------------------------------------
      * Set the start/end intervals that are expected; they're
      * rounded to the log record times.
      *-----------------------------------------------------------------
           MOVE '2024-05-26 10:30' TO CS-START-INTERVAL
           MOVE '2024-05-26 11:05' TO CS-END-INTERVAL

      *-----------------------------------------------------------------
      * No errors or warnings are expected.
      *-----------------------------------------------------------------
           MOVE 0 TO CS-EXPECTED-RETURN-CODE

      *-----------------------------------------------------------------
      * Prepare TRENDY, instrument it, and run.
      *-----------------------------------------------------------------
           PERFORM 300-PREPARE-TRENDY
           PERFORM 310-SPY-VARIABLE-STATS
           PERFORM 320-SPY-QSAM-FILE-OPS
           PERFORM 330-SPY-KSDS-FILE-OPS
           PERFORM 340-SPY-BASIC-EXECUTION
           PERFORM 350-WATCH-STATS-REPORT-TOTALS
           PERFORM 360-BREAKPOINT-STATS-REPORT
           PERFORM 400-RUN-TRENDY

      *-----------------------------------------------------------------
      * TRENDY is finished, so double-check its work.
      *-----------------------------------------------------------------
           PERFORM 410-ASSERT-BASIC-EXECUTION
           PERFORM 420-VERIFY-SPY-VARIABLE-STATS
           PERFORM 450-VERIFY-HEALTH-TREND-RECD
           PERFORM 460-VERIFY-RETURN-CODE

      *-----------------------------------------------------------------
      * Print the expected results.
      *-----------------------------------------------------------------
           PERFORM 630-PRINT-MOCKED-STATS-REPORT

      *-----------------------------------------------------------------
      * For debug purposes, optionally echo the input/output files.
      *-----------------------------------------------------------------
           PERFORM 600-PRINT-MOCKED-EXEC-LOG
           PERFORM 610-PRINT-MOCKED-HEALTH-LOG
           PERFORM 620-PRINT-MOCKED-HEALTH-TREND

           DISPLAY 'ZTTRNDY end unit test 4: '
                   'testPartialHarnessData'

           GOBACK.

      ******************************************************************
      * UNIT TEST 5: Test with mix of pre-recorded data from a harness *
      *              and some live data.                               *
      ******************************************************************
           ENTRY 'testHarnessLiveDataMix'.

           DISPLAY 'ZTTRNDY unit test 5: testHarnessLiveDataMix'

      *-----------------------------------------------------------------
      * This is a development-only unit test, so continue
      * for non-fatal errors.
      *-----------------------------------------------------------------
           SET WS-CONTINUE-ON-FAIL TO TRUE

      *-----------------------------------------------------------------
      * Let the test harness mock some the input and output files from
      * a previous recording. The EXEC-LOG content would be provided
      * via a local data set, uploaded by the Test4z CLI option "--dd".
      *-----------------------------------------------------------------
           PERFORM 260-CREATE-HARNESS-LIVE-EXLOG

      *-----------------------------------------------------------------
      * This is a test of LIVE data, so the expected results
      * will have to be validated manually.
      *
      * This is a development-time only option, i.e., the developer
      * can use Test4z to mock everything in the environment EXCEPT
      * the live resources they want to test with
      * (in this case, to test various real EXEC-LOG input).
      *
      * The expected values below would need to be updated accordingly,
      * otherwise this unit test will fail.
      *-----------------------------------------------------------------
           SET WS-CONTINUE-ON-FAIL TO TRUE
           INITIALIZE CHECK-STATS-REPORT-TOTALS
           MOVE SPACES TO CS-MESSAGE-TEXT
           MOVE 'testHarnessLiveDataMix is development-time only'
                TO CS-MESSAGE-TEXT
           MOVE 0 TO CS-VALID-EXEC-LOGS
           MOVE 0 TO CS-HEALTH-LOG-UPDATES
           MOVE 0 TO CS-HEALTH-LOG-ADDITIONS
           MOVE 0 TO CS-HEALTH-TREND-COUNT
           MOVE 0 TO CS-ERROR-EXEC-LOGS
           MOVE 0 TO CS-INVALID-EXEC-LOGS

      *-----------------------------------------------------------------
      * In this default example, EXEC-LOG is empty, so there's
      * no records to validate.
      *-----------------------------------------------------------------
           MOVE 0 TO CHECK-HRT-COMPARE-CNT

      *-----------------------------------------------------------------
      * No records are provided in this testcase, so expect a warning.
      *-----------------------------------------------------------------
           MOVE 4 TO CS-EXPECTED-RETURN-CODE

      *-----------------------------------------------------------------
      * NB: If this is run in a live environment without a defining
      * data set for the EXEC-LOG, it will fail with a runtime error.
      *
      * Set the start/end intervals that are expected; they're
      * rounded to the log record times.
      *-----------------------------------------------------------------
           MOVE '0000-00-00 00:00' TO CS-START-INTERVAL
           MOVE '0000-00-00 00:00' TO CS-END-INTERVAL

      *-----------------------------------------------------------------
      * Prepare TRENDY, instrument it, and run.
      *-----------------------------------------------------------------
           PERFORM 300-PREPARE-TRENDY
           PERFORM 310-SPY-VARIABLE-STATS
           PERFORM 320-SPY-QSAM-FILE-OPS
           PERFORM 330-SPY-KSDS-FILE-OPS
           PERFORM 340-SPY-BASIC-EXECUTION
           PERFORM 350-WATCH-STATS-REPORT-TOTALS
           PERFORM 360-BREAKPOINT-STATS-REPORT
           PERFORM 400-RUN-TRENDY

      *-----------------------------------------------------------------
      * TRENDY is finished, so double-check its work.
      *-----------------------------------------------------------------
           PERFORM 410-ASSERT-BASIC-EXECUTION
           PERFORM 420-VERIFY-SPY-VARIABLE-STATS
           PERFORM 460-VERIFY-RETURN-CODE

      *-----------------------------------------------------------------
      * Print the expected results.
      *-----------------------------------------------------------------
           PERFORM 630-PRINT-MOCKED-STATS-REPORT

      *-----------------------------------------------------------------
      * For debug purposes, optionally echo the input/output files.
      *-----------------------------------------------------------------
           PERFORM 600-PRINT-MOCKED-EXEC-LOG
           PERFORM 610-PRINT-MOCKED-HEALTH-LOG
           PERFORM 620-PRINT-MOCKED-HEALTH-TREND

           DISPLAY 'ZTTRNDY end unit test 5: testHarnessLiveDataMix'

           GOBACK.

      ******************************************************************
      * UNIT TEST 6: Force a "unhappy path" by modifying TRENDY's      *
      *              EXEC-LOG READs to inject a high number of         *
      *              errors in otherwise good data.                    *
      ******************************************************************
           ENTRY 'testUnhappyData'.

           DISPLAY 'ZTTRNDY unit test 6: testUnhappyData'
           SET WS-STOP-ON-FAIL TO TRUE

      *-----------------------------------------------------------------
      * Mock all the input and output files;
      * EXEC-LOG is created by this unit test.
      *-----------------------------------------------------------------
           PERFORM 200-MOCK-EXEC-LOG-GENERATED
           PERFORM 240-MOCK-HEALTH-LOG
           PERFORM 250-MOCK-HEALTH-TREND
           PERFORM 260-MOCK-STATUS-REPORT

      *-----------------------------------------------------------------
      * Prepare TRENDY, instrument it, and run.
      *-----------------------------------------------------------------
           PERFORM 300-PREPARE-TRENDY
           PERFORM 310-SPY-VARIABLE-STATS

      *-----------------------------------------------------------------
      * Override the expected values from 200-MOCK-EXEC-LOG-GENERATED
      * since they'll ALL be modified to be invalid by the
      * 'changeExecLogRecord' watch callback established below.
      *-----------------------------------------------------------------
           PERFORM 340-WATCH-EXEC-LOG-RECORD
           SET WS-CONTINUE-ON-FAIL TO TRUE

           MOVE '0000-00-00 00:00' TO CS-START-INTERVAL
           MOVE '0000-00-00 00:00' TO CS-END-INTERVAL
           MOVE 0 TO CS-VALID-EXEC-LOGS
           MOVE 0 TO CS-HEALTH-LOG-UPDATES
           MOVE 0 TO CS-HEALTH-LOG-ADDITIONS
           MOVE 0 TO CS-HEALTH-TREND-COUNT
           MOVE 0 TO CS-ERROR-EXEC-LOGS
           MOVE 181 TO CS-INVALID-EXEC-LOGS

      *-----------------------------------------------------------------
      * Now that the "perfect" (ha!) generated log entries are
      * far from it, let's run and confirm they're flagged as bogus.
      * For this unit test, skip most of the other validations since
      * the input data has been forced to be invalid.
      *-----------------------------------------------------------------
           PERFORM 400-RUN-TRENDY

      *-----------------------------------------------------------------
      * TRENDY is finished, so double-check its work.
      *-----------------------------------------------------------------
           PERFORM 420-VERIFY-SPY-VARIABLE-STATS
           PERFORM 460-VERIFY-RETURN-CODE

      *-----------------------------------------------------------------
      * Print the expected results.
      *-----------------------------------------------------------------
           PERFORM 630-PRINT-MOCKED-STATS-REPORT

      *-----------------------------------------------------------------
      * For debug purposes, optionally echo the input/output files.
      *-----------------------------------------------------------------
           PERFORM 600-PRINT-MOCKED-EXEC-LOG
           PERFORM 610-PRINT-MOCKED-HEALTH-LOG
           PERFORM 620-PRINT-MOCKED-HEALTH-TREND

           DISPLAY 'ZTTRNDY end unit test 6: testUnhappyData'

           GOBACK.

      ******************************************************************
      * UNIT TEST 7: Force another "unhappy path" by injecting a file  *
      *              I/O error.                                        *
      ******************************************************************
           ENTRY 'testUnhappyIO'.

           DISPLAY 'ZTTRNDY unit test 7: testUnhappyIO'

           SET WS-FORCE-STATS-REPORT-ERROR TO TRUE
           SET WS-CONTINUE-ON-FAIL TO TRUE

      *-----------------------------------------------------------------
      * The other unit test's data is fine for this unit test since
      * we're forcing an early file I/O error anyway. So rather
      * than do the setup here, accept its "happy path" setup.
      *-----------------------------------------------------------------
           CALL 'testGeneratedData'

           DISPLAY 'ZTTRNDY end unit test 7: testUnhappyIO'

           GOBACK.

      ******************************************************************
      * UNIT TEST 8: Override the ratings program ZTPTRNDC and         *
      *              substitute our own preferred answers to (a)       *
      *              further test the response to various ratings      *
      *              and (b) decouple the dependency on the rating     *
      *              code (e.g., if it's developed by another team).   *
      ******************************************************************
           ENTRY 'testStubbedRatings'.

           DISPLAY 'ZTTRNDY unit test 8: testStubbedRatings'
           SET WS-STOP-ON-FAIL TO TRUE

           MOVE LOW-VALUES TO I_STUBPROGRAM
           MOVE 'ZTPTRNDC' TO MODULENAME IN ZWS_STUBPROGRAM
           MOVE 'ZTPTRNDC' TO FUNCTIONNAME IN ZWS_STUBPROGRAM
           SET STUBROUTINE IN ZWS_STUBPROGRAM
                TO ENTRY 'calculateHealthRatingCallback'
           CALL ZTESTUT USING ZWS_STUBPROGRAM

      *-----------------------------------------------------------------
      * The other unit test's data is fine for this unit test since
      * we're manipulating ratings anyway. So rather
      * than do the setup here, accept its "happy path" setup.
      *-----------------------------------------------------------------
           SET CALLING-UNIT-TEST-1 TO TRUE
           CALL 'testGeneratedData'

           DISPLAY 'ZTTRNDY end unit test 8: testStubbedRatings'

           GOBACK.

      ******************************************************************
      * CALLBACK: Invoked when TRENDY performs 300-WRITE-STATS-REPORT. *
      *           See 360-BREAKPOINT-STATS-REPORT for details.         *
      ******************************************************************
           ENTRY 'writeStatsReportBreakpoint'
                USING ZLS_GOBLOCK, ZLS_B_ITBLOCK.

           IF WHEN_AFTER IN ZLS_B_ITBLOCK
                DISPLAY 'ZTTRNDY writeStatsReportBreakpoint'

                PERFORM 430-VERIFY-STATS-GETVARIABLE
                PERFORM 640-PRINT-STATS-REPORT-TOTALS
           END-IF

           GOBACK.

      ******************************************************************
      * CALLBACK: When TRENDY changes to RPT-INVALID-EXEC-LOGS.        *
      *           See 350-WATCH-STATS-REPORT-TOTALS for details.       *
      ******************************************************************
           ENTRY 'watchStatsReportTotals'
                USING ZLS_GOBLOCK, ZLS_W_ITBLOCK, ZLS_W_IFBLOCK.

           IF VARIABLENAME IN ZLS_W_ITBLOCK
                NOT = 'RPT-STATS-REPORT-TOTALS'

                SET ADDRESS OF LPT-STATS-REPORT-TOTALS
                     TO CURRENTADDRESS IN ZLS_W_IFBLOCK

                IF LPT-INVALID-EXEC-LOGS NOT =
                     WS-INVALID-EXEC-LOGS-LAST

      *-----------------------------------------------------------------
      * To enforce proper handling, TRENDY's RPT-INVALID-EXEC-LOGS
      * variable should only be updated by
      * 530-REPORT-INVALID-EXEC-LOG, e.g., to assure it's logged
      * and later investigated. Fail the unit test if
      * this isn't true (which suggests a coding error or rogue
      * program).
      *-----------------------------------------------------------------
                     IF CURRENTSECTION IN ZLS_W_IFBLOCK
                               NOT = '530-REPORT-INVALID-EXEC-LOG'

                          MOVE SPACES TO WS-FAIL-MESSAGE
                          STRING
                               'Invalid RPT-INVALID-EXEC-LOGS change: '
                               CURRENTSECTION IN ZLS_W_IFBLOCK
                               DELIMITED BY SIZE
                               INTO WS-FAIL-MESSAGE
                          PERFORM 500-FAIL-TEST
                     END-IF

                     MOVE LPT-INVALID-EXEC-LOGS TO
                          WS-INVALID-EXEC-LOGS-LAST
                END-IF
           END-IF

           GOBACK.

      ******************************************************************
      * CALLBACK: When TRENDY reads in a new ELR-EXEC-LOG-RECORD,      *
      *           use this callback to force an "unhappy path" by      *
      *           injecting errors into all EXEC-LOG records.          *
      *           See 340-WATCH-EXEC-LOG-RECORD for details.           *
      ******************************************************************
           ENTRY 'changeExecLogRecord'
                USING ZLS_GOBLOCK, ZLS_W_ITBLOCK, ZLS_W_IFBLOCK.

      *-----------------------------------------------------------------
      * Spies are notified before and after the middleware call; for
      * this particular spy, only the "after" is considered since
      * the retrieved values will be overwritten anyway.
      *-----------------------------------------------------------------
           IF WHEN_AFTER IN ZLS_W_ITBLOCK

      *-----------------------------------------------------------------
      * Get a reference to the current EXEC-LOG record that will be
      * returned to the SUT (i.e., within its working storage section).
      *-----------------------------------------------------------------
                SET ADDRESS OF LSR-EXEC-LOG-RECORD
                     TO CURRENTADDRESS IN ZLS_W_IFBLOCK

      *-----------------------------------------------------------------
      * A watch callback can modify the variable prior to the SUT
      * accessing it. This gives the UT an opportunity to modify it.
      * The watch also includes the previous value, for historical
      * reference.
      *
      * Modify the input record to a nightmarishly bad one. This
      * will verify TRENDY handles "bad" records correctly (e.g.,
      * by logging them, which in a real production environment, would
      * trigger a post-analysis investigation).
      *-----------------------------------------------------------------
                SET LSR-RESPONSE-BAD TO TRUE
                SET LSR-WORKLOAD-LOW TO TRUE
                SET LSR-TYPE-ERROR TO TRUE
                MOVE 7200 TO LSR-CPU-TIME
                MOVE 16 TO LSR-RETURN-CODE
                MOVE 100 TO LSR-WORKLOAD
           END-IF

           GOBACK.

      ******************************************************************
      * CALLBACK: When TRENDY changes the HEALTH-TREND file.           *
      *           See 330-SPY-KSDS-FILE-OPS for details.               *
      ******************************************************************
           ENTRY 'spyHealthTrendCallback'
                USING LS-ZSPKSDS-HEALTH-LOG-CALLBACK.

           SET ADDRESS OF ZLS_KSDS_RECORD
                TO LASTCALL IN LS-ZSPKSDS-HEALTH-LOG-CALLBACK

           IF COMMAND IN ZLS_KSDS_RECORD = 'WRITE'
              ADD 1 TO CHECK-SPY-TREND-HEALTH-WRITE
           ELSE
                IF COMMAND IN ZLS_KSDS_RECORD = 'REWRITE'
                   ADD 1 TO CHECK-SPY-TREND-HEALTH-REWRITE
                END-IF
           END-IF

           GOBACK.

      ******************************************************************
      * CALLBACK: To simulate a file open error to the STATS-REPORT.   *
      *           See 270-FORCE-ERROR-STATS-REPORT for details.        *
      *                                                                *
      *           To simulate an I/O error, use Test4z's intercepted   *
      *           middleware call interface block (ZLS_Q_IFBLOCK). It  *
      *           includes the status and condition code that will be  *
      *           returned to the program under test.                  *
      ******************************************************************
           ENTRY 'forceStatsReportError'
               USING LS-ZSPQSAM-REPORT-CALLBACK

           SET ADDRESS OF ZLS_QSAM_RECORD 
                TO LASTCALL IN LS-ZSPQSAM-REPORT-CALLBACK

           IF COMMAND IN ZLS_QSAM_RECORD(1:4) = 'OPEN'
      *-----------------------------------------------------------------   
      * The command matches our targeted error scenario, so set the
      * status and condition code accordingly; it will then
      * be returned to the program under test.

      * NB: In order that COBOL I/O error statements are handled
      *     correctly, the status AND condition code must be set.
      *-----------------------------------------------------------------    
                SET ADDRESS OF ZLS_Q_IFBLOCK
                     TO IFBLOCK IN LS-ZSPQSAM-REPORT-CALLBACK

                SET ADDRESS OF LS-IFBLOCK-STATUSCODE 
                     TO STATUSCODE IN ZLS_Q_IFBLOCK
                SET ADDRESS OF LS-IFBLOCK-CONDCODE 
                     TO CONDCODE IN ZLS_Q_IFBLOCK

                MOVE 39 TO LS-IFBLOCK-STATUSCODE
                MOVE 32 TO LS-IFBLOCK-CONDCODE
           END-IF

           GOBACK.

      ******************************************************************
      * CALLBACK: To simulate a program invocation of ZTPTRNDC. See    *
      *           "Unit test 8: testStubbedRatings" for details.  *
      ******************************************************************
           ENTRY 'calculateHealthRatingCallback'
                USING LTT-HEALTH-TREND-TOTALS, LTR-HEALTH-TREND-RECORD.

           MOVE LTT-REQUESTS-TOTAL TO LTR-REQUESTS

      *-----------------------------------------------------------------
      * Change TRENDY's assessments to demonstrate how a stubbed
      * program works. The changes are somewhat arbitrary to
      * demonstrate the point, so simply change the response based on
      * how many times this callback has been invoked.
      *-----------------------------------------------------------------     
           ADD 1 TO WS-HEALTH-RATING-CALLBACK-CNT

           EVALUATE WS-HEALTH-RATING-CALLBACK-CNT
                WHEN 1
                     MOVE 5 TO LTR-RESPONSE-RATING
                     MOVE 5 TO LTR-HAPPY-RATING
                     MOVE 5 TO LTR-WORKLOAD-RATING

                WHEN 2
                     MOVE 3 TO LTR-RESPONSE-RATING
                     MOVE 3 TO LTR-HAPPY-RATING
                     MOVE 3 TO LTR-WORKLOAD-RATING

                WHEN 3
                     MOVE 1 TO LTR-RESPONSE-RATING
                     MOVE 1 TO LTR-HAPPY-RATING
                     MOVE 1 TO LTR-WORKLOAD-RATING

               WHEN OTHER
                     MOVE 0 TO LTR-RESPONSE-RATING
                     MOVE 0 TO LTR-HAPPY-RATING
                     MOVE 0 TO LTR-WORKLOAD-RATING
           END-EVALUATE

           GOBACK.

      ******************************************************************
      * Register the UT entry points that will be called by Test4z.    *
      ******************************************************************
       100-REGISTER-UNIT-TESTS.

           DISPLAY 'ZTTRNDY 100-REGISTER-UNIT-TESTS'

           MOVE LOW-VALUES TO I_TEST
           SET TESTFUNCTION IN ZWS_TEST TO
               ENTRY 'simpleTestNoSpies'
           MOVE 'Unit test 0: Test with no Test4z instrumentation'
                TO TESTNAME IN ZWS_TEST
           CALL ZTESTUT USING ZWS_TEST

           MOVE LOW-VALUES TO I_TEST
           SET TESTFUNCTION IN ZWS_TEST TO
               ENTRY 'testGeneratedData'
           MOVE 'Unit test 1: Test with generated test data'
                TO TESTNAME IN ZWS_TEST
           CALL ZTESTUT USING ZWS_TEST

           MOVE LOW-VALUES TO I_TEST
           SET TESTFUNCTION IN ZWS_TEST TO
               ENTRY 'testLoadData'
           MOVE 'Unit test 2: Test with recorded data'
                TO TESTNAME IN ZWS_TEST
           CALL ZTESTUT USING ZWS_TEST

           MOVE LOW-VALUES TO I_TEST
           SET TESTFUNCTION IN ZWS_TEST TO
                ENTRY 'testAllHarnessData'
           MOVE 'Unit test 3: Test with harness data'
                TO TESTNAME IN ZWS_TEST
           CALL ZTESTUT USING ZWS_TEST

           MOVE LOW-VALUES TO I_TEST
           SET TESTFUNCTION IN ZWS_TEST TO
                ENTRY 'testPartialHarnessData'
           MOVE 'Unit test 4: Test with partial harness data'
                TO TESTNAME IN ZWS_TEST
           CALL ZTESTUT USING ZWS_TEST

           MOVE LOW-VALUES TO I_TEST
           SET TESTFUNCTION IN ZWS_TEST TO
                ENTRY 'testHarnessLiveDataMix'
           MOVE 'Unit test 5: Test with live/recorded data mix'
                TO TESTNAME IN ZWS_TEST
           CALL ZTESTUT USING ZWS_TEST

           MOVE LOW-VALUES TO I_TEST
           SET TESTFUNCTION IN ZWS_TEST TO
                ENTRY 'testUnhappyData'
           MOVE 'Unit test 6: Test with intentionally bad data'
                TO TESTNAME IN ZWS_TEST
           CALL ZTESTUT USING ZWS_TEST

           MOVE LOW-VALUES TO I_TEST
           SET TESTFUNCTION IN ZWS_TEST TO
                ENTRY 'testUnhappyIO'
           MOVE 'Unit test 7: Test with early file I/O error'
                TO TESTNAME IN ZWS_TEST
           CALL ZTESTUT USING ZWS_TEST

           MOVE LOW-VALUES TO I_TEST
           SET TESTFUNCTION IN ZWS_TEST TO
                ENTRY 'testStubbedRatings'
           MOVE 'Unit test 8: Test with stubbed ZTPTRNDC'
                TO TESTNAME IN ZWS_TEST
           CALL ZTESTUT USING ZWS_TEST

           EXIT.

      ******************************************************************
      * Prepare the SUT, afterwhich the UT can set breakpoints.        *
      ******************************************************************
       300-PREPARE-TRENDY.

           DISPLAY 'ZTTRNDY 300-PREPARE-TRENDY'

           MOVE LOW-VALUES TO I_PREPAREMODULE
           MOVE 'ZTPTRNDY' TO MODULENAME IN ZWS_PREPAREMODULE
           CALL ZTESTUT USING ZWS_PREPAREMODULE

           EXIT.

      ******************************************************************
      * Run the SUT.                                                   *
      ******************************************************************
       400-RUN-TRENDY.

           DISPLAY 'ZTTRNDY 400-RUN-TRENDY'

           MOVE LOW-VALUES TO I_GETFUNCTION
           MOVE 'ZTPTRNDY' TO MODULENAME IN ZWS_GETFUNCTION
           MOVE 'ZTPTRNDY' TO FUNCTIONNAME IN ZWS_GETFUNCTION
           CALL ZTESTUT USING ZWS_GETFUNCTION, WS-RUN-PROGRAM

      *-----------------------------------------------------------------
      * Run the SUT and save its return code for validation.
      *-----------------------------------------------------------------
           CALL WS-RUN-PROGRAM
           MOVE RETURN-CODE TO WS-RETURN-CODE-SUT

           EXIT.

      ******************************************************************
      * Run TRENDY with everything mocked from pre-recorded data.      *
      *                                                                *
      * Input:  ZTPTRNDY.json recording from previous "live" run       *
      * Output: Mocks for all files                                    *
      ******************************************************************
       240-CREATE-HARNESS-ALL.

           DISPLAY 'ZTTRNDY 240-CREATE-HARNESS-ALL'

           MOVE LOW-VALUES TO I_CREATEHARNESS
           MOVE 'ZTPTRNDY' TO MEMBERNAME IN ZWS_CREATEHARNESS
           CALL ZTESTUT USING ZWS_CREATEHARNESS,
                HARNESSOBJECT IN WS-ZHRNESS-DATA

      *-----------------------------------------------------------------
      * Retrieve the HEALTH-TREND QSAM object created by the harness;
      * this will later be used to validate the records written
      * to it.
      *
      * For usage of LS-ZQSAM-HEALTH-TREND-FILE, see these 
      * validation paragraphs:
      *
      *    440-VERIFY-HEALTH-TREND-COMP
      *    450-VERIFY-HEALTH-TREND-RECD
      *
      * The validation paragraphs above are very similar, but were
      * coded separately for easier understanding of the example code.
      *-----------------------------------------------------------------
           MOVE LOW-VALUES TO I_GETFILEOBJECT
           MOVE 'HLTREND' TO FILENAME IN ZWS_GETFILEOBJECT
           SET HARNESSOBJECT IN ZWS_GETFILEOBJECT TO
               ADDRESS OF HARNESSOBJECT IN WS-ZHRNESS-DATA
           CALL ZTESTUT USING ZWS_GETFILEOBJECT,
                ADDRESS OF LS-ZQSAM-HEALTH-TREND-FILE

           EXIT.

      ******************************************************************
      * By default, a test harness mocks EVERYTHING in the recording   *
      * except programs. This paragraph creates a test harness where   *
      * the recorded data for files HEALTH-LOG and STATUS-REPORT       *
      * is ignored.                                                    *
      *                                                                *
      * Input:  ZTPTRNDY.json recording from previous "live" run       *
      * Output: Mocks for all except HEALTH-LOG and STATUS-REPORT      *
      ******************************************************************
       250-CREATE-HARNESS-PARTIAL.

           DISPLAY 'ZTTRNDY 250-CREATE-HARNESS-PARTIAL'

      *-----------------------------------------------------------------
      * We want to start with a locally-created HEALTH-TREND file, so
      * excluded it from the test harness. Instead, let the caller
      * create their own mock file. Same for the STATS-REPORT.
      *-----------------------------------------------------------------
           MOVE LOW-VALUES TO I_CREATEHARNESS
           MOVE 'ZTPTRNDY' TO MEMBERNAME IN ZWS_CreateHarness

           SET KIND_KSDS IN HO-HARNESS-OPTION(1) TO TRUE
           MOVE 'HLLOG' TO ARTIFACTNAMES IN HO-HARNESS-OPTION(1)
           MOVE 'ZTPTRNDY' TO MODULENAMES IN HO-HARNESS-OPTION(1)

           SET KIND_QSAM IN HO-HARNESS-OPTION(2) TO TRUE
           MOVE 'STREPORT' TO ARTIFACTNAMES IN HO-HARNESS-OPTION(2)
           MOVE 'ZTPTRNDY' TO MODULENAMES IN HO-HARNESS-OPTION(2)

           SET PTR IN MOCKOPTIONS IN ZWS_CREATEHARNESS TO
               ADDRESS OF WS-ZHRNOPT-OPTIONS
           MOVE 2 TO SIZ IN MOCKOPTIONS IN ZWS_CREATEHARNESS

           CALL ZTESTUT USING ZWS_CREATEHARNESS,
                HARNESSOBJECT IN WS-ZHRNESS-DATA

      *-----------------------------------------------------------------
      * Retrieve the HEALTH-TREND QSAM object created by the harness;
      * this will later be used to validate the records written
      * to it using the mock stored in LS-ZQSAM-HEALTH-TREND-FILE.
      *-----------------------------------------------------------------
           MOVE LOW-VALUES TO I_GETFILEOBJECT
           MOVE 'HLTREND' TO FILENAME IN ZWS_GETFILEOBJECT
           SET HARNESSOBJECT IN ZWS_GETFILEOBJECT TO
               ADDRESS OF HARNESSOBJECT IN WS-ZHRNESS-DATA
           CALL ZTESTUT USING ZWS_GETFILEOBJECT,
                ADDRESS OF LS-ZQSAM-HEALTH-TREND-FILE

           EXIT.

      ******************************************************************
      * By default, a test harness mocked EVERYTHING in the recording  *
      * except programs. This paragraph creates a test harness where   *
      * the recorded data for EXEC-LOG and STATUS-REPORT is ignored.   *
      *                                                                *
      * This assumes the EXEC-LOG will be defined via a EXLOG          *
      * DD so "real" data is processed.                                *
      *                                                                *
      * Input: ZTPTRNDY.json recording from previous "live" run        *
      *        (or local file with EXLOG content and --dd option       *
      *        of Test4z CLI)                                          *
      * Output: Mocks for all except EXEC-LOG and STATUS-REPORT        *
      ******************************************************************
       260-CREATE-HARNESS-LIVE-EXLOG.

           DISPLAY 'ZTTRNDY 260-CREATE-HARNESS-LIVE-EXLOG'

      *-----------------------------------------------------------------
      * We want a dynamically chosen EXEC-LOG file, so
      * excluded it from the test harness. Instead, it will
      * be read from the live environment - not mocked.
      *-----------------------------------------------------------------
           MOVE LOW-VALUES TO I_CREATEHARNESS
           MOVE 'ZTPTRNDY' TO MEMBERNAME IN ZWS_CREATEHARNESS

           SET KIND_QSAM IN HO-HARNESS-OPTION(1) TO TRUE
           MOVE 'EXLOG' TO ARTIFACTNAMES IN HO-HARNESS-OPTION(1)
           MOVE 'ZTPTRNDY' TO MODULENAMES IN HO-HARNESS-OPTION(1)

           SET KIND_QSAM IN HO-HARNESS-OPTION(2) TO TRUE
           MOVE 'STREPORT' TO ARTIFACTNAMES IN HO-HARNESS-OPTION(2)
           MOVE 'ZTPTRNDY' TO MODULENAMES IN HO-HARNESS-OPTION(2)

           SET PTR IN MOCKOPTIONS IN ZWS_CREATEHARNESS TO
               ADDRESS OF WS-ZHRNOPT-OPTIONS
           MOVE 2 TO SIZ IN MOCKOPTIONS IN ZWS_CREATEHARNESS

           CALL ZTESTUT USING ZWS_CREATEHARNESS,
                HARNESSOBJECT IN WS-ZHRNESS-DATA

      *-----------------------------------------------------------------
      * Retrieve the HEALTH-TREND QSAM object created by the harness;
      * this will later be used to validate the records written
      * to it using the mock stored in LS-ZQSAM-HEALTH-TREND-FILE.
      *-----------------------------------------------------------------
           MOVE LOW-VALUES TO I_GETFILEOBJECT
           MOVE 'HLTREND' TO FILENAME IN ZWS_GETFILEOBJECT
           SET HARNESSOBJECT IN ZWS_GETFILEOBJECT TO
               ADDRESS OF HARNESSOBJECT IN WS-ZHRNESS-DATA
           CALL ZTESTUT USING ZWS_GETFILEOBJECT,
                ADDRESS OF LS-ZQSAM-HEALTH-TREND-FILE

      *-----------------------------------------------------------------
      * Since the EXEC-LOG was excluded from the harness,
      * it will be live. But the STATUS-REPORT should still mocked,
      * so create an empty mock of it.
      *
      * IMPORTANT:
      *
      *     For convenient demonstration, an empty EXEC-LOG
      *     is mocked below if the PARM(ZTTRNDY=ZLNEXT) option is _not_
      *     present.
      *
      * For example, to use a local EXEC-LOG file:
      *
      *     t4z --dd "//EXLOG DD PATH=test/data/ZTPTRND1.txt"
      *         --zlopts "PARM(ZTTRNDY=ZLNEXT)"
      *
      * NB: The above should be entered as one line to the Test4z CLI.
      *
      * This will upload the local file to EXLOG and include a
      * DD statement for EXLOG. This enables you to easily
      * test various input files during development.
      *
      * The expected results will need to be adjusted as well; they
      * are all set to zero in this associated unit test.
      *-----------------------------------------------------------------

           MOVE LOW-VALUES TO I_GETPARM IN ZWS_GETPARM
           CALL ZTESTUT USING ZWS_GETPARM, WS-ZPARM-GET-MYOPTION

           MOVE 0 TO WS-PARM-SEARCH

           IF SIZ IN WS-ZPARM-GET-MYOPTION > 0
                SET ADDRESS OF LS-GET-MYOPTION
                     TO PTR IN WS-ZPARM-GET-MYOPTION
                INSPECT LS-GET-MYOPTION(1:SIZ IN WS-ZPARM-GET-MYOPTION)
                     TALLYING WS-PARM-SEARCH FOR ALL 'ZTTRNDY=ZLNEXT'
           END-IF

      *-----------------------------------------------------------------
      * No PARM(ZTTRNDY=ZLNEXT) in ZLOPTS was specified,
      * so create an empty mock of the EXEC-LOG file.
      *-----------------------------------------------------------------
           IF WS-PARM-SEARCH = 0
                PERFORM 210-MOCK-EXEC-LOG-EMPTY
           ELSE
                DISPLAY 'ZTTRNDY option PARM(ZTTRNDY=ZLNEXT) specified'
           END-IF

           PERFORM 260-MOCK-STATUS-REPORT

           EXIT.

      ******************************************************************
      * Create EXEC-LOG data that's empty.                             *
      ******************************************************************
       210-MOCK-EXEC-LOG-EMPTY.

           DISPLAY 'ZTTRNDY 210-MOCK-EXEC-LOG-EMPTY'

      *-----------------------------------------------------------------
      * Create a mock of the EXEC-LOG file.
      *-----------------------------------------------------------------
           MOVE LOW-VALUES TO I_MOCKQSAM
           MOVE 'EXLOG' TO FILENAME IN ZWS_MOCKQSAM
           MOVE 80 TO RECORDSIZE IN ZWS_MOCKQSAM
           CALL ZTESTUT USING ZWS_MOCKQSAM,
                QSAMOBJECT IN WS-ZQSAM-EXEC-LOG

           EXIT.

      ******************************************************************
      * Create EXEC-LOG data from bespoke, trivial data.               *
      ******************************************************************
       200-MOCK-EXEC-LOG-GENERATED.

           DISPLAY 'ZTTRNDY 200-MOCK-EXEC-LOG-GENERATED'

           PERFORM 210-MOCK-EXEC-LOG-EMPTY

      *-----------------------------------------------------------------
      * Use a fixed test date and hour, then add exactly 180 records
      * covering a period of 3 hours.
      *-----------------------------------------------------------------
           MOVE SPACES TO ELR-EXEC-LOG-RECORD
           INITIALIZE ELR-EXEC-LOG-RECORD
           MOVE '0000-00-00 00:00:00' TO ELR-EXEC-LOG-RECORD

           MOVE TD-YEAR TO ELR-YEAR
           MOVE TD-MONTH TO ELR-MONTH
           MOVE TD-DAY TO ELR-DAY

           MOVE 'RAL' TO ELR-SYSTEM-ID

      *-----------------------------------------------------------------
      * Add a record every minute on the minute (EMOM) for 3 hours.
      *-----------------------------------------------------------------
           PERFORM VARYING ELR-HOUR FROM 1 BY 1 UNTIL ELR-HOUR > 3
                PERFORM VARYING ELR-MINUTE FROM 0 BY 1
                          UNTIL ELR-MINUTE > 59

                     EVALUATE TRUE
      *-----------------------------------------------------------------
      * Initialize an error record.
      *-----------------------------------------------------------------
                          WHEN ELR-MINUTE = 13
                               SET ELR-RESPONSE-BAD TO TRUE
                               MOVE 12 TO ELR-RETURN-CODE
                               MOVE 500 TO ELR-CPU-TIME
                               MOVE 50 TO ELR-WORKLOAD
                               SET ELR-WORKLOAD-HIGH TO TRUE
                               SET ELR-TYPE-ERROR TO TRUE

      *-----------------------------------------------------------------
      * Initialize an invalid record.
      *-----------------------------------------------------------------
                          WHEN ELR-MINUTE = 42
                               SET ELR-RESPONSE-SLOW TO TRUE
                               MOVE 8 TO ELR-RETURN-CODE
                               MOVE 4000 TO ELR-CPU-TIME
                               MOVE 100 TO ELR-WORKLOAD
                               SET ELR-WORKLOAD-HIGH TO TRUE
                               SET ELR-TYPE-FORBID TO TRUE

      *-----------------------------------------------------------------
      * Initialize an "OK" record.
      *-----------------------------------------------------------------
                          WHEN ELR-MINUTE < 10
                               SET ELR-RESPONSE-OK TO TRUE
                               MOVE 0 TO ELR-RETURN-CODE
                               MOVE 200 TO ELR-CPU-TIME
                               MOVE 40 TO ELR-WORKLOAD
                               SET ELR-WORKLOAD-MED TO TRUE
                               SET ELR-TYPE-OK TO TRUE

      *-----------------------------------------------------------------
      * Initialize a slow record.
      *-----------------------------------------------------------------
                          WHEN ELR-MINUTE >= 20 AND ELR-MINUTE < 30
                               SET ELR-RESPONSE-SLOW TO TRUE
                               MOVE 4 TO ELR-RETURN-CODE
                               MOVE 300 TO ELR-CPU-TIME
                               MOVE 60 TO ELR-WORKLOAD
                               SET ELR-WORKLOAD-HIGH TO TRUE

      *-----------------------------------------------------------------
      * Initialize a perfectly fast record.
      *-----------------------------------------------------------------
                         WHEN OTHER
                               SET ELR-RESPONSE-FAST TO TRUE
                               MOVE 0 TO ELR-RETURN-CODE
                               MOVE 10 TO ELR-CPU-TIME
                               MOVE 50 TO ELR-WORKLOAD
                               SET ELR-WORKLOAD-HIGH TO TRUE
                               SET ELR-TYPE-OK TO TRUE
                    END-EVALUATE

      *-----------------------------------------------------------------
      * Now that the record is initialized, give it to the mock.
      *-----------------------------------------------------------------
                    MOVE LOW-VALUES TO I_ADDRECORD
                    SET FILEOBJECT IN ZWS_ADDRECORD TO
                        ADDRESS OF QSAMOBJECT IN WS-ZQSAM-EXEC-LOG
                    SET RECORDADDRESS IN ZWS_ADDRECORD TO
                        ADDRESS OF ELR-EXEC-LOG-RECORD
                    MOVE LENGTH OF ELR-EXEC-LOG-RECORD TO
                        RECORDSIZE IN ZWS_ADDRECORD
                    CALL ZTESTUT USING ZWS_ADDRECORD

                END-PERFORM
           END-PERFORM

      *-----------------------------------------------------------------
      * TRENDY creates a HEALTH-TREND record at the start of a new
      * interval, so create a "kickoff" record to force that process.
      * This means we're adding 181 records to the EXEC-LOG mock.
      *-----------------------------------------------------------------
           MOVE '04' TO ELR-HOUR
           MOVE '00' TO ELR-MINUTE

           MOVE LOW-VALUES TO I_ADDRECORD
           SET FILEOBJECT IN ZWS_ADDRECORD TO
              ADDRESS OF QSAMOBJECT IN WS-ZQSAM-EXEC-LOG
           SET RECORDADDRESS IN ZWS_ADDRECORD TO
              ADDRESS OF ELR-EXEC-LOG-RECORD
           MOVE LENGTH OF ELR-EXEC-LOG-RECORD TO
              RECORDSIZE IN ZWS_ADDRECORD
           CALL ZTESTUT USING ZWS_ADDRECORD

      *-----------------------------------------------------------------
      * Set the expected results for 430-VERIFY-STATS-GETVARIABLE
      * (there are 181 records: 178 valid, 3 invalid)
      *-----------------------------------------------------------------
           SET WS-STOP-ON-FAIL TO TRUE
           INITIALIZE CHECK-STATS-REPORT-TOTALS
           MOVE SPACES TO CS-MESSAGE-TEXT
           MOVE 178 TO CS-VALID-EXEC-LOGS
           MOVE 141 TO CS-HEALTH-LOG-UPDATES
           MOVE 37 TO CS-HEALTH-LOG-ADDITIONS
           MOVE 3 TO CS-HEALTH-TREND-COUNT
           MOVE 3 TO CS-ERROR-EXEC-LOGS
           MOVE 3 TO CS-INVALID-EXEC-LOGS

      *-----------------------------------------------------------------
      * Generated date/times are set dynamically and will be
      * set later; for now, initialize to an "empty" value.
      *-----------------------------------------------------------------
           MOVE '0000-00-00 00:00' TO CS-START-INTERVAL
           MOVE '0000-00-00 00:00' TO CS-END-INTERVAL

      *-----------------------------------------------------------------
      * Set the start/end intervals that are expected; they're
      * rounded to the log records that started at 01:00
      * and continued until 04:00.
      *
      * NB: The last "kickoff" log record at 04:00 is in the interval
      *     04:00 to 04:05, hence the 5 minute end time difference.
      *-----------------------------------------------------------------
           MOVE '01' TO ELR-HOUR
           MOVE '00' TO ELR-MINUTE
           MOVE ELR-DATE-TIME TO CS-START-INTERVAL
           MOVE '04' TO ELR-HOUR
           MOVE '05' TO ELR-MINUTE
           MOVE ELR-DATE-TIME TO CS-END-INTERVAL

           EXIT.

      ******************************************************************
      * Create EXEC-LOG data from recorded data using LoadData.        *
      ******************************************************************
       230-MOCK-EXEC-LOG-LOADDATA.

           DISPLAY 'ZTTRNDY 230-MOCK-EXEC-LOG-LOADDATA'

      *-----------------------------------------------------------------
      * Use EXEC-LOG data from a previous recording.
      *-----------------------------------------------------------------
           MOVE LOW-VALUES TO I_LOADDATA
           MOVE 'ZTPTRNDY' TO MEMBERNAME IN ZWS_LOADDATA
           CALL ZTESTUT USING ZWS_LOADDATA, 
                LOADOBJECT IN WS-ZDATA-LOADDATA

      *-----------------------------------------------------------------
      * Create a mock of the EXEC-LOG file.
      *-----------------------------------------------------------------
           MOVE LOW-VALUES TO I_MOCKQSAM
           MOVE 'EXLOG' TO FILENAME IN ZWS_MOCKQSAM
           SET LOADOBJECT IN ZWS_MOCKQSAM 
                TO LOADOBJECT IN WS-ZDATA-LOADDATA
           MOVE 80 TO RECORDSIZE IN ZWS_MOCKQSAM
           CALL ZTESTUT USING ZWS_MOCKQSAM,
                QSAMOBJECT IN WS-ZQSAM-EXEC-LOG

      *-----------------------------------------------------------------
      * Set the expected results for 430-VERIFY-STATS-GETVARIABLE
      *-----------------------------------------------------------------
           SET WS-STOP-ON-FAIL TO TRUE
           INITIALIZE CHECK-STATS-REPORT-TOTALS
           MOVE SPACES TO CS-MESSAGE-TEXT
           MOVE 100 TO CS-VALID-EXEC-LOGS
           MOVE 93 TO CS-HEALTH-LOG-UPDATES
           MOVE 7 TO CS-HEALTH-LOG-ADDITIONS
           MOVE 1 TO CS-HEALTH-TREND-COUNT
           MOVE 2 TO CS-ERROR-EXEC-LOGS
           MOVE 0 TO CS-INVALID-EXEC-LOGS

      *-----------------------------------------------------------------
      * Set the start/end intervals that are expected; they're
      * rounded to the log record times.
      *-----------------------------------------------------------------
           MOVE '2024-05-26 10:30' TO CS-START-INTERVAL
           MOVE '2024-05-26 11:05' TO CS-END-INTERVAL

           EXIT.

      ******************************************************************
      * The mock for HEALTH-LOG is input/output and initially empty.
      ******************************************************************
       240-MOCK-HEALTH-LOG.

           DISPLAY 'ZTTRNDY 240-MOCK-HEALTH-LOG'

      *-----------------------------------------------------------------
      * Create a KSDS mock for the HEALTH-LOG file that's empty.
      *-----------------------------------------------------------------
           MOVE LOW-VALUES TO I_MOCKKSDS
           MOVE 'HLLOG' TO FILENAME IN ZWS_MOCKKSDS
           MOVE 1 TO KEYOFFSET IN ZWS_MOCKKSDS
           MOVE 16 TO KEYLENGTH IN ZWS_MOCKKSDS
           MOVE 80 TO RECORDSIZE IN ZWS_MOCKKSDS
           CALL ZTESTUT USING ZWS_MOCKKSDS,
                KSDSOBJECT IN WS-ZKSDS-HEALTH-LOG

           EXIT.

      ******************************************************************
      * The mock for HEALTH-TREND is output only.                      *
      ******************************************************************
       250-MOCK-HEALTH-TREND.

           DISPLAY 'ZTTRNDY 250-MOCK-HEALTH-TREND'

      *-----------------------------------------------------------------
      * Create a QSAM mock for the HEALTH-TREND file that's empty.
      *-----------------------------------------------------------------
           MOVE LOW-VALUES TO I_MOCKQSAM
           MOVE 'HLTREND' TO FILENAME IN ZWS_MOCKQSAM
           MOVE 80 TO RECORDSIZE IN ZWS_MOCKQSAM
           CALL ZTESTUT USING ZWS_MOCKQSAM,
                QSAMOBJECT IN WS-ZQSAM-HEALTH-TREND

      *-----------------------------------------------------------------
      * See 440-VERIFY-HEALTH-TREND-COMP and 250-CREATE-HARNESS-PARTIAL
      * for usage of LS-ZQSAM-HEALTH-TREND-FILE.
      *
      * This is a shared QSAM object, depending on whether the mock
      * was created explicitly as above, or implicitly created by the
      * _CREATEHARNESS API.
      *-----------------------------------------------------------------
           SET ADDRESS OF LS-ZQSAM-HEALTH-TREND-FILE
                TO ADDRESS OF WS-ZQSAM-HEALTH-TREND

           EXIT.

      ******************************************************************
      * The mock for STATS-REPORT is output only.                      *
      ******************************************************************
       260-MOCK-STATUS-REPORT.

           DISPLAY 'ZTTRNDY 260-MOCK-STATUS-REPORT'

      *-----------------------------------------------------------------
      * Create a QSAM mock for the STATS-REPORT file that's empty.
      *-----------------------------------------------------------------
           MOVE LOW-VALUES TO I_MOCKQSAM
           MOVE 'STREPORT' TO FILENAME IN ZWS_MOCKQSAM
           MOVE 80 TO RECORDSIZE IN ZWS_MOCKQSAM
           CALL ZTESTUT USING ZWS_MOCKQSAM,
                QSAMOBJECT IN WS-ZQSAM-STATS-REPORT

           EXIT.

      ******************************************************************
      * Spy on the variable RPT-STATS-REPORT-TOTALS in TRENDY
      * so it can be validated once the program ends.
      ******************************************************************
       310-SPY-VARIABLE-STATS.

           DISPLAY 'ZTTRNDY 310-SPY-VARIABLE-STATS'

      *-----------------------------------------------------------------
      * After TRENDY processes all the EXEC-LOG records, it
      * writes a final report. To validate its processing,
      * verify the totals look reasonable.
      *-----------------------------------------------------------------
           MOVE LOW-VALUES TO I_SPYVARIABLE
           MOVE 'ZTPTRNDY' TO MODULENAME IN ZWS_SPYVARIABLE
           MOVE 'ZTPTRNDY' TO FUNCTIONNAME IN ZWS_SPYVARIABLE
           MOVE 'RPT-STATS-REPORT-TOTALS' TO
                VARIABLENAME IN ZWS_SPYVARIABLE
           CALL ZTESTUT USING ZWS_SPYVARIABLE,
                VARIABLESPYOBJECT IN WS-ZSPVAR-STATS-TOTALS

           EXIT.

      ******************************************************************
      * CALLBACK: Register a spy on the STATS-REPORT. This spy's       *
      *           callback is given the option of changing the actual  *
      *           middleware result the SUT sees.                      *
      *                                                                *
      *           In this case, the callback will force an I/O error   *
      *           verify TRENDY reports it with a non-zero return      *
      *           code.                                                *
      ******************************************************************
       270-FORCE-ERROR-STATS-REPORT.

           MOVE LOW-VALUES TO I_SPYQSAM
           MOVE 'STREPORT' TO FILENAME IN ZWS_SPYQSAM
           SET CALLBACK IN ZWS_SPYQSAM
                TO ENTRY 'forceStatsReportError'
           CALL ZTESTUT USING ZWS_SPYQSAM, 
                QSAMSPYOBJECT IN WS-ZSPQSAM-REPORT-ERROR-SPY

           EXIT.

      ******************************************************************
      * Establish file I/O spies in TRENDY used for post-execution     *
      * validation.                                                    *
      ******************************************************************
       320-SPY-QSAM-FILE-OPS.

           MOVE LOW-VALUES TO I_SPYQSAM
           MOVE 'EXLOG' TO FILENAME IN ZWS_SPYQSAM
           CALL ZTESTUT USING ZWS_SPYQSAM,
                QSAMSPYOBJECT IN WS-ZSPQSAM-EXEC-LOG-SPY

           MOVE LOW-VALUES TO I_SPYQSAM
           MOVE 'HLTREND' TO FILENAME IN ZWS_SPYQSAM
           CALL ZTESTUT USING ZWS_SPYQSAM,
                QSAMSPYOBJECT IN WS-ZSPQSAM-HEALTH-TREND-SPY

           MOVE LOW-VALUES TO I_SPYQSAM
           MOVE 'STREPORT' TO FILENAME IN ZWS_SPYQSAM
           CALL ZTESTUT USING ZWS_SPYQSAM,
                QSAMSPYOBJECT IN WS-ZSPQSAM-REPORT-SPY

           EXIT.

      ******************************************************************
      * Establish file I/O spies in TRENDY (with callback).            *
      ******************************************************************
       330-SPY-KSDS-FILE-OPS.

           DISPLAY 'ZTTRNDY 330-SPY-KSDS-FILE-OPS'

           MOVE 0 TO CHECK-SPY-TREND-HEALTH-WRITE
           MOVE 0 TO CHECK-SPY-TREND-HEALTH-REWRITE

      *-----------------------------------------------------------------
      * Create the mock and establish a callback for file operations.
      *-----------------------------------------------------------------
           MOVE LOW-VALUES TO I_SPYKSDS
           MOVE 'HLLOG' TO FILENAME IN ZWS_SPYKSDS
           SET CALLBACK IN ZWS_SPYKSDS
                TO ENTRY 'spyHealthTrendCallback'
           CALL ZTESTUT USING ZWS_SPYKSDS,
                KSDSSPYOBJECT IN WS-ZSPKSDS-HEALTH-LOG-SPY

           EXIT.

      ******************************************************************
      * Set a section breakpoint in TRENDY just before it exits.       *
      ******************************************************************
       360-BREAKPOINT-STATS-REPORT.

           DISPLAY 'ZTTRNDY 360-BREAKPOINT-STATS-REPORT'

      *-----------------------------------------------------------------
      * After TRENDY processes all the EXEC-LOG records, it
      * writes a final report. Intercept at that point and
      * verify the totals look reasonable.
      *-----------------------------------------------------------------
           MOVE LOW-VALUES TO I_SETBREAKPOINT
           SET CALLBACK IN ZWS_SETBREAKPOINT
                TO ENTRY 'writeStatsReportBreakpoint'
           MOVE 'ZTPTRNDY' TO MODULENAME IN ZWS_SETBREAKPOINT
           MOVE 'ZTPTRNDY' TO FUNCTIONNAME IN ZWS_SETBREAKPOINT
           MOVE '300-WRITE-STATS-REPORT' TO
                SECTIONNAME IN ZWS_SETBREAKPOINT
           CALL ZTESTUT USING ZWS_SETBREAKPOINT

           EXIT.

      ******************************************************************
      * Establish section/paragraph spies in TRENDY.                   *
      ******************************************************************
       340-SPY-BASIC-EXECUTION.

           DISPLAY 'ZTTRNDY 340-SPY-BASIC-EXECUTION'

           MOVE LOW-VALUES TO I_SPYSECTION
           MOVE 'ZTPTRNDY' TO MODULENAME IN ZWS_SPYSECTION
           MOVE 'ZTPTRNDY' TO FUNCTIONNAME IN ZWS_SPYSECTION
           MOVE '500-END-PROGRAM' TO SECTIONNAME IN ZWS_SPYSECTION
           CALL ZTESTUT USING ZWS_SPYSECTION,
              SECTIONSPYOBJECT IN WS-ZSPSECT-END-PROGRAM-SPY

           EXIT.

      ******************************************************************
      * Establish variable watch in TRENDY. This watch variable is     *
      * used for validating the calculated totals match expectations.  *
      ******************************************************************
       350-WATCH-STATS-REPORT-TOTALS.

           DISPLAY 'ZTTRNDY 350-WATCH-STATS-REPORT-TOTALS'

           MOVE LOW-VALUES TO I_WATCHVARIABLE
           SET CALLBACK IN ZWS_WATCHVARIABLE
                TO ENTRY 'watchStatsReportTotals'
           MOVE 'ZTPTRNDY' TO MODULENAME IN ZWS_WATCHVARIABLE
           MOVE 'ZTPTRNDY' TO FUNCTIONNAME IN ZWS_WATCHVARIABLE
           MOVE 'RPT-STATS-REPORT-TOTALS' TO
                VARIABLENAME IN ZWS_WATCHVARIABLE
           CALL ZTESTUT USING ZWS_WATCHVARIABLE

           EXIT.

      ******************************************************************
      * Establish variable watch in TRENDY. We use this watch          *
      * variable callback to modify the EXEC-LOG record to force       *
      * an "unhappy" path for invalid input data.                      *
      *                                                                *
      * See changeExecLogRecord for more details.                      *
      ******************************************************************
       340-WATCH-EXEC-LOG-RECORD.

           DISPLAY 'ZTTRNDY 340-WATCH-EXEC-LOG-RECORD'

           MOVE LOW-VALUES TO I_WATCHVARIABLE
           SET CALLBACK IN ZWS_WATCHVARIABLE
                TO ENTRY 'changeExecLogRecord'
           MOVE 'ZTPTRNDY' TO MODULENAME IN ZWS_WATCHVARIABLE
           MOVE 'ZTPTRNDY' TO FUNCTIONNAME IN ZWS_WATCHVARIABLE
           MOVE 'ELR-EXEC-LOG-RECORD' TO
                VARIABLENAME IN ZWS_WATCHVARIABLE
           CALL ZTESTUT USING ZWS_WATCHVARIABLE

           EXIT.

      ******************************************************************
      * Any file I/O errors?                                           *
      ******************************************************************
       410-ASSERT-BASIC-EXECUTION.

           DISPLAY 'ZTTRNDY 410-ASSERT-BASIC-EXECUTION'

      *-----------------------------------------------------------------
      * Were there any file I/O errors?
      *-----------------------------------------------------------------
           MOVE LOW-VALUES TO I_ASSERTNOERRORSQSAM
           SET SPYOBJECT IN ZWS_ASSERTNOERRORSQSAM TO
               ADDRESS OF QSAMSPYOBJECT IN WS-ZSPQSAM-EXEC-LOG-SPY
           CALL ZTESTUT USING ZWS_ASSERTNOERRORSQSAM

           MOVE LOW-VALUES TO I_ASSERTNOERRORSKSDS
           SET SPYOBJECT IN ZWS_ASSERTNOERRORSKSDS TO
               ADDRESS OF KSDSSPYOBJECT IN WS-ZSPKSDS-HEALTH-LOG-SPY
           CALL ZTESTUT USING ZWS_ASSERTNOERRORSKSDS

           MOVE LOW-VALUES TO I_ASSERTNOERRORSQSAM
           SET SPYOBJECT IN ZWS_ASSERTNOERRORSQSAM TO
               ADDRESS OF QSAMSPYOBJECT IN WS-ZSPQSAM-HEALTH-TREND-SPY
           CALL ZTESTUT USING ZWS_ASSERTNOERRORSQSAM

           MOVE LOW-VALUES TO I_ASSERTNOERRORSQSAM
           SET SPYOBJECT IN ZWS_ASSERTNOERRORSQSAM TO
               ADDRESS OF QSAMSPYOBJECT IN WS-ZSPQSAM-REPORT-SPY
           CALL ZTESTUT USING ZWS_ASSERTNOERRORSQSAM

      *-----------------------------------------------------------------
      * Did TRENDY end cleanly? It's a minor test, but catches
      * the case where STOP RUN was called due to a major error.
      * The 500-END-PROGRAM is the last paragraph PERFORMed before
      * TRENDY exits.
      *-----------------------------------------------------------------
           MOVE LOW-VALUES TO I_ASSERTCALLEDONCESECTION
           SET SPYOBJECT IN ZWS_ASSERTCALLEDONCESECTION TO
               ADDRESS OF SECTIONSPYOBJECT IN WS-ZSPSECT-END-PROGRAM-SPY
           CALL ZTESTUT USING ZWS_ASSERTCALLEDONCESECTION

           EXIT.

      ******************************************************************
      * Compare TRENDY's actual stats with the expected ones using     *
      * a spied variable captured by Test4z during execution.          *
      *                                                                *
      * This paragraph and 430-VERIFY-STATS-GETVARIABLE demonstrate    *
      * two different ways of validating variable values; this one     *
      * can be done after SUT termination.                             *
      ******************************************************************
       420-VERIFY-SPY-VARIABLE-STATS.

           DISPLAY '==> ZTTRNDY verifying STATS-REPORT-TOTALS '
                'using SpyVariable API'

      *-----------------------------------------------------------------
      * In the unlikely event that the SUT wasn't compiled with
      * DWARF (TEST) data, the variable can't be watched. Since we're
      * checking LASTCALL outside of a callback, it's _possible_ that
      * it's NULL. It is a little overkill as this is an edge case
      * that should never occur. But since it may confuse, let's
      * fail the testcase (rather than abend with an 0C4).
      *-----------------------------------------------------------------
           IF LASTCALL IN WS-ZSPVAR-STATS-TOTALS = NULL
                MOVE SPACES TO WS-FAIL-MESSAGE
                MOVE '420-VERIFY-STATS missing DWARF data'
                     TO WS-FAIL-MESSAGE
                PERFORM 500-FAIL-TEST
                EXIT PARAGRAPH
           END-IF

           SET ADDRESS OF ZLS_VARIABLE_CALL TO
               LASTCALL IN WS-ZSPVAR-STATS-TOTALS
           SET ADDRESS OF LPT-STATS-REPORT-TOTALS TO
                PTR IN ZLS_VARIABLE_CALL

      *-----------------------------------------------------------------
      * For debug purposes, show what was expected and gotten.
      *-----------------------------------------------------------------
           DISPLAY 'Expected=' CS-START-INTERVAL
           DISPLAY '     Got=' LPT-START-INTERVAL
           DISPLAY 'Expected=' CS-END-INTERVAL
           DISPLAY '     Got=' LPT-END-INTERVAL
           DISPLAY 'Expected=' CS-DATA
           DISPLAY '     Got=' LPT-DATA

      *-----------------------------------------------------------------
      * The unit test initializes CS-DATA to the expected values;
      * LPT-DATA is a reference to TRENDY's stats report.
      *-----------------------------------------------------------------
           IF CHECK-STATS-REPORT-TOTALS NOT = LPT-STATS-REPORT-TOTALS
                DISPLAY 'ZTTRNDY 420-VERIFY-STATS failed: '
                DISPLAY 'Expected: ' CHECK-STATS-REPORT-TOTALS
                DISPLAY '     Got: ' LPT-STATS-REPORT-TOTALS

                MOVE SPACES TO WS-FAIL-MESSAGE
                MOVE '420-VERIFY-STATS failed' TO WS-FAIL-MESSAGE
                PERFORM 500-FAIL-TEST
           ELSE
                DISPLAY 'ZTTRNDY 420-VERIFY-STATS passed '
                     'using SpyVariable API'
           END-IF

           EXIT.

      ******************************************************************
      * Compare TRENDY's actual stats with the expected ones by        *
      * retrieving the value directly from the SUT.                    *
      *                                                                *
      * This paragraph and 420-VERIFY-SPY-VARIABLE-STATS demonstrate   *
      * two different ways of validating variable values; this one     *
      * combines SetBreakpoint on a SUT paragraph and GetVariable      *
      * to retrieve the SUT's variable value at runtime.               *
      ******************************************************************
       430-VERIFY-STATS-GETVARIABLE.

           DISPLAY '==> ZTTRNDY verifying STATS-REPORT-TOTALS '
                'using SetBreakpoint and GetVariable APIs'

           MOVE LOW-VALUES TO I_GETVARIABLE
           MOVE 'RPT-STATS-REPORT-TOTALS' TO
                VARIABLENAME IN ZWS_GETVARIABLE
           CALL ZTESTUT USING ZWS_GETVARIABLE,
              ADDRESS OF LPT-STATS-REPORT-TOTALS

      *-----------------------------------------------------------------
      * For debug purposes, show what was expected and gotten.
      *-----------------------------------------------------------------
           DISPLAY 'Expected=' CS-START-INTERVAL
           DISPLAY '     Got=' LPT-START-INTERVAL
           DISPLAY 'Expected=' CS-END-INTERVAL
           DISPLAY '     Got=' LPT-END-INTERVAL
           DISPLAY 'Expected=' CS-DATA
           DISPLAY '     Got=' LPT-DATA

      *-----------------------------------------------------------------
      * The unit test initializes CS-DATA to the expected values;
      * LPT-DATA is a reference to TRENDY's stats report.
      *-----------------------------------------------------------------
           IF CHECK-STATS-REPORT-TOTALS NOT = LPT-STATS-REPORT-TOTALS
                DISPLAY 'ZTTRNDY 430-VERIFY-STATS failed: '
                     CS-MESSAGE-TEXT

                MOVE SPACES TO WS-FAIL-MESSAGE
                MOVE '430-VERIFY-STATS failed' TO WS-FAIL-MESSAGE
                PERFORM 500-FAIL-TEST
           ELSE
                DISPLAY 'ZTTRNDY 430-VERIFY-STATS passed '
                     'using SetBreakpoint and GetVariable APIs'
           END-IF

           EXIT.

      ******************************************************************
      * Compare TRENDY's actual health trend records with the expected *
      * retrieving the value directly from the SUT.                    *
      ******************************************************************
       440-VERIFY-HEALTH-TREND-COMP.

           DISPLAY '==> ZTTRNDY verifying HEALTH-TREND by examining '
                'output records using MockQSAM API object'

      *-----------------------------------------------------------------
      * This generated test data is shared by two unit tests. Although
      * they share the same input data, Test4z callbacks are used to
      * affect the outcome. Subsequently the comparison records are
      * changed and need to be correctly associated with the underlying
      * test.
      *-----------------------------------------------------------------
           IF RUNNING-UNIT-TEST-1
                MOVE CHECK-HRT-GENERATED
                     TO CHECK-HRT-COMPARE-RECORDS
                MOVE 3 TO CHECK-HRT-COMPARE-CNT
           ELSE
                MOVE CHECK-HRT-GENERATED-STUB
                     TO CHECK-HRT-COMPARE-RECORDS
                MOVE 3 TO CHECK-HRT-COMPARE-CNT
           END-IF

      *-----------------------------------------------------------------
      * The generated EXEC-LOG records have today's date.
      * Those are included in the HEALTH-TREND records. So, to
      * compare against the expected result, the comparison
      * records need to be set to today's date. The rest of the
      * values (time, metrics) are defined in the static expected value.
      *-----------------------------------------------------------------
           MOVE TD-YEAR TO CHT-YEAR(1)
           MOVE TD-YEAR TO CHT-YEAR(2)
           MOVE TD-YEAR TO CHT-YEAR(3)
           MOVE TD-MONTH TO CHT-MONTH(1)
           MOVE TD-MONTH TO CHT-MONTH(2)
           MOVE TD-MONTH TO CHT-MONTH(3)
           MOVE TD-DAY TO CHT-DAY(1)
           MOVE TD-DAY TO CHT-DAY(2)
           MOVE TD-DAY TO CHT-DAY(3)

           SET ADDRESS OF LS-QSAM-HEALTH-TREND-RECORDS TO
                PTR IN RECORDS_ IN LS-ZQSAM-HEALTH-TREND-FILE

      *-----------------------------------------------------------------
      * Compare the record actual and expected values.
      *-----------------------------------------------------------------
           PERFORM VARYING I FROM 1 BY 1 UNTIL
                     NOT (I <= SIZE_
                     IN RECORDS_ IN LS-ZQSAM-HEALTH-TREND-FILE)

                IF I <= CHECK-HRT-COMPARE-CNT
                     DISPLAY 'Expected=' CHECK-HRT-COMPARE-RECORD(I)
                     DISPLAY '     Got=' LS-QSAM-HEALTH-TREND-RECORD(I)

                     IF LS-QSAM-HEALTH-TREND-RECORD(I) NOT =
                          CHECK-HRT-COMPARE-RECORD(I)

                          MOVE SPACES TO WS-FAIL-MESSAGE
                          MOVE '440-VERIFY-HEALTH-TREND failed'
                               TO WS-FAIL-MESSAGE
                          PERFORM 500-FAIL-TEST
                     END-IF
                ELSE
                     DISPLAY 'Expected=index in range 1 to '
                          CHECK-HRT-COMPARE-CNT
                     DISPLAY '     Got=' I

                     MOVE SPACES TO WS-FAIL-MESSAGE
                     MOVE '440-VERIFY-HEALTH-TREND failed'
                          TO WS-FAIL-MESSAGE
                     PERFORM 500-FAIL-TEST
                END-IF
           END-PERFORM

           IF SIZE_ IN RECORDS_ IN LS-ZQSAM-HEALTH-TREND-FILE
                     NOT = CHECK-HRT-COMPARE-CNT
                DISPLAY 'Expected=' CHECK-HRT-COMPARE-CNT ' records'
                DISPLAY '     Got='
                     SIZE_ IN RECORDS_ IN LS-ZQSAM-HEALTH-TREND-FILE
                     ' records'

                MOVE SPACES TO WS-FAIL-MESSAGE
                MOVE '440-VERIFY-HEALTH-TREND record count mismatch'
                     TO WS-FAIL-MESSAGE
                PERFORM 500-FAIL-TEST
           END-IF

      *-----------------------------------------------------------------
      * There's a total of 181 records, of which 3 are invalid.
      * So a total of 178 is expected. The writes vs. rewrites
      * is determined by the rounding of the log timestamps
      * to intervals; since the input is fixed, we know the expected
      * writes/rewrites, so verify them.
      *-----------------------------------------------------------------
           IF CHECK-SPY-TREND-HEALTH-WRITE NOT = 37 OR
                     CHECK-SPY-TREND-HEALTH-REWRITE NOT = 141

                DISPLAY '440-VERIFY-HEALTH-TREND incorrect,'
                     ' writes=' CHECK-SPY-TREND-HEALTH-WRITE
                     ' (expected 37)'
                     ' rewrites=' CHECK-SPY-TREND-HEALTH-REWRITE
                     ' (expected 141)'

                MOVE SPACES TO WS-FAIL-MESSAGE
                MOVE '440-VERIFY-HEALTH-TREND failed' TO WS-FAIL-MESSAGE
                PERFORM 500-FAIL-TEST
           END-IF

           EXIT.

      ******************************************************************
      * Compare TRENDY's actual health trend records with the expected *
      * retrieving the value directly from the SUT.                    *
      *                                                                *
      * NB: This code _could_ be consolidated with                     *
      *     440-VERIFY-HEALTH-TREND-COMP, but was left separate for    *
      *     ease of explanation given the former's generated date      *
      *     information set at runtime versus this one's static        *
      *     comparison of recorded data.                               *
      ******************************************************************
       450-VERIFY-HEALTH-TREND-RECD.

           DISPLAY '==> ZTTRNDY verifying HEALTH-TREND by examining '
                'output records using MockQSAM API object'

           SET ADDRESS OF LS-QSAM-HEALTH-TREND-RECORDS TO
                PTR IN RECORDS_ IN LS-ZQSAM-HEALTH-TREND-FILE

      *-----------------------------------------------------------------
      * Compare the record actual and expected values.
      *-----------------------------------------------------------------
           PERFORM VARYING I FROM 1 BY 1 UNTIL
                     NOT (I <= SIZE_
                     IN RECORDS_ IN LS-ZQSAM-HEALTH-TREND-FILE)

                IF I <= CHECK-HRT-RECORDED-CNT
                     DISPLAY 'Expected=' CHECK-HRT-RECORDED-RECORD(I)
                     DISPLAY '     Got=' LS-QSAM-HEALTH-TREND-RECORD(I)

                     IF LS-QSAM-HEALTH-TREND-RECORD(I) NOT =
                          CHECK-HRT-RECORDED-RECORD(I)

                          MOVE SPACES TO WS-FAIL-MESSAGE
                          MOVE '450-VERIFY-HEALTH-TREND failed'
                               TO WS-FAIL-MESSAGE
                          PERFORM 500-FAIL-TEST
                     END-IF
                ELSE
                     DISPLAY 'Expected=index in range 1 to '
                          CHECK-HRT-RECORDED-CNT
                     DISPLAY '     Got=' I

                     MOVE SPACES TO WS-FAIL-MESSAGE
                     MOVE '450-VERIFY-HEALTH-TREND failed'
                          TO WS-FAIL-MESSAGE
                     PERFORM 500-FAIL-TEST
                END-IF
           END-PERFORM

           IF SIZE_ IN RECORDS_ IN LS-ZQSAM-HEALTH-TREND-FILE
                     NOT = CHECK-HRT-RECORDED-CNT
                DISPLAY 'Expected=' CHECK-HRT-RECORDED-CNT ' records'
                DISPLAY '     Got='
                     SIZE_ IN RECORDS_ IN LS-ZQSAM-HEALTH-TREND-FILE
                     ' records'

                MOVE SPACES TO WS-FAIL-MESSAGE
                MOVE '450-VERIFY-HEALTH-TREND record count mismatch'
                     TO WS-FAIL-MESSAGE
                PERFORM 500-FAIL-TEST
           END-IF

      *-----------------------------------------------------------------
      * There's a total of 500 records, of which 7 are invalid.
      * So a total of 493 is expected. The writes vs. rewrites
      * is determined by the rounding of the log timestamps
      * to intervals; since the input is fixed, we know the expected
      * writes/rewrites, so verify them.
      *-----------------------------------------------------------------
           IF CHECK-SPY-TREND-HEALTH-WRITE NOT = 7 OR
                     CHECK-SPY-TREND-HEALTH-REWRITE NOT = 93

                DISPLAY '450-VERIFY-HEALTH-TREND incorrect,'
                     ' writes=' CHECK-SPY-TREND-HEALTH-WRITE
                     ' (expected 7)'
                     ' rewrites=' CHECK-SPY-TREND-HEALTH-REWRITE
                     ' (expected 93)'

                MOVE SPACES TO WS-FAIL-MESSAGE
                MOVE '450-VERIFY-HEALTH-TREND failed' TO WS-FAIL-MESSAGE
                PERFORM 500-FAIL-TEST
           END-IF

           EXIT.

      ******************************************************************
      * Compare TRENDY's actual return code with the expected one.     *
      * Normally it's 0, but it could be 4 for warnings, or 8 or 12    *
      * for serious file I/O problems.                                 *
      ******************************************************************
       460-VERIFY-RETURN-CODE.

           DISPLAY 'ZTTRNDY 460-VERIFY-RETURN-CODE'

           IF WS-RETURN-CODE-SUT NOT = CS-EXPECTED-RETURN-CODE
                MOVE SPACES TO WS-FAIL-MESSAGE
                MOVE '460-VERIFY-RETURN-CODE failed' TO WS-FAIL-MESSAGE

                DISPLAY WS-FAIL-MESSAGE
                DISPLAY 'Expected=' CS-EXPECTED-RETURN-CODE
                DISPLAY '     Got=' WS-RETURN-CODE-SUT

                PERFORM 500-FAIL-TEST
           END-IF

           EXIT.

      ******************************************************************
      * Add a message to ZLMSG, or optionally end the test.            *
      ******************************************************************
       500-FAIL-TEST.

           DISPLAY 'ZTTRNDY 500-FAIL-TEST'

           IF WS-STOP-ON-FAIL AND NOT WS-DEBUG-MSG-ON-FAIL
                MOVE LOW-VALUES TO I_FAIL IN ZWS_FAIL
                MOVE WS-FAIL-MESSAGE TO FAILMESSAGE IN ZWS_FAIL
                CALL ZTESTUT USING ZWS_FAIL
           ELSE
                MOVE LOW-VALUES TO I_MESSAGE IN ZWS_MESSAGE
                MOVE WS-FAIL-MESSAGE TO MESSAGETEXT IN ZWS_MESSAGE
                CALL ZTESTUT USING ZWS_MESSAGE

                MOVE LOW-VALUES TO I_MESSAGE IN ZWS_MESSAGE
                MOVE '[CONTINUING DESPITE REPORTED FAILURE]'
                     TO MESSAGETEXT IN ZWS_MESSAGE
                CALL ZTESTUT USING ZWS_MESSAGE
           END-IF

           EXIT.

      ******************************************************************
      * Various print routines to dump the file contents (debug only). *
      ******************************************************************
       600-PRINT-MOCKED-EXEC-LOG.

           IF WS-DEBUG-ON
                DISPLAY '==> ZTTRNDY printing EXEC-LOG'
                MOVE LOW-VALUES TO I_PRINTFILE
                SET FILEOBJECT IN ZWS_PRINTFILE TO
                     ADDRESS OF QSAMOBJECT IN WS-ZQSAM-EXEC-LOG
                CALL ZTESTUT USING ZWS_PRINTFILE
           END-IF

           EXIT.

       610-PRINT-MOCKED-HEALTH-LOG.

           IF WS-DEBUG-ON
                DISPLAY '==> ZTTRNDY printing HEALTH-LOG'
                MOVE LOW-VALUES TO I_PRINTFILE
                SET FILEOBJECT IN ZWS_PRINTFILE TO
                     ADDRESS OF KSDSOBJECT IN WS-ZKSDS-HEALTH-LOG
                CALL ZTESTUT USING ZWS_PRINTFILE
           END-IF

           EXIT.

       620-PRINT-MOCKED-HEALTH-TREND.

           IF WS-DEBUG-ON
                DISPLAY '==> ZTTRNDY printing HEALTH-TREND'
                MOVE LOW-VALUES TO I_PRINTFILE
                SET FILEOBJECT IN ZWS_PRINTFILE TO
                     ADDRESS OF QSAMOBJECT IN LS-ZQSAM-HEALTH-TREND-FILE
                CALL ZTESTUT USING ZWS_PRINTFILE
           END-IF

           EXIT.

       630-PRINT-MOCKED-STATS-REPORT.

           IF WS-DEBUG-ON
                DISPLAY '==> ZTTRNDY printing STATS-REPORT'
                MOVE LOW-VALUES TO I_PRINTFILE
                SET FILEOBJECT IN ZWS_PRINTFILE TO
                     ADDRESS OF QSAMOBJECT IN WS-ZQSAM-STATS-REPORT
                CALL ZTESTUT USING ZWS_PRINTFILE
           END-IF

           EXIT.

       640-PRINT-STATS-REPORT-TOTALS.

           DISPLAY '==> ZTTRNDY displaying stats totals'

           DISPLAY 'start=' LPT-START-INTERVAL ' end=' LPT-END-INTERVAL
           DISPLAY 'valid=' LPT-VALID-EXEC-LOGS
                ' updates=' LPT-HEALTH-LOG-UPDATES
                ' additions=' LPT-HEALTH-LOG-ADDITIONS
                ' trends=' LPT-HEALTH-TREND-COUNT
                ' errors=' LPT-ERROR-EXEC-LOGS
                ' invalid=' LPT-INVALID-EXEC-LOGS

           EXIT.
