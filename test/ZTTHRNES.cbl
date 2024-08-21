       PROCESS PGMN(LM),NODYNAM
       IDENTIFICATION DIVISION.
       PROGRAM-ID. 'ZTTHRNES' RECURSIVE.
       
      ******************************************************************
      * Broadcom Test4z System Under Test (SUT) example.               *
      * Copyright (c) 2024 Broadcom. All Rights Reserved.              *
      *                                                                *
      * The use of a test harness is the preferred method of using     *
      * recorded data. The test harness allows a user to define which  *
      * pieces of middleware are real and which are mocked.            *
      * Additionally, individual CSECTs within load modules can be     *
      * defined as real or mocked. By default, all middleware is       *
      * mocked with the recorded data and all programs are real.       *
      *                                                                *
      * This unit test suite validates the operation of three SUT      *
      * programs that collectively support the promotion               *
      * decision-making process of a fictional "widget" marketing      *
      * team:                                                          *
      *                                                                *
      * 1. ZTPHRNMM - main program that reads a list of "watched"      *
      *               widgets for possible sales promotion             *
      * 2. ZTPHRNDD - called program to fetch the associated recent    *
      *               sales data of the watched widgets (i.e.,         *
      *               last 30 days, updated regularly)                 *
      * 3. ZTPHRNAA - called program to analyze recent sales data      *
      *               and "score" popular widgets that are promotion   *
      *               candidates for the marketing team.               *
      *                                                                *
      * This unit test suite, ZTTHRNES, is responsible for validating  *
      * the correct operation of ZTPHRNMM/ZTPHRNDD and especially      *
      * the analytics program, ZTPHRNAA.                               *
      *                                                                *
      * This example demonstrates how to mock other resources like     *
      * files and called programs in order to test a given             *
      * program in isolation using the Test4z harness.                 *
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
      * ZWS_CREATEHARNESS -> I_CREATEHARNESS IN ZWS_CREATEHARNESS      *
      *                                                                *
      * The "I" interface sub-block should be initialized to           *
      * LOW-VALUES prior to calling ZTESTUT. For Test4z API control    *
      * blocks in the LINKAGE SECTION, look for the prefix ZLS_.       *
      ******************************************************************
       COPY ZTESTWS.

       01  WS-ZFILE-SALES-WATCH-FILE.
           COPY ZFILE.
       01  WS-ZQSAM-SALES-WATCH-MOCK.
           COPY ZQSAM.

       01  WS-ZFILE-PROMOREC-FILE.
           COPY ZFILE.
       01  WS-ZQSAM-PROMOREC-MOCK.
           COPY ZQSAM.

       01  WS-ZSPQSAM-PROMOREC-FILE-SPY.
           COPY ZSPQSAM.

       01  WS-ZHRNESS-DATA.
           COPY ZHRNESS.

       01  WS-ZHRNOPT-OPTIONS.
           02 HO-HARNESS-OPTION OCCURS 1 TIMES.
           COPY ZHRNOPT.

      *-----------------------------------------------------------------
      * These are input records for the mocked file SALESWCH processed
      * by the main program, ZTPHRNMM. See unit test
      * 'staticDataPromoSalesTest' for more details.
      *-----------------------------------------------------------------
       01 QSAM-SALES-WATCH-RECORDS.
           05 FILLER PIC X(7) VALUE 'B420042'.
           05 FILLER PIC X(7) VALUE 'B000217'.
           05 FILLER PIC X(7) VALUE 'M120146'.
           05 FILLER PIC X(7) VALUE 'P002893'.
           05 FILLER PIC X(7) VALUE 'B080712'.
           05 FILLER PIC X(7) VALUE 'B240924'.
           05 FILLER PIC X(7) VALUE 'B049846'.
           05 FILLER PIC X(7) VALUE 'M058673'.
           05 FILLER PIC X(7) VALUE 'B198723'.
           05 FILLER PIC X(7) VALUE 'B110906'.
          
      *-----------------------------------------------------------------
      * These are validation records for the mocked output file SALESPRM
      * produced by the main program, ZTPHRNMM. Because the Test4z
      * harness has provided recorded values for the data program
      * (ZTPHRNDD), the analytics called program (ZTPHRNAD) will always
      * get the same data to analyze, so the output report should be
      * unchanged. 
      * 
      * See unit test 'harnessDataPromoSalesTest' for more details.
      *-----------------------------------------------------------------
       01  WS-EXPECTED-PROMOS.
           05 FILLER PIC X(80) VALUE
                'Widget: B420042 Score:  9 Avg:   36 Max:   77 Hot: 3'.
           05 FILLER PIC X(80) VALUE
                'Widget: B000217 Score: 10 Avg:   40 Max:   79 Hot: 3'.
           05 FILLER PIC X(80) VALUE
                'Widget: M120146 Score:  7 Avg:  147 Max:  277 Hot: 2'.
           05 FILLER PIC X(80) VALUE
                'Widget: P002893 Score:  3 Avg: 1000 Max: 1594 Hot: 0'.
           05 FILLER PIC X(80) VALUE
                'Widget: B080712 Score:  9 Avg:   40 Max:   73 Hot: 2'.
           05 FILLER PIC X(80) VALUE
                'Widget: B240924 Score: 10 Avg:   43 Max:   77 Hot: 2'.
           05 FILLER PIC X(80) VALUE
                'Widget: B049846 Score: 10 Avg:   43 Max:   79 Hot: 3'.
           05 FILLER PIC X(80) VALUE
                'Widget: M058673 Score: 10 Avg:  107 Max:  271 Hot: 2'.
           05 FILLER PIC X(80) VALUE
                'Widget: B198723 Score:  9 Avg:   37 Max:   79 Hot: 2'.
           05 FILLER PIC X(80) VALUE
                'Widget: B110906 Score: 10 Avg:   44 Max:   80 Hot: 2'.
       
       01  WS-EXPECTED-PROMOS-REDEF REDEFINES WS-EXPECTED-PROMOS.
           05 WS-EXPECTED-PROMO-RECORDS OCCURS 10 TIMES.
                10 WS-EXPECTED-PROMO-RECORD PIC X(80).
       01  WS-EXPECTED-PROMOS-COUNT PIC 9(2) VALUE 10.

      ******************************************************************
      * Define any working-storage items that we need.                 *
      ******************************************************************

       01  I PIC 9(2).
       01  J PIC 9(2).

       LINKAGE SECTION.

      ******************************************************************
      * Copy in our required control blocks from Test4z.               *
      ******************************************************************
       COPY ZTESTLS.

       01  LS-ZQSAM-PROMOREC-MOCK.
           COPY ZQSAM.

      *-----------------------------------------------------------------
      * These linkage variables are used to validate the records
      * captured in the SALESPRM mock file.
      *-----------------------------------------------------------------
       01  LS-RECORD PIC X(80).
       01  LS-RECORD-HISTORIES.
           05 LS-RECORD-HISTORY PIC X(80) OCCURS UNBOUNDED.
           
       PROCEDURE DIVISION.

      ******************************************************************
      * Register two unit tests:                                       *
      *                                                                *
      * == Unit test #1 uses harness data from a previous recording    *
      *    of all input and output files as well as the two called     *
      *    programs, ZTPHRNDD and ZTPHRNAA. The unit test will then    *
      *    exclude ZTPHRNAA from the harness so it will be real, not   *
      *    mocked.                                                     *
      *                                                                *
      *    As a result, ZTPHRNAA will be provided an immutable         *
      *    environment (input file, called programs), so it can be     *
      *    validated independently with known inputs from all sources. *
      *                                                                *
      * == Unit test 2 will provide statically defined input data      *
      *    for the SALESWCH ("sales watch") file. The two called       *
      *    programs, ZTPHRNDD and ZTPHRNAA, will be real. As a         *
      *    consequence, the time-sensitive nature of the QSAM input    *
      *    (widgets on the marketing team's "watch list") and the      *
      *    recent sales are volatile, making it more difficult to      *
      *    validate ZTPHRNAA.                                          *
      *                                                                *
      *    In fact, because of this volatility, unit test 2 doesn't    *
      *    actually validate ZTPHRNAA's results; it only displays      *
      *    them to demonstrate the above point versus the easily       *
      *    validated results in unit test #1 with the Test4z harness.  *
      *                                                                *
      *    To see the difference between unit test 1 and 2, review the *
      *    SYSOUT logs. Notice that the SALESPRM output from one unit  *
      *    test is always the same and the other is always different.  *
      *                                                                *
      * NB: The implementation of ZTPHRNDD uses pseudo-random numbers  *
      *     to demonstrate the points above. It is for illustrative    *
      *     purposes only and should not be taken literally as an      *
      *     example of synthetic data generation.                      *
      ******************************************************************
           MOVE LOW-VALUES TO I_TEST
           SET TESTFUNCTION IN ZWS_TEST 
                TO ENTRY 'harnessDataPromoSalesTest'
           MOVE 'Unit test 1: Sales test w/harness data (validated)'
                TO TESTNAME IN ZWS_TEST
           CALL ZTESTUT USING ZWS_TEST

           MOVE LOW-VALUES TO I_TEST
           SET TESTFUNCTION IN ZWS_TEST
                TO ENTRY 'staticDataPromoSalesTest'
           MOVE 'Unit test 2: Sales test w/static data (not validated)'
                TO TESTNAME IN ZWS_TEST
           CALL ZTESTUT USING ZWS_TEST

           GOBACK.

      ******************************************************************
      * UNIT TEST 1: Demonstrate the result when executed with a       *
      *              recorded session, including the invocations       *
      *              of ZTPHRNDD, which retrieves sales data           *
      *              associated with the input widget IDs. However,    *
      *              sales the invocations of ZTPHRNAA, which "scores" *
      *              the performance will be called live.              *
      *                                                                *
      *              The validation of ZTPHRNAA is then immutable,     *
      *              because its input will not change, whereas if     *
      *              ZTPHRNDD was "live", ZTPHRNAA's input would       *
      *              change with each execution.                       *
      ******************************************************************
           ENTRY 'harnessDataPromoSalesTest'.

      *-----------------------------------------------------------------
      * The harness recording was created from a previous live
      * execution -- it includes the input file (SALESWCH), output
      * file (SALEPRM), as well as the responses from the two
      * called programs (ZTPHRNDD and ZTPHRNAA).
      *
      * For this unit test, we want to validate the analytics output
      * from ZTPHRNAA, thus we'l mock the input file and results
      * from ZTPHRNDD. This isolates the called program ZTPHRNAA,
      * so we can then assert its (now static) results are correct.
      *
      * NB: By default, middleware is mocked and programs are real.
      *     The harness option below changes that for ZTPHRNDD so 
      *     it will be mocked; the ZTPHRNAA called program will
      *     remain real.
      *-----------------------------------------------------------------
           MOVE LOW-VALUES TO I_CREATEHARNESS
           MOVE LOW-VALUES TO WS-ZHRNOPT-OPTIONS
           MOVE 'ZTPHRNES' TO MEMBERNAME IN ZWS_CREATEHARNESS

           SET MOCK_PROGRAM IN HO-HARNESS-OPTION(1) TO TRUE
           MOVE 'ZTPHRNDD' TO ARTIFACTNAMES IN HO-HARNESS-OPTION(1)
           MOVE 'ZTPHRNDD' TO MODULENAMES IN HO-HARNESS-OPTION(1)

           SET PTR IN MOCKOPTIONS IN ZWS_CREATEHARNESS TO
               ADDRESS OF WS-ZHRNOPT-OPTIONS
           MOVE 1 TO SIZ IN MOCKOPTIONS IN ZWS_CREATEHARNESS

           CALL ZTESTUT USING ZWS_CREATEHARNESS,
                HARNESSOBJECT IN WS-ZHRNESS-DATA

      *-----------------------------------------------------------------
      * To validate the results of ZTPHRNAA and ZTPHRNDD, spy on the
      * output file. This is a simple black box test, i.e., recorded
      * input (SALESWCH) and validate the live output (SALESPRM).
      *
      * Ask Test4z to run the main program, ZTPHRNMM.
      *-----------------------------------------------------------------
           MOVE LOW-VALUES TO I_SPYQSAM
           MOVE 'SALESPRM' TO FILENAME IN ZWS_SPYQSAM
           CALL ZTESTUT USING ZWS_SPYQSAM, 
                QSAMSPYOBJECT IN WS-ZSPQSAM-PROMOREC-FILE-SPY

           MOVE LOW-VALUES TO I_RUNFUNCTION
           MOVE 'ZTPHRNMM' TO MODULENAME IN ZWS_RUNFUNCTION
           CALL ZTESTUT USING ZWS_RUNFUNCTION

      *-----------------------------------------------------------------
      * The SUT has ended. Display the output using _PRINTFILE.
      *-----------------------------------------------------------------
           MOVE LOW-VALUES TO I_GETFILEOBJECT
           MOVE 'SALESPRM' TO FILENAME IN ZWS_GETFILEOBJECT
           SET HARNESSOBJECT IN ZWS_GETFILEOBJECT TO
                ADDRESS OF HARNESSOBJECT IN WS-ZHRNESS-DATA
           CALL ZTESTUT USING ZWS_GETFILEOBJECT,
                ADDRESS OF LS-ZQSAM-PROMOREC-MOCK

           DISPLAY 'ZTTHRNES begin print of SALESPRM (harness)'
           MOVE LOW-VALUES TO I_PRINTFILE
           SET FILEOBJECT IN ZWS_PRINTFILE TO
                ADDRESS OF QSAMOBJECT IN LS-ZQSAM-PROMOREC-MOCK
           CALL ZTESTUT USING ZWS_PRINTFILE
           DISPLAY 'ZTTHRNES end print of SALESPRM (harness)'

      *-----------------------------------------------------------------
      * Display the file I/O operations using the QSAM spy.
      *-----------------------------------------------------------------
           DISPLAY 'ZTTHRNES begin spy history of SALESPRM (harness)'
           
           MOVE SIZE_ IN CALLS IN WS-ZSPQSAM-PROMOREC-FILE-SPY TO J
           SET ADDRESS OF ZLS_QSAM_HISTORIES
                TO PTR IN CALLS IN WS-ZSPQSAM-PROMOREC-FILE-SPY

      *-----------------------------------------------------------------
      * A QSAM spy captures the command (OPEN, WRITE, READ, CLOSE, etc.)
      * and saves it in a history. For the READ and WRITE commands,
      * this history includes the record itself. Unit tests can use 
      * that history to validate the output of the QSAM file.
      *-----------------------------------------------------------------
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > J
                IF COMMAND IN ZLS_QSAM_HISTORY(I) = 'WRITE'
                     SET ADDRESS OF LS-RECORD
                          TO PTR IN RECORD_ IN ZLS_QSAM_HISTORY(I)
                     DISPLAY 
                          'ZTTHRNES WRITE   command ' I ' of ' J
                          ' record=' LS-RECORD
                ELSE
                     DISPLAY 
                          'ZTTHRNES skipped command ' I ' of ' J ' '
                          COMMAND IN ZLS_QSAM_HISTORY(I)
                END-IF
           END-PERFORM

           DISPLAY 'ZTTHRNES end spy history of SALESPRM (harness)'

      *-----------------------------------------------------------------
      * Display the output using the QSAM mock records. 
      *
      * NB: The mock file was retrieved from the harness, hence why
      *     the code below refers to LS-ZQSAM-PROMOREC-MOCK and not
      *     WS-ZQSAM-PROMOREC-MOCK, as is the case for the explicitly
      *     created mock file in unit test 2.
      *-----------------------------------------------------------------
           DISPLAY 'ZTTHRNES begin mock records of SALESPRM (harness)'

           MOVE SIZE_ IN RECORDS_ IN LS-ZQSAM-PROMOREC-MOCK TO J
           SET ADDRESS OF LS-RECORD-HISTORIES
                TO PTR IN RECORDS_ IN FILE_ IN LS-ZQSAM-PROMOREC-MOCK

           PERFORM VARYING I FROM 1 BY 1 UNTIL I > J
              DISPLAY I ' of ' J ': ' LS-RECORD-HISTORY(I)
           END-PERFORM.

           DISPLAY 'ZTTHRNES end mock records of SALESPRM (harness)'

      *-----------------------------------------------------------------
      * Validate the output using the captured QSAM mock records. 
      *-----------------------------------------------------------------
           IF WS-EXPECTED-PROMOS-COUNT NOT = J
                MOVE LOW-VALUES TO I_FAIL IN ZWS_FAIL
                MOVE 'ZTTHRNES record count mismatch for SALESPRM' 
                     TO FAILMESSAGE IN ZWS_FAIL

                DISPLAY FAILMESSAGE IN ZWS_FAIL
                DISPLAY 'Expected: ' WS-EXPECTED-PROMOS-COUNT
                DISPLAY '     Got: ' J

                CALL ZTESTUT USING ZWS_FAIL
           END-IF

      *-----------------------------------------------------------------
      * Calling _FAIL ends the unit test immediately. If we've
      * reached this point, the record count must be correct, so
      * verify the content of the records. 
      *-----------------------------------------------------------------
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > J
              IF WS-EXPECTED-PROMO-RECORD(I) NOT = LS-RECORD-HISTORY(I)
                MOVE LOW-VALUES TO I_FAIL IN ZWS_FAIL
                MOVE 'ZTTHRNES record mismatch for SALESPRM' 
                     TO FAILMESSAGE IN ZWS_FAIL

                DISPLAY FAILMESSAGE IN ZWS_FAIL
                DISPLAY '  Record: ' I
                DISPLAY 'Expected: ' WS-EXPECTED-PROMO-RECORD(I)
                DISPLAY '     Got: ' LS-RECORD-HISTORY(I)

                CALL ZTESTUT USING ZWS_FAIL
              END-IF
           END-PERFORM

           DISPLAY 'ZTTHRNES PASS no record mismatches' 

           GOBACK.

      ******************************************************************
      * UNIT TEST 2: Demonstrate the result when executed with fixed   *
      *              input, a series of widget IDs. Because the        *
      *              retrieved data changes rapidly over time, the     *
      *              unit test validation is not immutable.            *
      *                                                                *
      * NB: This unit test is counter-example, i.e., it has no         *
      *     validation because the called program, ZTPNHRDD, is        *
      *     time-sensitive and it is NOT mocked. Subsequently, its     *
      *     input to ZTPNHRAA is volatile, which presents a problem    *
      *     for validation. Refer to how unit test #1 addresses this   *
      *     by using Test4z's harness recording.                       *
      ******************************************************************
           ENTRY 'staticDataPromoSalesTest'.

      *-----------------------------------------------------------------
      * Mock the input file, SALESWCH, using records statically
      * defined by this unit test. The FILEOBJECT holds the data
      * that will be input to the mock, SALESWCH.
      *-----------------------------------------------------------------
           MOVE LOW-VALUES TO I_FILE
           SET RECORDADDRESS IN ZWS_FILE
                TO ADDRESS OF QSAM-SALES-WATCH-RECORDS
           MOVE 10 TO RECORDCOUNT IN ZWS_FILE
           MOVE 7 TO RECORDSIZE IN ZWS_FILE
           CALL ZTESTUT USING ZWS_FILE, 
                FILEOBJECT IN WS-ZFILE-SALES-WATCH-FILE

           MOVE LOW-VALUES TO I_MOCKQSAM
           MOVE 'SALESWCH' TO FILENAME IN ZWS_MOCKQSAM
           SET FILEOBJECT IN ZWS_MOCKQSAM
                TO ADDRESS OF FILEOBJECT IN WS-ZFILE-SALES-WATCH-FILE
           CALL ZTESTUT USING ZWS_MOCKQSAM,
                QSAMOBJECT IN WS-ZQSAM-SALES-WATCH-MOCK

      *-----------------------------------------------------------------
      * Mock the output file, SALESPRM. Since it's output-only,
      * there's no need to provide it initial data.
      *-----------------------------------------------------------------
           MOVE LOW-VALUES TO I_MOCKQSAM
           MOVE 'SALESPRM' TO FILENAME IN ZWS_MOCKQSAM
           MOVE 80 TO RECORDSIZE IN ZWS_MOCKQSAM
           CALL ZTESTUT USING ZWS_MOCKQSAM,
                QSAMOBJECT IN WS-ZQSAM-PROMOREC-MOCK

      *-----------------------------------------------------------------
      * To observe the results of ZTPHRNAA and ZTPHRNDD, spy on the
      * output file. This is a simple black box test, i.e., statically
      * defined input and validate the live expected output.
      *
      * NB: In this unit test, ZTPHRNDD is real, not mocked. And 
      *     because of the volatile nature of ZTPHRNDD's data
      *     retrieval, validation is less straightforward than if
      *     it were mocked. Subsequently, this unit test will only
      *     display the observed results, using three different
      *     methods (print, spy history, mock records).
      *-----------------------------------------------------------------
           MOVE LOW-VALUES TO I_SPYQSAM
           MOVE 'SALESPRM' TO FILENAME IN ZWS_SPYQSAM
           CALL ZTESTUT USING ZWS_SPYQSAM, 
                QSAMSPYOBJECT IN WS-ZSPQSAM-PROMOREC-FILE-SPY

           MOVE LOW-VALUES TO I_RUNFUNCTION
           MOVE 'ZTPHRNMM' TO MODULENAME IN ZWS_RUNFUNCTION
           CALL ZTESTUT USING ZWS_RUNFUNCTION

      *-----------------------------------------------------------------
      * The SUT has ended. Display the output using _PRINTFILE.
      *-----------------------------------------------------------------
           DISPLAY 'ZTTHRNES begin print of SALESPRM (static)'
           MOVE LOW-VALUES TO I_PRINTFILE
           SET FILEOBJECT IN ZWS_PRINTFILE TO
                ADDRESS OF QSAMOBJECT IN WS-ZQSAM-PROMOREC-MOCK
           CALL ZTESTUT USING ZWS_PRINTFILE
           DISPLAY 'ZTTHRNES end print of SALESPRM (static)'

      *-----------------------------------------------------------------
      * Display the file I/O operations using the QSAM spy. 
      *-----------------------------------------------------------------
           DISPLAY 'ZTTHRNES begin spy history of SALESPRM (static)'

           MOVE SIZE_ IN CALLS IN WS-ZSPQSAM-PROMOREC-FILE-SPY TO J
           SET ADDRESS OF ZLS_QSAM_HISTORIES
                TO PTR IN CALLS IN WS-ZSPQSAM-PROMOREC-FILE-SPY

           PERFORM VARYING I FROM 1 BY 1 UNTIL I > J
      *-----------------------------------------------------------------
      * A QSAM spy captures the command (OPEN, WRITE, READ, CLOSE, etc.)
      * and saves it in a history. We're only interested in WRITEs.
      *-----------------------------------------------------------------
                IF COMMAND IN ZLS_QSAM_HISTORY(I) = 'WRITE'
                     SET ADDRESS OF LS-RECORD
                          TO PTR IN RECORD_ IN ZLS_QSAM_HISTORY(I)
                     DISPLAY 
                          'ZTTHRNES WRITE   command ' I ' of ' J
                          ' record=' LS-RECORD
                ELSE
                     DISPLAY 
                          'ZTTHRNES skipped command ' I ' of ' J ' '
                          COMMAND IN ZLS_QSAM_HISTORY(I)
                END-IF
           END-PERFORM

           DISPLAY 'ZTTHRNES end spy history of SALESPRM (static)'

      *-----------------------------------------------------------------
      * Display the output using the QSAM mock records. See the
      * validation approach in Unit test 1 using mock records.
      *-----------------------------------------------------------------
           DISPLAY 'ZTTHRNES begin mock records of SALESPRM (static)'

           MOVE SIZE_ IN RECORDS_ IN WS-ZQSAM-PROMOREC-MOCK TO J
           SET ADDRESS OF LS-RECORD-HISTORIES
                TO PTR IN RECORDS_ IN FILE_ IN WS-ZQSAM-PROMOREC-MOCK

           PERFORM VARYING I FROM 1 BY 1 UNTIL I > J
              DISPLAY I ' of ' J ': ' LS-RECORD-HISTORY(I)
           END-PERFORM.

           DISPLAY 'ZTTHRNES end mock records of SALESPRM (static)'

           GOBACK.
