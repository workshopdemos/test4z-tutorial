       PROCESS PGMN(LM),NODYNAM
       IDENTIFICATION DIVISION.
       PROGRAM-ID. 'ZTTQSAMD' RECURSIVE.

      ******************************************************************
      * Broadcom Test4z Unit Test Suite (UT) example.                  *
      * Copyright (c) 2024 Broadcom. All Rights Reserved.              *
      *                                                                *
      * This test example shows how to set up two tests:               *
      *                                                                *
      * 1. The first test will mock a fixed-length QSAM                *
      * 2. The second test will mock a variable-length QSAM file.      *
      *                                                                *
      * In both of these tests, the initial data comes from the unit   *
      * test itself. The Test4z QSAM Spy will be used to observe the   *
      * READ and WRITE operations against the test file.               *
      *                                                                *
      * For the complete list of Test4z APIs and helpful reminders     *
      * when writing a unit test suite, see the Test4z COBOL API       *
      * document (PDF). The APIs are also documente in the copybook    *
      * ZTESTWSn.cpy where "n" is the document revision number.        * 
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
      * ZWS_MOCKQSAM -> I_MOCKQSAM IN ZWS_MOCKQSAM                     *
      *                                                                *
      * The "I" interface sub-block should be initialized to           *
      * LOW-VALUES prior to calling ZTESTUT. For Test4z API control    *
      * blocks in the LINKAGE SECTION, look for the prefix ZLS_.       *
      ******************************************************************
       COPY ZTESTWS.

      ******************************************************************
      * For those APIs with return values, the copybook block contains *
      * an output field (e.g., ZFILE -> FILEOBJECT). The copybooks     *
      * only define one control block and they start at the 03         *
      * level, so you can define your own 01 level variable.           *
      ******************************************************************
       01  WS-ZFILE-TESTQSAM-FILE.
           COPY ZFILE.
       01  WS-ZQSAM-TESTQSAM-MOCK.
           COPY ZQSAM.
       01  WS-ZSPQSAM-TESTQSAM-SPY.
           COPY ZSPQSAM.

      *-----------------------------------------------------------------  
      * Define the file records that will be pre-loaded into the QSAM
      * mock as a FILEOBJECT (see Test4z _FILE API for details).
      *-----------------------------------------------------------------  
       01 QSAM-FIXED-RECORDS.
           05 QSAM-FIXED-RECORD PIC X(80) VALUE 'Fixed record 01'.
           05 QSAM-FIXED-RECORD PIC X(80) VALUE 'Fixed record 02'.
           05 QSAM-FIXED-RECORD PIC X(80) VALUE 'Fixed record 03'.
           05 QSAM-FIXED-RECORD PIC X(80) VALUE 'Fixed record 04'.
           05 QSAM-FIXED-RECORD PIC X(80) VALUE 'Fixed record 05'.
           05 QSAM-FIXED-RECORD PIC X(80) VALUE 'Fixed record 06'.
           05 QSAM-FIXED-RECORD PIC X(80) VALUE 'Fixed record 07'.
           05 QSAM-FIXED-RECORD PIC X(80) VALUE 'Fixed record 08'.
           05 QSAM-FIXED-RECORD PIC X(80) VALUE 'Fixed record 09'.
           05 QSAM-FIXED-RECORD PIC X(80) VALUE 'Fixed record 10'.

       01 QSAM-VARIABLE-RECORDS.
           05 QSAM-VARIABLE-RECORD.
                10 DATA-LTH  PIC 9(4) COMP-5 VALUE 21.
                10 QSAM-DATA PIC X(21) 
                     VALUE 'Variable record 01 ab'.
           05 QSAM-VARIABLE-RECORD.
                10 DATA-LTH  PIC 9(4) COMP-5 VALUE 23.
                10 QSAM-DATA PIC X(23) 
                     VALUE 'Variable record 02 abcd'.
           05 QSAM-VARIABLE-RECORD.
                10 DATA-LTH  PIC 9(4) COMP-5 VALUE 25.
                10 QSAM-DATA PIC X(25) 
                     VALUE 'Variable record 03 abcdef'.
           05 QSAM-VARIABLE-RECORD.
                10 DATA-LTH  PIC 9(4) COMP-5 VALUE 27.
                10 QSAM-DATA PIC X(27) 
                     VALUE 'Variable record 04 abcdefgh'.
           05 QSAM-VARIABLE-RECORD.
                10 DATA-LTH  PIC 9(4) COMP-5 VALUE 29.
                10 QSAM-DATA PIC X(29) 
                     VALUE 'Variable record 05 abcdefghij'.
           05 QSAM-VARIABLE-RECORD.
                10 DATA-LTH  PIC 9(4) COMP-5 VALUE 31.
                10 QSAM-DATA PIC X(31) 
                     VALUE 'Variable record 06 abcdefghijkl'.
           05 QSAM-VARIABLE-RECORD.
                10 DATA-LTH  PIC 9(4) COMP-5 VALUE 33.
                10 QSAM-DATA PIC X(33) 
                     VALUE 'Variable record 07 abcdefghijklmn'.
           05 QSAM-VARIABLE-RECORD.
                10 DATA-LTH  PIC 9(4) COMP-5 VALUE 35.
                10 QSAM-DATA PIC X(35) 
                     VALUE 'Variable record 08 abcdefghijklmnop'.
           05 QSAM-VARIABLE-RECORD.
                10 DATA-LTH  PIC 9(4) COMP-5 VALUE 37.
                10 QSAM-DATA PIC X(37) 
                     VALUE 'Variable record 09 abcdefghijklmnopqr'.
           05 QSAM-VARIABLE-RECORD.
                10 DATA-LTH  PIC 9(4) COMP-5 VALUE 41.
                10 QSAM-DATA PIC X(41) 
                     VALUE 'Variable record 10 abcdefghijklmnopqrst'.

      *-----------------------------------------------------------------  
      * Various unit test control variables.
      *-----------------------------------------------------------------  
       01  WS-RECORDS-WRITTEN    PIC 9(2) VALUE 0.
       01  WS-RECORDS-READ       PIC 9(2) VALUE 0.
       01  WS-PROGRAM-UNDER-TEST PIC X(8).   

       LINKAGE SECTION.

      ******************************************************************
      * Copy in our required control blocks from Test4z.               *
      ******************************************************************
       COPY ZTESTLS.

      ******************************************************************
      * Define any linkage section items that we need.                 *
      ******************************************************************
       01  LS-ZSPQSAM-TESTQSAM-SPY.
           COPY ZSPQSAM.

       01  LS-MY-QSAM-RECORD PIC X(80).

       PROCEDURE DIVISION.

      ******************************************************************
      * Register a set of tests to be run and then return control      *
      * to Test4z.                                                     *
      ******************************************************************
           MOVE LOW-VALUES TO I_TEST
           SET TESTFUNCTION IN ZWS_TEST TO ENTRY 'unitTestFixedQsam'
           MOVE 'Fixed QSAM test with spy' TO TESTNAME IN ZWS_TEST
           CALL ZTESTUT USING ZWS_TEST

           MOVE LOW-VALUES TO I_TEST
           SET TESTFUNCTION IN ZWS_TEST TO ENTRY 'unitTestVariableQsam'
           MOVE 'Variable QSAM test with spy' TO TESTNAME IN ZWS_TEST
           CALL ZTESTUT USING ZWS_TEST

           GOBACK.

      ******************************************************************
      * UNIT TEST 1: Provide the callback routine entry point for the  *
      *              fixed-length QSAM test with data provided by the  *
      *              test case.                                        *
      ******************************************************************
           ENTRY 'unitTestFixedQsam'.

      *-----------------------------------------------------------------
      * Create a base file object for QSAM file TESTQSAM with data
      * provided. The data for a fixed-length file is a set of records
      * of the same length as the record size of the file. 
      *-----------------------------------------------------------------
           MOVE LOW-VALUES TO I_FILE
           SET RECORDADDRESS IN ZWS_FILE
                TO ADDRESS OF QSAM-FIXED-RECORDS
           MOVE 10 TO RECORDCOUNT IN ZWS_FILE
           MOVE 80 TO RECORDSIZE IN ZWS_FILE
           CALL ZTESTUT USING ZWS_FILE, 
                FILEOBJECT IN WS-ZFILE-TESTQSAM-FILE

      *-----------------------------------------------------------------
      * Using the previous base object pointer, create a QSAM file 
      * object to be intercepted when being accessed by program
      * ZTPQSAMT. 
      *-----------------------------------------------------------------
           MOVE LOW-VALUES TO I_MOCKQSAM
           MOVE 'TESTQSAM' TO FILENAME IN ZWS_MOCKQSAM
           SET FILEOBJECT IN ZWS_MOCKQSAM TO
               ADDRESS OF FILEOBJECT IN WS-ZFILE-TESTQSAM-FILE
           CALL ZTESTUT USING ZWS_MOCKQSAM,
                QSAMOBJECT IN WS-ZQSAM-TESTQSAM-MOCK

      *-----------------------------------------------------------------
      * Spy on the QSAM file. 
      *-----------------------------------------------------------------
           MOVE LOW-VALUES TO I_SPYQSAM
           MOVE 'TESTQSAM' TO FILENAME IN ZWS_SPYQSAM
           SET CALLBACK IN ZWS_SPYQSAM TO
               ENTRY 'echoQsamSpyCallback'
           CALL ZTESTUT USING ZWS_SPYQSAM,
                QSAMSPYOBJECT IN WS-ZSPQSAM-TESTQSAM-SPY

      *-----------------------------------------------------------------
      * Load and prepare the user application program ZTPQSAMT for
      * use. Get the function address and call the function. This can
      * be done all in one call to _RunFunction or as individual calls
      * to _PrepareModule, _GetFunction and then call the function.
      *-----------------------------------------------------------------
           MOVE 'ZTPQSAMT' TO WS-PROGRAM-UNDER-TEST
           MOVE LOW-VALUES TO I_RUNFUNCTION
           MOVE WS-PROGRAM-UNDER-TEST TO MODULENAME IN ZWS_RUNFUNCTION
           MOVE WS-PROGRAM-UNDER-TEST TO FUNCTIONNAME IN ZWS_RUNFUNCTION
           CALL ZTESTUT USING ZWS_RUNFUNCTION

      *-----------------------------------------------------------------
      * The SUT has ended. Do a quick check to confirm all the
      * provided records were written. The program under test
      * ZTPQSAMT only reads the records, no writes. All the records
      * it read were provided by this unit test.
      *-----------------------------------------------------------------  
           IF WS-RECORDS-READ NOT = 10 OR WS-RECORDS-WRITTEN NOT = 0
                MOVE LOW-VALUES TO I_FAIL IN ZWS_FAIL
                MOVE 'ZTTQSAMD record count mismatch (fixed)'
                     TO FAILMESSAGE IN ZWS_FAIL

                DISPLAY FAILMESSAGE IN ZWS_FAIL
                DISPLAY 'Expected=10 (read)'
                DISPLAY '     Got=' WS-RECORDS-READ
                DISPLAY 'Expected=0  (write)'
                DISPLAY '     Got=' WS-RECORDS-WRITTEN

                CALL ZTESTUT USING ZWS_FAIL
           ELSE
                DISPLAY 'ZTTQSAMD correct number of record operations'
                DISPLAY 'ZTTQSAMD read=' WS-RECORDS-READ 
                        ' write=' WS-RECORDS-WRITTEN
                        ' pre-loaded=10'
           END-IF

           GOBACK.

      ******************************************************************
      * UNIT TEST 2: Provide the callback routine entry point for the  *
      *              variable-length QSAM test with the initial data   * 
      *              provided by the test case.                        *
      ******************************************************************
           ENTRY 'unitTestVariableQsam'.

      *-----------------------------------------------------------------
      * Create a base file object for QSAM file TESTQSAM with data 
      * provided. The data for a variable-length file is a set of 
      * records with for each record (a PIC 9(4) COMP-5 initialized
      * to the length of the data for that specific record).
      *
      * A variable-length QSAM file object must have a minimum record 
      * length specified that is greater than 0, otherwise the file
      * object is treated as fixed-length.
      *-----------------------------------------------------------------
           MOVE LOW-VALUES TO I_FILE
           SET RECORDADDRESS IN ZWS_FILE
                TO ADDRESS OF QSAM-VARIABLE-RECORDS
           MOVE 10 TO RECORDCOUNT IN ZWS_FILE
           MOVE 1 TO RECORDMINIMUMSIZE IN ZWS_FILE
           MOVE 80 TO RECORDSIZE IN ZWS_FILE
           CALL ZTESTUT USING ZWS_FILE, 
                FILEOBJECT IN WS-ZFILE-TESTQSAM-FILE

      *-----------------------------------------------------------------
      * Using the previous base object pointer, create a QSAM file 
      * object to be intercepted when being accessed by program 
      * ZTPQSAMV.
      *-----------------------------------------------------------------
           MOVE LOW-VALUES TO I_MOCKQSAM
           MOVE 'TESTQSAM' TO FILENAME IN ZWS_MOCKQSAM
           SET FILEOBJECT IN ZWS_MOCKQSAM TO
               ADDRESS OF FILEOBJECT IN WS-ZFILE-TESTQSAM-FILE
           CALL ZTESTUT USING ZWS_MOCKQSAM,
                QSAMOBJECT IN WS-ZQSAM-TESTQSAM-MOCK

      *-----------------------------------------------------------------
      * Spy on the QSAM file.
      *-----------------------------------------------------------------
           MOVE LOW-VALUES TO I_SPYQSAM
           MOVE 'TESTQSAM' TO FILENAME IN ZWS_SPYQSAM
           SET CALLBACK IN ZWS_SPYQSAM TO
               ENTRY 'echoQsamSpyCallback'
           CALL ZTESTUT USING ZWS_SPYQSAM,
                QSAMSPYOBJECT IN WS-ZSPQSAM-TESTQSAM-SPY

      *-----------------------------------------------------------------
      * Run the application program ZTPQSAMV, then verify its results.
      *-----------------------------------------------------------------
           MOVE 'ZTPQSAMV' TO WS-PROGRAM-UNDER-TEST
           MOVE LOW-VALUES TO I_RUNFUNCTION
           MOVE WS-PROGRAM-UNDER-TEST TO MODULENAME IN ZWS_RUNFUNCTION
           MOVE WS-PROGRAM-UNDER-TEST TO FUNCTIONNAME IN ZWS_RUNFUNCTION
           CALL ZTESTUT USING ZWS_RUNFUNCTION

      *-----------------------------------------------------------------
      * The SUT has ended. Do a quick check to confirm all the
      * provided records were read/written. The program under test 
      * ZTPQSAMV writes some records and then reads all of them back,
      * including the initial ones provided by this unit test.
      *-----------------------------------------------------------------      
           IF WS-RECORDS-READ NOT = 20 OR WS-RECORDS-WRITTEN NOT = 10
                MOVE LOW-VALUES TO I_FAIL IN ZWS_FAIL
                MOVE 'ZTTQSAMD record count mismatch (variable)'
                     TO FAILMESSAGE IN ZWS_FAIL

                DISPLAY FAILMESSAGE IN ZWS_FAIL
                DISPLAY 'Expected=20 (read)'
                DISPLAY '     Got=' WS-RECORDS-READ
                DISPLAY 'Expected=10 (write)'
                DISPLAY '     Got=' WS-RECORDS-WRITTEN

                CALL ZTESTUT USING ZWS_FAIL
           ELSE
                DISPLAY 'ZTTQSAMD correct number of record operations'
                DISPLAY 'ZTTQSAMD read=' WS-RECORDS-READ 
                        ' write=' WS-RECORDS-WRITTEN
                        ' pre-loaded=10'
           END-IF

           GOBACK.

      ******************************************************************
      * CALLBACK: Provide the spy callback entry point for the QSAM    *
      *           spy.                                                 *
      *                                                                *
      * NB: This common callback is used for both unit tests since     *
      *     they run independently and have no unique processing.      *
      *     However, in a production unit test environment, there      *
      *     would likely be two separate spy entry points to handle    *
      *     the specific validation requirements of their underlying   *
      *     resources.                                                 *
      ******************************************************************
           ENTRY 'echoQsamSpyCallback' USING LS-ZSPQSAM-TESTQSAM-SPY.

      *-----------------------------------------------------------------
      * The spy keeps a history of file operations; map the linkage
      * section to the address of the last record.
      *-----------------------------------------------------------------
           SET ADDRESS OF ZLS_QSAM_RECORD 
                TO LASTCALL IN LS-ZSPQSAM-TESTQSAM-SPY

           IF STATUSCODE IN ZLS_QSAM_RECORD = '00' AND 
                     (COMMAND IN ZLS_QSAM_RECORD = 'READ' OR
                     COMMAND IN ZLS_QSAM_RECORD = 'WRITE')

      *-----------------------------------------------------------------
      * It's a small validation, but let's keep it simple. Tally
      * how many records were read/written and we'll confirm the
      * totals once the SUT ends.
      *-----------------------------------------------------------------
                IF COMMAND IN ZLS_QSAM_RECORD = 'WRITE'
                     ADD 1 TO WS-RECORDS-WRITTEN
                ELSE
                     ADD 1 TO WS-RECORDS-READ           
                END-IF

                SET ADDRESS OF LS-MY-QSAM-RECORD TO
                     PTR IN RECORD_ IN ZLS_QSAM_RECORD
                DISPLAY 'ZTTQSAMD QSAM spy on ' WS-PROGRAM-UNDER-TEST
                     ' command=' COMMAND IN ZLS_QSAM_RECORD(1:5)
                     ' size=' SIZ IN RECORD_ IN ZLS_QSAM_RECORD 
                     ' record=' LS-MY-QSAM-RECORD 
                          (1: SIZ IN RECORD_ IN ZLS_QSAM_RECORD)
           END-IF

           GOBACK.
