       PROCESS PGMN(LM),NODYNAM
       IDENTIFICATION DIVISION.
       PROGRAM-ID. 'ZTTDB2PH' RECURSIVE.

      ******************************************************************
      * Broadcom Test4z Unit Test Suite (UT) example.                  *
      * Copyright (c) 2024 Broadcom. All Rights Reserved.              *
      *                                                                *
      * This unit test suite validates the operation of ZTD2PHN, a     *
      * short DB2 program that maintains an employee directory in a    *
      * corporate database. The unit test suite is composed of several *
      * unit tests. The first is a demonstration-only unit test:       *
      *                                                                *
      * == Unit test 0: will only run if parameter DB2=DEMO-RESET is   *
      *    specified in the JCL to run ZTD2PHN with DB2 real (not      *
      *    mocked). It will reset the real DB2 table EMPPHONE to a     *
      *    known state for demonstration purposes.                     *
      *                                                                *
      * The remaining three unit tests have varying levels of mocking  *
      * for the input and output:                                      *
      *                                                                *
      * == Unit test 1: will only run if the Test4z JCL running        *
      *    ZESTRUN includes "PARM(DB2=REAL)" in the ZLOPTS parameter.  *
      *    ZTDB2PHN will access real DB2, not mocked by Test4z.        *
      *                                                                *
      * == Unit test 2: uses the Test4z _CreateHarness API to mock     *
      *    all input, including DB2. No real DB2 connection is         *
      *    required and it can be run directly from Test4z CLI via     *
      *    the test command. This is the default test scenario.        *
      *                                                                *
      * == Unit test 3: similar to unit test 2, except it excludes     *
      *    both DB2 and the input file PHUPDATE from the test harness. *
      *    It defines input data to ZTDB2PHN via a Test4z file mock    *
      *    created from hardcoded records (WS-PHONE-UPDATE-RECORDS).   *
      *                                                                *
      * Unit test 0 and Unit test 1 must be run directly from JCL      *
      * as they require real DB2 connections. See the Test4z           *
      * documentation, section "Record, Replay, and Verification       *
      * Processing" (PDF) and JCL samples (e.g., "Run Record of a      *
      * batch DB2 program") for more details.                          *
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

      ******************************************************************
      * For those APIs with return values, the copybook block contains *
      * an output field (e.g., ZQSAM -> QSAMOBJECT). The copybooks     *
      * only define one control block and they start at the 03         *
      * level, so you can define your own 01 level variable.           *
      ******************************************************************
       01  WS-ZQSAM-PHONE-UPDATE-MOCK.
           COPY ZQSAM.
       01  WS-ZQSAM-PHONE-LOG-MOCK.
           COPY ZQSAM.

       01  WS-ZSPDB2-DATABASE-SPY.
           COPY ZSPDB2.

       01  WS-ZHRNESS-DATA.
           COPY ZHRNESS.
       01  WS-ZHRNOPT-OPTIONS.
           02 HO-HARNESS-OPTION OCCURS 2 TIMES.
           COPY ZHRNOPT.

       01  WS-ZPARM-GET-MYOPTION.
           COPY ZPARM.

      *-----------------------------------------------------------------
      * For DISPLAY output control of long fields.
      *-----------------------------------------------------------------
       77  WS-LOG-FIELD-LENGTH-LG   PIC 9(2) VALUE 14.
       77  WS-LOG-FIELD-LENGTH-SM   PIC 9(2) VALUE 10.

      *-----------------------------------------------------------------
      * See Test4z _GETFUNCTION for details.
      *-----------------------------------------------------------------
       01  WS-RUN-PROGRAM USAGE FUNCTION-POINTER.

      *-----------------------------------------------------------------  
      * Define the file records for mocking in Unit test 3.
      *-----------------------------------------------------------------  
       01  WS-PHONE-UPDATE-RECORDS.
           05 PHONE-UPDATE-RECORD-1.
               10 REQUEST-TYPE      PIC X VALUE 'S'.
               10 REQUEST-KEY       PIC X(6) VALUE '000005'.
               10 REQUEST-MESSAGE   PIC X(40) VALUE 'Validate'.
               10 REQUEST-LASTNAME  PIC X(40) VALUE SPACES.
               10 REQUEST-FIRSTNAME PIC X(40) VALUE SPACES.
               10 REQUEST-PHONE     PIC X(10) VALUE SPACES.
               10 FILLER            PIC X(63) VALUE SPACES.
           05 PHONE-UPDATE-RECORD-2.
               10 REQUEST-TYPE      PIC X VALUE 'K'.
               10 REQUEST-KEY       PIC X(6) VALUE '000001'.
               10 REQUEST-MESSAGE   PIC X(40) VALUE 'Keep'.
               10 REQUEST-LASTNAME  PIC X(40) VALUE SPACES.
               10 REQUEST-FIRSTNAME PIC X(40) VALUE SPACES.
               10 REQUEST-PHONE     PIC X(10) VALUE SPACES.
               10 FILLER            PIC X(63) VALUE SPACES.               
           05 PHONE-UPDATE-RECORD-3.
               10 REQUEST-TYPE      PIC X VALUE 'U'.
               10 REQUEST-KEY       PIC X(6) VALUE '000001'.
               10 REQUEST-MESSAGE   PIC X(40) VALUE 'Work phone'.
               10 REQUEST-LASTNAME  PIC X(40) VALUE SPACES.
               10 REQUEST-FIRSTNAME PIC X(40) VALUE SPACES.
               10 REQUEST-PHONE     PIC X(10) VALUE '9195551212'.
               10 FILLER            PIC X(63) VALUE SPACES.
           05 PHONE-UPDATE-RECORD-4.
               10 REQUEST-TYPE      PIC X VALUE 'U'.
               10 REQUEST-KEY       PIC X(6)  VALUE '000002'.
               10 REQUEST-MESSAGE   PIC X(40) VALUE 'Update phone'.
               10 REQUEST-LASTNAME  PIC X(40) VALUE SPACES.
               10 REQUEST-FIRSTNAME PIC X(40) VALUE SPACES.
               10 REQUEST-PHONE     PIC X(10) VALUE '9195554242'.
               10 FILLER            PIC X(63) VALUE SPACES.
           05 PHONE-UPDATE-RECORD-5.
               10 REQUEST-TYPE      PIC X VALUE 'C'.
               10 REQUEST-KEY       PIC X(6)  VALUE '000006'.
               10 REQUEST-MESSAGE   PIC X(40) VALUE 'New employee'.
               10 REQUEST-LASTNAME  PIC X(40) VALUE 'Quincy'.
               10 REQUEST-FIRSTNAME PIC X(40) VALUE 'Adams'.
               10 REQUEST-PHONE     PIC X(10) VALUE '9195551767'.
               10 FILLER            PIC X(63) VALUE SPACES.
           05 PHONE-UPDATE-RECORD-6.
               10 REQUEST-TYPE      PIC X VALUE 'D'.
               10 REQUEST-KEY       PIC X(6)  VALUE '000003'.
               10 REQUEST-MESSAGE   PIC X(40) VALUE 'Left company'.
               10 REQUEST-LASTNAME  PIC X(40) VALUE SPACES.
               10 REQUEST-FIRSTNAME PIC X(40) VALUE SPACES.
               10 REQUEST-PHONE     PIC X(10) VALUE SPACES.               
               10 FILLER            PIC X(63) VALUE SPACES.
            05 PHONE-UPDATE-RECORD-7.
               10 REQUEST-TYPE      PIC X VALUE 'X'.
               10 REQUEST-KEY       PIC X(6)  VALUE '000000'.
               10 REQUEST-MESSAGE   PIC X(40) VALUE 'Invalid request'.
               10 REQUEST-LASTNAME  PIC X(40) VALUE SPACES.
               10 REQUEST-FIRSTNAME PIC X(40) VALUE SPACES.
               10 REQUEST-PHONE     PIC X(10) VALUE SPACES.               
               10 FILLER            PIC X(63) VALUE SPACES.
             05 PHONE-UPDATE-RECORD-8.
               10 REQUEST-TYPE      PIC X VALUE 'U'.
               10 REQUEST-KEY       PIC X(6) VALUE '000002'.
               10 REQUEST-MESSAGE   PIC X(40) VALUE 'Correct name'.
               10 REQUEST-LASTNAME  PIC X(40) VALUE SPACES.
               10 REQUEST-FIRSTNAME PIC X(40) VALUE 'Benjamin'.
               10 REQUEST-PHONE     PIC X(10) VALUE '9195551767'.
               10 FILLER            PIC X(63) VALUE SPACES.

       01  WS-PHONE-UPDATE-RECORDS-REDEF 
                REDEFINES WS-PHONE-UPDATE-RECORDS.
           05 PHONE-UPDATE-RECORD OCCURS 8 TIMES.
               10 REQUEST-TYPE      PIC X.
               10 REQUEST-KEY       PIC X(6).
               10 REQUEST-MESSAGE   PIC X(40).
               10 REQUEST-LASTNAME  PIC X(40).
               10 REQUEST-FIRSTNAME PIC X(40).
               10 REQUEST-PHONE     PIC X(10).
               10 FILLER            PIC X(63).               
       01  WS-PHONE-UPDATE-RECORDS-CNT PIC 9(2) VALUE 8.

      *-----------------------------------------------------------------  
      * Rather than compare record-by-record the actual and expected
      * results, let's verify the totals per request type. That
      * will catch the majority of mismatches. Then a field-by-field
      * spot check of a record rounds out the validation.
      *-----------------------------------------------------------------  
       01  WS-PHONE-UPDATE-STATS-EXPECTED.
           05 UT-DELETES-VALID      PIC 9(2) VALUE 1.
           05 UT-DELETES-INVALID    PIC 9(2) VALUE 0.
           05 UT-UPDATES-VALID      PIC 9(2) VALUE 3.
           05 UT-UPDATES-INVALID    PIC 9(2) VALUE 0.
           05 UT-CREATES-VALID      PIC 9(2) VALUE 1.
           05 UT-CREATES-INVALID    PIC 9(2) VALUE 0.
           05 UT-KEEPS-VALID        PIC 9(2) VALUE 1.
           05 UT-KEEPS-INVALID      PIC 9(2) VALUE 0.
           05 UT-REQUESTS-INVALID   PIC 9(2) VALUE 1.

       01  WS-PHONE-UPDATE-STATS-ACTUAL.
           05 SUT-DELETES-VALID     PIC 9(2).
           05 SUT-DELETES-INVALID   PIC 9(2).
           05 SUT-UPDATES-VALID     PIC 9(2).
           05 SUT-UPDATES-INVALID   PIC 9(2).
           05 SUT-CREATES-VALID     PIC 9(2).
           05 SUT-CREATES-INVALID   PIC 9(2).
           05 SUT-KEEPS-VALID       PIC 9(2).
           05 SUT-KEEPS-INVALID     PIC 9(2).
           05 SUT-REQUESTS-INVALID  PIC 9(2).

       77  WS-VALID-REQUEST         PIC 9(1).
       77  WS-INVALID-REQUEST       PIC 9(1).

       01  WS-SQLCODE               PIC S9(9) SIGN IS LEADING.
       01  WS-NEW-PHONE-UPDATE-RECORD PIC X(200) VALUE SPACES.
       01  I                        PIC 9(2).

      *-----------------------------------------------------------------   
      * These flags control which unit tests are run, notably Unit
      * test 0 (reset DB2) and Unit test 1 (real DB2). Note: Since
      * these two unit tests require real DB2, they must be run from
      * JCL to accommodate DB2 bind requirements.
      *-----------------------------------------------------------------
       01  WS-DB2-REAL-FLAG         PIC 9(2) VALUE 0.
           88 WS-DB2-IS-MOCKED      VALUE 0.
           88 WS-DB2-IS-REAL        VALUE 1.
       01  WS-DEMO-RESET-FLAG       PIC 9(2) VALUE 0.
           88 WS-DO-DEMO-RESET      VALUE 1.
           88 WS-SKIP-DEMO-RESET    VALUE 0.

       01  WS-PARM-DEMO-RESET.
           05 WS-PARM-LENGTH        PIC S9(4) COMP VALUE 100.
           05 WS-PARM-DATA          PIC X(100) VALUE 'DB2=DEMO-RESET'.

      *-----------------------------------------------------------------  
      * This copybook defines the records in PHLOG / PHONE-LOG-FILE.
      * It is used during the unit test validation step to compare
      * the expected records and actual records.
      *-----------------------------------------------------------------  
           COPY ZTDB2PHR.

       LINKAGE SECTION.

      ******************************************************************
      * Copy in our required control blocks from Test4z.               *
      ******************************************************************
       COPY ZTESTLS.

       01  LS-ZQSAM-PHONE-UPDATE-MOCK.
           COPY ZQSAM.
       01  LS-ZQSAM-PHONE-LOG-MOCK.
           COPY ZQSAM.
       01  LS-ZSPDB2-DATABASE-SPY.
           COPY ZSPDB2.

       01  LS-GET-PARM-MYOPTION PIC X(100).

       01  LS-PHONE-UPDATE-RECORDS.
           05 LS-PHONE-UPDATE-RECORD PIC X(200) OCCURS UNBOUNDED TIMES.
       01  LS-PHONE-LOG-RECORDS.
           05 LS-PHONE-LOG-RECORD PIC X(132) OCCURS UNBOUNDED TIMES.

       PROCEDURE DIVISION.

      ******************************************************************
      * Register a set of tests to be run and then return control      *
      * to Test4z.                                                     *
      ******************************************************************
           PERFORM GET-TEST-OPTIONS
           PERFORM REGISTER-UNIT-TESTS

           GOBACK.

      ******************************************************************
      * UNIT TEST 0: Reset the EMPPHONE table (demonstration only).    *
      ******************************************************************
           ENTRY 'doDemoResetDB2'.

           IF NOT WS-DO-DEMO-RESET
                MOVE LOW-VALUES TO I_MESSAGE IN ZWS_MESSAGE
                MOVE 'ZTTDB2PH Skipped - DB2=DEMO-RESET not specified' 
                     TO MESSAGETEXT IN ZWS_MESSAGE
                    
                DISPLAY MESSAGETEXT IN ZWS_MESSAGE
                CALL ZTESTUT USING ZWS_MESSAGE
                GOBACK
           END-IF

      *-----------------------------------------------------------------
      * Call ZTDB2PHN, asking it to reset the EMPPHONE table.
      *-----------------------------------------------------------------
           MOVE LOW-VALUES TO I_PREPAREMODULE
           MOVE 'ZTDB2PHN' TO MODULENAME IN ZWS_PREPAREMODULE
           CALL ZTESTUT USING ZWS_PREPAREMODULE
 
           MOVE LOW-VALUES TO I_GETFUNCTION
           MOVE 'ZTDB2PHN' TO MODULENAME IN ZWS_GETFUNCTION
           MOVE 'ZTDB2PHN' TO FUNCTIONNAME IN ZWS_GETFUNCTION
           CALL ZTESTUT USING ZWS_GETFUNCTION, WS-RUN-PROGRAM

           DISPLAY 'ZTTDB2PH Unit test 0: Perform DB2=DEMO-RESET option'
           CALL WS-RUN-PROGRAM USING WS-PARM-DEMO-RESET

           GOBACK.

      ******************************************************************
      * UNIT TEST 1: Use recorded data except for DB2, which is real.  *
      ******************************************************************
           ENTRY 'testHarnessInputRealDB2'.

           IF WS-DB2-IS-MOCKED
                MOVE LOW-VALUES TO I_MESSAGE IN ZWS_MESSAGE
                MOVE 'ZTTDB2PH Skipped - DB2=REAL not specified' 
                     TO MESSAGETEXT IN ZWS_MESSAGE
                    
                DISPLAY MESSAGETEXT IN ZWS_MESSAGE
                CALL ZTESTUT USING ZWS_MESSAGE
                GOBACK
           END-IF

           MOVE LOW-VALUES TO I_CREATEHARNESS
           MOVE LOW-VALUES TO WS-ZHRNOPT-OPTIONS
           MOVE 'ZTDB2PHN' TO MEMBERNAME IN ZWS_CREATEHARNESS

           SET REAL_DB2 IN HO-HARNESS-OPTION(1) TO TRUE
           MOVE 'ZTDB2PHN' TO MODULENAMES IN HO-HARNESS-OPTION(1)

           SET PTR IN MOCKOPTIONS IN ZWS_CREATEHARNESS TO
               ADDRESS OF WS-ZHRNOPT-OPTIONS
           MOVE 1 TO SIZ IN MOCKOPTIONS IN ZWS_CREATEHARNESS

           CALL ZTESTUT USING ZWS_CREATEHARNESS,
                HARNESSOBJECT IN WS-ZHRNESS-DATA

      *-----------------------------------------------------------------
      * Display input.
      *-----------------------------------------------------------------
           MOVE LOW-VALUES TO I_GETFILEOBJECT
           MOVE 'PHUPDATE' TO FILENAME IN ZWS_GETFILEOBJECT
           SET HARNESSOBJECT IN ZWS_GETFILEOBJECT TO
                ADDRESS OF HARNESSOBJECT IN WS-ZHRNESS-DATA
           CALL ZTESTUT USING ZWS_GETFILEOBJECT,
                ADDRESS OF LS-ZQSAM-PHONE-UPDATE-MOCK

           DISPLAY 'ZTTDB2PH Unit test 1: testHarnessInputRealDB2'
           DISPLAY 'ZTTDB2PH requested updates from PHUPDATE:'
           PERFORM PRINT-PHONE-UPDATES                  

      *-----------------------------------------------------------------
      * Mock output.
      *-----------------------------------------------------------------
           MOVE LOW-VALUES TO I_MOCKQSAM
           MOVE 'PHLOG' TO FILENAME IN ZWS_MOCKQSAM
           MOVE 132 TO RECORDSIZE IN ZWS_MOCKQSAM
           CALL ZTESTUT USING ZWS_MOCKQSAM, 
                QSAMOBJECT IN WS-ZQSAM-PHONE-LOG-MOCK

      *-----------------------------------------------------------------
      * Spy on DB2.
      *-----------------------------------------------------------------
           PERFORM REGISTER-DB2-SPY.

      *-----------------------------------------------------------------
      * Run it.
      *-----------------------------------------------------------------
           MOVE LOW-VALUES TO I_RUNFUNCTION
           MOVE 'ZTDB2PHN' TO MODULENAME IN ZWS_RUNFUNCTION
           MOVE 'ZTDB2PHN' TO FUNCTIONNAME IN ZWS_RUNFUNCTION
           CALL ZTESTUT USING ZWS_RUNFUNCTION

      *-----------------------------------------------------------------
      * Validate it.
      *-----------------------------------------------------------------
           PERFORM VALIDATE-RESULTS

           GOBACK.

      ******************************************************************
      * UNIT TEST 2: Use recorded data for all input, including DB2.   *
      ******************************************************************
           ENTRY 'testAllHarnessInput'.

           MOVE LOW-VALUES TO I_CREATEHARNESS
           MOVE LOW-VALUES TO WS-ZHRNOPT-OPTIONS
           MOVE 'ZTDB2PHN' TO MEMBERNAME IN ZWS_CREATEHARNESS

           SET REAL_QSAM IN HO-HARNESS-OPTION(1) TO TRUE
           MOVE 'PHLOG' TO ARTIFACTNAMES IN HO-HARNESS-OPTION(1)
           MOVE 'ZTDB2PHN' TO MODULENAMES IN HO-HARNESS-OPTION(1)

           SET PTR IN MOCKOPTIONS IN ZWS_CREATEHARNESS TO
               ADDRESS OF WS-ZHRNOPT-OPTIONS
           MOVE 1 TO SIZ IN MOCKOPTIONS IN ZWS_CREATEHARNESS

           CALL ZTESTUT USING ZWS_CREATEHARNESS,
                HARNESSOBJECT IN WS-ZHRNESS-DATA

      *-----------------------------------------------------------------
      * Display input; the mock was already created by the test harness.
      *-----------------------------------------------------------------
           MOVE LOW-VALUES TO I_GETFILEOBJECT
           MOVE 'PHUPDATE' TO FILENAME IN ZWS_GETFILEOBJECT
           SET HARNESSOBJECT IN ZWS_GETFILEOBJECT TO
                ADDRESS OF HARNESSOBJECT IN WS-ZHRNESS-DATA
           CALL ZTESTUT USING ZWS_GETFILEOBJECT,
                ADDRESS OF LS-ZQSAM-PHONE-UPDATE-MOCK

           DISPLAY 'ZTTDB2PH Unit test 2: testAllHarnessInput'
           DISPLAY 'ZTTDB2PH requested updates from PHUPDATE:'
           PERFORM PRINT-PHONE-UPDATES       

      *-----------------------------------------------------------------
      * Mock output.
      *-----------------------------------------------------------------
           MOVE LOW-VALUES TO I_MOCKQSAM
           MOVE 'PHLOG' TO FILENAME IN ZWS_MOCKQSAM
           MOVE 132 TO RECORDSIZE IN ZWS_MOCKQSAM
           CALL ZTESTUT USING ZWS_MOCKQSAM, 
                QSAMOBJECT IN WS-ZQSAM-PHONE-LOG-MOCK

      *-----------------------------------------------------------------
      * Spy on DB2.
      *-----------------------------------------------------------------
           PERFORM REGISTER-DB2-SPY.

      *-----------------------------------------------------------------
      * Run it.
      *-----------------------------------------------------------------
           MOVE LOW-VALUES TO I_RUNFUNCTION
           MOVE 'ZTDB2PHN' TO MODULENAME IN ZWS_RUNFUNCTION
           MOVE 'ZTDB2PHN' TO FUNCTIONNAME IN ZWS_RUNFUNCTION
           CALL ZTESTUT USING ZWS_RUNFUNCTION

      *-----------------------------------------------------------------
      * Validate it.
      *-----------------------------------------------------------------
           PERFORM VALIDATE-RESULTS

           GOBACK.

      ******************************************************************
      * UNIT TEST 3: Use recorded data for DB2, mock PHUPDATE input.   *
      ******************************************************************
           ENTRY 'testHarnessWithMockMix'.

           MOVE LOW-VALUES TO I_CREATEHARNESS
           MOVE LOW-VALUES TO WS-ZHRNOPT-OPTIONS
           MOVE 'ZTDB2PHN' TO MEMBERNAME IN ZWS_CREATEHARNESS

           SET REAL_QSAM IN HO-HARNESS-OPTION(1) TO TRUE
           MOVE 'PHUPDATE' TO ARTIFACTNAMES IN HO-HARNESS-OPTION(1)
           MOVE 'ZTDB2PHN' TO MODULENAMES IN HO-HARNESS-OPTION(1)

           SET REAL_QSAM IN HO-HARNESS-OPTION(2) TO TRUE
           MOVE 'PHLOG' TO ARTIFACTNAMES IN HO-HARNESS-OPTION(2)
           MOVE 'ZTDB2PHN' TO MODULENAMES IN HO-HARNESS-OPTION(2)

           SET PTR IN MOCKOPTIONS IN ZWS_CREATEHARNESS TO
               ADDRESS OF WS-ZHRNOPT-OPTIONS
           MOVE 2 TO SIZ IN MOCKOPTIONS IN ZWS_CREATEHARNESS

           CALL ZTESTUT USING ZWS_CREATEHARNESS,
                HARNESSOBJECT IN WS-ZHRNESS-DATA

      *-----------------------------------------------------------------
      * Mock input using fixed records from this unit test.
      *-----------------------------------------------------------------
           MOVE LOW-VALUES TO I_MOCKQSAM
           MOVE 'PHUPDATE' TO FILENAME IN ZWS_MOCKQSAM
           MOVE LENGTH OF WS-NEW-PHONE-UPDATE-RECORD
                TO RECORDSIZE IN ZWS_MOCKQSAM
           CALL ZTESTUT USING ZWS_MOCKQSAM, 
                QSAMOBJECT IN WS-ZQSAM-PHONE-UPDATE-MOCK

           MOVE LOW-VALUES TO I_ADDRECORD
           SET FILEOBJECT IN ZWS_ADDRECORD 
                TO ADDRESS OF QSAMOBJECT IN WS-ZQSAM-PHONE-UPDATE-MOCK
           SET RECORDADDRESS IN ZWS_ADDRECORD
                TO ADDRESS OF WS-NEW-PHONE-UPDATE-RECORD
           MOVE LENGTH OF WS-NEW-PHONE-UPDATE-RECORD
                TO RECORDSIZE IN ZWS_ADDRECORD

           PERFORM VARYING I FROM 1 BY 1
                     UNTIL I > WS-PHONE-UPDATE-RECORDS-CNT
                MOVE PHONE-UPDATE-RECORD(I)
                     TO WS-NEW-PHONE-UPDATE-RECORD
                CALL ZTESTUT USING ZWS_ADDRECORD
           END-PERFORM

      *-----------------------------------------------------------------
      * Display it.
      *-----------------------------------------------------------------
           SET ADDRESS OF LS-ZQSAM-PHONE-UPDATE-MOCK
                TO ADDRESS OF WS-ZQSAM-PHONE-UPDATE-MOCK
           DISPLAY 'ZTTDB2PH Unit test 3: testHarnessWithMockMix'
           DISPLAY 'ZTTDB2PH requested updates from PHUPDATE:'    
           PERFORM PRINT-PHONE-UPDATES       

      *-----------------------------------------------------------------
      * Mock output.
      *-----------------------------------------------------------------
           MOVE LOW-VALUES TO I_MOCKQSAM
           MOVE 'PHLOG' TO FILENAME IN ZWS_MOCKQSAM
           MOVE 132 TO RECORDSIZE IN ZWS_MOCKQSAM
           CALL ZTESTUT USING ZWS_MOCKQSAM, 
                QSAMOBJECT IN WS-ZQSAM-PHONE-LOG-MOCK

      *-----------------------------------------------------------------
      * Spy on DB2.
      *-----------------------------------------------------------------
           PERFORM REGISTER-DB2-SPY.

      *-----------------------------------------------------------------
      * Run it.
      *-----------------------------------------------------------------
           MOVE LOW-VALUES TO I_RUNFUNCTION
           MOVE 'ZTDB2PHN' TO MODULENAME IN ZWS_RUNFUNCTION
           MOVE 'ZTDB2PHN' TO FUNCTIONNAME IN ZWS_RUNFUNCTION
           CALL ZTESTUT USING ZWS_RUNFUNCTION

      *-----------------------------------------------------------------
      * Validate it.
      *-----------------------------------------------------------------
           PERFORM VALIDATE-RESULTS

           GOBACK.

      ******************************************************************
      * CALLBACK: Echo spied DB2 commands to SYSOUT.                   *
      ******************************************************************
           ENTRY 'db2ObserveAndReport' USING LS-ZSPDB2-DATABASE-SPY.

           SET ADDRESS OF ZLS_DB2_CALL 
                TO LASTCALL IN LS-ZSPDB2-DATABASE-SPY

           MOVE SQLCODE IN ZLS_DB2_CALL TO WS-SQLCODE
           DISPLAY 'ZTTDB2PH DB2 callback' 
                ' COMMAND=' COMMAND IN ZLS_DB2_CALL(1:6)
                ' CODE=' SQLCODE IN ZLS_DB2_CALL

           GOBACK.

      ******************************************************************
      * Validate report output to PHLOG, confirming the expected       *
      * number of updates by request type.                             *
      ******************************************************************
       VALIDATE-RESULTS.

      *-----------------------------------------------------------------
      * This is a simple comparison of the expected number of
      * valid/invalid requests by type. Go through the processing
      * log and generate totals.
      *-----------------------------------------------------------------
           MOVE ZEROS TO WS-PHONE-UPDATE-STATS-ACTUAL

      *-----------------------------------------------------------------
      * Add the update log to SYSOUT for reference should the
      * validation fail.
      *-----------------------------------------------------------------
           DISPLAY 'ZTTDB2PH logged updates to PHLOG:'
           SET ADDRESS OF LS-ZQSAM-PHONE-LOG-MOCK 
                TO ADDRESS OF WS-ZQSAM-PHONE-LOG-MOCK
           PERFORM PRINT-PHONE-LOG           

      *-----------------------------------------------------------------
      * The output file was mocked, but it has all the written records.
      * Go through them and verify the counts match.
      *-----------------------------------------------------------------
           SET ADDRESS OF LS-PHONE-LOG-RECORDS
                TO PTR IN RECORDS_ IN WS-ZQSAM-PHONE-LOG-MOCK

           PERFORM VARYING I FROM 1 BY 1
                     UNTIL I > SIZE_ IN RECORDS_
                     IN WS-ZQSAM-PHONE-LOG-MOCK

                MOVE LS-PHONE-LOG-RECORD(I) TO PHL-PHONE-LOG-RECORD

      *-----------------------------------------------------------------
      * Check the valid/invalid flag; it's supposed to be Y/N.
      * We'll treat any other (unexpected) value as a miss.
      *-----------------------------------------------------------------
                IF PHL-VALID-REQUEST
                     MOVE 1 TO WS-VALID-REQUEST
                     MOVE 0 TO WS-INVALID-REQUEST
                ELSE
                     IF PHL-INVALID-REQUEST
                          MOVE 0 TO WS-VALID-REQUEST
                          MOVE 1 TO WS-INVALID-REQUEST
                     ELSE
                          MOVE 0 TO WS-VALID-REQUEST
                          MOVE 0 TO WS-INVALID-REQUEST
                     END-IF
                END-IF

      *-----------------------------------------------------------------
      * Add up the valid/invalid requests by type. It's an imperfect
      * test as two "opposite" errors could cancel each other out, but
      * that's unlikely for a large enough data set.
      *-----------------------------------------------------------------
                EVALUATE PHL-REQUEST-TYPE
                     WHEN 'D'
                          ADD WS-VALID-REQUEST TO SUT-DELETES-VALID
                          ADD WS-INVALID-REQUEST TO SUT-DELETES-INVALID
                     WHEN 'U'
                          ADD WS-VALID-REQUEST TO SUT-UPDATES-VALID
                          ADD WS-INVALID-REQUEST TO SUT-UPDATES-INVALID
                     WHEN 'C'
                          ADD WS-VALID-REQUEST TO SUT-CREATES-VALID
                          ADD WS-INVALID-REQUEST TO SUT-CREATES-INVALID
                     WHEN 'K'
                          ADD WS-VALID-REQUEST TO SUT-KEEPS-VALID
                          ADD WS-INVALID-REQUEST TO SUT-KEEPS-INVALID
                     WHEN OTHER 
                          ADD WS-INVALID-REQUEST TO SUT-REQUESTS-INVALID
                END-EVALUATE
           END-PERFORM

           IF WS-PHONE-UPDATE-STATS-ACTUAL NOT =
                     WS-PHONE-UPDATE-STATS-EXPECTED
                MOVE LOW-VALUES TO I_FAIL IN ZWS_FAIL
                MOVE 'ZTTDB2PH mismatch valid/invalid request counts' 
                     TO FAILMESSAGE IN ZWS_FAIL

                DISPLAY FAILMESSAGE IN ZWS_FAIL
                DISPLAY 'Expected=' WS-PHONE-UPDATE-STATS-EXPECTED
                DISPLAY '     Got=' WS-PHONE-UPDATE-STATS-ACTUAL

                CALL ZTESTUT USING ZWS_FAIL       
           ELSE
                DISPLAY 'ZTTDB2PH PASS valid/invalid request counts'
           END-IF

      *-----------------------------------------------------------------
      * Spot check a log record (#6); this one should be deleting 
      * James Madison. The other fields should be as previously
      * recorded.
      *-----------------------------------------------------------------
           MOVE LS-PHONE-LOG-RECORD(6) TO PHL-PHONE-LOG-RECORD

           IF NOT PHL-VALID-REQUEST OR
                     PHL-KEY NOT = '000003' OR
                     PHL-REQUEST-TYPE NOT = 'D' OR
                     PHL-STATUS NOT = 'Success' OR
                     PHL-LASTNAME NOT = 'Madison' OR                     
                     PHL-FIRSTNAME NOT = 'James' OR
                     PHL-PHONE NOT = '9195551717'

                MOVE LOW-VALUES TO I_FAIL IN ZWS_FAIL
                MOVE 'ZTTDB2PH mismatched log record' 
                     TO FAILMESSAGE IN ZWS_FAIL

                DISPLAY FAILMESSAGE IN ZWS_FAIL
                DISPLAY 'Expected='
                        'D|000003|Y|Success|Madison|James|9195551717' 
                DISPLAY '     Got='
                        PHL-PHONE-LOG-RECORD

                CALL ZTESTUT USING ZWS_FAIL       
           END-IF

           EXIT.

      ******************************************************************
      * Register the unit tests.                                       *
      ******************************************************************
       REGISTER-UNIT-TESTS.

           MOVE LOW-VALUES TO I_TEST
           SET TESTFUNCTION IN ZWS_TEST
                TO ENTRY 'doDemoResetDB2'
           MOVE 'Unit test 0: Reset EMPPHONE in real DB2' 
                TO TESTNAME IN ZWS_TEST
           CALL ZTESTUT USING ZWS_TEST

           MOVE LOW-VALUES TO I_TEST
           SET TESTFUNCTION IN ZWS_TEST
                TO ENTRY 'testHarnessInputRealDB2'
           MOVE 'Unit test 1: Harness recording with real DB2' 
                TO TESTNAME IN ZWS_TEST
           CALL ZTESTUT USING ZWS_TEST

           MOVE LOW-VALUES TO I_TEST
           SET TESTFUNCTION IN ZWS_TEST
                TO ENTRY 'testAllHarnessInput'
           MOVE 'Unit test 2: Harness recording for all' 
                TO TESTNAME IN ZWS_TEST
           CALL ZTESTUT USING ZWS_TEST
 
           MOVE LOW-VALUES TO I_TEST
           SET TESTFUNCTION IN ZWS_TEST
                TO ENTRY 'testHarnessWithMockMix'
           MOVE 'Unit test 3: Harness recording mixed'
                TO TESTNAME IN ZWS_TEST
           CALL ZTESTUT USING ZWS_TEST

           EXIT.

      ******************************************************************
      * Spy on DB2.                                                    *
      ******************************************************************
       REGISTER-DB2-SPY.

           MOVE LOW-VALUES TO I_SPYDB2
           MOVE 'ZTDB2PHN' TO MODULENAME IN ZWS_SPYDB2
           MOVE 'ZTDB2PHN' TO FUNCTIONNAME IN ZWS_SPYDB2
           SET CALLBACK IN ZWS_SPYDB2
                TO ENTRY 'db2ObserveAndReport'
           CALL ZTESTUT USING ZWS_SPYDB2, 
                DB2SPYOBJECT IN WS-ZSPDB2-DATABASE-SPY

           EXIT.

      ******************************************************************
      * Print the log records for quick validation/debug.              *
      ******************************************************************
       PRINT-PHONE-LOG.

           SET ADDRESS OF LS-PHONE-LOG-RECORDS
                TO PTR IN RECORDS_ IN LS-ZQSAM-PHONE-LOG-MOCK
           PERFORM VARYING I FROM 1 BY 1
                     UNTIL I > SIZE_ IN RECORDS_
                     IN LS-ZQSAM-PHONE-LOG-MOCK

                MOVE LS-PHONE-LOG-RECORD(I) TO PHL-PHONE-LOG-RECORD
                DISPLAY I ': REQ=' PHL-REQUEST-TYPE 
                     ' KEY=' PHL-KEY
                     ' OK=' PHL-REQUEST-VALIDITY-FLAG
                     ' ST=' PHL-STATUS
                     ' MSG=' PHL-MESSAGE(1:WS-LOG-FIELD-LENGTH-LG)
                     ' LN=' PHL-LASTNAME(1:WS-LOG-FIELD-LENGTH-SM)  
                     ' FN=' PHL-FIRSTNAME(1:WS-LOG-FIELD-LENGTH-SM)  
                     ' PH=' PHL-PHONE                     
           END-PERFORM

           EXIT.

      ******************************************************************
      * Print the update request records for quick validation/debug.   *
      ******************************************************************
       PRINT-PHONE-UPDATES.

           SET ADDRESS OF LS-PHONE-UPDATE-RECORDS
                TO PTR IN RECORDS_ IN LS-ZQSAM-PHONE-UPDATE-MOCK
           PERFORM VARYING I FROM 1 BY 1
                     UNTIL I > SIZE_ IN RECORDS_
                     IN LS-ZQSAM-PHONE-UPDATE-MOCK

                MOVE LS-PHONE-UPDATE-RECORD(I)
                     TO UPD-PHONE-UPDATE-FILE-RECORD
                DISPLAY I ': REQ=' UPD-REQUEST-TYPE 
                     ' KEY=' UPD-KEY
                     ' MSG=' UPD-MESSAGE(1:WS-LOG-FIELD-LENGTH-LG)
                     ' LN=' UPD-LASTNAME(1:WS-LOG-FIELD-LENGTH-SM)  
                     ' FN=' UPD-FIRSTNAME(1:WS-LOG-FIELD-LENGTH-SM)  
                     ' PH=' UPD-PHONE                     
           END-PERFORM

           EXIT.

      ******************************************************************
      * When running a unit test, parameters can be passed in via      *
      * ZLOPTS in the JCL running a DB2 unit test suite.               *
      *                                                                *
      * This specific unit test suite, ZTTDB2PH, uses this to control  *
      * which unit tests are run, and if DB2 should be mocked or real. *
      * A parameter of "DB2=REAL" tells the unit test suite that DB2   *
      * should not be mocked.                                          *
      *                                                                *
      *     //ZLOPTS DD *                                              *
      *     PARM(DB2=REAL)                                             *
      *     ZESTPARM(D=YOUR.TEST4Z.HLQ.TEST.LOAD,M=ZTTDB2PH)           *
      *     /*                                                         *
      *                                                                *
      * The above JCL executing this DB2 unit test suite would be      *
      * passed the parameter "DB2=REAL", which is checked when         *
      * deciding how to execute the test suite.                        *
      *                                                                *
      * Another parameter value is used for demonstrations,            *
      * DB2=DEMO-RESET. It clears the EMPPHONE table and adds a        *
      * well-known set of employees for demonstration purposes.        *
      *                                                                *
      * NB: Because of DB2 bind requirements, the DB2=REAL option      *
      *     will only work as expected from JCL; it will fail with     *
      *     SQL errors if specified in the Test4z CLI. See             *
      *     "Unit Test Processing" in the product documentation for    *
      *     more details and JCL examples.                             *
      ******************************************************************
       GET-TEST-OPTIONS.

           SET WS-SKIP-DEMO-RESET TO TRUE
           SET WS-DB2-IS-MOCKED TO TRUE
       
           MOVE LOW-VALUES TO I_GETPARM IN ZWS_GETPARM
           CALL ZTESTUT USING ZWS_GETPARM, WS-ZPARM-GET-MYOPTION
           SET ADDRESS OF LS-GET-PARM-MYOPTION
                TO PTR IN WS-ZPARM-GET-MYOPTION

      *-----------------------------------------------------------------
      * If it's a demo reset, DB2 must be real otherwise the DELETE
      * and INSERTs will be captured in the recording (not good), or 
      * it will fail because DB2 is mocked and the recording doesn't
      * include these operations.
      *-----------------------------------------------------------------   
           MOVE 0 TO I
           INSPECT LS-GET-PARM-MYOPTION
                (1:SIZ IN WS-ZPARM-GET-MYOPTION)
                TALLYING I FOR ALL 'DB2=DEMO-RESET'
           IF I > 0
                SET WS-DO-DEMO-RESET TO TRUE
                SET WS-DB2-IS-REAL TO TRUE                
           END-IF

      *-----------------------------------------------------------------
      * If it's not a demo reset, it still could be a live/real DB2
      * run. Check for the DB=REAL parameter; that will enable
      * unit test(s) that expect a real database versus recorded.
      *-----------------------------------------------------------------      
           IF WS-SKIP-DEMO-RESET
                MOVE 0 TO I
                INSPECT LS-GET-PARM-MYOPTION
                     (1:SIZ IN WS-ZPARM-GET-MYOPTION)
                     TALLYING I FOR ALL 'DB2=REAL'
                IF I > 0
                     SET WS-DB2-IS-REAL TO TRUE
                END-IF
           END-IF

           EXIT.
