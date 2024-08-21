       PROCESS PGMN(LM),NODYNAM
       IDENTIFICATION DIVISION.
       PROGRAM-ID. 'ZTTFILES' RECURSIVE.
       
      ******************************************************************
      * Broadcom Test4z Unit Test Suite (UT) example.                  *
      * Copyright (c) 2024 Broadcom. All Rights Reserved.              *
      *                                                                *
      * This test example shows how to code three unit tests:          *
      *                                                                *
      * 1. qsamQuickTest - This unit test initializes ZTPQSAMP's       *
      *    input QSAM file, intercepts file operations, and displays   *
      *    the written records. It then validates the expected number  *
      *    of written records.                                         *
      *                                                                *
      * 2. ksdsQuickTest - This unit test will intercept ZTPKSDSP's    *
      *    KSDS file operations and display the results. The KSDS      *
      *    file is initially empty and the SUT writes/deletes/reads    *
      *    its own dataset records. It then validates the expected     *
      *    number of written records.                                  *
      *                                                                *
      *  3. forceIOErrorSpyCallback - This unit test uses a spy that   *
      *     simulates an I/O error for the QSAM file.                  *
      *                                                                *
      * Below is the summary of the Test4z APIs demonstrated by        *
      * this example unit test suite.                                  *
      *                                                                *
      * 1). APIs for setting up input and output datasets:             *
      *                                                                *
      * == _AddRecord - add records to previously created mock file    *
      * == _MockKSDS - create mock (UT created) KSDS file              *
      * == _MockQSAM - create mock (UT created) QSAM file              *
      *                                                                *
      * 2). APIs for interrogating SUT execution:                      *
      *                                                                *
      * == _SpyKSDS - track KSDS file operations                       *
      * == _SpyQSAM - track QSAM file operations                       *
      *                                                                *
      * 3). APIs for reporting the unit test results:                  *
      *                                                                *
      * == _Fail - unconditionally ends the unit test                  *
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
      * ZWS_MOCKKSDS -> I_MOCKKSDS IN ZWS_MOCKKSDS                     *
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

      *-----------------------------------------------------------------       
      * QSAM mock and its associated spy.
      *-----------------------------------------------------------------
       01  WS-ZQSAM-ALPHA-FILE.
           COPY ZQSAM.
       01  WS-ZSPQSAM-ALPHA-FILE-SPY.
           COPY ZSPQSAM.

      *-----------------------------------------------------------------
      * KSDS mock and its associated spy.
      *-----------------------------------------------------------------
       01  WS-ZKSDS-BETA-FILE.
           COPY ZKSDS.
       01  WS-ZSPKSDS-BETA-FILE-SPY.
           COPY ZSPKSDS.

      *-----------------------------------------------------------------
      * Used to generate records A, BB, CCC, DDDD, EEEEE, etc.
      *-----------------------------------------------------------------
       01  LETTERS PIC X(10) VALUE 'ABCDEFGHIJ'.
       01  LETTER-REDEF REDEFINES LETTERS.
           05 ALPHA PIC X OCCURS 10 TIMES.

       77  I PIC 9(2).
       77  J PIC 9(2).

       01  WS-NEW-ALPHA-RECORD   PIC X(80) VALUE SPACES.

      *-----------------------------------------------------------------
      * See forceIOErrorSpyCallback for details.
      *-----------------------------------------------------------------
       01  WS-FORCE-IO-ERROR-CALLBACK.
           05 COMMAND                PIC X(32) VALUE 'READ'.
           05 COUNTER                PIC 9(2)  VALUE 5.
           05 STATUSCODE             PIC X(2)  VALUE '46'.
           05 CONDCODE               PIC X     VALUE X'40'.
       01  WS-FORCE-IO-ERROR-COUNTER PIC 9(2)  VALUE 0.

       LINKAGE SECTION.

      ******************************************************************
      * Copy in our required control blocks from Test4z.               *
      ******************************************************************
       COPY ZTESTLS.

      *-----------------------------------------------------------------
      * Spy object's callback parameters on entry.
      *-----------------------------------------------------------------
       01  LS-ZSPQSAM-SPY-CALLBACK.
           COPY ZSPQSAM.

       01  LS-ZSPKSDS-SPY-CALLBACK.
           COPY ZSPKSDS.

      *-----------------------------------------------------------------
      * See forceIOErrorSpyCallback for details.
      *-----------------------------------------------------------------
       01  LS-IFBLOCK-STATUSCODE PIC X(2).
       01  LS-IFBLOCK-CONDCODE   PIC X.

      *-----------------------------------------------------------------
      * Used to map records provided to the spy during a callback.
      *-----------------------------------------------------------------      
       01  LS-ALPHA-RECORD       PIC X(80).
       01  LS-BETA-RECORD        PIC X(128).

       PROCEDURE DIVISION.

      *-----------------------------------------------------------------
      * Register the tests to be run.
      *-----------------------------------------------------------------
           MOVE LOW-VALUES TO I_TEST
           SET TESTFUNCTION IN ZWS_TEST TO ENTRY 'qsamQuickTest'
           MOVE 'Unit test 1: QSAM write/read test'
                TO TESTNAME IN ZWS_TEST
           CALL ZTESTUT USING ZWS_TEST

           MOVE LOW-VALUES TO I_TEST
           SET TESTFUNCTION IN ZWS_TEST
                TO ENTRY 'ksdsQuickTest'
           MOVE 'Unit test 2: KSDS write/read test'
                TO TESTNAME IN ZWS_TEST
           CALL ZTESTUT USING ZWS_TEST

           MOVE LOW-VALUES TO I_TEST
           SET TESTFUNCTION IN ZWS_TEST
                TO ENTRY 'qsamUnhappyPathTest'
           MOVE 'Unit test 3: QSAM I/O error "unhappy path" test'
                TO TESTNAME IN ZWS_TEST
           CALL ZTESTUT USING ZWS_TEST
           
      *-----------------------------------------------------------------
      * Once all of the tests have been registered, return back to
      * Test4z to start processing.
      *-----------------------------------------------------------------

           GOBACK.

      ******************************************************************
      * UNIT TEST 1: Entry point for the QSAM test.                    *
      ******************************************************************
           ENTRY 'qsamQuickTest'.

      *-----------------------------------------------------------------
      * Create a QSAM file object to be intercepted when being
      * accessed by program ZTPQSAMP.
      *-----------------------------------------------------------------
           MOVE LOW-VALUES TO I_MOCKQSAM
           MOVE 'TESTQSAM' TO FILENAME IN ZWS_MOCKQSAM
           MOVE 80 TO RECORDSIZE IN ZWS_MOCKQSAM
           CALL ZTESTUT USING ZWS_MOCKQSAM, 
                QSAMOBJECT IN WS-ZQSAM-ALPHA-FILE

      *-----------------------------------------------------------------
      * Let's add some records A, BB, CCC, DDDD, EEEEE... through J.
      *-----------------------------------------------------------------
           MOVE LOW-VALUES TO I_ADDRECORD
           SET FILEOBJECT IN ZWS_ADDRECORD
                TO ADDRESS OF QSAMOBJECT IN WS-ZQSAM-ALPHA-FILE
           SET RECORDADDRESS IN ZWS_ADDRECORD 
                TO ADDRESS OF WS-NEW-ALPHA-RECORD
           MOVE LENGTH OF WS-NEW-ALPHA-RECORD
                TO RECORDSIZE IN ZWS_ADDRECORD

           PERFORM VARYING I FROM 1 BY 1 UNTIL I > 10
                PERFORM VARYING J FROM 1 BY 1 UNTIL J > I
                      MOVE ALPHA(I) TO WS-NEW-ALPHA-RECORD(J:1)
                END-PERFORM

                CALL ZTESTUT USING ZWS_ADDRECORD
           END-PERFORM

      *-----------------------------------------------------------------
      * Spy on the QSAM file; the callback will be invoked on all
      * file operations.
      *-----------------------------------------------------------------
           MOVE LOW-VALUES TO I_SPYQSAM
           MOVE 'TESTQSAM' TO FILENAME IN ZWS_SPYQSAM
           SET CALLBACK IN ZWS_SPYQSAM
                TO ENTRY 'qsamAlphaSpyCallback'
           CALL ZTESTUT USING ZWS_SPYQSAM, 
                QSAMSPYOBJECT IN WS-ZSPQSAM-ALPHA-FILE-SPY

      *-----------------------------------------------------------------
      * Start ZTPQSAMP and receive callbacks in qsamAlphaSpyCallback.
      *-----------------------------------------------------------------
           MOVE LOW-VALUES TO I_RUNFUNCTION
           MOVE 'ZTPQSAMP' TO MODULENAME IN ZWS_RUNFUNCTION
           MOVE 'ZTPQSAMP' TO FUNCTIONNAME IN ZWS_RUNFUNCTION
           CALL ZTESTUT USING ZWS_RUNFUNCTION

      *-----------------------------------------------------------------
      * The SUT has ended.
      *
      * It's a minor test, but let's check the number of records.
      * There were 10 records added by this unit test and 16
      * that were appended by the SUT, so the totals should be 26.
      * They should also be in alphabetical order (although validating
      * that is an exercise for the reader).
      *-----------------------------------------------------------------
           IF SIZE_ IN RECORDS_ IN WS-ZQSAM-ALPHA-FILE NOT = 26
                MOVE LOW-VALUES TO I_FAIL IN ZWS_FAIL
                MOVE 'ZTTFILES record count incorrect for ALPHA-FILE'
                     TO FAILMESSAGE IN ZWS_FAIL

                DISPLAY FAILMESSAGE IN ZWS_FAIL
                DISPLAY 'Expected=26'
                DISPLAY '     Got=' 
                     SIZE_ IN RECORDS_ IN WS-ZQSAM-ALPHA-FILE

                CALL ZTESTUT USING ZWS_FAIL                     
           ELSE
                DISPLAY 'ZTTFILES correct record count for ALPHA-FILE'
           END-IF

           GOBACK.

      ******************************************************************
      * Unit test 2: Entry point for the KSDS test.                    *
      ******************************************************************
           entry 'ksdsQuickTest'.

      *-----------------------------------------------------------------
      * Create a KSDS file object to be intercepted when being
      * accessed by program ZTPKSDSP. As there is no data being
      * provided by this test case, we do not set the address of any
      * file object. Instead we set the record size in the mocking
      * object.
      *-----------------------------------------------------------------
           MOVE LOW-VALUES TO I_MOCKKSDS
           MOVE 'TESTKSDS' TO FILENAME IN ZWS_MOCKKSDS
           MOVE 1 TO KEYOFFSET IN ZWS_MOCKKSDS
           MOVE 8 TO KEYLENGTH IN ZWS_MOCKKSDS
           MOVE 128 TO RECORDSIZE IN ZWS_MOCKKSDS
           CALL ZTESTUT USING ZWS_MOCKKSDS, 
                KSDSOBJECT IN WS-ZKSDS-BETA-FILE

      *-----------------------------------------------------------------
      * KSDS mock's associated spy.
      *-----------------------------------------------------------------
           MOVE LOW-VALUES TO I_SPYKSDS
           MOVE 'TESTKSDS' TO FILENAME IN ZWS_SPYKSDS
           SET CALLBACK IN ZWS_SPYKSDS TO ENTRY 'ksdsBetaSpyCallback'
           CALL ZTESTUT USING ZWS_SPYKSDS, 
                KSDSSPYOBJECT IN WS-ZSPKSDS-BETA-FILE-SPY

      *-----------------------------------------------------------------
      * Start ZTPKSDSP and receive callbacks in ksdsBetaSpyCallback.
      *-----------------------------------------------------------------
           MOVE LOW-VALUES TO I_RUNFUNCTION
           MOVE 'ZTPKSDSP' TO MODULENAME IN ZWS_RUNFUNCTION
           MOVE 'ZTPKSDSP' TO FUNCTIONNAME IN ZWS_RUNFUNCTION
           CALL ZTESTUT USING ZWS_RUNFUNCTION
      
      *-----------------------------------------------------------------
      * The SUT has ended.
      *
      * It's a minor test, but let's check the number of records.
      * The SUT added 10 records and then deleted 5, so there should 
      * be 5 remaining records.
      *-----------------------------------------------------------------      
           IF SIZE_ IN RECORDS_ IN WS-ZKSDS-BETA-FILE NOT = 5
                MOVE LOW-VALUES TO I_FAIL IN ZWS_FAIL
                MOVE 'ZTTFILES record count incorrect for BETA-FILE'
                     TO FAILMESSAGE IN ZWS_FAIL

                DISPLAY FAILMESSAGE IN ZWS_FAIL
                DISPLAY 'Expected=5'
                DISPLAY '     Got=' 
                     SIZE_ IN RECORDS_ IN WS-ZKSDS-BETA-FILE

                CALL ZTESTUT USING ZWS_FAIL
           ELSE
                DISPLAY 'ZTTFILES correct record count for BETA-FILE'
           END-IF

           GOBACK.

      ******************************************************************
      * UNIT TEST 3: Entry point for the QSAM "unhappy path" test.     *
      ******************************************************************
           ENTRY 'qsamUnhappyPathTest'.

      *-----------------------------------------------------------------
      * Create a QSAM file object to be intercepted when being
      * accessed by program ZTPQSAMP.
      *-----------------------------------------------------------------
           MOVE LOW-VALUES TO I_MOCKQSAM
           MOVE 'TESTQSAM' TO FILENAME IN ZWS_MOCKQSAM
           MOVE 80 TO RECORDSIZE IN ZWS_MOCKQSAM
           CALL ZTESTUT USING ZWS_MOCKQSAM, 
                QSAMOBJECT IN WS-ZQSAM-ALPHA-FILE

      *-----------------------------------------------------------------
      * Register a spy that will simulate a file I/O error.
      *-----------------------------------------------------------------
           MOVE LOW-VALUES TO I_SPYQSAM
           MOVE 'TESTQSAM' TO FILENAME IN ZWS_SPYQSAM
           SET CALLBACK IN ZWS_SPYQSAM
                TO ENTRY 'forceIOErrorSpyCallback'
           CALL ZTESTUT USING ZWS_SPYQSAM, 
                QSAMSPYOBJECT IN WS-ZSPQSAM-ALPHA-FILE-SPY

      *-----------------------------------------------------------------
      * Start ZTPQSAMP and receive callbacks in forceIOErrorSpyCallback.
      *-----------------------------------------------------------------
           MOVE LOW-VALUES TO I_RUNFUNCTION
           MOVE 'ZTPQSAMP' TO MODULENAME IN ZWS_RUNFUNCTION
           MOVE 'ZTPQSAMP' TO FUNCTIONNAME IN ZWS_RUNFUNCTION
           CALL ZTESTUT USING ZWS_RUNFUNCTION

      *-----------------------------------------------------------------
      * The SUT has ended. Confirm that it detected the READ error
      * forced by the 'forceIOErrorSpyCallback' callback.
      *-----------------------------------------------------------------   
           IF RETURN-CODE NOT = 8
                MOVE LOW-VALUES TO I_FAIL IN ZWS_FAIL
                MOVE 'Expected error not set in RETURN-CODE'
                     TO FAILMESSAGE IN ZWS_FAIL

                DISPLAY 'ZTTFILES ' FAILMESSAGE IN ZWS_FAIL
                DISPLAY 'Expected=8'
                DISPLAY '     Got=' RETURN-CODE

                CALL ZTESTUT USING ZWS_FAIL
           ELSE
                DISPLAY 'ZTTFILES received expected RETURN-CODE '
                        RETURN-CODE ' from ZTPQSAMP'
           END-IF

           GOBACK.

      ******************************************************************
      * CALLBACK: This entry point is called for the ALPHA-FILE        *
      *           QSAM file after an I/O operation.                    *
      ******************************************************************
           ENTRY 'qsamAlphaSpyCallback' USING LS-ZSPQSAM-SPY-CALLBACK.
           
      *-----------------------------------------------------------------
      * Map the linkage section to the address of the last
      * file operation (OPEN, CLOSE, READ, WRITE).
      *-----------------------------------------------------------------
           SET ADDRESS OF ZLS_QSAM_RECORD 
                TO LASTCALL IN LS-ZSPQSAM-SPY-CALLBACK

      *-----------------------------------------------------------------
      * Display each read/written record when intercepted.
      *-----------------------------------------------------------------      
           IF STATUSCODE IN ZLS_QSAM_RECORD = '00' AND   
                     (COMMAND IN ZLS_QSAM_RECORD = 'READ' OR
                     COMMAND IN ZLS_QSAM_RECORD = 'WRITE')
                SET ADDRESS OF LS-ALPHA-RECORD TO
                     PTR IN RECORD_ IN ZLS_QSAM_RECORD
                DISPLAY 'ZTTFILES callback'
                     ' command=' COMMAND IN ZLS_QSAM_RECORD(1:5)
                     ' size=' SIZ IN RECORD_ IN ZLS_QSAM_RECORD
                     ' record=' LS-ALPHA-RECORD
           ELSE
                DISPLAY 'ZTTFILES callback skipping command='
                     COMMAND IN ZLS_QSAM_RECORD
           END-IF

           GOBACK.

      ******************************************************************
      * CALLBACK: This entry point is called for the BETA-FILE         *
      *           KSDS file after an I/O operation.                    *
      ******************************************************************
           ENTRY 'ksdsBetaSpyCallback' USING LS-ZSPKSDS-SPY-CALLBACK.

      *-----------------------------------------------------------------
      * Map the linkage section to the address of the last
      * file operation (OPEN, CLOSE, READ, WRITE).
      *-----------------------------------------------------------------
           SET ADDRESS OF ZLS_KSDS_RECORD
                TO LASTCALL IN LS-ZSPKSDS-SPY-CALLBACK

      *-----------------------------------------------------------------
      * Display each written record when intercepted.
      *-----------------------------------------------------------------
           IF STATUSCODE1 IN ZLS_KSDS_RECORD = '00' AND
                     (COMMAND IN ZLS_KSDS_RECORD = 'READ' OR 
                     COMMAND IN ZLS_KSDS_RECORD = 'WRITE')
                SET ADDRESS OF LS-BETA-RECORD TO
                     PTR IN RECORD_ IN ZLS_KSDS_RECORD
                DISPLAY 'ZTTFILES callback'
                     ' command=' COMMAND IN ZLS_KSDS_RECORD(1:5)
                     ' size=' SIZ IN RECORD_ IN ZLS_KSDS_RECORD
                     ' record=' LS-BETA-RECORD
           ELSE
                DISPLAY 'ZTTFILES callback skipping command='
                     COMMAND IN ZLS_KSDS_RECORD
           END-IF

           GOBACK.

      ******************************************************************
      * CALLBACK: This entry point forces an I/O "unhappy path" error  *
      *           by setting a failing status and condition code       *
      *           under the conditions defined in                      *
      *           WS-FORCE-IO-ERROR-CALLBACK. The program under test   *
      *           should reflect this I/O back by setting its          *
      *           final return code.                                   *
      ******************************************************************
           ENTRY 'forceIOErrorSpyCallback' USING LS-ZSPQSAM-SPY-CALLBACK

      *-----------------------------------------------------------------
      * To simulate an I/O error, use Test4z's intercepted middleware
      * call interface block (ZLS_Q_IFBLOCK). That includes the
      * status and condition code that will be returned to the
      * program under test.
      *-----------------------------------------------------------------
           SET ADDRESS OF ZLS_QSAM_RECORD 
                TO LASTCALL IN LS-ZSPQSAM-SPY-CALLBACK
           SET ADDRESS OF ZLS_Q_IFBLOCK
                TO IFBLOCK IN LS-ZSPQSAM-SPY-CALLBACK
  
           IF COMMAND IN ZLS_QSAM_RECORD =
                COMMAND IN WS-FORCE-IO-ERROR-CALLBACK

      *-----------------------------------------------------------------
      * Simulate an error on the operation defined in 
      * WS-FORCE-IO-ERROR-CALLBACK. This definition chooses
      * the error to be returned, e.g., OPEN, READ, WRITE.
      *
      * In this specific example, WS-FORCE-IO-ERROR-CALLBACK is
      * initialized to indicate we want to fail the 5th READ request. 
      * In a more robust unit test suite, it would include OPEN
      * and WRITE failures to verify the SUT handles them correctly
      * (e.g., by setting a RETURN-CODE and logging the error).
      *-----------------------------------------------------------------
                ADD 1 TO WS-FORCE-IO-ERROR-COUNTER
                
                IF WS-FORCE-IO-ERROR-COUNTER =
                     COUNTER IN WS-FORCE-IO-ERROR-CALLBACK

                     DISPLAY 'ZTTFILES forcing ' 
                          COMMAND IN WS-FORCE-IO-ERROR-CALLBACK(1:5) 
                          ' error for invocation #'
                          COUNTER IN WS-FORCE-IO-ERROR-CALLBACK
                          ' by setting status code to '
                          STATUSCODE IN WS-FORCE-IO-ERROR-CALLBACK
                          ' with its respective condition code'

      *-----------------------------------------------------------------   
      * WS-FORCE-IO-ERROR-CALLBACK matches our targeted error scenario,
      * so set the status and condition code accordingly; it will then
      * be returned to the program under test.
      *
      * NB: In order that COBOL I/O error statements are handled
      *     correctly, the status AND condition code must be set.
      *-----------------------------------------------------------------    
                     SET ADDRESS OF LS-IFBLOCK-STATUSCODE 
                          TO STATUSCODE IN ZLS_Q_IFBLOCK
                     SET ADDRESS OF LS-IFBLOCK-CONDCODE 
                          TO CONDCODE IN ZLS_Q_IFBLOCK

                     MOVE STATUSCODE IN WS-FORCE-IO-ERROR-CALLBACK
                          TO LS-IFBLOCK-STATUSCODE
                     MOVE CONDCODE IN WS-FORCE-IO-ERROR-CALLBACK
                          TO LS-IFBLOCK-CONDCODE
                END-IF
           END-IF
            
           GOBACK.
