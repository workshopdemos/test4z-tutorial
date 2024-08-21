       PROCESS PGMN(LM),NODYNAM
      ******************************************************************
      * Broadcom Test4z System Under Test (SUT) example.               *
      * Copyright (c) 2024 Broadcom. All Rights Reserved.              *
      *                                                                *
      * Technical Preview: Evaluation purposes only.                   *
      ******************************************************************

      *----------------------------------------------------------------*
      * This test example shows how to run a multiple CICS unit tests  *
      * using previously recorded data. Each of these test cases are   *
      * from recordings of individual CICS transactions that receive   *
      * and process channel/container-style programs that respond to   *
      * web requests.                                                  *
      *                                                                *
      * A similar example to this is is located in the ZTTCICSM member *
      * that shows how to set up test cases for the same transactions  *
      * but by using mocked data entirely.                             *
      *----------------------------------------------------------------*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. 'ZTTCICSR' RECURSIVE.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
      ******************************************************************
      * Copy in our required control blocks from Test4z.               *
      ******************************************************************
       COPY ZTESTWS.

      ******************************************************************
      * Define any working-storage items that we need.                 *
      ******************************************************************
       1 HARNESS_DATA.
         COPY ZHRNESS.

       1 CICS_DATA.
         COPY ZSPCICS.

      *----------------------------------------------------------------*
      * Data specific to all tests                                     *
      *----------------------------------------------------------------*
       1 RECORDED-DATA-MEMBER PIC X(8).
       1 EXPECTED-RETURN-POINTER USAGE POINTER.
       1 ACTUAL-RETURN-LENGTH PIC 9(9) COMP-5.

      *----------------------------------------------------------------*
      * Data specific to test 1                                        *
      *----------------------------------------------------------------*
       1 EXPECTED-RETURN-1.
         3 PIC X(18) VALUE 'CSYY/MM/DDHH:MM:SS'.
         3 PIC X(97) VALUE '000001CREATE SUCCESSFUL'.

      *----------------------------------------------------------------*
      * Data specific to test 2                                        *
      *----------------------------------------------------------------*
       1 EXPECTED-RETURN-2.
         3 PIC X(18) VALUE 'USYY/MM/DDHH:MM:SS'.
         3 PIC X(97) VALUE '000001UPDATE SUCCESSFUL'.

      *----------------------------------------------------------------*
      * Data specific to test 3                                        *
      *----------------------------------------------------------------*
       1 EXPECTED-RETURN-3.
         3 PIC X(18) VALUE 'USYY/MM/DDHH:MM:SS'.
         3 PIC X(97) VALUE '000001UPDATE SUCCESSFUL'.

      *----------------------------------------------------------------*
      * Data specific to test 4                                        *
      *----------------------------------------------------------------*
       1 EXPECTED-RETURN-4.
         3 PIC X(18) VALUE 'USYY/MM/DDHH:MM:SS'.
         3 PIC X(97) VALUE '000001UPDATE SUCCESSFUL'.

      *----------------------------------------------------------------*
      * Data specific to test 5                                        *
      *----------------------------------------------------------------*
       1 EXPECTED-RETURN-5.
         3 PIC X(18) VALUE 'CSYY/MM/DDHH:MM:SS'.
         3 PIC X(97) VALUE '000002CREATE SUCCESSFUL'.

      *----------------------------------------------------------------*
      * Data specific to test 6                                        *
      *----------------------------------------------------------------*
       1 EXPECTED-RETURN-6.
         3 PIC X(18) VALUE 'USYY/MM/DDHH:MM:SS'.
         3 PIC X(97) VALUE '000002UPDATE SUCCESSFUL'.

      *----------------------------------------------------------------*
      * Data specific to test 7                                        *
      *----------------------------------------------------------------*
       1 EXPECTED-RETURN-7.
         3 PIC X(18) VALUE 'USYY/MM/DDHH:MM:SS'.
         3 PIC X(97) VALUE '000002UPDATE SUCCESSFUL'.

      *----------------------------------------------------------------*
      * Data specific to test 8                                        *
      *----------------------------------------------------------------*
       1 EXPECTED-RETURN-8.
         3 PIC X(18) VALUE 'USYY/MM/DDHH:MM:SS'.
         3 PIC X(97) VALUE '000002UPDATE SUCCESSFUL'.

      *----------------------------------------------------------------*
      * Data specific to test 9                                        *
      *----------------------------------------------------------------*
       1 EXPECTED-RETURN-9.
         3 PIC X(18) VALUE 'KSYY/MM/DDHH:MM:SS'.
         3 PIC X(97) VALUE '000001KEEP SUCCESSFUL'.

      *----------------------------------------------------------------*
      * Data specific to test 10                                       *
      *----------------------------------------------------------------*
       1 EXPECTED-RETURN-10.
         3 PIC X(18) VALUE 'DFYY/MM/DDHH:MM:SS'.
         3 PIC X(97) VALUE 'ATTEMPT TO DELETE KEPT RECORD 000001'.

      *----------------------------------------------------------------*
      * Data specific to test 11                                       *
      *----------------------------------------------------------------*
       1 EXPECTED-RETURN-11.
         3 PIC X(18) VALUE 'SSYY/MM/DDHH:MM:SS'.
         3 PIC X(06) VALUE '000001'.
         3 PIC X(01) VALUE 'K'.
         3 PIC X(40) VALUE 'SMITH'.
         3 PIC X(40) VALUE 'JOHN'.
         3 PIC X(10) VALUE '2123456789'.

       1 WORK-RETURN.
         3 RETURN-TYPE      PIC X(1).
         3 RETURN-STATUS    PIC X(1).
         3 RETURN-DATE      PIC X(8).
         3 RETURN-TIME      PIC X(8).
         3 RETURN-AREA.
           5 RETURN-ID        PIC X(6).
           5 RETURN-MESSAGE   PIC X(91).

       LINKAGE SECTION.
      ******************************************************************
      * Copy in our required control blocks from Test4z.               *
      ******************************************************************
       COPY ZTESTLS.

      ******************************************************************
      * Define any linkage section items that we need.                 *
      ******************************************************************
       1 LS_CICS_DATA.
         COPY ZSPCICS.

       1 EXPECTED-RETURN.
         3 RETURN-TYPE      PIC X(1).
         3 RETURN-STATUS    PIC X(1).
         3 RETURN-DATE      PIC X(8).
         3 RETURN-TIME      PIC X(8).
         3 RETURN-AREA.
           5 RETURN-ID        PIC X(6).
           5 RETURN-MESSAGE   PIC X(91).

       1 CONTAINER-RETURN.
         3 RETURN-TYPE      PIC X(1).
         3 RETURN-STATUS    PIC X(1).
         3 RETURN-DATE      PIC X(8).
         3 RETURN-TIME      PIC X(8).
         3 RETURN-AREA.
           5 RETURN-ID        PIC X(6).
           5 RETURN-MESSAGE   PIC X(91).

      ******************************************************************
      * Register a set of tests to be run. There are 11 tests in this  *
      * test suite. Each is from a different transaction that runs a   *
      * different channel/container-style application.                 *
      ******************************************************************
       PROCEDURE DIVISION.
           MOVE LOW-VALUES TO I_TEST
           SET TESTFUNCTION IN ZWS_TEST TO ENTRY 'myDataTest1'
           MOVE 'Create new record 1 test' TO TESTNAME IN ZWS_TEST
           CALL ZTESTUT USING ZWS_TEST

           MOVE LOW-VALUES TO I_TEST
           SET TESTFUNCTION IN ZWS_TEST TO ENTRY 'myDataTest2'
           MOVE 'Update record 1 last name' TO TESTNAME IN ZWS_TEST
           CALL ZTESTUT USING ZWS_TEST

           MOVE LOW-VALUES TO I_TEST
           SET TESTFUNCTION IN ZWS_TEST TO ENTRY 'myDataTest3'
           MOVE 'Update record 1 first name' TO TESTNAME IN ZWS_TEST
           CALL ZTESTUT USING ZWS_TEST

           MOVE LOW-VALUES TO I_TEST
           SET TESTFUNCTION IN ZWS_TEST TO ENTRY 'myDataTest4'
           MOVE 'Update record 1 phone number' TO TESTNAME IN ZWS_TEST
           CALL ZTESTUT USING ZWS_TEST

           MOVE LOW-VALUES TO I_TEST
           SET TESTFUNCTION IN ZWS_TEST TO ENTRY 'myDataTest5'
           MOVE 'Create new record 2 test' TO TESTNAME IN ZWS_TEST
           CALL ZTESTUT USING ZWS_TEST

           MOVE LOW-VALUES TO I_TEST
           SET TESTFUNCTION IN ZWS_TEST TO ENTRY 'myDataTest6'
           MOVE 'Update record 2 last name' TO TESTNAME IN ZWS_TEST
           CALL ZTESTUT USING ZWS_TEST

           MOVE LOW-VALUES TO I_TEST
           SET TESTFUNCTION IN ZWS_TEST TO ENTRY 'myDataTest7'
           MOVE 'Update record 2 first name' TO TESTNAME IN ZWS_TEST
           CALL ZTESTUT USING ZWS_TEST

           MOVE LOW-VALUES TO I_TEST
           SET TESTFUNCTION IN ZWS_TEST TO ENTRY 'myDataTest8'
           MOVE 'Update record 2 phone number' TO TESTNAME IN ZWS_TEST
           CALL ZTESTUT USING ZWS_TEST

           MOVE LOW-VALUES TO I_TEST
           SET TESTFUNCTION IN ZWS_TEST TO ENTRY 'myDataTest9'
           MOVE 'Set record 1 to be kept' TO TESTNAME IN ZWS_TEST
           CALL ZTESTUT USING ZWS_TEST

           MOVE LOW-VALUES TO I_TEST
           SET TESTFUNCTION IN ZWS_TEST TO ENTRY 'myDataTest10'
           MOVE 'Attempt delete on kept record 1' TO
                TESTNAME IN ZWS_TEST
           CALL ZTESTUT USING ZWS_TEST

           MOVE LOW-VALUES TO I_TEST
           SET TESTFUNCTION IN ZWS_TEST TO ENTRY 'myDataTest11'
           MOVE 'Search on last name' TO TESTNAME IN ZWS_TEST
           CALL ZTESTUT USING ZWS_TEST

      *----------------------------------------------------------------*
      * Once all of the tests have been registered, return back to     *
      * Test4z to start processing.                                    *
      *----------------------------------------------------------------*
           GOBACK.

      ******************************************************************
      *                                                                *
      *                                                                *
      * This test will test the logic that creates record 1.           *
      * It will load the recording in the test harness.                *
      * It will create a spy handler with a callback for CICS calls.   *
      * The spy callback will verify the results in the returned PUT   *
      * CONTAINER call.                                                *
      *                                                                *
      *                                                                *
      ******************************************************************
           ENTRY 'myDataTest1'.
      *----------------------------------------------------------------*
      * Set up address of our expected return data and recorded member.*
      * Then perform the test itself.                                  *
      *----------------------------------------------------------------*
           SET EXPECTED-RETURN-POINTER TO ADDRESS OF EXPECTED-RETURN-1
           MOVE 'ZTPCCXR1' TO RECORDED-DATA-MEMBER
           PERFORM RUN-TEST
           GOBACK.

      ******************************************************************
      *                                                                *
      *                                                                *
      * This test will test the update record 1 last name logic.       *
      * It will load the recording in the test harness.                *
      * It will create a spy handler with a callback for CICS calls.   *
      * The spy callback will verify the results in the returned PUT   *
      * CONTAINER call.                                                *
      *                                                                *
      *                                                                *
      ******************************************************************
           ENTRY 'myDataTest2'.
      *----------------------------------------------------------------*
      * Set up address of our expected return data and recorded member.*
      * Then perform the test itself.                                  *
      *----------------------------------------------------------------*
           SET EXPECTED-RETURN-POINTER TO ADDRESS OF EXPECTED-RETURN-2
           MOVE 'ZTPCCXR2' TO RECORDED-DATA-MEMBER
           PERFORM RUN-TEST
           GOBACK.

      ******************************************************************
      *                                                                *
      *                                                                *
      * This test will test the update record 1 first name logic.      *
      * It will load the recording in the test harness.                *
      * It will create a spy handler with a callback for CICS calls.   *
      * The spy callback will verify the results in the returned PUT   *
      * CONTAINER call.                                                *
      *                                                                *
      *                                                                *
      ******************************************************************
           ENTRY 'myDataTest3'.
      *----------------------------------------------------------------*
      * Set up address of our expected return data and recorded member.*
      * Then perform the test itself.                                  *
      *----------------------------------------------------------------*
           SET EXPECTED-RETURN-POINTER TO ADDRESS OF EXPECTED-RETURN-3
           MOVE 'ZTPCCXR3' TO RECORDED-DATA-MEMBER
           PERFORM RUN-TEST
           GOBACK.

      ******************************************************************
      *                                                                *
      *                                                                *
      * This test will test the update record 1 phone number logic.    *
      * It will load the recording in the test harness.                *
      * It will create a spy handler with a callback for CICS calls.   *
      * The spy callback will verify the results in the returned PUT   *
      * CONTAINER call.                                                *
      *                                                                *
      *                                                                *
      ******************************************************************
           ENTRY 'myDataTest4'.
      *----------------------------------------------------------------*
      * Set up address of our expected return data and recorded member.*
      * Then perform the test itself.                                  *
      *----------------------------------------------------------------*
           SET EXPECTED-RETURN-POINTER TO ADDRESS OF EXPECTED-RETURN-4
           MOVE 'ZTPCCXR4' TO RECORDED-DATA-MEMBER
           PERFORM RUN-TEST
           GOBACK.

      ******************************************************************
      *                                                                *
      *                                                                *
      * This test will test the logic that creates record 2.           *
      * It will load the recording in the test harness.                *
      * It will create a spy handler with a callback for CICS calls.   *
      * The spy callback will verify the results in the returned PUT   *
      * CONTAINER call.                                                *
      *                                                                *
      *                                                                *
      ******************************************************************
           ENTRY 'myDataTest5'.
      *----------------------------------------------------------------*
      * Set up address of our expected return data and recorded member.*
      * Then perform the test itself.                                  *
      *----------------------------------------------------------------*
           SET EXPECTED-RETURN-POINTER TO ADDRESS OF EXPECTED-RETURN-5
           MOVE 'ZTPCCXR5' TO RECORDED-DATA-MEMBER
           PERFORM RUN-TEST
           GOBACK.

      ******************************************************************
      *                                                                *
      *                                                                *
      * This test will test the update record 2 last name logic.       *
      * It will load the recording in the test harness.                *
      * It will create a spy handler with a callback for CICS calls.   *
      * The spy callback will verify the results in the returned PUT   *
      * CONTAINER call.                                                *
      *                                                                *
      *                                                                *
      ******************************************************************
           ENTRY 'myDataTest6'.
      *----------------------------------------------------------------*
      * Set up address of our expected return data and recorded member.*
      * Then perform the test itself.                                  *
      *----------------------------------------------------------------*
           SET EXPECTED-RETURN-POINTER TO ADDRESS OF EXPECTED-RETURN-6
           MOVE 'ZTPCCXR6' TO RECORDED-DATA-MEMBER
           PERFORM RUN-TEST
           GOBACK.

      ******************************************************************
      *                                                                *
      *                                                                *
      * This test will test the update record 2 first name logic.      *
      * It will load the recording in the test harness.                *
      * It will create a spy handler with a callback for CICS calls.   *
      * The spy callback will verify the results in the returned PUT   *
      * CONTAINER call.                                                *
      *                                                                *
      *                                                                *
      ******************************************************************
           ENTRY 'myDataTest7'.
      *----------------------------------------------------------------*
      * Set up address of our expected return data and recorded member.*
      * Then perform the test itself.                                  *
      *----------------------------------------------------------------*
           SET EXPECTED-RETURN-POINTER TO ADDRESS OF EXPECTED-RETURN-7
           MOVE 'ZTPCCXR7' TO RECORDED-DATA-MEMBER
           PERFORM RUN-TEST
           GOBACK.

      ******************************************************************
      *                                                                *
      *                                                                *
      * This test will test the update record 2 phone number logic.    *
      * It will load the recording in the test harness.                *
      * It will create a spy handler with a callback for CICS calls.   *
      * The spy callback will verify the results in the returned PUT   *
      * CONTAINER call.                                                *
      *                                                                *
      *                                                                *
      ******************************************************************
           ENTRY 'myDataTest8'.
      *----------------------------------------------------------------*
      * Set up address of our expected return data and recorded member.*
      * Then perform the test itself.                                  *
      *----------------------------------------------------------------*
           SET EXPECTED-RETURN-POINTER TO ADDRESS OF EXPECTED-RETURN-8
           MOVE 'ZTPCCXR8' TO RECORDED-DATA-MEMBER
           PERFORM RUN-TEST
           GOBACK.

      ******************************************************************
      *                                                                *
      *                                                                *
      * This test will test the set record 1 to be kept logic.         *
      * It will load the recording in the test harness.                *
      * It will create a spy handler with a callback for CICS calls.   *
      * The spy callback will verify the results in the returned PUT   *
      * CONTAINER call.                                                *
      *                                                                *
      *                                                                *
      ******************************************************************
           ENTRY 'myDataTest9'.
      *----------------------------------------------------------------*
      * Set up address of our expected return data and recorded member.*
      * Then perform the test itself.                                  *
      *----------------------------------------------------------------*
           SET EXPECTED-RETURN-POINTER TO ADDRESS OF EXPECTED-RETURN-9
           MOVE 'ZTPCCXR9' TO RECORDED-DATA-MEMBER
           PERFORM RUN-TEST
           GOBACK.

      ******************************************************************
      *                                                                *
      *                                                                *
      * This test will test the attempt delete on kept record 1 logic. *
      * It will load the recording in the test harness.                *
      * It will create a spy handler with a callback for CICS calls.   *
      * The spy callback will verify the results in the returned PUT   *
      * CONTAINER call.                                                *
      *                                                                *
      *                                                                *
      ******************************************************************
           ENTRY 'myDataTest10'.
      *----------------------------------------------------------------*
      * Set up address of our expected return data and recorded member.*
      * Then perform the test itself.                                  *
      *----------------------------------------------------------------*
           SET EXPECTED-RETURN-POINTER TO ADDRESS OF EXPECTED-RETURN-10
           MOVE 'ZTPCCXRA' TO RECORDED-DATA-MEMBER
           PERFORM RUN-TEST
           GOBACK.

      ******************************************************************
      *                                                                *
      *                                                                *
      * This test will test the search on last name logic.             *
      * It will load the recording in the test harness.                *
      * It will create a spy handler with a callback for CICS calls.   *
      * The spy callback will verify the results in the returned PUT   *
      * CONTAINER call.                                                *
      *                                                                *
      *                                                                *
      ******************************************************************
           ENTRY 'myDataTest11'.
      *----------------------------------------------------------------*
      * Set up address of our expected return data and recorded member.*
      * Then perform the test itself.                                  *
      *----------------------------------------------------------------*
           SET EXPECTED-RETURN-POINTER TO ADDRESS OF EXPECTED-RETURN-11
           MOVE 'ZTPCCXRB' TO RECORDED-DATA-MEMBER
           PERFORM RUN-TEST
           GOBACK.

      ******************************************************************
      *                                                                *
      *                                                                *
      * This spy will be called on all CICS calls. We will look for    *
      * the PUT CONTAINER call and verify the results.                 *
      *                                                                *
      *                                                                *
      ******************************************************************
           ENTRY 'mySpyTest' USING LS_CICS_DATA.
      *----------------------------------------------------------------*
      * Set the address of the call block to the last call from CICS   *
      *----------------------------------------------------------------*
           SET ADDRESS OF ZLS_CICS_CALL TO LASTCALL IN LS_CICS_DATA
      *----------------------------------------------------------------*
      * We are interested in the returned value for our sample, which  *
      * is provided in the PUT CONTAINER call. Instesad we could watch *
      * a variable for change instead, but this CICS spy seems easier. *
      *----------------------------------------------------------------*
           IF COMMAND IN ZLS_CICS_CALL = 'PUT CONTAINER'
      *----------------------------------------------------------------*
      * We are in a PUT CONTAINER call. Get the FROM argument.         *
      * Map the FROM argument to a linkage section item.               *
      * In this get argument call, we add the optional field to        *
      * receive the length of the argument.                            *
      *----------------------------------------------------------------*
              MOVE LOW-VALUES TO I_GETCICSARGUMENT
              MOVE 'FROM' TO ARGUMENTNAME IN ZWS_GETCICSARGUMENT
              CALL ZTESTUT USING ZWS_GETCICSARGUMENT,
                   ADDRESS OF CONTAINER-RETURN, ACTUAL-RETURN-LENGTH
      *----------------------------------------------------------------*
      * Ensure there is a FROM argument. If not, then issue fail.      *
      * The fail API does not return, but causes the test case to end. *
      *----------------------------------------------------------------*
              IF ADDRESS OF CONTAINER-RETURN = NULL
                 MOVE LOW-VALUES TO I_FAIL IN ZWS_FAIL
                 MOVE 'Missing argument "FROM" on PUT CONTAINER call'
                      TO FAILMESSAGE IN ZWS_FAIL
                 CALL ZTESTUT USING ZWS_FAIL
              END-IF
      *----------------------------------------------------------------*
      * Ensure there is enough data returned by checking the length.   *
      *----------------------------------------------------------------*
              IF ACTUAL-RETURN-LENGTH NOT = LENGTH WORK-RETURN
                 MOVE LOW-VALUES TO I_FAIL IN ZWS_FAIL
                 MOVE 'Insufficient returned data on PUT CONTAINER call'
                      TO FAILMESSAGE IN ZWS_FAIL
                 CALL ZTESTUT USING ZWS_FAIL
              END-IF
      *----------------------------------------------------------------*
      * Compare with the values that we are expecting.                 *
      * For the date/time field we will make them the same to assist   *
      * in the comparison.                                             *
      *----------------------------------------------------------------*
              SET ADDRESS OF EXPECTED-RETURN TO EXPECTED-RETURN-POINTER
              MOVE EXPECTED-RETURN TO WORK-RETURN
              MOVE RETURN-DATE IN CONTAINER-RETURN TO
                   RETURN-DATE IN WORK-RETURN
              MOVE RETURN-TIME IN CONTAINER-RETURN TO
                   RETURN-TIME IN WORK-RETURN
      *----------------------------------------------------------------*
      * Check for a different in the expected and actual values.       *
      *----------------------------------------------------------------*
              IF WORK-RETURN NOT = CONTAINER-RETURN
      *----------------------------------------------------------------*
      * The values to not match. We want to produce several lines of   *
      * messages in the test result. The first lines use the Message   *
      * API which only writes the message, but allows us to continue.  *
      *----------------------------------------------------------------*
                 MOVE LOW-VALUES TO I_MESSAGE IN ZWS_MESSAGE
                 STRING 'Expected: ' DELIMITED BY SIZE
                        WORK-RETURN DELIMITED BY SIZE
                        INTO MESSAGETEXT IN ZWS_MESSAGE
                 CALL ZTESTUT USING ZWS_MESSAGE
      *----------------------------------------------------------------*
      * Write the second message line and continue.                    *
      *----------------------------------------------------------------*
                 MOVE LOW-VALUES TO I_MESSAGE IN ZWS_MESSAGE
                 STRING 'Received: ' DELIMITED BY SIZE
                        CONTAINER-RETURN DELIMITED BY SIZE
                        INTO MESSAGETEXT IN ZWS_MESSAGE
                 CALL ZTESTUT USING ZWS_MESSAGE
      *----------------------------------------------------------------*
      * Cause the failure with the third line of message text.         *
      *----------------------------------------------------------------*
                 MOVE LOW-VALUES TO I_FAIL IN ZWS_FAIL
                 MOVE 'Returned data invalid in test'
                      TO FAILMESSAGE IN ZWS_FAIL
                 CALL ZTESTUT USING ZWS_FAIL
              END-IF
           END-IF

           GOBACK.

      ******************************************************************
      * This is the common test case execution routine.                *
      ******************************************************************
       RUN-TEST.
      *----------------------------------------------------------------*
      * Create a test harness with loaded data for a CICS test program.*
      * The data comes from a previous recording and is stored in a    *
      * partitioned dataset member named 'ZTPCCXR1'. This recorded     *
      * data is in JSON format.                                        *
      *----------------------------------------------------------------*
           MOVE LOW-VALUES TO I_CREATEHARNESS
           MOVE RECORDED-DATA-MEMBER TO MEMBERNAME IN ZWS_CREATEHARNESS
           CALL ZTESTUT USING ZWS_CREATEHARNESS,
                HARNESSOBJECT IN HARNESS_DATA

      *----------------------------------------------------------------*
      * Create a spy on all CICS calls and when the PUT CONTAINER call *
      * is intercepted, check the returned data for expected data.     *
      *----------------------------------------------------------------*
           MOVE LOW-VALUES TO I_SPYCICS
           MOVE 'ZTPCICST' TO MODULENAME IN ZWS_SPYCICS
           SET CALLBACK IN ZWS_SPYCICS TO ENTRY 'mySpyTest'
           CALL ZTESTUT USING ZWS_SPYCICS, CICSSPYOBJECT IN CICS_DATA

      *----------------------------------------------------------------*
      * Create a loaded data object for a CICS test program. The data  *
      * comes from a previous recording and is stored in a partitioned *
      * dataset member named 'ZTPCICST'. This recorded data is in JSON *
      * format.                                                        *
      *----------------------------------------------------------------*
           MOVE LOW-VALUES TO I_DISPATCHCICS
           MOVE 'ZTPCICST' TO MODULENAME IN ZWS_DISPATCHCICS
           CALL ZTESTUT USING ZWS_DISPATCHCICS

           EXIT.
