       PROCESS PGMN(LM),NODYNAM
      ******************************************************************
      * Broadcom Test4z System Under Test (SUT) example.               *
      * Copyright (c) 2024 Broadcom. All Rights Reserved.              *
      *                                                                *
      * Technical Preview: Evaluation purposes only.                   *
      ******************************************************************

      *----------------------------------------------------------------*
      * This test example shows how to run a multiple CICS unit tests  *
      * using mocked data entirely. Each of these test cases are       *
      * individual CICS transactions that receive and process          *
      * channel/container-style programs that respond to web requests. *
      * Mocking objects will be set up for the VSAM/KSDS file I/O and  *
      * the channel/container will be created. Any other required CICS *
      * statements will be mocked with a MockCICSStatement API.        *
      *                                                                *
      * A similar example to this is is located in the ZTTCICSR member *
      * that shows how to set up test cases for the same transactions  *
      * but by using previously recorded data.                         *
      *----------------------------------------------------------------*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. 'ZTTCICSM' RECURSIVE.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
      ******************************************************************
      * Copy in our required control blocks from Test4z.               *
      ******************************************************************
       COPY ZTESTWS.

      ******************************************************************
      * Define any working-storage items that we need.                 *
      ******************************************************************
       1 MY_FORMATTIME.
         2 MY_FORMATTIME_ARGS OCCURS 5.
           COPY ZCICSMK.

       1 CICS_DATA.
         COPY ZCICS.
         COPY ZSPCICS.

       1 FILE_DATA.
         COPY ZFILE.

       1 KSDS_DATA.
         COPY ZKSDS.

       1 MY_ABSTIME    PIC X(8) VALUE X'003928673228860C'.
       1 MY_YYMMDD     PIC X(8) VALUE '24/06/29'.
       1 MY_TIME       PIC X(8) VALUE '18:07:08'.
       1 MY_DATESEP    PIC X(1) VALUE '-'.
       1 MY_TIMESEP    PIC X(1) VALUE ':'.

      *----------------------------------------------------------------*
      * Data specific to all tests                                     *
      *----------------------------------------------------------------*
       1 CONTAINER-DATA-POINTER USAGE POINTER.
       1 CONTAINER-DATA-LENGTH  PIC 9(9) COMP-5.
       1 KSDS-RECORD-POINTER USAGE POINTER.
       1 KSDS-RECORD-COUNT   PIC 9(9) COMP-5.
       1 EXPECTED-RETURN-POINTER USAGE POINTER.
       1 ACTUAL-RETURN-LENGTH PIC 9(9) COMP-5.

      *----------------------------------------------------------------*
      * Data specific to test 1                                        *
      *----------------------------------------------------------------*
       1 CONTAINER-DATA-1 PIC X(01) VALUE 'C'.
       1 KSDS-RECORD-COUNT-1 PIC 9(9) COMP-5 VALUE 0.
       1 EXPECTED-RETURN-1.
         3 PIC X(18) VALUE 'CSYY/MM/DDHH:MM:SS'.
         3 PIC X(97) VALUE '000001CREATE SUCCESSFUL'.

      *----------------------------------------------------------------*
      * Data specific to test 2                                        *
      *----------------------------------------------------------------*
       1 CONTAINER-DATA-2 PIC X(14) VALUE 'U000001L=SMITH'.
       1 KSDS-RECORD-COUNT-2 PIC 9(9) COMP-5 VALUE 2.
       1 KSDS-RECORD-DATA-2.
         3 RECORD-1 PIC X(97) VALUE '000000000001'.
         3 RECORD-2.
           5 PIC X(07) VALUE '000001 '.
           5 PIC X(40) VALUE ' '.
           5 PIC X(40) VALUE ' '.
           5 PIC X(10) VALUE ' '.
       1 EXPECTED-RETURN-2.
         3 PIC X(18) VALUE 'USYY/MM/DDHH:MM:SS'.
         3 PIC X(97) VALUE '000001UPDATE SUCCESSFUL'.

      *----------------------------------------------------------------*
      * Data specific to test 3                                        *
      *----------------------------------------------------------------*
       1 CONTAINER-DATA-3 PIC X(13) VALUE 'U000001F=JOHN'.
       1 KSDS-RECORD-COUNT-3 PIC 9(9) COMP-5 VALUE 2.
       1 KSDS-RECORD-DATA-3.
         3 RECORD-1 PIC X(97) VALUE '000000000001'.
         3 RECORD-2.
           5 PIC X(07) VALUE '000001 '.
           5 PIC X(40) VALUE 'SMITH'.
           5 PIC X(40) VALUE ' '.
           5 PIC X(10) VALUE ' '.
       1 EXPECTED-RETURN-3.
         3 PIC X(18) VALUE 'USYY/MM/DDHH:MM:SS'.
         3 PIC X(97) VALUE '000001UPDATE SUCCESSFUL'.

      *----------------------------------------------------------------*
      * Data specific to test 4                                        *
      *----------------------------------------------------------------*
       1 CONTAINER-DATA-4 PIC X(19) VALUE 'U000001P=2123456789'.
       1 KSDS-RECORD-COUNT-4 PIC 9(9) COMP-5 VALUE 2.
       1 KSDS-RECORD-DATA-4.
         3 RECORD-1 PIC X(97) VALUE '000000000001'.
         3 RECORD-2.
           5 PIC X(07) VALUE '000001 '.
           5 PIC X(40) VALUE 'SMITH'.
           5 PIC X(40) VALUE 'JOHN '.
           5 PIC X(10) VALUE ' '.
       1 EXPECTED-RETURN-4.
         3 PIC X(18) VALUE 'USYY/MM/DDHH:MM:SS'.
         3 PIC X(97) VALUE '000001UPDATE SUCCESSFUL'.

      *----------------------------------------------------------------*
      * Data specific to test 5                                        *
      *----------------------------------------------------------------*
       1 CONTAINER-DATA-5 PIC X(01) VALUE 'C'.
       1 KSDS-RECORD-COUNT-5 PIC 9(9) COMP-5 VALUE 2.
       1 KSDS-RECORD-DATA-5.
         3 RECORD-1 PIC X(97) VALUE '000000000001'.
         3 RECORD-2.
           5 PIC X(07) VALUE '000001 '.
           5 PIC X(40) VALUE 'SMITH'.
           5 PIC X(40) VALUE 'JOHN'.
           5 PIC X(10) VALUE '2123456789'.
       1 EXPECTED-RETURN-5.
         3 PIC X(18) VALUE 'CSYY/MM/DDHH:MM:SS'.
         3 PIC X(97) VALUE '000002CREATE SUCCESSFUL'.

      *----------------------------------------------------------------*
      * Data specific to test 6                                        *
      *----------------------------------------------------------------*
       1 CONTAINER-DATA-6 PIC X(14) VALUE 'U000002L=JONES'.
       1 KSDS-RECORD-COUNT-6 PIC 9(9) COMP-5 VALUE 3.
       1 KSDS-RECORD-DATA-6.
         3 RECORD-1 PIC X(97) VALUE '000000000002'.
         3 RECORD-2.
           5 PIC X(07) VALUE '000001 '.
           5 PIC X(40) VALUE 'SMITH'.
           5 PIC X(40) VALUE 'JOHN'.
           5 PIC X(10) VALUE '2123456789'.
         3 RECORD-3.
           5 PIC X(07) VALUE '000002 '.
           5 PIC X(40) VALUE ' '.
           5 PIC X(40) VALUE ' '.
           5 PIC X(10) VALUE ' '.
       1 EXPECTED-RETURN-6.
         3 PIC X(18) VALUE 'USYY/MM/DDHH:MM:SS'.
         3 PIC X(97) VALUE '000002UPDATE SUCCESSFUL'.

      *----------------------------------------------------------------*
      * Data specific to test 7                                        *
      *----------------------------------------------------------------*
       1 CONTAINER-DATA-7 PIC X(17) VALUE 'U000002F=SAMANTHA'.
       1 KSDS-RECORD-COUNT-7 PIC 9(9) COMP-5 VALUE 3.
       1 KSDS-RECORD-DATA-7.
         3 RECORD-1 PIC X(97) VALUE '000000000002'.
         3 RECORD-2.
           5 PIC X(07) VALUE '000001 '.
           5 PIC X(40) VALUE 'SMITH'.
           5 PIC X(40) VALUE 'JOHN'.
           5 PIC X(10) VALUE '2123456789'.
         3 RECORD-3.
           5 PIC X(07) VALUE '000002 '.
           5 PIC X(40) VALUE 'JONES'.
           5 PIC X(40) VALUE ' '.
           5 PIC X(10) VALUE ' '.
       1 EXPECTED-RETURN-7.
         3 PIC X(18) VALUE 'USYY/MM/DDHH:MM:SS'.
         3 PIC X(97) VALUE '000002UPDATE SUCCESSFUL'.

      *----------------------------------------------------------------*
      * Data specific to test 8                                        *
      *----------------------------------------------------------------*
       1 CONTAINER-DATA-8 PIC X(19) VALUE 'U000002P=9195430123'.
       1 KSDS-RECORD-COUNT-8 PIC 9(9) COMP-5 VALUE 3.
       1 KSDS-RECORD-DATA-8.
         3 RECORD-1 PIC X(97) VALUE '000000000002'.
         3 RECORD-2.
           5 PIC X(07) VALUE '000001 '.
           5 PIC X(40) VALUE 'SMITH'.
           5 PIC X(40) VALUE 'JOHN'.
           5 PIC X(10) VALUE '2123456789'.
         3 RECORD-3.
           5 PIC X(07) VALUE '000002 '.
           5 PIC X(40) VALUE 'JONES'.
           5 PIC X(40) VALUE 'SAMANTHA'.
           5 PIC X(10) VALUE ' '.
       1 EXPECTED-RETURN-8.
         3 PIC X(18) VALUE 'USYY/MM/DDHH:MM:SS'.
         3 PIC X(97) VALUE '000002UPDATE SUCCESSFUL'.

      *----------------------------------------------------------------*
      * Data specific to test 9                                        *
      *----------------------------------------------------------------*
       1 CONTAINER-DATA-9 PIC X(07) VALUE 'K000001'.
       1 KSDS-RECORD-COUNT-9 PIC 9(9) COMP-5 VALUE 3.
       1 KSDS-RECORD-DATA-9.
         3 RECORD-1 PIC X(97) VALUE '000000000002'.
         3 RECORD-2.
           5 PIC X(07) VALUE '000001 '.
           5 PIC X(40) VALUE 'SMITH'.
           5 PIC X(40) VALUE 'JOHN'.
           5 PIC X(10) VALUE '2123456789'.
         3 RECORD-3.
           5 PIC X(07) VALUE '000002 '.
           5 PIC X(40) VALUE 'JONES'.
           5 PIC X(40) VALUE 'SAMANTHA'.
           5 PIC X(10) VALUE '9195430123'.
       1 EXPECTED-RETURN-9.
         3 PIC X(18) VALUE 'KSYY/MM/DDHH:MM:SS'.
         3 PIC X(97) VALUE '000001KEEP SUCCESSFUL'.

      *----------------------------------------------------------------*
      * Data specific to test 10                                       *
      *----------------------------------------------------------------*
       1 CONTAINER-DATA-10 PIC X(07) VALUE 'D000001'.
       1 KSDS-RECORD-COUNT-10 PIC 9(9) COMP-5 VALUE 3.
       1 KSDS-RECORD-DATA-10.
         3 RECORD-1 PIC X(97) VALUE '000000000002'.
         3 RECORD-2.
           5 PIC X(07) VALUE '000001K'.
           5 PIC X(40) VALUE 'SMITH'.
           5 PIC X(40) VALUE 'JOHN'.
           5 PIC X(10) VALUE '2123456789'.
         3 RECORD-3.
           5 PIC X(07) VALUE '000002 '.
           5 PIC X(40) VALUE 'JONES'.
           5 PIC X(40) VALUE 'SAMANTHA'.
           5 PIC X(10) VALUE '9195430123'.
       1 EXPECTED-RETURN-10.
         3 PIC X(18) VALUE 'DFYY/MM/DDHH:MM:SS'.
         3 PIC X(97) VALUE 'ATTEMPT TO DELETE KEPT RECORD 000001'.

      *----------------------------------------------------------------*
      * Data specific to test 11                                       *
      *----------------------------------------------------------------*
       1 CONTAINER-DATA-11 PIC X(08) VALUE 'SL=SMITH'.
       1 KSDS-RECORD-COUNT-11 PIC 9(9) COMP-5 VALUE 3.
       1 KSDS-RECORD-DATA-11.
         3 RECORD-1 PIC X(97) VALUE '000000000002'.
         3 RECORD-2.
           5 PIC X(07) VALUE '000001K'.
           5 PIC X(40) VALUE 'SMITH'.
           5 PIC X(40) VALUE 'JOHN'.
           5 PIC X(10) VALUE '2123456789'.
         3 RECORD-3.
           5 PIC X(07) VALUE '000002 '.
           5 PIC X(40) VALUE 'JONES'.
           5 PIC X(40) VALUE 'SAMANTHA'.
           5 PIC X(10) VALUE '9195430123'.
       1 EXPECTED-RETURN-11.
         3 PIC X(18) VALUE 'SSYY/MM/DDHH:MM:SS'.
         3 PIC X(06) VALUE '000001'.
         3 PIC X(01) VALUE 'K'.
         3 PIC X(40) VALUE 'SMITH'.
         3 PIC X(40) VALUE 'JOHN'.
         3 PIC X(10) VALUE '2123456789'.

      *----------------------------------------------------------------*
      * Data specific to test 12                                       *
      *----------------------------------------------------------------*
       1 CONTAINER-DATA-12 PIC X(08) VALUE 'SL=SMITH'.
       1 KSDS-RECORD-COUNT-12 PIC 9(9) COMP-5 VALUE 3.
       1 KSDS-RECORD-DATA-12.
         3 RECORD-1 PIC X(97) VALUE '000000000002'.
         3 RECORD-2.
           5 PIC X(07) VALUE '000001K'.
           5 PIC X(40) VALUE 'SMITH'.
           5 PIC X(40) VALUE 'JOHN'.
           5 PIC X(10) VALUE '2123456789'.
         3 RECORD-3.
           5 PIC X(07) VALUE '000002 '.
           5 PIC X(40) VALUE 'JONES'.
           5 PIC X(40) VALUE 'SAMANTHA'.
           5 PIC X(10) VALUE '9195430123'.
       1 EXPECTED-RETURN-12.
         3 PIC X(18) VALUE 'SFYY/MM/DDHH:MM:SS'.
         3 PIC X(97) VALUE 'CONTAINER MESSAGE LENGTH WAS TOO LONG'.

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

       1 ABSTIME-RETURN PIC X(8).

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

           MOVE LOW-VALUES TO I_TEST
           SET TESTFUNCTION IN ZWS_TEST TO ENTRY 'myDataTest12'
           MOVE 'Test setting of LENGERR' TO TESTNAME IN ZWS_TEST
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
      * It will create a mocking object to attach to.                  *
      * It will create a spy handler with a callback for CICS calls.   *
      * The spy callback will verify the results in the returned PUT   *
      * CONTAINER call.                                                *
      *                                                                *
      *                                                                *
      ******************************************************************
           ENTRY 'myDataTest1'.

      *----------------------------------------------------------------*
      * Set up address of our provided KSDS and container data, and    *
      * the expected return data. Then perform the test itself.        *
      *----------------------------------------------------------------*
           SET KSDS-RECORD-POINTER TO NULL
           MOVE KSDS-RECORD-COUNT-1 TO KSDS-RECORD-COUNT
           SET CONTAINER-DATA-POINTER TO ADDRESS OF CONTAINER-DATA-1
           MOVE LENGTH OF CONTAINER-DATA-1 TO CONTAINER-DATA-LENGTH
           SET EXPECTED-RETURN-POINTER TO ADDRESS OF EXPECTED-RETURN-1
           PERFORM RUN-TEST
           GOBACK.

      ******************************************************************
      *                                                                *
      *                                                                *
      * This test will test the update record 1 last name logic.       *
      * It will create a mocking object to attach to.                  *
      * It will create a spy handler with a callback for CICS calls.   *
      * The spy callback will verify the results in the returned PUT   *
      * CONTAINER call.                                                *
      *                                                                *
      *                                                                *
      ******************************************************************
           ENTRY 'myDataTest2'.
      *----------------------------------------------------------------*
      * Set up address of our provided KSDS and container data, and    *
      * the expected return data. Then perform the test itself.        *
      *----------------------------------------------------------------*
           SET KSDS-RECORD-POINTER TO ADDRESS OF KSDS-RECORD-DATA-2
           MOVE KSDS-RECORD-COUNT-2 TO KSDS-RECORD-COUNT
           SET CONTAINER-DATA-POINTER TO ADDRESS OF CONTAINER-DATA-2
           MOVE LENGTH OF CONTAINER-DATA-2 TO CONTAINER-DATA-LENGTH
           SET EXPECTED-RETURN-POINTER TO ADDRESS OF EXPECTED-RETURN-2
           PERFORM RUN-TEST
           GOBACK.

      ******************************************************************
      *                                                                *
      *                                                                *
      * This test will test the update record 1 first name logic.      *
      * It will create a mocking object to attach to.                  *
      * It will create a spy handler with a callback for CICS calls.   *
      * The spy callback will verify the results in the returned PUT   *
      * CONTAINER call.                                                *
      *                                                                *
      *                                                                *
      ******************************************************************
           ENTRY 'myDataTest3'.
      *----------------------------------------------------------------*
      * Set up address of our provided KSDS and container data, and    *
      * the expected return data. Then perform the test itself.        *
      *----------------------------------------------------------------*
           SET KSDS-RECORD-POINTER TO ADDRESS OF KSDS-RECORD-DATA-3
           MOVE KSDS-RECORD-COUNT-3 TO KSDS-RECORD-COUNT
           SET CONTAINER-DATA-POINTER TO ADDRESS OF CONTAINER-DATA-3
           MOVE LENGTH OF CONTAINER-DATA-3 TO CONTAINER-DATA-LENGTH
           SET EXPECTED-RETURN-POINTER TO ADDRESS OF EXPECTED-RETURN-3
           PERFORM RUN-TEST
           GOBACK.

      ******************************************************************
      *                                                                *
      *                                                                *
      * This test will test the update record 1 phone number logic.    *
      * It will create a mocking object to attach to.                  *
      * It will create a spy handler with a callback for CICS calls.   *
      * The spy callback will verify the results in the returned PUT   *
      * CONTAINER call.                                                *
      *                                                                *
      *                                                                *
      ******************************************************************
           ENTRY 'myDataTest4'.
      *----------------------------------------------------------------*
      * Set up address of our provided KSDS and container data, and    *
      * the expected return data. Then perform the test itself.        *
      *----------------------------------------------------------------*

           SET KSDS-RECORD-POINTER TO ADDRESS OF KSDS-RECORD-DATA-4
           MOVE KSDS-RECORD-COUNT-4 TO KSDS-RECORD-COUNT
           SET CONTAINER-DATA-POINTER TO ADDRESS OF CONTAINER-DATA-4
           MOVE LENGTH OF CONTAINER-DATA-4 TO CONTAINER-DATA-LENGTH
           SET EXPECTED-RETURN-POINTER TO ADDRESS OF EXPECTED-RETURN-4
           PERFORM RUN-TEST
           GOBACK.

      ******************************************************************
      *                                                                *
      *                                                                *
      * This test will test the logic that creates record 2.           *
      * It will create a mocking object to attach to.                  *
      * It will create a spy handler with a callback for CICS calls.   *
      * The spy callback will verify the results in the returned PUT   *
      * CONTAINER call.                                                *
      *                                                                *
      *                                                                *
      ******************************************************************
           ENTRY 'myDataTest5'.
      *----------------------------------------------------------------*
      * Set up address of our provided KSDS and container data, and    *
      * the expected return data. Then perform the test itself.        *
      *----------------------------------------------------------------*
           SET KSDS-RECORD-POINTER TO ADDRESS OF KSDS-RECORD-DATA-5
           MOVE KSDS-RECORD-COUNT-5 TO KSDS-RECORD-COUNT
           SET CONTAINER-DATA-POINTER TO ADDRESS OF CONTAINER-DATA-5
           MOVE LENGTH OF CONTAINER-DATA-5 TO CONTAINER-DATA-LENGTH
           SET EXPECTED-RETURN-POINTER TO ADDRESS OF EXPECTED-RETURN-5
           PERFORM RUN-TEST
           GOBACK.

      ******************************************************************
      *                                                                *
      *                                                                *
      * This test will test the update record 2 last name logic.       *
      * It will create a mocking object to attach to.                  *
      * It will create a spy handler with a callback for CICS calls.   *
      * The spy callback will verify the results in the returned PUT   *
      * CONTAINER call.                                                *
      *                                                                *
      *                                                                *
      ******************************************************************
           ENTRY 'myDataTest6'.
      *----------------------------------------------------------------*
      * Set up address of our provided KSDS and container data, and    *
      * the expected return data. Then perform the test itself.        *
      *----------------------------------------------------------------*
           SET KSDS-RECORD-POINTER TO ADDRESS OF KSDS-RECORD-DATA-6
           MOVE KSDS-RECORD-COUNT-6 TO KSDS-RECORD-COUNT
           SET CONTAINER-DATA-POINTER TO ADDRESS OF CONTAINER-DATA-6
           MOVE LENGTH OF CONTAINER-DATA-6 TO CONTAINER-DATA-LENGTH
           SET EXPECTED-RETURN-POINTER TO ADDRESS OF EXPECTED-RETURN-6
           PERFORM RUN-TEST
           GOBACK.

      ******************************************************************
      *                                                                *
      *                                                                *
      * This test will test the update record 2 first name logic.      *
      * It will create a mocking object to attach to.                  *
      * It will create a spy handler with a callback for CICS calls.   *
      * The spy callback will verify the results in the returned PUT   *
      * CONTAINER call.                                                *
      *                                                                *
      *                                                                *
      ******************************************************************
           ENTRY 'myDataTest7'.
      *----------------------------------------------------------------*
      * Set up address of our provided KSDS and container data, and    *
      * the expected return data. Then perform the test itself.        *
      *----------------------------------------------------------------*
           SET KSDS-RECORD-POINTER TO ADDRESS OF KSDS-RECORD-DATA-7
           MOVE KSDS-RECORD-COUNT-7 TO KSDS-RECORD-COUNT
           SET CONTAINER-DATA-POINTER TO ADDRESS OF CONTAINER-DATA-7
           MOVE LENGTH OF CONTAINER-DATA-7 TO CONTAINER-DATA-LENGTH
           SET EXPECTED-RETURN-POINTER TO ADDRESS OF EXPECTED-RETURN-7
           PERFORM RUN-TEST
           GOBACK.

      ******************************************************************
      *                                                                *
      *                                                                *
      * This test will test the update record 2 phone number logic.    *
      * It will create a mocking object to attach to.                  *
      * It will create a spy handler with a callback for CICS calls.   *
      * The spy callback will verify the results in the returned PUT   *
      * CONTAINER call.                                                *
      *                                                                *
      *                                                                *
      ******************************************************************
           ENTRY 'myDataTest8'.
      *----------------------------------------------------------------*
      * Set up address of our provided KSDS and container data, and    *
      * the expected return data. Then perform the test itself.        *
      *----------------------------------------------------------------*
           SET KSDS-RECORD-POINTER TO ADDRESS OF KSDS-RECORD-DATA-8
           MOVE KSDS-RECORD-COUNT-8 TO KSDS-RECORD-COUNT
           SET CONTAINER-DATA-POINTER TO ADDRESS OF CONTAINER-DATA-8
           MOVE LENGTH OF CONTAINER-DATA-8 TO CONTAINER-DATA-LENGTH
           SET EXPECTED-RETURN-POINTER TO ADDRESS OF EXPECTED-RETURN-8
           PERFORM RUN-TEST
           GOBACK.

      ******************************************************************
      *                                                                *
      *                                                                *
      * This test will test the set record 1 to be kept logic.         *
      * It will create a mocking object to attach to.                  *
      * It will create a spy handler with a callback for CICS calls.   *
      * The spy callback will verify the results in the returned PUT   *
      * CONTAINER call.                                                *
      *                                                                *
      *                                                                *
      ******************************************************************
           ENTRY 'myDataTest9'.
      *----------------------------------------------------------------*
      * Set up address of our provided KSDS and container data, and    *
      * the expected return data. Then perform the test itself.        *
      *----------------------------------------------------------------*
           SET KSDS-RECORD-POINTER TO ADDRESS OF KSDS-RECORD-DATA-9
           MOVE KSDS-RECORD-COUNT-9 TO KSDS-RECORD-COUNT
           SET CONTAINER-DATA-POINTER TO ADDRESS OF CONTAINER-DATA-9
           MOVE LENGTH OF CONTAINER-DATA-9 TO CONTAINER-DATA-LENGTH
           SET EXPECTED-RETURN-POINTER TO ADDRESS OF EXPECTED-RETURN-9
           PERFORM RUN-TEST
           GOBACK.

      ******************************************************************
      *                                                                *
      *                                                                *
      * This test will test the attempt delete on kept record 1 logic. *
      * It will create a mocking object to attach to.                  *
      * It will create a spy handler with a callback for CICS calls.   *
      * The spy callback will verify the results in the returned PUT   *
      * CONTAINER call.                                                *
      *                                                                *
      *                                                                *
      ******************************************************************
           ENTRY 'myDataTest10'.
      *----------------------------------------------------------------*
      * Set up address of our provided KSDS and container data, and    *
      * the expected return data. Then perform the test itself.        *
      *----------------------------------------------------------------*
           SET KSDS-RECORD-POINTER TO ADDRESS OF KSDS-RECORD-DATA-10
           MOVE KSDS-RECORD-COUNT-10 TO KSDS-RECORD-COUNT
           SET CONTAINER-DATA-POINTER TO ADDRESS OF CONTAINER-DATA-10
           MOVE LENGTH OF CONTAINER-DATA-10 TO CONTAINER-DATA-LENGTH
           SET EXPECTED-RETURN-POINTER TO ADDRESS OF EXPECTED-RETURN-10
           PERFORM RUN-TEST
           GOBACK.

      ******************************************************************
      *                                                                *
      *                                                                *
      * This test will test the search on last name logic.             *
      * It will create a mocking object to attach to.                  *
      * It will create a spy handler with a callback for CICS calls.   *
      * The spy callback will verify the results in the returned PUT   *
      * CONTAINER call.                                                *
      *                                                                *
      *                                                                *
      ******************************************************************
           ENTRY 'myDataTest11'.
      *----------------------------------------------------------------*
      * Set up address of our provided KSDS and container data, and    *
      * the expected return data. Then perform the test itself.        *
      *----------------------------------------------------------------*
           SET KSDS-RECORD-POINTER TO ADDRESS OF KSDS-RECORD-DATA-11
           MOVE KSDS-RECORD-COUNT-11 TO KSDS-RECORD-COUNT
           SET CONTAINER-DATA-POINTER TO ADDRESS OF CONTAINER-DATA-11
           MOVE LENGTH OF CONTAINER-DATA-11 TO CONTAINER-DATA-LENGTH
           SET EXPECTED-RETURN-POINTER TO ADDRESS OF EXPECTED-RETURN-11
           PERFORM RUN-TEST
           GOBACK.

      ******************************************************************
      *                                                                *
      *                                                                *
      * This test show how to handle the setting up of error paths.    *
      *                                                                *
      * This test will test getting a LENGERR on a GET CONTAINER call. *
      * It does this by having the test case callback set the LENGERR. *
      * It will create a mocking object to attach to.                  *
      * It will create a spy handler with a callback for CICS calls.   *
      * The spy callback will verify the results in the returned PUT   *
      * CONTAINER call.                                                *
      *                                                                *
      *                                                                *
      ******************************************************************
           ENTRY 'myDataTest12'.
      *----------------------------------------------------------------*
      * Set up address of our provided KSDS and container data, and    *
      * the expected return data. Then perform the test itself.        *
      *----------------------------------------------------------------*
           SET KSDS-RECORD-POINTER TO ADDRESS OF KSDS-RECORD-DATA-12
           MOVE KSDS-RECORD-COUNT-12 TO KSDS-RECORD-COUNT
           SET CONTAINER-DATA-POINTER TO ADDRESS OF CONTAINER-DATA-12
           MOVE LENGTH OF CONTAINER-DATA-12 TO CONTAINER-DATA-LENGTH
           SET EXPECTED-RETURN-POINTER TO ADDRESS OF EXPECTED-RETURN-12
           PERFORM RUN-TEST-LENGERR
           GOBACK.

      ******************************************************************
      *                                                                *
      *                                                                *
      * This spy will be called on all CICS calls. We will look for    *
      * the PUT CONTAINER call and verify the results. We will look    *
      * for ASKTIME and replace its ABSTIME result in the call with    *
      * our static value so that the mock for the following FORMATTIME *
      * statement will match.                                          *
      *                                                                *
      *                                                                *
      ******************************************************************
           ENTRY 'mySpyTest' USING LS_CICS_DATA, ZLS_C_IFBLOCK.
      *----------------------------------------------------------------*
      * Set the address of the call block to the last call from CICS   *
      *----------------------------------------------------------------*
           SET ADDRESS OF ZLS_CICS_CALL TO LASTCALL IN LS_CICS_DATA
      *----------------------------------------------------------------*
      * We are interested in the returned ABSTIME value for our sample,*
      * which is provided in the ASKTIME call.                         *
      *----------------------------------------------------------------*
           IF COMMAND IN ZLS_CICS_CALL = 'ASKTIME'
      *----------------------------------------------------------------*
      * We are in an ASKTIME call. Get the ABSTIME argument.           *
      * Map the ABSTIME argument to a linkage section item.            *
      *----------------------------------------------------------------*
              MOVE LOW-VALUES TO I_GETCICSARGUMENT
              MOVE 'ABSTIME' TO ARGUMENTNAME IN ZWS_GETCICSARGUMENT
              CALL ZTESTUT USING ZWS_GETCICSARGUMENT,
                   ADDRESS OF ABSTIME-RETURN
      *----------------------------------------------------------------*
      * Ensure there is a ABSTIME argument. If not, then issue fail.   *
      * The fail API does not return, but causes the test case to end. *
      *----------------------------------------------------------------*
              IF ADDRESS OF ABSTIME-RETURN = NULL
                 MOVE LOW-VALUES TO I_FAIL IN ZWS_FAIL
                 MOVE 'Missing argument "ABSTIME" on ASKTIME call'
                      TO FAILMESSAGE IN ZWS_FAIL
                 CALL ZTESTUT USING ZWS_FAIL
              END-IF
      *----------------------------------------------------------------*
      * Replace the ABSTIME argument value with our static value.      *
      *----------------------------------------------------------------*
              MOVE MY_ABSTIME TO ABSTIME-RETURN
           END-IF

      *----------------------------------------------------------------*
      * Handle the checking of the returned output from PUT CONTAINER. *
      *----------------------------------------------------------------*
           PERFORM COMMON-OUTPUT-CHECK

           GOBACK.

      ******************************************************************
      *                                                                *
      *                                                                *
      * This spy will be called on all CICS calls. We will look for    *
      * the GET CONTAINER call and for this test case only, set a      *
      * LENGERR EIBRESP code.                                          *
      *                                                                *
      *                                                                *
      ******************************************************************
           ENTRY 'mySpyTestLengerr' USING LS_CICS_DATA, ZLS_C_IFBLOCK.
      *----------------------------------------------------------------*
      * Set the address of the call block to the last call from CICS   *
      *----------------------------------------------------------------*
           SET ADDRESS OF ZLS_CICS_CALL TO LASTCALL IN LS_CICS_DATA
      *----------------------------------------------------------------*
      * We will get a LENGERR on any GET CONTAINER call to cause an   **
      * alternate (unhappy) path to execute in the code.               *
      *----------------------------------------------------------------*
           IF COMMAND IN ZLS_CICS_CALL = 'GET CONTAINER'
      *----------------------------------------------------------------*
      * We are in a GET CONTAINER call. Set EIBRESP to LENGERR.        *
      *----------------------------------------------------------------*
              MOVE 22 TO EIBRESP
           END-IF

      *----------------------------------------------------------------*
      * Handle the checking of the returned output from PUT CONTAINER. *
      *----------------------------------------------------------------*
           PERFORM COMMON-OUTPUT-CHECK

           GOBACK.

      ******************************************************************
      * This is the common test case execution routine.                *
      ******************************************************************
       RUN-TEST.
      *----------------------------------------------------------------*
      * Perform common setup of the test case.                         *
      *----------------------------------------------------------------*
            PERFORM COMMON-SETUP
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

      ******************************************************************
      * This is the common test case execution routine.                *
      ******************************************************************
       RUN-TEST-LENGERR.
      *----------------------------------------------------------------*
      * Perform common setup of the test case.                         *
      *----------------------------------------------------------------*
            PERFORM COMMON-SETUP
      *----------------------------------------------------------------*
      * Create a spy on all CICS calls and when the GET CONTAINER call *
      * is intercepted, set the EIBRESP code to be a LENGERR.          *
      *----------------------------------------------------------------*
           MOVE LOW-VALUES TO I_SPYCICS
           MOVE 'ZTPCICST' TO MODULENAME IN ZWS_SPYCICS
           SET CALLBACK IN ZWS_SPYCICS TO ENTRY 'mySpyTestLengerr'
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

      ******************************************************************
      * This is the common test case setup code.                       *
      ******************************************************************
       COMMON-SETUP.
      *----------------------------------------------------------------*
      * Create a CICS object to be intercepted when being accessed by  *
      * the program-under-test.                                        *
      *----------------------------------------------------------------*
           MOVE LOW-VALUES TO I_MOCKCICS
           MOVE 'ZTPCICST' TO MODULENAME IN ZWS_MOCKCICS
           CALL ZTESTUT USING ZWS_MOCKCICS, CICSOBJECT IN CICS_DATA

      *----------------------------------------------------------------*
      * Create a container to define the data for the GET CONTAINER    *
      *----------------------------------------------------------------*
           MOVE LOW-VALUES TO I_ADDCONTAINERRECORD
           MOVE 'DFHTRANSACTION' TO
                CHANNELNAME IN ZWS_ADDCONTAINERRECORD
           MOVE 'MYCONTAINER' TO CONTAINERNAME IN ZWS_ADDCONTAINERRECORD
           SET RECORDADDRESS IN ZWS_ADDCONTAINERRECORD TO
               CONTAINER-DATA-POINTER
           MOVE CONTAINER-DATA-LENGTH TO
                RECORDSIZE IN ZWS_ADDCONTAINERRECORD
           SET CICSOBJECT IN ZWS_ADDCONTAINERRECORD TO
               ADDRESS OF CICSOBJECT IN CICS_DATA
           CALL ZTESTUT USING ZWS_ADDCONTAINERRECORD

      *----------------------------------------------------------------*
      * Alter the KSDS mocking based on whether we want initial data.  *
      *----------------------------------------------------------------*
           IF KSDS-RECORD-COUNT = 0
      *----------------------------------------------------------------*
      * Create a KSDS file object, with no initial data, to be         *
      * intercepted when being accessed by the program.                *
      *----------------------------------------------------------------*

              MOVE LOW-VALUES TO I_MOCKKSDS
              MOVE 'CUSTFILE' TO FILENAME IN ZWS_MOCKKSDS
              SET CICSOBJECT IN ZWS_MOCKKSDS TO
                  ADDRESS OF CICSOBJECT IN CICS_DATA
              MOVE 97 TO RECORDSIZE IN ZWS_MOCKKSDS
              MOVE 1 TO KEYOFFSET IN ZWS_MOCKKSDS
              MOVE 6 TO KEYLENGTH IN ZWS_MOCKKSDS
              CALL ZTESTUT USING ZWS_MOCKKSDS, KSDSOBJECT IN KSDS_DATA
           ELSE
      *----------------------------------------------------------------*
      * Create a base file object for KSDS file CUSTFILE with data     *
      * provided. The data for a fixed-length file is a set of records *
      * of the same length as the record size of the file.             *
      *----------------------------------------------------------------*
              MOVE LOW-VALUES TO I_FILE
              SET RECORDADDRESS IN ZWS_FILE TO KSDS-RECORD-POINTER
              MOVE KSDS-RECORD-COUNT TO RECORDCOUNT IN ZWS_FILE
              MOVE 97 TO RECORDSIZE IN ZWS_FILE
              CALL ZTESTUT USING ZWS_FILE, FILEOBJECT IN FILE_DATA
      *----------------------------------------------------------------*
      * Using the previous base object pointer, create a KSDS file     *
      * object to be intercepted when being accessed by the program.   *
      *----------------------------------------------------------------*
              MOVE LOW-VALUES TO I_MOCKKSDS
              MOVE 'CUSTFILE' TO FILENAME IN ZWS_MOCKKSDS
              SET FILEOBJECT IN ZWS_MOCKKSDS TO
                  ADDRESS OF FILEOBJECT IN FILE_DATA
              SET CICSOBJECT IN ZWS_MOCKKSDS TO
                  ADDRESS OF CICSOBJECT IN CICS_DATA
              MOVE 1 TO KEYOFFSET IN ZWS_MOCKKSDS
              MOVE 6 TO KEYLENGTH IN ZWS_MOCKKSDS
              CALL ZTESTUT USING ZWS_MOCKKSDS, KSDSOBJECT IN KSDS_DATA
           END-IF

      *----------------------------------------------------------------*
      * Create a CICS statement object to provide a common date/time.  *
      * Normally, the test case would create the mocked statement at   *
      * the time of the test case setup. By doing this at the time of  *
      * test case creation, you need to understand that the arguments  *
      * are copied into an internal framework control block and any    *
      * matches to find the mocked statement are done based on the     *
      * data at the time the _MockCICSStatement API call was made.     *
      * If your test case is unable to obtain (or define a static)     *
      * value when the mock statement is created, then an alternate    *
      * technique of setting up the mock statement would be during a   *
      * spy callback of a previous EXEC CICS statement, so that the    *
      * required values for this statement could be obtained.          *
      * In our example here however, we will create the mock of the    *
      * EXEC CICS FORMATTIME statement with a static ABSTIME value.    *
      * In the spy callback, we will replace the output value for the  *
      * previous EXEC CICS ASKTIME statament ABSTIME value. Then the   *
      * values will match when the mock of the FORMATTIME occurs.      *
      *----------------------------------------------------------------*
           MOVE LOW-VALUES TO MY_FORMATTIME
           MOVE 'ABSTIME' TO ARGNAME IN MY_FORMATTIME_ARGS (1)
           SET ARGADDRESS IN MY_FORMATTIME_ARGS (1) TO
               ADDRESS OF MY_ABSTIME
           MOVE 'YYMMDD' TO ARGNAME IN MY_FORMATTIME_ARGS (2)
           SET ARGADDRESS IN MY_FORMATTIME_ARGS (2) TO
               ADDRESS OF MY_YYMMDD
           MOVE 'TIME' TO ARGNAME IN MY_FORMATTIME_ARGS (3)
           SET ARGADDRESS IN MY_FORMATTIME_ARGS (3) TO
               ADDRESS OF MY_TIME
           MOVE 'DATESEP' TO ARGNAME IN MY_FORMATTIME_ARGS (4)
           SET ARGADDRESS IN MY_FORMATTIME_ARGS (4) TO
               ADDRESS OF MY_DATESEP
           MOVE 'TIMESEP' TO ARGNAME IN MY_FORMATTIME_ARGS (5)
           SET ARGADDRESS IN MY_FORMATTIME_ARGS (5) TO
               ADDRESS OF MY_TIMESEP

           MOVE LOW-VALUES TO I_MOCKCICSSTATEMENT
           MOVE 'FORMATTIME' TO COMMANDNAME IN ZWS_MOCKCICSSTATEMENT
           SET CICSOBJECT IN ZWS_MOCKCICSSTATEMENT TO
               ADDRESS OF CICSOBJECT IN CICS_DATA
           SET PTR IN ARGUMENTDATA IN ZWS_MOCKCICSSTATEMENT TO
               ADDRESS OF MY_FORMATTIME
           MOVE 5 TO SIZ IN ARGUMENTDATA IN ZWS_MOCKCICSSTATEMENT
           MOVE 0 TO EIBRESPONSE IN ZWS_MOCKCICSSTATEMENT
           MOVE 0 TO EIBRESPONSE2 IN ZWS_MOCKCICSSTATEMENT
           CALL ZTESTUT USING ZWS_MOCKCICSSTATEMENT

           EXIT.

      ******************************************************************
      * This is the common test case code to check results during a    *
      * PUT CONTAINER call.                                            *
      ******************************************************************
       COMMON-OUTPUT-CHECK.
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
      * Compare the FROM argument with the values that we are          *
      * expecting. For the date/time field we will make them the same  *
      * to assist in the comparison.                                   *
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

           EXIT.
