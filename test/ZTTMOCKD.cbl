       PROCESS PGMN(LM),NODYNAM
       IDENTIFICATION DIVISION.
       PROGRAM-ID. 'ZTTMOCKD' RECURSIVE.

      ******************************************************************
      * Broadcom Test4z System Under Test (SUT) example.               *
      * Copyright (c) 2024 Broadcom. All Rights Reserved.              *
      *                                                                *
      * This test example shows how to set up a Db2 test with the test *
      * case providing the data.                                       *
      *                                                                *
      * NB: "PROCESS PGMN(LM),NODYNAM" above is required for unit      *
      *     tests to enable long entry point names and locating entry  *
      *     points.                                                    *      
      ******************************************************************

       DATA DIVISION.
       WORKING-STORAGE SECTION.
      ******************************************************************
      * Copy in our required control blocks from Test4z.               *
      ******************************************************************
       COPY ZTESTWS.

      ******************************************************************
      * Define any working-storage items that we need.                 *
      ******************************************************************
       1 DB2_DATA.
         COPY ZDB2.
       1 DB2_FUNCTION USAGE FUNCTION-POINTER.

      ******************************************************************
      * Define the rowset block. This could be an array of contiguous  *
      * rowsets if needed, but for this sample program we are          *
      * providing 1.                                                   *
      ******************************************************************
       1 MY_ROWSET_OBJECTS.
         2 MY_ROWSET_OBJECT OCCURS 1.
           COPY ZROWSET.

       1 MY_KEY PIC X(20) VALUE 'Pedro Gonzales'.

       1 MY_ROWS.
         2 MY_ROW1.
           3 PIC X(20) VALUE 'Pedro Gonzales'.
           3 PIC X(20) VALUE '1234 Main Street'.
         2 MY_ROW2.
           3 PIC X(20) VALUE 'Pedro Gonzales'.
           3 PIC X(20) VALUE '5678 Second Street'.

       LINKAGE SECTION.
      ******************************************************************
      * Copy in our required control blocks from Test4z.               *
      ******************************************************************
       COPY ZTESTLS.

      ******************************************************************
      * Define any linkage section items that we need.                 *
      ******************************************************************

       PROCEDURE DIVISION.
      ******************************************************************
      * Register a set of tests to be run.                             *
      ******************************************************************
           MOVE LOW-VALUES TO I_TEST
           SET TESTFUNCTION IN ZWS_TEST TO ENTRY 'mytestDb2'
           MOVE 'Db2 provided data test' TO TESTNAME IN ZWS_TEST
           CALL ZTESTUT USING ZWS_TEST

      ******************************************************************
      * Once all of the tests have been registered, return back to     *
      * Test4z to start processing.                                    *
      ******************************************************************
           GOBACK.

      ******************************************************************
      *                                                                *
      *                                                                *
      * Provide the callback routine entry point for the Db2 test      *
      * with data provided by the test case.                           *
      *                                                                *
      *                                                                *
      ******************************************************************
           ENTRY 'mytestDb2'.

      ******************************************************************
      * Create a Db2 rowset object to be used by the Db2 mocking       *
      * object.                                                        *
      ******************************************************************
           MOVE LOW-VALUES TO I_ROWSET
           MOVE 1 TO CURSORID IN ZWS_ROWSET
           SET KEYADDRESS IN ZWS_ROWSET TO ADDRESS OF MY_KEY
           MOVE LENGTH OF MY_KEY TO KEY_SIZE IN ZWS_ROWSET
           SET ROWADDRESS IN ZWS_ROWSET TO ADDRESS OF MY_ROWS
           MOVE 2 TO ROWSET_SIZE IN ZWS_ROWSET
           MOVE 40 TO ROW_SIZE IN ZWS_ROWSET
           CALL ZTESTUT USING ZWS_ROWSET, MY_ROWSET_OBJECT (1)

      ******************************************************************
      * Create a Db2 object to be intercepted when being accessed by   *
      * program ZTDB2TE4.                                              *
      ******************************************************************
           MOVE LOW-VALUES TO I_MOCKDB2
           MOVE 'ZTDB2TE4' TO MODULENAME IN ZWS_MOCKDB2
           MOVE 'ZTDB2TE4' TO FUNCTIONNAME IN ZWS_MOCKDB2
           SET PTR IN DATAADDRESS IN ZWS_MOCKDB2 TO
               ADDRESS OF MY_ROWSET_OBJECTS
           MOVE 1 TO SIZ IN DATAADDRESS IN ZWS_MOCKDB2
           SET CURSORLOGICAL IN ZWS_MOCKDB2 TO TRUE
           CALL ZTESTUT USING ZWS_MOCKDB2, DB2OBJECT IN DB2_DATA

      ******************************************************************
      * Load and prepare the user application program ZTDB2TE4 for use *
      ******************************************************************
           MOVE LOW-VALUES TO I_PREPAREMODULE
           MOVE 'ZTDB2TE4' TO MODULENAME IN ZWS_PREPAREMODULE
           CALL ZTESTUT USING ZWS_PREPAREMODULE

      ******************************************************************
      * Get the entry point of the ZTDB2TE4 routine in load module     *
      * ZTDB2TE4.                                                      *
      ******************************************************************
           MOVE LOW-VALUES TO I_GETFUNCTION
           MOVE 'ZTDB2TE4' TO MODULENAME IN ZWS_GETFUNCTION
           MOVE 'ZTDB2TE4' TO FUNCTIONNAME IN ZWS_GETFUNCTION
           CALL ZTESTUT USING ZWS_GETFUNCTION, DB2_FUNCTION

      ******************************************************************
      * Start the ZTDB2TE4 function.                                   *
      * The passing of MY_key as an argument to the ZTDB2TE4 program   *
      * is only here because our example program uses it. This is not  *
      * typical for user programs.                                     *
      ******************************************************************
           CALL DB2_FUNCTION USING MY_KEY

           GOBACK.
