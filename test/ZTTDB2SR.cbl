       PROCESS PGMN(LM),NODYNAM
       IDENTIFICATION DIVISION.
       PROGRAM-ID. 'ZTTDB2SR' RECURSIVE.

      ******************************************************************
      * Broadcom Test4z System Under Test (SUT) example.               *
      * Copyright (c) 2024 Broadcom. All Rights Reserved.              *
      *                                                                *
      * This test example shows how to run a unit test using           *
      * previously recorded data for a program using Db2 with no       *
      * intercept spy.                                                 *
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
       1 LOAD_DATA.
         COPY ZDATA.
       1 PROGRAM_FUNCTION USAGE FUNCTION-POINTER.

       1 DB2_DATA.
         COPY ZDB2.

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
           SET TESTFUNCTION IN ZWS_TEST TO ENTRY 'myDataTest'
           MOVE 'Recorded Db2 data test' TO TESTNAME IN ZWS_TEST
           CALL ZTESTUT USING ZWS_TEST

      ******************************************************************
      * Once all of the tests have been registered, return back to     *
      * Test4z to start processing.                                    *
      ******************************************************************
           GOBACK.

      ******************************************************************
      *                                                                *
      *                                                                *
      * Provide the callback routine entry point for the Db2 loaded    *
      * data test with data provided by a previous recording in the    *
      * ZLDATA DD partitioned dataset with member ZTDB2TE1.            *
      *                                                                *
      *                                                                *
      ******************************************************************
           ENTRY 'myDataTest'.

      ******************************************************************
      * Create a loaded data object for a Db2 test program. The data   *
      * comes from a previous recording and is stored in a partitioned *
      * dataset member named 'ZTDB2TE1'. This recorded data is in JSON *
      * format.                                                        *
      ******************************************************************
           MOVE LOW-VALUES TO I_LOADDATA
           MOVE 'ZTDB2TE1' TO MEMBERNAME IN ZWS_LOADDATA
           CALL ZTESTUT USING ZWS_LOADDATA, LOADOBJECT IN LOAD_DATA

      ******************************************************************
      * Create a Db2 object to be intercepted when being accessed by   *
      * program ZTDB2TE1.                                              *
      ******************************************************************
           MOVE LOW-VALUES TO I_MOCKDB2
           MOVE 'ZTDB2TE1' TO MODULENAME IN ZWS_MOCKDB2
           SET LOADOBJECT IN ZWS_MOCKDB2 TO LOADOBJECT IN LOAD_DATA
           CALL ZTESTUT USING ZWS_MOCKDB2, DB2OBJECT IN DB2_DATA

      ******************************************************************
      * Load and prepare the user application program ZTDB2TE1 for use *
      ******************************************************************
           MOVE LOW-VALUES TO I_PREPAREMODULE
           MOVE 'ZTDB2TE1' TO MODULENAME IN ZWS_PREPAREMODULE
           CALL ZTESTUT USING ZWS_PREPAREMODULE

      ******************************************************************
      * Get the entry point of the ZTDB2TE1 routine in load module     *
      * ZTDB2TE1.                                                      *
      ******************************************************************
           MOVE LOW-VALUES TO I_GETFUNCTION
           MOVE 'ZTDB2TE1' TO MODULENAME IN ZWS_GETFUNCTION
           MOVE 'ZTDB2TE1' TO FUNCTIONNAME IN ZWS_GETFUNCTION
           CALL ZTESTUT USING ZWS_GETFUNCTION, PROGRAM_FUNCTION

      ******************************************************************
      * Start the ZTDB2TE1 function.                                   *
      ******************************************************************
           CALL PROGRAM_FUNCTION

           GOBACK.
