       PROCESS PGMN(LM),NODYNAM
       IDENTIFICATION DIVISION.
       PROGRAM-ID. 'ZTTHELLO' RECURSIVE.

      ******************************************************************
      * Broadcom Test4z Tutorial.                                      *
      * Copyright (c) 2024 Broadcom. All Rights Reserved.              *
      ******************************************************************

       DATA DIVISION.
       WORKING-STORAGE SECTION.

      ******************************************************************
      * Include copybook for Test4z's API control blocks.              *
      ******************************************************************       
           COPY ZTESTWS.

       PROCEDURE DIVISION.

      ******************************************************************
      * Register a unit test for Test4z to run.                        *
      ******************************************************************
           MOVE LOW-VALUES TO I_TEST
           SET TESTFUNCTION IN ZWS_TEST TO ENTRY 'sayHelloTest'
           MOVE 'Hello World test' TO TESTNAME IN ZWS_TEST
           CALL ZTESTUT USING ZWS_TEST

           GOBACK.

      ******************************************************************
      * UNIT TEST: Do nothing except run "Hello, World!" program.      *
      ******************************************************************
           ENTRY 'sayHelloTest'.

      *-----------------------------------------------------------------
      * TUTORIAL - Use the "t4z message" snippet.
      *-----------------------------------------------------------------
      *     MOVE LOW-VALUES TO I_MESSAGE IN ZWS_MESSAGE
      *     MOVE 'ZTTHELLO you there?' TO MESSAGETEXT IN ZWS_MESSAGE
      *     DISPLAY '[SYSOUT] ' MESSAGETEXT IN ZWS_MESSAGE 
      *     CALL ZTESTUT USING ZWS_MESSAGE

           MOVE LOW-VALUES TO I_RUNFUNCTION
           MOVE 'ZTPHELLO' TO MODULENAME IN ZWS_RUNFUNCTION
           MOVE 'ZTPHELLO' TO FUNCTIONNAME IN ZWS_RUNFUNCTION
           CALL ZTESTUT USING ZWS_RUNFUNCTION

           GOBACK.
