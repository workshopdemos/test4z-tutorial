CBL CICS
       IDENTIFICATION DIVISION.
       PROGRAM-ID. ZTPCICST.
      ******************************************************************
      *                                                                *
      * This is a CICS sample program that will perform web-style      *
      * transaction processing. This program will:                     *
      * (1) accept input from a channel's container                    *
      * (2) verify the input is in the proper format                   *
      * (3) perform file I/O operations based on the request type      *
      *     (a) request D will perform a delete on a record            *
      *     (b) request U will perform an update on a field            *
      *     (c) request C will perform create a new record             *
      *     (d) request K will set a record to be permanently kept     *
      *     (e) request S will search for ID based on criteria         *
      * (4) respond in the channel's container results of the request  *
      *                                                                *
      ******************************************************************
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  RESPONSE-CODE    PIC S9(9) COMP-4.
       01  NUMERIC-KEY      PIC 9(6).
       01  SEARCH-KEY       PIC X(6).
       01  FILE-ID          PIC X(8) VALUE 'CUSTFILE'.
       01  LOG-MSG          PIC X(100).
       01  CHANNEL-ID       PIC X(16) VALUE 'DFHTRANSACTION'.
       01  CONTAINER-ID     PIC X(16) VALUE 'MYCONTAINER'.
       01  CONTAINER-LENGTH PIC S9(9) COMP-4 VALUE 100.
       01  CONTAINER-DATA.
           03  REQUEST-TYPE     PIC X(1).
           03  REQUEST-AREA.
               05  REQUEST-ID       PIC X(6).
               05  REQUEST-MESSAGE  PIC X(93).
       01  CONTAINER-RETURN.
           03  RETURN-TYPE      PIC X(1).
           03  RETURN-STATUS    PIC X(1).
           03  RETURN-DATE      PIC X(8).
           03  RETURN-TIME      PIC X(8).
           03  RETURN-AREA.
               05  RETURN-ID        PIC X(6).
               05  RETURN-MESSAGE   PIC X(91).
       01  FILE-RECORD.
           03  FILE-KEY         PIC X(6).
           03  FILE-KEEP        PIC X(1).
               88  KEEP-YES     VALUE 'K'.
           03  FILE-LASTNAME    PIC X(40).
           03  FILE-FIRSTNAME   PIC X(40).
           03  FILE-PHONE       PIC X(10).
       01  CALL-STATUS      PIC X(1).
           88  RECORD-FOUND     VALUE 'F'.
           88  RECORD-NOTFOUND  VALUE 'N'.
       01  WS-ABSTIME       PIC X(8).
       PROCEDURE DIVISION.
      ******************************************************************
      * We are about to read a record from the channel/container. Set  *
      * up error processing routines for the various types of errors.  *
      ******************************************************************
           EXEC CICS HANDLE CONDITION
                CHANNELERR(WRONG-CHANNEL)
                CONTAINERERR(WRONG-CONTAINER)
                LENGERR(WRONG-CONTAINER-LENGTH)
           END-EXEC
      ******************************************************************
      * Try to get a record from the channel/container.                *
      ******************************************************************
           MOVE SPACES TO CONTAINER-DATA
           EXEC CICS GET CONTAINER(CONTAINER-ID)
                CHANNEL(CHANNEL-ID)
                INTO(CONTAINER-DATA)
                FLENGTH(CONTAINER-LENGTH)
           END-EXEC
      ******************************************************************
      * Get the current date and time for return messages.             *
      ******************************************************************
           EXEC CICS ASKTIME ABSTIME(WS-ABSTIME) END-EXEC
           EXEC CICS FORMATTIME ABSTIME(WS-ABSTIME)
               YYMMDD(RETURN-DATE) TIME(RETURN-TIME)
               DATESEP('-') TIMESEP(':')
           END-EXEC
      ******************************************************************
      * We have a record. Verify that the request type is valid.       *
      ******************************************************************
           EVALUATE REQUEST-TYPE
              WHEN 'D'
                   CONTINUE
              WHEN 'U'
                   CONTINUE
              WHEN 'C'
                   CONTINUE
              WHEN 'K'
                   CONTINUE
              WHEN 'S'
                   CONTINUE
              WHEN OTHER
                   MOVE SPACES TO LOG-MSG
                   STRING 'REQUEST TYPE ' DELIMITED BY SIZE
                          REQUEST-TYPE DELIMITED BY SIZE
                          ' WAS INVALID' DELIMITED BY SIZE
                          INTO LOG-MSG
                   PERFORM PROGRAM-FAILURE
           END-EVALUATE
      ******************************************************************
      * We have a delete request. Verify the request fields.           *
      ******************************************************************
           IF REQUEST-TYPE = 'D'
              IF REQUEST-MESSAGE NOT = SPACES
                 MOVE 'DELETE REQUEST MESSAGE WAS SPECIFIED' TO LOG-MSG
                 PERFORM PROGRAM-FAILURE
              END-IF
              IF REQUEST-ID NOT NUMERIC
                 MOVE SPACES TO LOG-MSG
                 STRING 'DELETE REQUEST ID'  DELIMITED BY SIZE
                        REQUEST-ID DELIMITED BY SIZE
                        ' IS NOT NUMERIC' DELIMITED BY SIZE
                        INTO LOG-MSG
                 PERFORM PROGRAM-FAILURE
              END-IF
              IF REQUEST-ID = '000000'
                 MOVE 'DELETE REQUEST ID 000000 NOT ALLOWED' TO LOG-MSG
                 PERFORM PROGRAM-FAILURE
              END-IF
           END-IF
      ******************************************************************
      * We have an update request. Verify the request fields.          *
      ******************************************************************
           IF REQUEST-TYPE = 'U'
              IF REQUEST-MESSAGE = SPACES
                 MOVE 'UPDATE REQUEST MESSAGE WAS NOT SPECIFIED' TO
                       LOG-MSG
                 PERFORM PROGRAM-FAILURE
              END-IF
              IF REQUEST-MESSAGE (1: 2) NOT = 'L=' AND
                 REQUEST-MESSAGE (1: 2) NOT = 'F=' AND
                 REQUEST-MESSAGE (1: 2) NOT = 'P='
                 MOVE SPACES TO LOG-MSG
                 STRING 'UPDATE REQUEST MESSAGE HAS INVALID TAG '
                        DELIMITED BY SIZE
                        REQUEST-MESSAGE (1: 2) DELIMITED BY SIZE
                        INTO LOG-MSG
                 PERFORM PROGRAM-FAILURE
              END-IF
              IF REQUEST-ID NOT NUMERIC
                 MOVE SPACES TO LOG-MSG
                 STRING 'UPDATE REQUEST ID'  DELIMITED BY SIZE
                        REQUEST-ID DELIMITED BY SIZE
                        ' IS NOT NUMERIC' DELIMITED BY SIZE
                        INTO LOG-MSG
                 PERFORM PROGRAM-FAILURE
              END-IF
              IF REQUEST-ID = '000000'
                 MOVE 'UPDATE REQUEST ID 000000 NOT ALLOWED' TO LOG-MSG
                 PERFORM PROGRAM-FAILURE
              END-IF
           END-IF
      ******************************************************************
      * We have a create request. Verify the request fields.           *
      ******************************************************************
           IF REQUEST-TYPE = 'C'
              IF REQUEST-MESSAGE NOT = SPACES
                 MOVE 'CREATE REQUEST MESSAGE WAS SPECIFIED' TO
                      LOG-MSG
                 PERFORM PROGRAM-FAILURE
              END-IF
              IF REQUEST-ID NOT = SPACES
                 MOVE 'CREATE REQUEST ID WAS SPECIFIED' TO LOG-MSG
                 PERFORM PROGRAM-FAILURE
              END-IF
           END-IF
      ******************************************************************
      * We have a keep record request. Verify the request fields.      *
      ******************************************************************
           IF REQUEST-TYPE = 'K'
              IF REQUEST-MESSAGE NOT = SPACES
                 MOVE 'KEEP REQUEST MESSAGE WAS SPECIFIED' TO LOG-MSG
                 PERFORM PROGRAM-FAILURE
              END-IF
              IF REQUEST-ID NOT NUMERIC
                 MOVE SPACES TO LOG-MSG
                 STRING 'KEEP REQUEST ID'  DELIMITED BY SIZE
                        REQUEST-ID DELIMITED BY SIZE
                        ' IS NOT NUMERIC' DELIMITED BY SIZE
                        INTO LOG-MSG
                 PERFORM PROGRAM-FAILURE
              END-IF
              IF REQUEST-ID = '000000'
                 MOVE 'KEEP REQUEST ID 000000 NOT ALLOWED' TO LOG-MSG
                 PERFORM PROGRAM-FAILURE
              END-IF
           END-IF
      ******************************************************************
      * We have a search request. Verify the request fields.           *
      ******************************************************************
           IF REQUEST-TYPE = 'S'
              IF REQUEST-AREA = SPACES
                 MOVE 'SEARCH REQUEST CRITERIA WAS NOT SPECIFIED' TO
                      LOG-MSG
                 PERFORM PROGRAM-FAILURE
              END-IF
              IF REQUEST-AREA (1: 2) NOT = 'L=' AND
                 REQUEST-AREA (1: 2) NOT = 'F=' AND
                 REQUEST-AREA (1: 2) NOT = 'P='
                 MOVE SPACES TO LOG-MSG
                 STRING 'SEARCH REQUEST MESSAGE HAS INVALID TAG '
                        DELIMITED BY SIZE
                        REQUEST-AREA (1: 2) DELIMITED BY SIZE
                        INTO LOG-MSG
                 PERFORM PROGRAM-FAILURE
              END-IF
           END-IF
      ******************************************************************
      * Verification is complete for the basic checks. Perform action. *
      ******************************************************************
           EVALUATE REQUEST-TYPE
              WHEN 'D'
                   PERFORM DELETE-REQUEST
              WHEN 'U'
                   PERFORM UPDATE-REQUEST
              WHEN 'C'
                   PERFORM CREATE-REQUEST
              WHEN 'K'
                   PERFORM KEEP-REQUEST
              WHEN 'S'
                   PERFORM SEARCH-REQUEST
           END-EVALUATE
           PERFORM PROGRAM-RETURN.

      ******************************************************************
      * Process the delete request.                                    *
      ******************************************************************
       DELETE-REQUEST.
           PERFORM FILE-GET
           IF RECORD-NOTFOUND
              MOVE SPACES TO LOG-MSG
              STRING 'NO RECORD TO DELETE FOR ID ' DELIMITED BY SIZE
                     REQUEST-ID DELIMITED BY SIZE
                     INTO LOG-MSG
              PERFORM PROGRAM-FAILURE
           END-IF
           IF KEEP-YES
              PERFORM FILE-UNLOCK
              MOVE SPACES TO LOG-MSG
              STRING 'ATTEMPT TO DELETE KEPT RECORD ' DELIMITED BY SIZE
                     REQUEST-ID DELIMITED BY SIZE
                     INTO LOG-MSG
              PERFORM PROGRAM-FAILURE
           END-IF
           PERFORM FILE-DELETE

           MOVE REQUEST-TYPE TO RETURN-TYPE
           MOVE 'S' TO RETURN-STATUS
           MOVE REQUEST-ID TO RETURN-ID
           MOVE 'DELETE SUCCESSFUL' TO RETURN-MESSAGE
           EXEC CICS PUT CONTAINER(CONTAINER-ID)
                FROM(CONTAINER-RETURN)
           END-EXEC.

      ******************************************************************
      * Process the update request.                                    *
      ******************************************************************
       UPDATE-REQUEST.
           PERFORM FILE-GET
           IF RECORD-NOTFOUND
              MOVE SPACES TO LOG-MSG
              STRING 'NO RECORD TO UPDATE FOR ID ' DELIMITED BY SIZE
                     REQUEST-ID DELIMITED BY SIZE
                     INTO LOG-MSG
              PERFORM PROGRAM-FAILURE
           END-IF
           IF REQUEST-MESSAGE (1: 2) = 'L='
              IF REQUEST-MESSAGE (3:) = SPACES
                 MOVE 'UPDATE ON LAST NAME CANNOT BE SPACES' TO LOG-MSG
                 PERFORM PROGRAM-FAILURE
              END-IF
              MOVE REQUEST-MESSAGE (3:) TO FILE-LASTNAME
           END-IF
           IF REQUEST-MESSAGE (1: 2) = 'F='
              IF REQUEST-MESSAGE (3:) = SPACES
                 MOVE 'UPDATE ON FIRST NAME CANNOT BE SPACES' TO LOG-MSG
                 PERFORM PROGRAM-FAILURE
              END-IF
              MOVE REQUEST-MESSAGE (3:) TO FILE-FIRSTNAME
           END-IF
           IF REQUEST-MESSAGE (1: 2) = 'P='
              IF REQUEST-MESSAGE (3: 10) NOT NUMERIC
                 MOVE SPACES TO LOG-MSG
                 STRING 'UPDATE REQUEST PHONE NUMBER '
                        DELIMITED BY SIZE
                        REQUEST-MESSAGE (3: 10) DELIMITED BY SIZE
                        ' IS NOT NUMERIC' DELIMITED BY SIZE
                        INTO LOG-MSG
                 PERFORM PROGRAM-FAILURE
              END-IF
              MOVE REQUEST-MESSAGE (3:) TO FILE-PHONE
           END-IF
           PERFORM FILE-UPDATE

           MOVE REQUEST-TYPE TO RETURN-TYPE
           MOVE 'S' TO RETURN-STATUS
           MOVE REQUEST-ID TO RETURN-ID
           MOVE 'UPDATE SUCCESSFUL' TO RETURN-MESSAGE
           EXEC CICS PUT CONTAINER(CONTAINER-ID)
                FROM(CONTAINER-RETURN)
           END-EXEC.

      ******************************************************************
      * Process the create request.                                    *
      ******************************************************************
       CREATE-REQUEST.
           PERFORM FILE-GET
           IF RECORD-FOUND
              PERFORM FILE-UNLOCK
              MOVE SPACES TO LOG-MSG
              STRING 'RECORD ALREADY EXISTS FOR ID ' DELIMITED BY SIZE
                     REQUEST-ID DELIMITED BY SIZE
                     INTO LOG-MSG
              PERFORM PROGRAM-FAILURE
           END-IF
           PERFORM FILE-CREATE

           MOVE REQUEST-TYPE TO RETURN-TYPE
           MOVE 'S' TO RETURN-STATUS
           MOVE FILE-KEY TO RETURN-ID
           MOVE 'CREATE SUCCESSFUL' TO RETURN-MESSAGE
           EXEC CICS PUT CONTAINER(CONTAINER-ID)
                FROM(CONTAINER-RETURN)
           END-EXEC.

      ******************************************************************
      * Process the keep record request.                               *
      ******************************************************************
       KEEP-REQUEST.
           PERFORM FILE-GET
           IF RECORD-NOTFOUND
              MOVE SPACES TO LOG-MSG
              STRING 'NO RECORD TO KEEP FOR ID ' DELIMITED BY SIZE
                     REQUEST-ID DELIMITED BY SIZE
                     INTO LOG-MSG
              PERFORM PROGRAM-FAILURE
           END-IF
           SET KEEP-YES TO TRUE
           PERFORM FILE-UPDATE

           MOVE REQUEST-TYPE TO RETURN-TYPE
           MOVE 'S' TO RETURN-STATUS
           MOVE REQUEST-ID TO RETURN-ID
           MOVE 'KEEP SUCCESSFUL' TO RETURN-MESSAGE
           EXEC CICS PUT CONTAINER(CONTAINER-ID)
                FROM(CONTAINER-RETURN)
           END-EXEC.

      ******************************************************************
      * Process the search request.                                    *
      ******************************************************************
       SEARCH-REQUEST.
           SET RECORD-NOTFOUND TO TRUE
           MOVE LOW-VALUES TO SEARCH-KEY
           EXEC CICS STARTBR FILE(FILE-ID)
                RIDFLD(SEARCH-KEY)
                GTEQ
                NOHANDLE RESP(RESPONSE-CODE)
           END-EXEC
           PERFORM WITH TEST BEFORE
              UNTIL RESPONSE-CODE NOT = DFHRESP(NORMAL)
                 OR RECORD-FOUND
              EXEC CICS READNEXT FILE(FILE-ID)
                   INTO(FILE-RECORD)
                   RIDFLD(SEARCH-KEY)
                   NOHANDLE RESP(RESPONSE-CODE)
              END-EXEC
              IF RESPONSE-CODE = DFHRESP(NORMAL)
                 IF REQUEST-AREA (1: 2) = 'L=' AND
                    REQUEST-AREA (3:) = FILE-LASTNAME
                    SET RECORD-FOUND TO TRUE
                 END-IF
                 IF REQUEST-AREA (1: 2) = 'F=' AND
                    REQUEST-AREA (3:) = FILE-FIRSTNAME
                    SET RECORD-FOUND TO TRUE
                 END-IF
                 IF REQUEST-AREA (1: 2) = 'P=' AND
                    REQUEST-AREA (3:) = FILE-PHONE
                    SET RECORD-FOUND TO TRUE
                 END-IF
              END-IF
           END-PERFORM
           EXEC CICS ENDBR FILE(FILE-ID)
                NOHANDLE RESP(RESPONSE-CODE)
           END-EXEC
           IF RECORD-NOTFOUND
              MOVE SPACES TO LOG-MSG
              STRING 'SEARCH FOR ' DELIMITED BY SIZE
                     REQUEST-AREA (3:) DELIMITED BY SPACE
                     ' DID NOT FIND RECORD' DELIMITED BY SIZE
                     INTO LOG-MSG
              PERFORM PROGRAM-FAILURE
           END-IF

           MOVE REQUEST-TYPE TO RETURN-TYPE
           MOVE 'S' TO RETURN-STATUS
           MOVE FILE-RECORD TO RETURN-AREA
           EXEC CICS PUT CONTAINER(CONTAINER-ID)
                FROM(CONTAINER-RETURN)
           END-EXEC.

      ******************************************************************
      * Create a new record using the next key stored in record 000000.*
      * If record 000000 does not exist, then create it.               *
      ******************************************************************
       FILE-CREATE.
           MOVE SPACES TO FILE-RECORD
           MOVE '000000' TO FILE-KEY
           EXEC CICS READ FILE(FILE-ID) UPDATE
                INTO(FILE-RECORD)
                RIDFLD(FILE-KEY)
                NOHANDLE RESP(RESPONSE-CODE)
           END-EXEC
           IF RESPONSE-CODE = DFHRESP(NORMAL)
              IF FILE-RECORD (7: 6) NOT NUMERIC
                 MOVE 0 TO NUMERIC-KEY
              ELSE
                 MOVE FILE-RECORD (7: 6) TO NUMERIC-KEY (1: 6)
              END-IF
              ADD 1 TO NUMERIC-KEY
              MOVE NUMERIC-KEY (1: 6) TO FILE-RECORD (7: 6)
              EXEC CICS REWRITE FILE(FILE-ID)
                   FROM(FILE-RECORD)
                   NOHANDLE RESP(RESPONSE-CODE)
              END-EXEC
              IF RESPONSE-CODE NOT = DFHRESP(NORMAL)
                 MOVE SPACES TO LOG-MSG
                 STRING 'UNABLE TO REWRITE TO ' DELIMITED BY SIZE
                        FILE-ID DELIMITED BY SPACE
                        ' FOR KEY 000000' DELIMITED BY SIZE
                        INTO LOG-MSG
                 PERFORM PROGRAM-FAILURE
              END-IF
           ELSE
              MOVE 1 TO NUMERIC-KEY
              MOVE NUMERIC-KEY (1: 6) TO FILE-RECORD (7: 6)
              EXEC CICS WRITE FILE(FILE-ID)
                   FROM(FILE-RECORD)
                   RIDFLD(FILE-KEY)
                   NOHANDLE RESP(RESPONSE-CODE)
              END-EXEC
              IF RESPONSE-CODE NOT = DFHRESP(NORMAL)
                 MOVE SPACES TO LOG-MSG
                 STRING 'UNABLE TO WRITE TO ' DELIMITED BY SIZE
                        FILE-ID DELIMITED BY SPACE
                        ' FOR KEY 000000' DELIMITED BY SIZE
                        INTO LOG-MSG
                 PERFORM PROGRAM-FAILURE
              END-IF
           END-IF
           MOVE SPACES TO FILE-RECORD
           MOVE NUMERIC-KEY TO FILE-KEY
           EXEC CICS WRITE FILE(FILE-ID)
                FROM(FILE-RECORD)
                RIDFLD(FILE-KEY)
                NOHANDLE RESP(RESPONSE-CODE)
           END-EXEC
           IF RESPONSE-CODE NOT = DFHRESP(NORMAL)
              MOVE SPACES TO LOG-MSG
              STRING 'UNABLE TO WRITE TO ' DELIMITED BY SIZE
                     FILE-ID DELIMITED BY SPACE
                     INTO LOG-MSG
              PERFORM PROGRAM-FAILURE
           END-IF.

      ******************************************************************
      * Try to get the record from the file.                           *
      ******************************************************************
       FILE-GET.
           SET RECORD-NOTFOUND TO TRUE
           EXEC CICS READ FILE(FILE-ID) UPDATE
                INTO(FILE-RECORD)
                RIDFLD(REQUEST-ID)
                NOHANDLE RESP(RESPONSE-CODE)
           END-EXEC
           IF RESPONSE-CODE = DFHRESP(NORMAL)
              SET RECORD-FOUND TO TRUE
           END-IF.

      ******************************************************************
      * Delete a record from the file.                                 *
      ******************************************************************
       FILE-DELETE.
           EXEC CICS DELETE FILE(FILE-ID)
                NOHANDLE RESP(RESPONSE-CODE)
           END-EXEC
           IF RESPONSE-CODE NOT = DFHRESP(NORMAL)
              MOVE SPACES TO LOG-MSG
              STRING 'UNABLE TO DELETE FROM ' DELIMITED BY SIZE
                     FILE-ID DELIMITED BY SPACE
                     INTO LOG-MSG
              PERFORM PROGRAM-FAILURE
           END-IF.

      ******************************************************************
      * Update a record in the file.                                   *
      ******************************************************************
       FILE-UPDATE.
           EXEC CICS REWRITE FILE(FILE-ID)
                FROM(FILE-RECORD)
                NOHANDLE RESP(RESPONSE-CODE)
           END-EXEC
           IF RESPONSE-CODE NOT = DFHRESP(NORMAL)
              MOVE SPACES TO LOG-MSG
              STRING 'UNABLE TO REWRITE TO ' DELIMITED BY SIZE
                     FILE-ID DELIMITED BY SPACE
                     INTO LOG-MSG
              PERFORM PROGRAM-FAILURE
           END-IF.

      ******************************************************************
      * Unlock a held record from the file.                            *
      ******************************************************************
       FILE-UNLOCK.
           EXEC CICS UNLOCK FILE(FILE-ID)
                NOHANDLE RESP(RESPONSE-CODE)
           END-EXEC
           IF RESPONSE-CODE NOT = DFHRESP(NORMAL)
              MOVE SPACES TO LOG-MSG
              STRING 'UNABLE TO UNLOCK FROM ' DELIMITED BY SIZE
                     FILE-ID DELIMITED BY SPACE
                     INTO LOG-MSG
              PERFORM PROGRAM-FAILURE
           END-IF.

      ******************************************************************
      * Return from the program.                                       *
      ******************************************************************
       PROGRAM-RETURN.
           EXEC CICS RETURN END-EXEC.

      ******************************************************************
      * We have an incorrect channel error.                            *
      ******************************************************************
       WRONG-CHANNEL.
           MOVE SPACES TO LOG-MSG
           STRING 'CHANNEL NAME ' DELIMITED BY SIZE
                  CHANNEL-ID DELIMITED BY SPACE
                  ' WAS INVALID' DELIMITED BY SIZE
                  INTO LOG-MSG
           PERFORM PROGRAM-FAILURE.

      ******************************************************************
      * We have an incorrect container error.                          *
      ******************************************************************
       WRONG-CONTAINER.
           MOVE SPACES TO LOG-MSG
           STRING 'CONTAINER NAME ' DELIMITED BY SIZE
                  CONTAINER-ID DELIMITED BY SPACE
                  ' WAS INVALID' DELIMITED BY SIZE
                  INTO LOG-MSG
           PERFORM PROGRAM-FAILURE.

      ******************************************************************
      * We have an incorrect container length error.                   *
      ******************************************************************
       WRONG-CONTAINER-LENGTH.
           MOVE 'CONTAINER MESSAGE LENGTH WAS TOO LONG' TO LOG-MSG
           PERFORM PROGRAM-FAILURE.

      ******************************************************************
      * Write a message to the log.                                    *
      ******************************************************************
       PROGRAM-FAILURE.
           MOVE REQUEST-TYPE TO RETURN-TYPE
           MOVE 'F' TO RETURN-STATUS
           MOVE LOG-MSG TO RETURN-AREA
           EXEC CICS PUT CONTAINER(CONTAINER-ID)
                FROM(CONTAINER-RETURN)
           END-EXEC
           PERFORM PROGRAM-RETURN.

