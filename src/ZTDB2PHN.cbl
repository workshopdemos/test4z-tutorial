       IDENTIFICATION DIVISION.
       PROGRAM-ID. ZTDB2PHN.

      ******************************************************************
      * Broadcom Test4z System Under Test (SUT) example.               *
      * Copyright (c) 2024 Broadcom. All Rights Reserved.              *
      *                                                                *
      * This short DB2 program maintains an employee directory in a    *
      * corporate database. Updates are processed based on requests    *
      * in a regularly-updated QSAM file. For each request type,       *
      * ZTDB2PHN can perform one of the following operations:          *
      *                                                                *
      *  D - Delete an employee record                                 *
      *  U - Update an employee record (name and/or phone number)      *
      *  C - Create a new employee record                              *
      *  K - Mark an employee record as do-not-delete                  *
      *  S - Search and validate the existance of an employee record   *
      *                                                                *
      * NB: This example intentionally omits I/O error handling to     *
      *     demonstrate what happens if "unhappy paths" aren't tested. *
      ******************************************************************

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT PHONE-UPDATE-FILE ASSIGN PHUPDATE
           FILE STATUS IS PHONE-UPDATE-STATUS.

           SELECT PHONE-LOG-FILE ASSIGN PHLOG
           FILE STATUS IS PHONE-LOG-STATUS.

       DATA DIVISION.
       
       FILE SECTION.
       FD  PHONE-UPDATE-FILE RECORD CONTAINS 200 CHARACTERS
           RECORDING MODE IS F
           DATA RECORD IS PHONE-UPDATE-FILE-RECORD.
       01  PHONE-UPDATE-FILE-RECORD PIC X(200).
       
       FD  PHONE-LOG-FILE RECORD CONTAINS 132 CHARACTERS
           RECORDING MODE IS F
           DATA RECORD IS PHONE-LOG-FILE-RECORD.
       01  PHONE-LOG-FILE-RECORD PIC X(132).
       
       WORKING-STORAGE SECTION.
       
       01  PHONE-UPDATE-STATUS PIC X(2).
       01  PHONE-LOG-STATUS    PIC X(2).
       01  WS-SQLCODE          PIC S9(9) SIGN IS LEADING.
       01  I                   PIC 9(2).

      *-----------------------------------------------------------------
      * For DISPLAY output control of long fields.
      *-----------------------------------------------------------------
       77  WS-LOG-FIELD-LENGTH PIC 9(2) VALUE 16.
       77  WS-PARM             PIC X(100) VALUE SPACES.

      *-----------------------------------------------------------------
      * Control variable for demonstrations; it clears the EMPPHONE
      * table and adds a well-known set of employees      
      *-----------------------------------------------------------------
       77  WS-DEMO-RESET       PIC 9(2).

      *-----------------------------------------------------------------  
      * This defines the records in PHUPDATE and PHLOG.
      * See ZTDB2PHR copybook for additional details.
      *-----------------------------------------------------------------  
       01  UPD-PHONE-UPDATE-FILE-RECORD.
           05 UPD-REQUIRED-FIELDS.
               10 UPD-REQUEST-TYPE  PIC X.
                  88 UPD-DELETE     VALUE 'D'.
                  88 UPD-UPDATE     VALUE 'U'.
                  88 UPD-CREATE     VALUE 'C'.
                  88 UPD-KEEP       VALUE 'K'.
                  88 UPD-SEARCH     VALUE 'S'.
               10 UPD-KEY           PIC X(6).
               10 UPD-MESSAGE       PIC X(40).
           05 UPD-REQUEST-FIELDS.
               10 UPD-LASTNAME      PIC X(40).
               10 UPD-FIRSTNAME     PIC X(40).
               10 UPD-PHONE         PIC X(10).
           05 FILLER                PIC X(63).

       01  PHL-PHONE-LOG-RECORD.
           05 PHL-REQUIRED-FIELDS.
               10 FILLER            PIC X(4) VALUE 'REQ='.
               10 PHL-REQUEST-TYPE  PIC X.
               10 FILLER            PIC X(5) VALUE ' KEY='.
               10 PHL-KEY           PIC X(6).
           05 PHL-PROCESS-FIELDS.
               10 FILLER            PIC X(4) VALUE ' OK='.
               10 PHL-REQUEST-VALIDITY-FLAG  PIC X.
                  88 PHL-VALID-REQUEST   VALUE 'Y'.
                  88 PHL-INVALID-REQUEST VALUE 'N'.
               10 FILLER            PIC X(4) VALUE ' ST='.
               10 PHL-STATUS        PIC X(20).
           05 PHL-REQUEST-FIELDS-1.
               10 FILLER            PIC X(5) VALUE ' MSG='.               
               10 PHL-MESSAGE       PIC X(20).           
           05 PHL-REQUEST-FIELDS-2.
               10 FILLER            PIC X(4) VALUE ' LN='.                 
               10 PHL-LASTNAME      PIC X(20).
               10 FILLER            PIC X(4) VALUE ' FN='.                     
               10 PHL-FIRSTNAME     PIC X(20).
               10 FILLER            PIC X(4) VALUE ' PH='.                     
               10 PHL-PHONE         PIC X(10).

      ******************************************************************
      * Employee phone record. The KEEP field, if set, means the       *
      * record cannot be deleted.                                      *
      *                                                                *
      * NB: The field names must match the table column names.         *
      ******************************************************************
       01  WS-EMPPHONE.
           03  EMP_KEY            PIC X(6).
           03  EMP_KEEP           PIC X(1).
           03  EMP_LASTNAME.
               49 EMP_LASTNAMEL   PIC S9(4) COMP.
               49 EMP_LASTNAMEC   PIC X(40) VALUE SPACES.
           03  EMP_FIRSTNAME.
               49 EMP_FIRSTNAMEL  PIC S9(4) COMP.
               49 EMP_FIRSTNAMEC  PIC X(40) VALUE SPACES.
           03  EMP_PHONE          PIC X(10).

      *-----------------------------------------------------------------
      * SQL INCLUDE FOR SQLCA
      *-----------------------------------------------------------------
           EXEC SQL INCLUDE SQLCA END-EXEC.

      *-----------------------------------------------------------------
      * SQL DECLARATION FOR VIEW EMPPHONE 
      *-----------------------------------------------------------------
           EXEC SQL DECLARE EMPPHONE TABLE
               (EMP_KEY      CHAR(06)    NOT NULL,
               EMP_KEEP      CHAR(01)    NOT NULL,
               EMP_LASTNAME  VARCHAR(40) NOT NULL,
               EMP_FIRSTNAME VARCHAR(40) NOT NULL,
               EMP_PHONE     CHAR(10)    NOT NULL)
           END-EXEC.
       
      *-----------------------------------------------------------------
      * CURSOR LISTS ALL EMPLOYEES
      *-----------------------------------------------------------------
           EXEC SQL DECLARE EMPS CURSOR FOR
               SELECT * FROM EMPPHONE ORDER BY EMP_KEY ASC
           END-EXEC.

       LINKAGE SECTION.
 
      *-----------------------------------------------------------------
      * For demonstration purposes only, the JCL can include
      * PARM='DB2=DEMO-RESET'. This will reset the EMPPHONE table
      * to a known state as part of Test4z demonstrations.
      *-----------------------------------------------------------------
       01  LS-PARM.
           05 LS-PARM-LENGTH PIC S9(4) COMP.
           05 LS-PARM-DATA   PIC X(100).

       PROCEDURE DIVISION USING LS-PARM.

      *-----------------------------------------------------------------
      * For demonstration purposes only!
      *
      * Check if the EMPPHONE table should be reset to a known state. 
      * If the return code is non-zero, it has been. If so, exit with
      * the expectation of another run to follow without the reset
      * request. See "Unit test 0" in ZTTDB2PH for more details.
      * 
      * NB: This is not part of the production code!
      *-----------------------------------------------------------------
           PERFORM CHECK-DEMO-RESET
           IF RETURN-CODE NOT = 0
               DISPLAY 'ZTDB2PHN exiting demo reset'
               GOBACK
           END-IF

      *-----------------------------------------------------------------
      * Echo the employee table entries before any updates.
      *-----------------------------------------------------------------
           DISPLAY 'ZTDB2PHN employees before updates:'
           PERFORM ECHO-EMPLOYEES

      *-----------------------------------------------------------------
      * Zip through the update requests, requesting the corresponding
      * DB2 update. Log the disposition, including those requests 
      * that are invalid.
      *-----------------------------------------------------------------
           OPEN OUTPUT PHONE-LOG-FILE           
           OPEN INPUT PHONE-UPDATE-FILE

           READ PHONE-UPDATE-FILE
           PERFORM UNTIL PHONE-UPDATE-STATUS > '04'
               MOVE PHONE-UPDATE-FILE-RECORD
                   TO UPD-PHONE-UPDATE-FILE-RECORD
               PERFORM PROCESS-REQUEST

               READ PHONE-UPDATE-FILE
           END-PERFORM

           CLOSE PHONE-UPDATE-FILE
           CLOSE PHONE-LOG-FILE

      *-----------------------------------------------------------------
      * Echo the employee table entries after the updates.
      *-----------------------------------------------------------------
           DISPLAY 'ZTDB2PHN employees after updates:'
           PERFORM ECHO-EMPLOYEES

           GOBACK.

      ******************************************************************
      * Process a delete, update, create, keep, or search request.     *
      *                                                                *
      * Input:                                                         *
      *   UPD-PHONE-LOG-FILE-RECORD                                    *
      * Output:                                                        *
      *   Updated EMPHONE table, log to PHLOG / PHONE-LOG-FILE         *
      ******************************************************************
       PROCESS-REQUEST.

           INITIALIZE PHL-PHONE-LOG-RECORD
           MOVE UPD-REQUEST-TYPE TO PHL-REQUEST-TYPE
           MOVE UPD-KEY TO PHL-KEY
           MOVE UPD-MESSAGE(1:WS-LOG-FIELD-LENGTH) TO PHL-MESSAGE
           MOVE UPD-LASTNAME(1:WS-LOG-FIELD-LENGTH) TO PHL-LASTNAME
           MOVE UPD-FIRSTNAME(1:WS-LOG-FIELD-LENGTH) TO PHL-FIRSTNAME
           MOVE UPD-PHONE TO PHL-PHONE

           EVALUATE TRUE
               WHEN UPD-DELETE
                   PERFORM DELETE-REQUEST
                   
               WHEN UPD-UPDATE
                   PERFORM UPDATE-REQUEST

               WHEN UPD-CREATE
                   PERFORM CREATE-REQUEST

               WHEN UPD-KEEP
                   PERFORM KEEP-REQUEST

               WHEN UPD-SEARCH
                   PERFORM SEARCH-REQUEST
            
               WHEN OTHER 
                   PERFORM UNRECOGNIZED-REQUEST
           END-EVALUATE

           DISPLAY 'ZTDB2PHN ------ request complete ------'

           EXIT.

      ******************************************************************
      * Delete a phone record if it's not immutable ('KEEP').          *
      *                                                                *
      * Input:                                                         *
      *   UPD-PHONE-LOG-FILE-RECORD                                    *
      * Output:                                                        *
      *   Updated EMPHONE table, log to PHLOG / PHONE-LOG-FILE         *
      ******************************************************************
       DELETE-REQUEST.

           DISPLAY 'ZTDB2PHN delete request for [' UPD-KEY ']'

      *-----------------------------------------------------------------
      * Verify it's not a KEEP record, then delete it.
      *-----------------------------------------------------------------
           PERFORM RECORD-LOOKUP
           IF EMP_KEY = SPACES OR EMP_KEEP
               SET PHL-INVALID-REQUEST TO TRUE
               IF EMP_KEY = SPACES
                   MOVE 'Not found' TO PHL-STATUS
               ELSE
                   MOVE 'Immutable record' TO PHL-STATUS
               END-IF

               DISPLAY 'ZTDB2PHN delete of [' UPD-KEY '] failed'
               PERFORM WRITE-LOG-RECORD
           ELSE          
               EXEC SQL
                   DELETE FROM EMPPHONE WHERE EMP_KEY = :UPD-KEY
               END-EXEC
      
               SET PHL-VALID-REQUEST TO TRUE
               DISPLAY 'ZTDB2PHN deleted [' UPD-KEY ']'
               PERFORM WRITE-LOG-RECORD
           END-IF

           EXIT.

      ******************************************************************
      * Update a record name or phone number.                          *
      *                                                                *
      * Input:                                                         *
      *   UPD-PHONE-LOG-FILE-RECORD                                    *
      * Output:                                                        *
      *   Updated EMPHONE table, log to PHLOG / PHONE-LOG-FILE         *
      ******************************************************************
       UPDATE-REQUEST.

           DISPLAY 'ZTDB2PHN update request for [' UPD-KEY ']'

      *-----------------------------------------------------------------
      * Verify it's valid and then update it.
      *-----------------------------------------------------------------
           PERFORM RECORD-LOOKUP
           IF EMP_KEY = SPACES
               SET PHL-INVALID-REQUEST TO TRUE
               MOVE 'Not found' TO PHL-STATUS

               DISPLAY 'ZTDB2PHN update of [' UPD-KEY '] failed'
               PERFORM WRITE-LOG-RECORD
           ELSE
               IF UPD-LASTNAME NOT = SPACES
                   MOVE UPD-LASTNAME(1:WS-LOG-FIELD-LENGTH)
                       TO PHL-LASTNAME
                   EXEC SQL 
                       UPDATE EMPPHONE
                           SET   EMP_LASTNAME = :UPD-LASTNAME
                           WHERE EMP_KEY = :UPD-KEY 
                   END-EXEC
                   DISPLAY 'ZTDB2PHN updated [' UPD-KEY '] last name '
                       UPD-LASTNAME
               END-IF
               IF UPD-FIRSTNAME NOT = SPACES
                   MOVE UPD-FIRSTNAME(1:WS-LOG-FIELD-LENGTH)
                       TO PHL-FIRSTNAME               
                   EXEC SQL 
                       UPDATE EMPPHONE
                           SET   EMP_FIRSTNAME = :UPD-FIRSTNAME
                           WHERE EMP_KEY = :UPD-KEY 
                   END-EXEC
                   DISPLAY 'ZTDB2PHN updated [' UPD-KEY '] first name '
                       UPD-FIRSTNAME                   
               END-IF
               IF UPD-PHONE NOT = SPACES
                   MOVE UPD-PHONE TO PHL-PHONE               
                   EXEC SQL 
                       UPDATE EMPPHONE
                           SET   EMP_PHONE = :UPD-PHONE
                           WHERE EMP_KEY = :UPD-KEY 
                   END-EXEC
                   DISPLAY 'ZTDB2PHN updated [' UPD-KEY '] phone '
                       UPD-PHONE                              
               END-IF

               SET PHL-VALID-REQUEST TO TRUE
               DISPLAY 'ZTDB2PHN updated [' UPD-KEY ']'
               PERFORM WRITE-LOG-RECORD
           END-IF

           EXIT.

      ******************************************************************
      * Create an employee record based on provided input.             *
      *                                                                *
      * Input:                                                         *
      *   UPD-PHONE-LOG-FILE-RECORD                                    *
      * Output:                                                        *
      *   Updated EMPHONE table, log to PHLOG / PHONE-LOG-FILE         *
      ******************************************************************
       CREATE-REQUEST.

           DISPLAY 'ZTDB2PHN create request for [' UPD-KEY ']'

      *-----------------------------------------------------------------
      * Verify it doesn't already exist.
      *-----------------------------------------------------------------
           PERFORM RECORD-LOOKUP
           IF EMP_KEY NOT = SPACES
               SET PHL-INVALID-REQUEST TO TRUE
               MOVE 'Already exists' TO PHL-STATUS

               DISPLAY 'ZTDB2PHN create of [' UPD-KEY '] failed'
               PERFORM WRITE-LOG-RECORD
           ELSE
               IF UPD-KEY = SPACES OR
                       UPD-LASTNAME = SPACES OR
                       UPD-FIRSTNAME = SPACES OR
                       UPD-PHONE = SPACES
                   SET PHL-INVALID-REQUEST TO TRUE
                   MOVE 'Invalid field(s)' TO PHL-STATUS

                   DISPLAY 'ZTDB2PHN create of [' UPD-KEY '] failed'
                   PERFORM WRITE-LOG-RECORD
               ELSE
                   EXEC SQL 
                       INSERT INTO EMPPHONE( 
                           EMP_KEY,
                           EMP_KEEP,
                           EMP_LASTNAME,
                           EMP_FIRSTNAME,
                           EMP_PHONE)
                       VALUES(
                           :UPD-KEY,
                           ' ',
                           :UPD-LASTNAME,
                           :UPD-FIRSTNAME,
                           :UPD-PHONE
                       ) 
                   END-EXEC
       
                   SET PHL-VALID-REQUEST TO TRUE

                   DISPLAY 'ZTDB2PHN created [' UPD-KEY ']'
                   PERFORM WRITE-LOG-RECORD
               END-IF
           END-IF

           EXIT.

      ******************************************************************
      * Mark a record as immutable / cannot be deleted.                *
      *                                                                *
      * Input:                                                         *
      *   UPD-PHONE-LOG-FILE-RECORD                                    *
      * Output:                                                        *
      *   Updated EMPHONE table, log to PHLOG / PHONE-LOG-FILE         *
      ******************************************************************
       KEEP-REQUEST.

           DISPLAY 'ZTDB2PHN keep request for [' UPD-KEY ']'

      *-----------------------------------------------------------------
      * Verify it doesn't already exist and not already KEEP.
      *-----------------------------------------------------------------
           PERFORM RECORD-LOOKUP
           IF EMP_KEY = SPACES OR EMP_KEEP = 'K'
               SET PHL-INVALID-REQUEST TO TRUE
               IF EMP_KEY = SPACES
                   MOVE 'Not found' TO PHL-STATUS
               ELSE
                   MOVE 'Immutable record' TO PHL-STATUS
               END-IF

               DISPLAY 'ZTDB2PHN keep of [' UPD-KEY '] failed'
               PERFORM WRITE-LOG-RECORD
           ELSE
               EXEC SQL 
                    UPDATE EMPPHONE
                        SET   EMP_KEEP = 'K'
                        WHERE EMP_KEY = :UPD-KEY 
               END-EXEC
      
               SET PHL-VALID-REQUEST TO TRUE
               DISPLAY 'ZTDB2PHN keep [' UPD-KEY ']'
               PERFORM WRITE-LOG-RECORD
           END-IF

           EXIT.

      ******************************************************************
      * Search for a given record (validate). This is a logged request *
      * versus the internal RECORD-LOOKUP request.                     *
      *                                                                *
      * Input:                                                         *
      *   UPD-PHONE-LOG-FILE-RECORD                                    *
      * Output:                                                        *
      *   Matching WS-EMPPHONE record, log to PHLOG / PHONE-LOG-FILE   *
      ******************************************************************
       SEARCH-REQUEST.

           DISPLAY 'ZTDB2PHN search request for [' UPD-KEY ']'   

           PERFORM RECORD-LOOKUP
           IF EMP_KEY = SPACES
               SET PHL-INVALID-REQUEST TO TRUE
               MOVE 'Not found' TO PHL-STATUS
               DISPLAY 'ZTDB2PHN search of [' UPD-KEY '] failed'
           ELSE
               SET PHL-VALID-REQUEST TO TRUE     
               DISPLAY 'ZTDB2PHN searched [' UPD-KEY ']'
           END-IF

           PERFORM WRITE-LOG-RECORD

           EXIT.

      ******************************************************************
      * Huh? Request for other than D-U-C-K-S. Log it.                 *
      *                                                                *
      * Input:                                                         *
      *   UPD-PHONE-LOG-FILE-RECORD                                    *
      * Output:                                                        *
      *   Log to PHLOG / PHONE-LOG-FILE                                *
      ******************************************************************
       UNRECOGNIZED-REQUEST.

           DISPLAY 'ZTDB2PHN unrecognized request for [' UPD-KEY ']'   

           SET PHL-INVALID-REQUEST TO TRUE
           MOVE 'Invalid request' TO PHL-STATUS

           PERFORM WRITE-LOG-RECORD

           EXIT.

      ******************************************************************
      * Search for a given record. Note this is an internal request    *
      * and isn't logged.                                              *
      *                                                                *
      * Input:                                                         *
      *   UPD-PHONE-LOG-FILE-RECORD                                    *
      * Output:                                                        *
      *   Matching WS-EMPPHONE record                                  *
      ******************************************************************
       RECORD-LOOKUP.

           DISPLAY 'ZTDB2PHN record lookup for [' UPD-KEY ']'  

      *-----------------------------------------------------------------
      * Single row SELECT lookup. If not there, EMP_KEY will still
      * be SPACES.  
      *-----------------------------------------------------------------
           INITIALIZE WS-EMPPHONE
           IF UPD-KEY NOT = SPACES
               EXEC SQL 
                   SELECT * INTO :WS-EMPPHONE
                       FROM EMPPHONE
                       WHERE EMP_KEY = :UPD-KEY 
               END-EXEC
           END-IF

      *-----------------------------------------------------------------
      * Update the log record with the request; the caller
      * will update the remaining log fields (e.g., LOG-REQUEST-TYPE).
      *-----------------------------------------------------------------
           IF EMP_KEY NOT = SPACES
               DISPLAY 'ZTDB2PHN found [' UPD-KEY '] for ' EMP_LASTNAMEC
               
               MOVE EMP_KEY TO PHL-KEY
               MOVE EMP_LASTNAMEC(1:WS-LOG-FIELD-LENGTH)
                   TO PHL-LASTNAME
               MOVE EMP_FIRSTNAMEC(1:WS-LOG-FIELD-LENGTH)
                   TO PHL-FIRSTNAME
               MOVE EMP_PHONE TO PHL-PHONE
           ELSE
               DISPLAY 'ZTDB2PHN not found [' UPD-KEY ']'
           END-IF

           EXIT.

      ******************************************************************
      * Keep track of changes for audit purposes.                      *
      *                                                                *
      * Input:                                                         *
      *   UPD-PHONE-LOG-FILE-RECORD                                    *
      *   LOG-VALID-REQUEST or LOG-INVALID-REQUEST                     *
      *   LOG-STATUS (set to 'Success' if LOG-VALID-REQUEST is TRUE)   *
      * Output:                                                        *
      *   Record to PHLOG / PHONE-LOG-FILE                             *
      ******************************************************************
       WRITE-LOG-RECORD.

           MOVE SPACES TO PHONE-LOG-FILE-RECORD

           IF PHL-VALID-REQUEST
               MOVE 'Success' TO PHL-STATUS
           END-IF
           MOVE PHL-PHONE-LOG-RECORD TO PHONE-LOG-FILE-RECORD
           
           WRITE PHONE-LOG-FILE-RECORD
           DISPLAY 'ZTDB2PHN written to log:' 
           DISPLAY 'ZTDB2PHN ' PHONE-LOG-FILE-RECORD

           EXIT.

      ******************************************************************
      * Echo the employee phone entries to SYSOUT for demonstration    *
      * purposes.                                                      *
      ******************************************************************
       ECHO-EMPLOYEES.
           EXEC SQL OPEN EMPS END-EXEC

           PERFORM VARYING I FROM 1 BY 1 
                   UNTIL SQLCODE NOT = 0 OR I > 10
               MOVE SPACES TO WS-EMPPHONE
               EXEC SQL FETCH EMPS INTO :WS-EMPPHONE END-EXEC

               IF EMP_KEY NOT = SPACES
                   DISPLAY 'ZTDB2PHN record ' I ':' 
                   DISPLAY 'key=' EMP_KEY ' keep=[' EMP_KEEP ']'
                   DISPLAY 'last name='
                       EMP_LASTNAMEC(1:EMP_LASTNAMEL)
                   DISPLAY 'first name=' 
                       EMP_FIRSTNAMEC(1:EMP_FIRSTNAMEL)
                   DISPLAY 'phone=' EMP_PHONE
                   DISPLAY '---'
               END-IF
           END-PERFORM

           IF SQLCODE NOT = 100
               SUBTRACT 1 FROM I
               MOVE SQLCODE TO WS-SQLCODE
               DISPLAY 'ZTDB2PHN stopped after ' I ' records '
                       ' due to SQLCODE ' WS-SQLCODE
           END-IF

           EXEC SQL CLOSE EMPS END-EXEC

           EXIT.

      ******************************************************************
      *   DEMO ONLY - DEMO ONLY - DEMO ONLY - DEMO ONLY - DEMO ONLY    *
      *                                                                *
      * If demonstrating this example using a "real" DB2 database      *
      * (versus a recording), the EMPPHONE table may need to be reset  *
      * to a known state. This is for demonstration purposes only and  *
      * is not part of a production implementation.                    *
      ******************************************************************
       CHECK-DEMO-RESET.
   
           MOVE 0 TO WS-DEMO-RESET
           MOVE LS-PARM-DATA(1:LS-PARM-LENGTH) TO WS-PARM 
           INSPECT WS-PARM TALLYING WS-DEMO-RESET
               FOR ALL 'DB2=DEMO-RESET'
        
           IF WS-DEMO-RESET = 0
               COMPUTE RETURN-CODE = 0
               EXIT PARAGRAPH
           END-IF

           DISPLAY 'ZTDB2PHN resetting EMPPHONE table for demo'

      *-----------------------------------------------------------------
      * Empty the EMPPHONE table...
      *-----------------------------------------------------------------
           EXEC SQL
               DELETE FROM EMPPHONE WHERE 1 = 1
           END-EXEC

      *-----------------------------------------------------------------
      * ...and add demonstration entries.
      *-----------------------------------------------------------------
           EXEC SQL 
               INSERT INTO EMPPHONE
                 (EMP_KEY,EMP_KEEP,EMP_LASTNAME,EMP_FIRSTNAME,EMP_PHONE)
               VALUES('000001',' ','Adams','John','9195551735')
           END-EXEC
           EXEC SQL 
               INSERT INTO EMPPHONE
                 (EMP_KEY,EMP_KEEP,EMP_LASTNAME,EMP_FIRSTNAME,EMP_PHONE)
                 VALUES('000002',' ','Franklin','Ben','9195551705')
           END-EXEC 
           EXEC SQL 
               INSERT INTO EMPPHONE
                 (EMP_KEY,EMP_KEEP,EMP_LASTNAME,EMP_FIRSTNAME,EMP_PHONE)
                 VALUES('000003',' ','Madison','James','9195551717')
           END-EXEC
           EXEC SQL 
               INSERT INTO EMPPHONE
                 (EMP_KEY,EMP_KEEP,EMP_LASTNAME,EMP_FIRSTNAME,EMP_PHONE)
                 VALUES('000004',' ','Jefferson','Thomas','9195551709')
           END-EXEC
           EXEC SQL 
               INSERT INTO EMPPHONE
                 (EMP_KEY,EMP_KEEP,EMP_LASTNAME,EMP_FIRSTNAME,EMP_PHONE)
                 VALUES('000005','K','Washington','George','9195551732')
           END-EXEC

      *-----------------------------------------------------------------
      * Signal that the EMPPHONE table has been reset for demos.
      *-----------------------------------------------------------------
           COMPUTE RETURN-CODE = 4

           EXIT.
           