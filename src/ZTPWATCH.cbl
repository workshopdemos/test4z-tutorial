       IDENTIFICATION DIVISION.
       PROGRAM-ID. ZTPWATCH.

      ******************************************************************
      * Broadcom Test4z System Under Test (SUT) example.               *
      * Copyright (c) 2024 Broadcom. All Rights Reserved.              *
      *                                                                *
      * This program demonstrates the Test4z watch and spy variable    *
      * capability by (a) changing WORKING-STORAGE variable values     *
      * that are then detected by the unit test ZTTWATCH/ZTTWATCS,     *
      * and (b) having the variables in this program modified by the   *
      * unit test's watch variable callback in ZTTWATCH.               *
      *                                                                *
      * The ability to access the variable changes of the SUT and      *
      * make real-time changes enables the unit test to force "unhappy *
      * paths" in execution that otherwise would be difficult to       *
      * recreate in a live environment.                                *
      ******************************************************************

       DATA DIVISION.
       WORKING-STORAGE SECTION.

      *-----------------------------------------------------------------
      * These are the watched/spied variables of this program.
      *-----------------------------------------------------------------
       01  WS-COUNTER        PIC 9(4)  VALUE 0.
       01  WS-RECORD         PIC X(80) VALUE SPACES.
       01  WS-CURRENT-DATE.
           05 CD-YEAR        PIC 9(4).
           05 CD-MONTH       PIC 9(2).
           05 CD-DAY         PIC 9(2).
           05 CD-HOUR        PIC 9(2).
           05 CD-MINUTE      PIC 9(2).
           05 FILLER         PIC X(9).

      *-----------------------------------------------------------------
      * A few working variables (not watched or spied).
      *-----------------------------------------------------------------
       01  I                 PIC 9(2).
       01  WS-RECORD-FORMATTED.
           05 RECORD-NUM     PIC 9(2).
           05 FILLER         PIC X VALUE SPACE.
           05 RECORD-MESSAGE PIC X(73).

       PROCEDURE DIVISION.

           PERFORM 100-INIT-WATCH
           PERFORM 200-RUN-WATCH
           PERFORM 300-END-WATCH-WITH-WARNING

           GOBACK.

      ******************************************************************
      * Establish the initial state for the working storage variables. *
      ****************************************************************** 
       100-INIT-WATCH.

           PERFORM 110-INIT-DATE
           PERFORM 120-INIT-COUNTER

           EXIT.

      ******************************************************************
      * The unit test often has recorded data with dates in it         *
      * that must match the CURRENT-DATE value. A watch variable       *
      * callback can be used to modify the stored data, in this case,  *
      * the current date stored in the variable WS-CURRENT-DATE.       *
      ******************************************************************
       110-INIT-DATE.

      *-----------------------------------------------------------------
      * Get CURRENT-DATE and trigger a watch callback.
      *-----------------------------------------------------------------
           MOVE FUNCTION CURRENT-DATE TO WS-CURRENT-DATE
           DISPLAY 'ZTPWATCH date is ' 
               CD-YEAR '-' CD-MONTH '-' CD-DAY ' at '
               CD-HOUR ':' CD-MINUTE
           EXIT.

      ******************************************************************
      * A trivial example of a paragraph that can be PERFORMed         *
      * and potentially spied upon with Test4z _SpySection API.        *
      ******************************************************************
       120-INIT-COUNTER.

           MOVE 1 TO WS-COUNTER

           EXIT.

      ******************************************************************
      * It's not much, but demonstrates watched variables changing.    *
      ******************************************************************   
       200-RUN-WATCH.

           PERFORM VARYING I FROM 1 BY 1 UNTIL I > 5
               PERFORM 210-INCREMENT-COUNTER 
           END-PERFORM

           EXIT.

      ******************************************************************
      * The most common use case for watch variables is monitoring     *
      * variable values; the watch callback can also modify the        *
      * SUT's variable value. Another use is detecting where the       *
      * change occurred by section/paragraph, e.g., for debugging      *
      * purposes or limiting access to sensitive data.                 *
      *                                                                *
      * Spy variables serve a similar purpose-- monitoring variables-- *
      * with the added ability to validate variable values             *
      * post-execution using the spy variable's history.               *
      ******************************************************************
       210-INCREMENT-COUNTER.

           ADD 1 TO WS-COUNTER

      *-----------------------------------------------------------------
      * Update the format record, then copy it into the final one.
      *-----------------------------------------------------------------
           INITIALIZE WS-RECORD-FORMATTED
           MOVE WS-COUNTER TO RECORD-NUM
           MOVE 'Counter incremented' TO RECORD-MESSAGE
      
      *-----------------------------------------------------------------
      * Update the final record. This MOVE will trigger a spy/watch
      * callback since it modifies the watched variable WS-RECORD.
      *-----------------------------------------------------------------
           MOVE WS-RECORD-FORMATTED TO WS-RECORD

      *-----------------------------------------------------------------
      * NB: The watch callback can modify the SUT's variable.
      *     In this case, ZTTWATCH will sometimes change the record,
      *     overlaying the values set by the MOVE above.
      *-----------------------------------------------------------------
           DISPLAY 'ZTPWATCH 210-INCREMENT-COUNTER=' WS-RECORD

           IF WS-RECORD NOT = WS-RECORD-FORMATTED
               DISPLAY 'ZTPWATCH [WS-RECORD CHANGED BY WATCH CALLBACK]'
           END-IF

           EXIT.

      ******************************************************************
      * Sometimes the unit test wants to know what RETURN-CODE the     *
      * SUT set at the time it actually changes versus waiting for     *
      * execution to end. Or, the unit test may want to change the     *
      * RETURN-CODE to force an unhappy path. Set it here and ZTTWATCH *
      * will see the change occur in real-time via a watch variable.   *
      ******************************************************************
       300-END-WATCH-WITH-WARNING.

      *-----------------------------------------------------------------
      * Set RETURN-CODE and trigger a watch callback.
      *-----------------------------------------------------------------
           MOVE 4 TO RETURN-CODE

           EXIT.
           