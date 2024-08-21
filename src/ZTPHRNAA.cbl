       IDENTIFICATION DIVISION.
       PROGRAM-ID. ZTPHRNAA.

      ******************************************************************
      * Broadcom Test4z System Under Test (SUT) example.               *
      * Copyright (c) 2024 Broadcom. All Rights Reserved.              *
      *                                                                *
      * This is one of three programs that collectively support the    *
      * promotion decision-making process of a fictional "widget"      *
      * marketing team:                                                *
      *                                                                *
      * 1. ZTPHRNMM - main program that reads a list of "watched"      *
      *               widgets for possible sales promotion             *
      * 2. ZTPHRNDD - called program to fetch the associated recent    *
      *               sales data of the watched widgets (e.g.,         *
      *               last 30 days, updated regularly)                 *
      * 3. ZTPHRNAA - called program to analyze recent sales data      *
      *               and "score" popular widgets that are promotion   *
      *               candidates for the marketing team.               *
      *                                                                *
      * The unit test suite, ZTTHRNES, is responsible for validating   *
      * the correct operation of ZTPHRNMM/ZTPHRNDD/ZTPHRNAA.           *
      *                                                                *
      * This example demonstrates how to mock other resources like     *
      * files and called programs in order to test a given called      *
      * program in isolation using the Test4z harness.                 *
      *                                                                *
      * Specifically, ZTTHRNES will use a previous recording of a      *
      * live run of ZTPHRNMM to mock its input file and responses      *
      * from ZTPHRNDD. This isolates the tested called program,        *
      * ZTPHRNAA, from the inherent volatility of the data coming      *
      * from ZTPHRNDD, which is time-sensitive.                        *
      *                                                                *
      * For the sake of this example, ZTPHRNAA calculates basic        *
      * metrics for sales like average, maximum, etc. Then, based on   *
      * these metrics, determines a "sales score" that can help guide  *
      * the marketing team's decisions on promotion efforts.           *
      *                                                                *
      * Of course, this is all ficticious, but makes for a more        *
      * interesting example because the metrics for calculating this   *
      * score, like the real world of development, may change over     *
      * time. With Test4z's harness and a recording of this routine's  *
      * responses, OTHER teams that depend on this code can be         *
      * isolated from changes in THIS team's implementation.           *
      ******************************************************************

       DATA DIVISION.
       WORKING-STORAGE SECTION.

       01  I                      PIC 9(2).
       01  J                      PIC 9(2).
       01  WS-TOP-SALES-DAY       PIC 9(2).
       01  WS-WIDGET-SALES-MAX    PIC 9(4).
       01  WS-WIDGET-SALES-AVG    PIC 9(6).
       01  WS-WIDGET-SALES-RECENT-REC.
           05 WS-WIDGET-SALES-RECENT OCCURS 30 TIMES.
               10 WS-WIDGET-SALES-DAY PIC 9(4).

       LINKAGE SECTION.

           COPY ZTPHRNRR
               REPLACING ==:HRN:== BY ==LS==.

       PROCEDURE DIVISION USING LS-WIDGET-SALES-RECENT-REC,
                LS-WIDGET-SALES-PROMO-REC.

           DISPLAY 'ZTPHRNAA providing promo sales data for ' 
               LS-WIDGET-ID IN LS-WIDGET-SALES-RECENT-REC

           MOVE LS-WIDGET-ID IN LS-WIDGET-SALES-RECENT-REC
               TO LS-WIDGET-ID IN LS-WIDGET-SALES-PROMO-REC
           MOVE ZEROS TO LS-WIDGET-SALES-PROMO-DATA

      *-----------------------------------------------------------------
      * Calculate basic statistics that carry some weight in the final  
      * sales score calculation, like maximum and average. For example,
      * a sales day's maximum that's significantly higher than the
      * average and recent sales would carry more weight than a slightly
      * higher-than-average maximum in the more distant past.
      *-----------------------------------------------------------------
           MOVE 0 TO WS-WIDGET-SALES-MAX
           MOVE 0 TO WS-WIDGET-SALES-AVG

           PERFORM VARYING I FROM 1 BY 1 UNTIL I > 30
               IF LS-WIDGET-SALES-RECENT(I) > WS-WIDGET-SALES-MAX
                   MOVE LS-WIDGET-SALES-RECENT(I) TO WS-WIDGET-SALES-MAX
               END-IF
               
               ADD LS-WIDGET-SALES-RECENT(I) TO WS-WIDGET-SALES-AVG
           END-PERFORM

           MOVE WS-WIDGET-SALES-MAX TO LS-WIDGET-SALES-MAX
           DIVIDE WS-WIDGET-SALES-AVG BY 30 GIVING LS-WIDGET-SALES-AVG

      *-----------------------------------------------------------------
      * Calculate the top 3 sales day. When calculating the final sales
      * score, more recent "top sales days" carry more weight than
      * top sales further in the past.
      *-----------------------------------------------------------------
           MOVE LS-WIDGET-SALES-RECENT-REC 
               TO WS-WIDGET-SALES-RECENT-REC
           MOVE 0 TO WS-TOP-SALES-DAY

           PERFORM VARYING I FROM 1 BY 1 UNTIL I > 3
               PERFORM VARYING J FROM 1 BY 1 UNTIL J > 30
                   IF WS-TOP-SALES-DAY = 0 OR 
                           WS-WIDGET-SALES-DAY(J) > 
                           WS-WIDGET-SALES-DAY(WS-TOP-SALES-DAY)
                       MOVE J TO WS-TOP-SALES-DAY
                   END-IF
               END-PERFORM

               IF WS-TOP-SALES-DAY NOT = 0
                   MOVE 0 TO WS-WIDGET-SALES-DAY(WS-TOP-SALES-DAY)
                   MOVE WS-TOP-SALES-DAY TO LS-WIDGET-SALES-TOP3-DAYS(I)
                   MOVE 0 TO WS-TOP-SALES-DAY
               END-IF
           END-PERFORM

      *-----------------------------------------------------------------
      * Assume that a bigger spike in sales is given more weight
      * than a steady sales cycle since it could be an indicator
      * of viral interest or seasonal/short-term interest that the
      * marketing team should capitalize on. In other words,
      * the bigger the boom, the bigger marketing attention.
      *-----------------------------------------------------------------
           IF LS-WIDGET-SALES-MAX > 0 AND LS-WIDGET-SALES-AVG > 0
               IF LS-WIDGET-SALES-MAX > (0.75 * LS-WIDGET-SALES-AVG)
                   ADD 1 TO LS-WIDGET-SALES-SCORE
               END-IF
               IF LS-WIDGET-SALES-MAX > LS-WIDGET-SALES-AVG
                   ADD 1 TO LS-WIDGET-SALES-SCORE
               END-IF
               IF LS-WIDGET-SALES-MAX > (1.2 * LS-WIDGET-SALES-AVG)
                   ADD 1 TO LS-WIDGET-SALES-SCORE
               END-IF
           END-IF

      *-----------------------------------------------------------------
      * For this ficticious analysis, boost the score if the sales
      * peaked more recently. For example, if the top sales days
      * are within the last 7 days.
      *
      * Again, this is fictitous, but meant to demonstrate that
      * one team working on a business solution may be introducing
      * changes without knowledge of those who could be impacted.
      * With Test4z's harness and a recording of dependent resource
      * responses, the two teams can work independently by taking
      * a "snapshot" of the other team's recorded program responses.
      *-----------------------------------------------------------------
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > 3
               IF LS-WIDGET-SALES-TOP-DAY(I) <= 7
                   ADD 1 TO LS-WIDGET-SALES-HOT-COUNT
               END-IF

               IF LS-WIDGET-SALES-TOP-DAY(I) <= 3
                   ADD 3 TO LS-WIDGET-SALES-SCORE
               ELSE
                   IF LS-WIDGET-SALES-TOP-DAY(I) <= 5
                       ADD 2 TO LS-WIDGET-SALES-SCORE
                   ELSE
                       IF LS-WIDGET-SALES-TOP-DAY(I) <= 15
                           ADD 1 TO LS-WIDGET-SALES-SCORE
                       END-IF
                   END-IF
               END-IF
           END-PERFORM

      *-----------------------------------------------------------------
      * Make sure our "synthetic data" score is reasonable.
      *-----------------------------------------------------------------
           IF LS-WIDGET-SALES-SCORE > 10
               MOVE 10 TO LS-WIDGET-SALES-SCORE
           END-IF

           GOBACK.
