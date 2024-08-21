       IDENTIFICATION DIVISION.
       PROGRAM-ID. ZTPHRNDD.

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
      * The unit test suite, ZTTHRNES, is responsible for validati     *
      * the correct operation of ZTPHRNMM/ZTPHRNDD/ZTPHRNAA.           *
      *                                                                *
      * This example demonstrates how to mock other resources like     *
      * files and called programs in order to test a given called      *
      * program in isolation using the Test4z harness.                 *
      *                                                                *
      * NB: This implementation of ZTPHRNDD uses pseudo-random numbers *
      *     to generate time-sensitive data. It is for illustrative    *
      *     purposes only and should not be taken literally as an      *
      *     example of synthetic data generation.                      *      
      ******************************************************************

       DATA DIVISION.
       WORKING-STORAGE SECTION.

       01  I                    PIC 9(2).
       01  WS-WIDGET-HIGH-SALES PIC 9(4).
       01  WS-CURRENT-DATE.
           05 FILLER            PIC X(12).
           05 CD-SEED           PIC 9(9) VALUE 0.
       01  WS-RANDOM            COMP-2.

       LINKAGE SECTION.

           COPY ZTPHRNRR
               REPLACING ==:HRN:== BY ==LS==.

       PROCEDURE DIVISION USING LS-WIDGET-SALES-RECENT-REC.

           DISPLAY 'ZTPHRNDD providing recent sales data for '
               LS-WIDGET-ID IN LS-WIDGET-SALES-RECENT-REC

           IF CD-SEED = 0
               MOVE FUNCTION CURRENT-DATE TO WS-CURRENT-DATE
               COMPUTE WS-RANDOM = FUNCTION RANDOM(CD-SEED)
           END-IF

      *-----------------------------------------------------------------
      * Generate synthetic sales data, but try to keep it somewhat
      * realistic. That helps increase the testworthiness for the 
      * code that processes this generated data. 
      *
      * In this fictitious example, we have different classes of
      * products:
      * 
      * 1. "B" category (basic) products = lowest price range
      * 2. "M" category (medium) products = higher price range
      * 3. "P" category (premium) products = highest price range.
      *
      * We use the RANDOM function to generate values within these
      * ranges. With a wider range of data, other routines like those
      * in ZTPHRNAA (analytics) will be more thoroughly tested.
      *-----------------------------------------------------------------
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > 30  
               EVALUATE LS-WIDGET-CATEGORY
                       IN LS-WIDGET-SALES-RECENT-REC
                   WHEN 'B'
                       MOVE 20 TO WS-WIDGET-HIGH-SALES     
      
                   WHEN 'M'
                       MOVE 70 TO WS-WIDGET-HIGH-SALES
      
                   WHEN 'P'
                       MOVE 400 TO WS-WIDGET-HIGH-SALES
      
                   WHEN OTHER
                       MOVE 50 TO WS-WIDGET-HIGH-SALES                    
               END-EVALUATE

               COMPUTE WS-RANDOM = FUNCTION RANDOM
               COMPUTE LS-WIDGET-SALES-RECENT(I) = 
                   (4 * WS-WIDGET-HIGH-SALES) * WS-RANDOM         
           END-PERFORM

           GOBACK.
