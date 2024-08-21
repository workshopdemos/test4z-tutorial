       IDENTIFICATION DIVISION.
       PROGRAM-ID. ZTPTRNDC.

      ******************************************************************
      * Broadcom Test4z System Under Test (SUT) example.               *
      * Copyright (c) 2024 Broadcom. All Rights Reserved.              *
      *                                                                *
      * Calculate the health trend rating for TRENDY. These are        *
      * relative performance ratings used for capacity planning        *
      * and early problem detection of monitored systems.              *
      *                                                                *
      * See ZTPTRNDY for more details about this program under test    *
      * and ZTTRNDY for the associated unit test suite.                *
      ******************************************************************

       ENVIRONMENT DIVISION.

       DATA DIVISION.

       WORKING-STORAGE SECTION.

      ******************************************************************
      * Configuration parameters.                                      *
      *   - Weights are percentages (e.g., 80 = 80%) and can be > 100  *
      *                                                                *
      * NB: These are hardcoded defaults in this example, but for a    *
      *     real implementation, they would be loaded externally or    *
      *     dynamically determined based on historical data.           *
      ******************************************************************
       01  CONFIGURATION-PARAMETERS-TRNDC.
           05 CFG-RESPONSE-RATING-WEIGHTS.
               10 CFG-RESPONSE-WEIGHT-FAST   PIC 9(3) VALUE 100.
               10 CFG-RESPONSE-WEIGHT-OK     PIC 9(3) VALUE 75.
               10 CFG-RESPONSE-WEIGHT-SLOW   PIC 9(3) VALUE 150.
               10 CFG-RESPONSE-WEIGHT-BAD    PIC 9(3) VALUE 200.
               10 CFG-RESPONSE-WEIGHT-ERROR  PIC 9(3) VALUE 400.
           05 CFG-UNHAPPY-PANIC-PERCENT      PIC 9(6) VALUE 20.           

      ******************************************************************
      * Temporary variables and indices                                *
      ******************************************************************
       77  WS-REQUESTS-WEIGHTED         PIC 9(8).
       77  WS-PANIC-TOTAL               PIC 9(6).
       77  WS-WORRY-TOTAL               PIC 9(6).
       77  WS-UNHAPPY-RATING            PIC 9.

       LINKAGE SECTION.
       
      ******************************************************************
      * Definition of the TRENDY file-related records; this program    *
      * is using the HTT-HEALTH-TREND-TOTALS (input) and               *
      * HTR-HEALTH-TREND-RECORD (output).                              *
      ******************************************************************
           COPY ZTPTRREC REPLACING
                   ==:ELR:== BY ==ELR==
                   ==:HLR:== BY ==HLR==
                   ==:HTT:== BY ==LTT==
                   ==:HTR:== BY ==LTR==
                   ==:RPT:== BY ==RPT==.

       PROCEDURE DIVISION 
           USING LTT-HEALTH-TREND-TOTALS, LTR-HEALTH-TREND-RECORD.

           PERFORM 100-CALCULATE-HEALTH-RATINGS

           EXIT.

      ******************************************************************
      * Health ratings are based on the health trend records within    *
      * a given interval. As an example, we'll combine these           *
      * statistics to arrive at a final rating representing the        *
      * relative performance over the interval on a scale of 1-10.     *
      *                                                                *
      * Input: HTT-HEALTH-TREND-TOTALS Output: HTR-HEALTH-TREND-RECORD *
      *                                                                *
      * Definitions:                                                   *
      *                                                                *
      * -- Response rating: Based off the response times, with         *
      *    weighting. For example "10" would mean every response was   *
      *    fast. Note that mediocre response times are weighted        *
      *    more heavily because a few bad responses can impact user    *
      *    satisfaction dramatically.                                  *
      *                                                                *
      * -- Workload rating: For simplicity's sake, this 1-10 rating is *
      *    the overall average workload (0-100) divided by 10.         *
      *                                                                *
      * -- Happy rating: For simplicity's sake, it's the inverse       *
      *    percentage of errors and bad response times where 20% is    *
      *    considered the most serious (i.e., 2% bad responses =       *
      *    rating of 9, 4% bad responses = rating of 8... 6% bad       *
      *    responses = rating of 7... and greater than 20% = rating    *
      *    of 0).                                                      *
      *                                                                *
      * NB: There should never be an interval with no requests, so     *
      *     that's treated as an error and assigned ratings of 0.      *
      ******************************************************************
       100-CALCULATE-HEALTH-RATINGS.

           MOVE LTT-REQUESTS-TOTAL TO LTR-REQUESTS
           IF LTT-REQUESTS-TOTAL = 0
               MOVE 0 TO LTR-RESPONSE-RATING
               MOVE 0 TO LTR-WORKLOAD-RATING
               MOVE 0 TO LTR-HAPPY-RATING
           ELSE
               COMPUTE WS-REQUESTS-WEIGHTED =
                   ((LTT-FAST-TOTAL * CFG-RESPONSE-WEIGHT-FAST) +
                   (LTT-OK-TOTAL * CFG-RESPONSE-WEIGHT-OK)) -
                   ((LTT-SLOW-TOTAL * CFG-RESPONSE-WEIGHT-SLOW) +
                   (LTT-BAD-TOTAL * CFG-RESPONSE-WEIGHT-BAD) +
                   (LTT-ERROR-TOTAL * CFG-RESPONSE-WEIGHT-ERROR))

               IF WS-REQUESTS-WEIGHTED <= 0
                   MOVE 1 TO LTR-RESPONSE-RATING
               ELSE
                   IF WS-REQUESTS-WEIGHTED >=
                           (LTT-REQUESTS-TOTAL * 100)
                       MOVE 10 TO LTR-RESPONSE-RATING
                   ELSE
                       COMPUTE LTR-RESPONSE-RATING =
                          (WS-REQUESTS-WEIGHTED /
                          (LTT-REQUESTS-TOTAL * 10))
                   END-IF
               END-IF

               COMPUTE LTR-WORKLOAD-RATING =
                   (LTT-WORKLOAD-TOTAL / LTT-REQUESTS-TOTAL) / 10

      *-----------------------------------------------------------------
      * The happy rating is a measure of the overall performance;
      * slow and system errors drive it down.
      *
      * CFG-UNHAPPY-PANIC-PERCENT defines the maximum before
      * hitting the panic button. The trouble-free request
      * percentages are scaled to 0-10, similar to star rating.
      *-----------------------------------------------------------------
               COMPUTE WS-WORRY-TOTAL = LTT-BAD-TOTAL + LTT-ERROR-TOTAL
               COMPUTE WS-PANIC-TOTAL =
                   (LTT-REQUESTS-TOTAL *
                   CFG-UNHAPPY-PANIC-PERCENT) / 100

               IF WS-WORRY-TOTAL > 0
                   IF WS-PANIC-TOTAL > 0
                       COMPUTE WS-UNHAPPY-RATING =
                           (10 * WS-WORRY-TOTAL) / WS-PANIC-TOTAL
                   ELSE
                       MOVE 10 TO WS-UNHAPPY-RATING
                   END-IF
               ELSE
                   MOVE 10 TO WS-UNHAPPY-RATING
               END-IF

               IF WS-UNHAPPY-RATING >= 10
                   MOVE 0 TO LTR-HAPPY-RATING
               ELSE
                   SUBTRACT WS-UNHAPPY-RATING FROM 10
                       GIVING LTR-HAPPY-RATING
               END-IF
           END-IF

           EXIT.
      