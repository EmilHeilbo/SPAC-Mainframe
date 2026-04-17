       IDENTIFICATION DIVISION.
       PROGRAM-ID. ASSIGNMENT-11.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT BANK-FILE ASSIGN TO "data/BANKS.txt"
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT TRANSACTION-FILE ASSIGN TO "data/TRANSACTIONS.txt"
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT STATEMENTS-FILE ASSIGN TO "docs/BANK-STATEMENTS.txt"
               ORGANIZATION IS LINE SEQUENTIAL.


       DATA DIVISION.
       FILE SECTION.
       FD  BANK-FILE.
       01  BANK-RECORD.
           COPY BANK.
       FD  TRANSACTION-FILE.
       01  TRANS-RECORD.
           COPY TRANSACT.
       FD  STATEMENTS-FILE.
       01  STMNT-RECORD             PIC U(120)  VALUE SPACES.

       WORKING-STORAGE SECTION.
       01  BANKS.
           05  BANK-COUNT           PIC 999    VALUE ZEROES.
           05  BANK-INFO OCCURS 1 TO 500 TIMES
                   DEPENDING ON BANK-COUNT
                   INDEXED BY BANK-INDEX.
               COPY BANK.

       01  TRANSACTIONS.
           05  CUST-MAX             PIC 9(5)    VALUE ZEROES.
           05  HIGHEST-BALANCES OCCURS 3 TIMES
               INDEXED BY HIGH-IX.
               10  TOP-CUSTOMERS    PIC 9(5)    VALUE ZEROES.
           05  TEMP-CUSTOMER        PIC X(20032).
           05  CUSTOMER OCCURS 1 TO 10000 TIMES
                   DEPENDING ON CUST-MAX
                   INDEXED BY CUST-IX.
               06  CUST-CPR         PIC X(11)   VALUE SPACES.
               06  CUST-BALANCE     PIC S9(15)V99 VALUE 50000.00.
               06  TRANS-COUNT      PIC 9(3)    VALUE ZEROES.
               06  TRANSACTION OCCURS 100 TIMES INDEXED BY TRANS-IX.
                   COPY TRANSACT.

       77  USD-RATE      PIC 9V99    VALUE 6.80.
       77  EUR-RATE      PIC 9V99    VALUE 7.50.
       77  RECIPIENT-INDENT         PIC X(20)   VALUE SPACES.
       77  BUBBLE-IX                PIC 9(5).

       01  STATISTICS.
           05  MAX-REPORTS          PIC 9(5)    VALUE ZEROES.
           05  TEMP-REPORT          PIC X(50)   VALUE SPACES.
           05  PRETTY-INBOUND       PIC Z(11)9.99.
           05  PRETTY-OUTBOUND      PIC Z(11)9.99+.
           05  MONTHLY-STATS OCCURS 1 TO 10000 TIMES
                   DEPENDING ON MAX-REPORTS
                   INDEXED BY REPORT-IX.
               10  STAT-PERIOD      PIC X(7)    VALUE SPACES. *> YYYY-MM
               10  INBOUND-DKK      PIC S9(15)V99  VALUE ZEROES.
               10  OUTBOUND-DKK     PIC S9(15)V99  VALUE ZEROES.

       01  CURRENT-INFO.
           05  CURRENT-AMOUNT       PIC S9(13)V99 VALUE ZEROES.
           05  CURRENT-CPR          PIC X(15).
           05  CURRENT-IX           PIC 9(5)    VALUE ZEROES.
           05  CURRENT-CURRENCY     PIC X(3).
           05  CURRENT-BALANCE      PIC Z(14)9.99+.

       01  INSTANCE-FOUND           PIC X       VALUE "N".
           88  FOUND                            VALUE "Y".
       01  EOF-SWITCH               PIC X       VALUE "N".
           88  EOF                              VALUE "Y".


       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           PERFORM READ-BANKS.
           PERFORM READ-TRANSACTIONS.
           DISPLAY "Calculating balances of " CUST-MAX " customers"
           PERFORM CALCULATE-BALANCE VARYING CUST-IX FROM 1 BY 1
                   UNTIL CUST-IX > CUST-MAX.
      *    Option to sort customers by balance exists as a sanity-check
           *>PERFORM BUBBLE-SORT-CUSTOMERS.
           *>PERFORM VARYING HIGH-IX FROM 1 BY 1 UNTIL HIGH-IX > 3
           *>    MOVE HIGH-IX TO TOP-CUSTOMERS(HIGH-IX)
           *>END-PERFORM
      *    Quick pass over CUSTOMERS to find the highest balances
           PERFORM FIND-HIGHEST-BALANCES
           PERFORM DISPLAY-TOP-3-CUSTOMERS.
           PERFORM GENERATE-MONTHLY-STATS.
           PERFORM DISPLAY-MONTHLY-STATS
           STOP RUN.

       READ-BANKS.
           DISPLAY "Banks going in!"
           MOVE "N" TO EOF-SWITCH
           OPEN INPUT BANK-FILE
           PERFORM UNTIL EOF
           READ BANK-FILE
               AT END
                   MOVE "Y" TO EOF-SWITCH
               NOT AT END
                   ADD 1 TO BANK-COUNT
                   MOVE BANK-RECORD TO BANK-INFO(BANK-COUNT)
           END-READ
           END-PERFORM
           DISPLAY "Read " BANK-COUNT " banks."
           CLOSE BANK-FILE
           EXIT.

       READ-TRANSACTIONS.
           DISPLAY "Transactions incoming!"
           OPEN INPUT TRANSACTION-FILE
           MOVE "N" TO EOF-SWITCH
           PERFORM UNTIL EOF
           READ TRANSACTION-FILE
               AT END
                   MOVE "Y" TO EOF-SWITCH
               NOT AT END
                   *> Check if customer already exists
                   IF NOT TRANS-CPR IN TRANS-RECORD = CURRENT-CPR
                           AND NOT CUST-MAX = 10000
                       MOVE "N" TO INSTANCE-FOUND
                       IF CUST-MAX > 0
                       SET CUST-IX TO 1
                       SEARCH CUSTOMER VARYING CUST-IX
                       WHEN CUST-CPR(CUST-IX) = FUNCTION TRIM(
                               TRANS-CPR IN TRANS-RECORD)
                       PERFORM
                           MOVE TRANS-CPR IN TRANS-RECORD TO CURRENT-CPR
                           MOVE CUST-IX TO CURRENT-IX
                           MOVE "Y" TO INSTANCE-FOUND
                       END-PERFORM
                       END-IF
                       IF NOT FOUND
                           ADD 1 TO CUST-MAX
                           MOVE CUST-MAX TO CUST-IX
                       ELSE
                           SET CUST-IX TO CURRENT-IX
                       END-IF
                       MOVE TRANS-CPR IN TRANS-RECORD TO CURRENT-CPR
                       MOVE FUNCTION TRIM(CURRENT-CPR) TO CUST-CPR
                               IN CUSTOMER(CUST-IX)
                   END-IF
                   IF TRANS-COUNT(CUST-IX) < 100
                       ADD 1 TO TRANS-COUNT(CUST-IX)
                       MOVE TRANS-RECORD TO TRANSACTION(CUST-IX
                                               TRANS-COUNT(CUST-IX))
                   END-IF
           END-READ
           END-PERFORM
           CLOSE TRANSACTION-FILE
           DISPLAY "Read " CUST-MAX " customers."
           MOVE SPACES TO CURRENT-CPR
           EXIT.

       CONVERT-CURRENT-AMOUNT.
           IF CURRENT-CURRENCY = "DKK"
               CONTINUE
           ELSE
               IF CURRENT-CURRENCY = "USD"
                   MULTIPLY USD-RATE BY CURRENT-AMOUNT
               ELSE
                   IF CURRENT-CURRENCY = "EUR"
                       MULTIPLY EUR-RATE BY CURRENT-AMOUNT
                   END-IF
               END-IF
           END-IF
           EXIT.

       CALCULATE-BALANCE.
           PERFORM VARYING CURRENT-IX FROM 1 BY 1
                   UNTIL CURRENT-IX > TRANS-COUNT OF CUSTOMER(CUST-IX)
               INITIALIZE CURRENT-AMOUNT
               MOVE TRANS-CURRENCY OF TRANSACTION(CUST-IX, CURRENT-IX)
                       TO CURRENT-CURRENCY
               MOVE TRANS-AMOUNT OF TRANSACTION(CUST-IX, CURRENT-IX)
                       TO CURRENT-AMOUNT
               PERFORM CONVERT-CURRENT-AMOUNT
               ADD CURRENT-AMOUNT TO CUST-BALANCE(CUST-IX)
           END-PERFORM
           EXIT.

       FIND-HIGHEST-BALANCES.
           PERFORM VARYING CUST-IX FROM 1 BY 1 UNTIL CUST-IX > CUST-MAX
               MOVE "N" TO INSTANCE-FOUND
               PERFORM VARYING HIGH-IX FROM 1 BY 1
                       UNTIL HIGH-IX > 3 OR FOUND
                   IF TOP-CUSTOMERS(HIGH-IX) = 0
                       MOVE CUST-IX TO TOP-CUSTOMERS(HIGH-IX)
                       MOVE "Y" TO INSTANCE-FOUND
                   ELSE IF CUST-BALANCE(CUST-IX)
                           > CUST-BALANCE(TOP-CUSTOMERS(HIGH-IX))
                       IF HIGH-IX < 3
                           MOVE TOP-CUSTOMERS(HIGH-IX)
                               TO TOP-CUSTOMERS(HIGH-IX + 1)
                       END-IF
                       MOVE CUST-IX TO TOP-CUSTOMERS(HIGH-IX)
                       MOVE "Y" TO INSTANCE-FOUND
                   END-IF
                   END-IF
               END-PERFORM
           END-PERFORM
           EXIT.

       BUBBLE-SORT-CUSTOMERS.
           DISPLAY "Sorting "CUST-MAX" customers, this may take a bit.."
           DISPLAY "Estimated time is 7 seconds per 1'000 customers"
           PERFORM VARYING CUST-IX FROM 1 BY 1 UNTIL CUST-IX > CUST-MAX
               PERFORM VARYING BUBBLE-IX FROM 1 BY 1
                       UNTIL BUBBLE-IX >= CUST-MAX - CUST-IX + 1
                   IF CUST-BALANCE OF CUSTOMER(BUBBLE-IX)
                           < CUST-BALANCE OF CUSTOMER(BUBBLE-IX + 1)
                       MOVE CUSTOMER(BUBBLE-IX) TO TEMP-CUSTOMER
                       MOVE CUSTOMER(BUBBLE-IX + 1)
                               TO CUSTOMER(BUBBLE-IX)
                       MOVE TEMP-CUSTOMER TO CUSTOMER(BUBBLE-IX + 1)
                   END-IF
               END-PERFORM
           END-PERFORM
           EXIT.

       DISPLAY-TOP-3-CUSTOMERS.
           PERFORM VARYING HIGH-IX FROM 1 BY 1 UNTIL HIGH-IX > 3
               INITIALIZE CURRENT-BALANCE
               MOVE CUST-BALANCE(TOP-CUSTOMERS(HIGH-IX))
                   TO CURRENT-BALANCE
               DISPLAY "Customer ID: " FUNCTION TRIM(TRANS-ACCT-ID OF
                       TRANSACTION(TOP-CUSTOMERS(HIGH-IX), 1))
                   "; Name: " FUNCTION TRIM(TRANS-NAME OF
                           TRANSACTION(TOP-CUSTOMERS(HIGH-IX), 1))
                   "; Balance: " FUNCTION TRIM(CURRENT-BALANCE) " DKK"
           END-PERFORM
           EXIT.

       BUBBLE-SORT-REPORTS.
           DISPLAY "Sorting " MAX-REPORTS " reports using BUBBLE SORT"
           PERFORM VARYING REPORT-IX FROM 1 BY 1
                   UNTIL REPORT-IX > MAX-REPORTS
               PERFORM VARYING BUBBLE-IX FROM 1 BY 1
                       UNTIL BUBBLE-IX >= MAX-REPORTS - REPORT-IX + 1
                   IF STAT-PERIOD(BUBBLE-IX)
                           > STAT-PERIOD(BUBBLE-IX + 1)
                       MOVE MONTHLY-STATS(BUBBLE-IX) TO TEMP-REPORT
                       MOVE MONTHLY-STATS(BUBBLE-IX + 1)
                               TO MONTHLY-STATS(BUBBLE-IX)
                       MOVE TEMP-REPORT TO MONTHLY-STATS(BUBBLE-IX + 1)
                   END-IF
               END-PERFORM
           END-PERFORM
           EXIT.

       GENERATE-MONTHLY-STATS.
      *    Iterate over all customers and all transactions,
      *    calculating the DKK value of each transaction
      *    and checking if the current period, eg. 2026-04, exists.
           *> Outer loop - customers:
           PERFORM VARYING CUST-IX FROM 1 BY 1 UNTIL CUST-IX > CUST-MAX
               *> Inner loop - transactions:
               PERFORM VARYING TRANS-IX FROM 1 BY 1
                       UNTIL TRANS-IX > TRANS-COUNT OF CUSTOMER(CUST-IX)
                   *> Convert amount to DKK
                   MOVE TRANS-CURRENCY OF TRANSACTION(CUST-IX, TRANS-IX)
                           TO CURRENT-CURRENCY
                   MOVE TRANS-AMOUNT OF TRANSACTION(CUST-IX, TRANS-IX)
                           TO CURRENT-AMOUNT
                   PERFORM CONVERT-CURRENT-AMOUNT
                   *> Check if current period exists, create new if not
                   MOVE "N" TO INSTANCE-FOUND
                   INITIALIZE CURRENT-IX
                   IF MAX-REPORTS > 0
                       SET REPORT-IX TO 1
                       SEARCH MONTHLY-STATS VARYING REPORT-IX
                               WHEN TRANS-TIMESTAMP OF TRANSACTION(
                                   CUST-IX, TRANS-IX)(1:7)
                               = STAT-PERIOD(REPORT-IX)
                           PERFORM
                               MOVE "Y" TO INSTANCE-FOUND
                               MOVE REPORT-IX TO CURRENT-IX
                           END-PERFORM
                       END-IF
                   IF NOT FOUND
                       ADD 1 TO MAX-REPORTS
                       MOVE MAX-REPORTS TO CURRENT-IX
                       MOVE TRANS-TIMESTAMP OF TRANSACTION(
                               CUST-IX, TRANS-IX)(1:7)
                           TO STAT-PERIOD(CURRENT-IX)
                   END-IF
                   IF CURRENT-AMOUNT < 0
                       ADD CURRENT-AMOUNT TO OUTBOUND-DKK(CURRENT-IX)
                   ELSE
                       ADD CURRENT-AMOUNT TO INBOUND-DKK(CURRENT-IX)
                   END-IF
               END-PERFORM
           END-PERFORM
      *    Now it's time to sort them all from earliest to latest
           PERFORM BUBBLE-SORT-REPORTS
           EXIT.

       DISPLAY-MONTHLY-STATS.
           DISPLAY "|---------|-----------------|------------------|"
           DISPLAY "| YYYY-MM |  Inbound (DKK)  |  Outbound (DKK)  |"
           DISPLAY "|---------|-----------------|------------------|"
           PERFORM VARYING REPORT-IX FROM 1 BY 1
                   UNTIL REPORT-IX > MAX-REPORTS
               INITIALIZE PRETTY-INBOUND PRETTY-OUTBOUND
               MOVE INBOUND-DKK(REPORT-IX) TO PRETTY-INBOUND
               MOVE OUTBOUND-DKK(REPORT-IX) TO PRETTY-OUTBOUND
               DISPLAY "| " STAT-PERIOD(REPORT-IX) " | "
                   PRETTY-INBOUND " | " PRETTY-OUTBOUND " |"
           END-PERFORM
           DISPLAY "|---------|-----------------|------------------|"
           EXIT.
