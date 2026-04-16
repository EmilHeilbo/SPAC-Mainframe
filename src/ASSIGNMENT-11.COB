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
           05  TEMP-CUSTOMER.
               06  TEMP-CPR         PIC X(11)   VALUE SPACES.
               06  TEMP-BALANCE     PIC S9(15)V99 VALUE 50000.00.
               06  TEMP-COUNT       PIC 9(3)    VALUE ZEROES.
               06  TEMP-TRANS OCCURS 100 TIMES.
                   COPY TRANSACT.
           05  CUSTOMER OCCURS 1 TO 10000 TIMES
                   DEPENDING ON CUST-MAX
                   INDEXED BY CUST-IX.
               06  CUST-CPR         PIC X(11)   VALUE SPACES.
               06  CUST-BALANCE     PIC S9(15)V99 VALUE 50000.00.
               06  TRANS-COUNT      PIC 9(3)    VALUE ZEROES.
               06  TRANSACTION OCCURS 100 TIMES.
                   COPY TRANSACT.

       77  USD-CONVERSION-RATE      PIC 9V99    VALUE 6.80.
       77  EUR-CONVERSION-RATE      PIC 9V99    VALUE 7.50.
       77  RECIPIENT-INDENT         PIC X(20)   VALUE SPACES.
       77  BUBBLE-IX                PIC 999     VALUE ZEROES.

       01  CURRENT-INFO.
           COPY A10-BOOK.

       01  CPR-FOUND                PIC X       VALUE "N".
           88  FOUND                            VALUE "Y".
       01  EOF-SWITCH               PIC X       VALUE "N".
           88  EOF                              VALUE "Y".


       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           PERFORM READ-BANKS.
           PERFORM READ-TRANSACTIONS.
           PERFORM CALCULATE-BALANCE VARYING CUST-IX FROM 1 BY 1
                   UNTIL CUST-IX > CUST-MAX
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
                       MOVE "N" TO CPR-FOUND
                       IF CUST-MAX > 0
                       SET CUST-IX TO 1
                       SEARCH CUSTOMER VARYING CUST-IX
                       WHEN CUST-CPR(CUST-IX) = FUNCTION TRIM(
                               TRANS-CPR IN TRANS-RECORD)
                       PERFORM
                           MOVE TRANS-CPR IN TRANS-RECORD TO CURRENT-CPR
                           MOVE CUST-IX TO CURRENT-INDEX
                           MOVE "Y" TO CPR-FOUND
                       END-PERFORM
                       END-IF
                       IF NOT FOUND
                           ADD 1 TO CUST-MAX
                           MOVE CUST-MAX TO CUST-IX
                       ELSE
                           SET CUST-IX TO CURRENT-INDEX
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

       CALCULATE-BALANCE.

           EXIT.

       DISPLAY-TOP-3-CUSTOMERS.
           EXIT.
