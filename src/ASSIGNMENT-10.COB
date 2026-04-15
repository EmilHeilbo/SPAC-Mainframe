       IDENTIFICATION DIVISION.
       PROGRAM-ID. ASSIGNMENT-10.

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
       01  BANK-RECORD              PIC X(130).
       FD  TRANSACTION-FILE.
       01  TRANS-RECORD             PIC X(200).
       FD  STATEMENTS-FILE.
       01  STMNT-RECORD             PIC U(120)  VALUE SPACES.

       WORKING-STORAGE SECTION.
       01  BANKS.
           05  BANK-COUNT           PIC 999    VALUE ZEROES.
           05  BANK-INFO OCCURS 1 TO 999 TIMES
                   DEPENDING ON BANK-COUNT
                   INDEXED BY BANK-INDEX.
               COPY BANK.

       01  TRANSACTIONS.
           05  CUST-MAX            PIC 9(5)    VALUE ZEROES.
           05  CUSTOMER OCCURS 1 TO 10000 TIMES
                   DEPENDING ON CUST-MAX
                   INDEXED BY CUST-IX.
               06  CUST-CPR        PIC X(11)   VALUE SPACES.
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
           PERFORM READ-BANKS
           PERFORM READ-TRANSACTIONS
           OPEN OUTPUT STATEMENTS-FILE
           PERFORM PRINT-STATEMENT VARYING CUST-IX FROM 1 BY 1
                   UNTIL CUST-IX > CUST-MAX
           CLOSE STATEMENTS-FILE
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
                   IF NOT TRANS-RECORD(1:15) = CURRENT-CPR
                           AND NOT CUST-MAX = 10000
                       MOVE "N" TO CPR-FOUND
                       IF CUST-MAX > 0
                       SET CUST-IX TO 1
                       SEARCH CUSTOMER VARYING CUST-IX
                       WHEN CUST-CPR(CUST-IX) = TRANS-RECORD(1:11)
                       PERFORM
                           MOVE TRANS-RECORD(1:15) TO CURRENT-CPR
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
                       MOVE TRANS-RECORD(1:15) TO CURRENT-CPR
                       MOVE CURRENT-CPR(1:11) TO CUST-CPR
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

       WRITE-LINE-OUT.
           WRITE STMNT-RECORD
           INITIALIZE STMNT-RECORD
           EXIT.

       WRITE-TRANSACTION.
           INITIALIZE CURRENT-AMOUNT CURRENT-TYPE CURRENT-AMOUNT-DKK
                   DKK-STRING AMOUNT-STRING

           MOVE FUNCTION TRIM(TRANS-AMOUNT(CUST-IX CURRENT-INDEX))
                   TO CURRENT-AMOUNT
           MOVE CURRENT-AMOUNT TO PRETTY-NUMBER
           MOVE TRANS-TIMESTAMP
                   IN TRANSACTION(CUST-IX CURRENT-INDEX)
                   TO CURRENT-TIMESTAMP
           INSPECT CURRENT-TIMESTAMP REPLACING ALL '.' BY ':'
           MOVE TRANS-TYPE
                   IN TRANSACTION(CUST-IX CURRENT-INDEX)
                   TO CURRENT-TYPE
           IF TRANS-CURRENCY(CUST-IX CURRENT-INDEX) = "USD"
               MULTIPLY USD-CONVERSION-RATE BY CURRENT-AMOUNT
           ELSE
               IF TRANS-CURRENCY(CUST-IX CURRENT-INDEX) = "EUR"
                   MULTIPLY EUR-CONVERSION-RATE BY CURRENT-AMOUNT
               END-IF
           END-IF
           IF FUNCTION TRIM(CURRENT-TYPE) = "Indbetaling"
               MOVE "Deposit" TO CURRENT-TYPE
           ELSE
               IF FUNCTION TRIM(CURRENT-TYPE) = "Udbetaling"
                   MOVE "Withdrawal" TO CURRENT-TYPE
               ELSE
                   MOVE "Direct Transfer" TO CURRENT-TYPE
               END-IF
           END-IF
           IF CURRENT-AMOUNT > 0
               ADD CURRENT-AMOUNT TO CURRENT-INBOUND
           ELSE
               ADD CURRENT-AMOUNT TO CURRENT-OUTBOUND
           END-IF
           MOVE CURRENT-AMOUNT TO CURRENT-AMOUNT-DKK
           MOVE FUNCTION TRIM(CURRENT-AMOUNT-DKK) TO DKK-STRING
           STRING FUNCTION TRIM(PRETTY-NUMBER) " "
               TRANS-CURRENCY(CUST-IX CURRENT-INDEX)
               DELIMITED BY SIZE INTO AMOUNT-STRING

           STRING "| " CURRENT-TIMESTAMP(1:10)
               " |  " CURRENT-TIMESTAMP(12:8)
               "  |  " CURRENT-TYPE
               " | " DKK-STRING
               " | " AMOUNT-STRING
               " | " TRANS-STORE(CUST-IX CURRENT-INDEX)
               "|" DELIMITED BY SIZE INTO STMNT-RECORD
           PERFORM WRITE-LINE-OUT
           EXIT.

       SORT-TRANSACTIONS.
           PERFORM VARYING CURRENT-INDEX FROM 1 BY 1
                   UNTIL CURRENT-INDEX > TRANS-COUNT(CUST-IX)
                PERFORM VARYING BUBBLE-IX FROM 1 BY 1
                        UNTIL BUBBLE-IX >= (TRANS-COUNT(CUST-IX)
                            + 1 - CURRENT-INDEX)
                   IF TRANS-TIMESTAMP(CUST-IX, BUBBLE-IX + 1)
                           < TRANS-TIMESTAMP(CUST-IX, BUBBLE-IX)
                       MOVE TRANSACTION(CUST-IX, BUBBLE-IX)
                           TO TRANS-RECORD
                       MOVE TRANSACTION(CUST-IX, BUBBLE-IX + 1)
                           TO TRANSACTION(CUST-IX, BUBBLE-IX)
                       MOVE TRANS-RECORD
                           TO TRANSACTION(CUST-IX, BUBBLE-IX + 1)
                   END-IF
               END-PERFORM
           END-PERFORM.
           EXIT.

       PRINT-STATEMENT.
           INITIALIZE BALANCE CURRENT-INBOUND CURRENT-OUTBOUND
                   CURRENT-BANK
           MOVE TRANS-BANK-CODE(CUST-IX 1) TO CURRENT-BANK
           MOVE 50000.00 TO BALANCE
           MOVE "====================================" TO STMNT-RECORD
           PERFORM WRITE-LINE-OUT.
           STRING "Customer: " FUNCTION TRIM(
                   TRANS-NAME IN TRANSACTION(CUST-IX 1))
               DELIMITED BY SIZE INTO STMNT-RECORD
           PERFORM WRITE-LINE-OUT
           STRING "Address: " FUNCTION TRIM(
                   TRANS-ADDRESS IN TRANSACTION(CUST-IX 1))
               DELIMITED BY SIZE INTO STMNT-RECORD
           PERFORM WRITE-LINE-OUT
           PERFORM WRITE-LINE-OUT
           STRING RECIPIENT-INDENT "Bank: "
               BANK-NAME IN BANK-INFO(CURRENT-BANK)
               DELIMITED BY SIZE INTO STMNT-RECORD
           PERFORM WRITE-LINE-OUT
           STRING RECIPIENT-INDENT "Bank Address: " FUNCTION TRIM(
                   BANK-ADDRESS IN BANK-INFO(CURRENT-BANK))
               DELIMITED BY SIZE INTO STMNT-RECORD
           PERFORM WRITE-LINE-OUT
           STRING RECIPIENT-INDENT "Phone Number: " FUNCTION TRIM(
                   BANK-PHONE IN BANK-INFO(CURRENT-BANK))
               DELIMITED BY SIZE INTO STMNT-RECORD
           PERFORM WRITE-LINE-OUT
           STRING RECIPIENT-INDENT "Email Address: "
               FUNCTION TRIM(BANK-EMAIL IN BANK-INFO(CURRENT-BANK))
               DELIMITED BY SIZE INTO STMNT-RECORD
           PERFORM WRITE-LINE-OUT
           PERFORM WRITE-LINE-OUT
           PERFORM WRITE-LINE-OUT.
           STRING "Account statement for account number: "
               FUNCTION TRIM(TRANS-ACCT-ID
                   IN TRANSACTION(CUST-IX 1))
               DELIMITED BY SIZE INTO STMNT-RECORD
           PERFORM WRITE-LINE-OUT.
           STRING
               "|    Date    |    Time    | Transaction Type |"
               "   Amount (DKK)   | Amount (Currency) |"
               "     Store name      |"
               DELIMITED BY SIZE INTO STMNT-RECORD
           PERFORM WRITE-LINE-OUT
           STRING
               "|------------|------------|------------------|"
               "------------------|-------------------|"
               "---------------------|"
               DELIMITED BY SIZE INTO STMNT-RECORD.
           PERFORM WRITE-LINE-OUT.
           *> In-place bubble sort of transactions
           PERFORM SORT-TRANSACTIONS
           PERFORM WRITE-TRANSACTION VARYING CURRENT-INDEX FROM 1 BY 1
                   UNTIL CURRENT-INDEX > TRANS-COUNT(CUST-IX)
           STRING
               "|------------|------------|------------------|"
               "------------------|-------------------|"
               "---------------------|"
               DELIMITED BY SIZE INTO STMNT-RECORD.
           PERFORM WRITE-LINE-OUT.
           ADD CURRENT-INBOUND TO BALANCE
           ADD CURRENT-OUTBOUND TO BALANCE
           MOVE CURRENT-INBOUND TO PRETTY-NUMBER
           STRING "Total inbound (DKK): " FUNCTION TRIM(PRETTY-NUMBER)
               DELIMITED BY SIZE INTO STMNT-RECORD
           PERFORM WRITE-LINE-OUT
           MOVE CURRENT-OUTBOUND TO PRETTY-NUMBER
           STRING "Total outbound (DKK): "FUNCTION TRIM(PRETTY-NUMBER)
               DELIMITED BY SIZE INTO STMNT-RECORD
           PERFORM WRITE-LINE-OUT
           MOVE BALANCE TO PRETTY-NUMBER
           STRING "Balance (DKK): " FUNCTION TRIM(PRETTY-NUMBER)
               DELIMITED BY SIZE INTO STMNT-RECORD
           PERFORM WRITE-LINE-OUT
           PERFORM WRITE-LINE-OUT
           MOVE "Kind regards," TO STMNT-RECORD
           PERFORM WRITE-LINE-OUT
           MOVE FUNCTION TRIM(BANK-NAME IN BANK-INFO(CURRENT-BANK))
               TO STMNT-RECORD
           PERFORM WRITE-LINE-OUT.
           EXIT.
