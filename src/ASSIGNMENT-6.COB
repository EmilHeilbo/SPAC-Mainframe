       IDENTIFICATION DIVISION.
       PROGRAM-ID. ASSIGNMENT-6.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CUSTOMER-FILE ASSIGN TO "data/CUSTOMERS.txt"
               ORGANIZATION IS LINE SEQUENTIAL.


       DATA DIVISION.

       FILE SECTION.
       FD  CUSTOMER-FILE.
       01  CUSTOMER-RECORD      PIC X(211).

       WORKING-STORAGE SECTION.
       01  READ-LOOP-INFO.
           COPY A6-LOOP.

       01  CUSTOMER-TABLE.
           02 CUSTOMERS OCCURS 1 TO 1000 TIMES
                       DEPENDING ON CUSTOMER-COUNT
                       INDEXED BY CUSTOMER-INDEX.
               03 CUSTOMER-DATA.
                   COPY CUSTOMER.

       77  PRETTY-BALANCE       PIC Z(7).99.
       01  EOF-SWITCH           PIC X        VALUE "N".
           88  EOF                           VALUE "Y".


       PROCEDURE DIVISION.
       MAIN-RUNTIME.
           OPEN INPUT CUSTOMER-FILE.
           PERFORM READ-LOOP UNTIL EOF
           CLOSE CUSTOMER-FILE

           PERFORM PRINT-CUSTOMER VARYING LOOP-INDEX FROM 1 BY 1
               UNTIL LOOP-INDEX > CUSTOMER-COUNT.
           MOVE CUSTOMER-COUNT TO FINAL-COUNT
           DISPLAY "Customers added: " FUNCTION TRIM(FINAL-COUNT)

           STOP RUN.

       READ-LOOP.
           READ CUSTOMER-FILE
               AT END
                   MOVE "Y" TO EOF-SWITCH
               NOT AT END
                   IF CUSTOMER-COUNT < MAX-CUSTOMERS
                       ADD 1 TO CUSTOMER-COUNT
                       MOVE CUSTOMER-RECORD
                           TO CUSTOMER-DATA (CUSTOMER-COUNT)
                    END-IF
           END-READ.

       PRINT-CUSTOMER.
           MOVE ACCOUNT-BALANCE(LOOP-INDEX) TO PRETTY-BALANCE
           DISPLAY "========================================"
           DISPLAY " Customer ID     | " CUSTOMER-ID(LOOP-INDEX)
           DISPLAY " Customer Name   | "
           FUNCTION TRIM(FIRST-NAME(LOOP-INDEX))
           " " FUNCTION TRIM(LAST-NAME(LOOP-INDEX))
           DISPLAY " Account Number  | " ACCOUNT-NUMBER(LOOP-INDEX)
           DISPLAY " Account Balance | "
           FUNCTION TRIM(PRETTY-BALANCE) " " CURRENCY-CODE(LOOP-INDEX)
           DISPLAY "========================================".
