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
       01  CUSTOMER-RECORD.
           COPY "CUSTOMER".

       WORKING-STORAGE SECTION.

       77  PRETTY-BALANCE       PIC Z(7).99.
       77  EOF                  PIC X         VALUE "N".

       PROCEDURE DIVISION.
       POPULATE-CUSTOMER-TABLE.
           OPEN INPUT CUSTOMER-FILE.
           PERFORM UNTIL EOF = "Y"
           READ CUSTOMER-FILE INTO CUSTOMER-RECORD
           AT END
               MOVE "Y" TO EOF
           NOT AT END
               MOVE ACCOUNT-BALANCE TO PRETTY-BALANCE
               DISPLAY "========================================"
               DISPLAY " Customer ID     | " CUSTOMER-ID
               DISPLAY " Customer Name   | "FUNCTION TRIM(FIRST-NAME)
               " " FUNCTION TRIM(LAST-NAME)
               DISPLAY " Account Number  | " ACCOUNT-NUMBER
               DISPLAY " Account Balance | "
               FUNCTION TRIM(PRETTY-BALANCE) " " CURRENCY-CODE
               DISPLAY "========================================"
           END-READ
           END-PERFORM

           CLOSE CUSTOMER-FILE

           STOP RUN.
