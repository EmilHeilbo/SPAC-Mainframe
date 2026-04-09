CUSTO *    Copybook of existing customer record structure
       05  CUSTOMER-ID              PIC X(10)       VALUE SPACES.
       05  CONTACT-INFO.
           10  FIRST-NAME           PIC X(20)       VALUE SPACES.
           10  LAST-NAME            PIC X(20)       VALUE SPACES.
           10  PHONE-NUMBER         PIC X(8)        VALUE SPACES.
           10  EMAIL-ADDRESS        PIC X(50)       VALUE SPACES.
       05  ACCOUNT-INFO.
           10  ACCOUNT-NUMBER       PIC X(20)       VALUE SPACES.
           10  ACCOUNT-BALANCE      PIC 9(7)V99     VALUE ZEROES.
           10  CURRENCY-CODE        PIC X(3)        VALUE SPACES.
       05  CUSTOMER-ADDRESS.
           10  STREET-NAME          PIC X(30)       VALUE SPACES.
           10  BUILDING-NUMBER      PIC X(5)        VALUE SPACES.
           10  FLOOR                PIC X(5)        VALUE SPACES.
           10  SIDE                 PIC X(5)        VALUE SPACES.
           10  CITY                 PIC X(20)       VALUE SPACES.
           10  POSTAL-CODE          PIC X(4)        VALUE SPACES.
           10  COUNTRY-CODE         PIC X(2)        VALUE SPACES.


