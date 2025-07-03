       IDENTIFICATION                   DIVISION.
       PROGRAM-ID. SEM01EXE02.
      *== AUTOR: JOSE OLIVEIRA    EMPRESA: CDB
      *== OBJETIVO: CÁLCULO DO IMC
      *== DATA: 07/02/2025
      *== OBSERVACOES: UTILIZAR VALOR DO EXE
      *========================================
       ENVIRONMENT                       DIVISION.
       CONFIGURATION                     SECTION.

       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.

       DATA                              DIVISION.
       WORKING-STORAGE                   SECTION.

       77 WRK-PESO                  PIC 9(05)V99      VALUE ZEROS.
       77 WRK-ALTURA                PIC 9(05)V99      VALUE ZEROS.
       77 WRK-IMC                   PIC S9(05)V99     VALUE ZEROS.
       77 WRK-IMC-ED                PIC -ZZZ9,99      VALUE ZEROS.


       PROCEDURE                         DIVISION.
       0100-RECEBE                       SECTION.
            DISPLAY "DIGITE O SEU PESO"
               ACCEPT WRK-PESO.
            DISPLAY "DIGITE A SUA ALTURA"
               ACCEPT WRK-ALTURA.
            COMPUTE WRK-IMC = WRK-PESO / (WRK-ALTURA * WRK-ALTURA).
            MOVE WRK-IMC TO WRK-IMC-ED.

       0200-MOSTRA                       SECTION.
            DISPLAY "SEU INDICE DE IMC É: " WRK-IMC-ED.

       0300-FINALIZAR                    SECTION.
                        STOP RUN.
