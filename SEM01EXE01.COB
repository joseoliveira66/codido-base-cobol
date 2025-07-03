       IDENTIFICATION                   DIVISION.
       PROGRAM-ID. SEM01EXE01.
      *== AUTOR: JOSE OLIVEIRA    EMPRESA: CDB
      *== OBJETIVO: CONVERSAO DE DOLAR PARA REAL
      *== DATA: 07/02/2025
      *== OBSERVACOES: UTILIZAR VALOR DO EXE
      *========================================
       ENVIRONMENT                       DIVISION.
       CONFIGURATION                     SECTION.

       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.

       DATA                              DIVISION.
       WORKING-STORAGE                   SECTION.

       77 WRK-VLRDOLAR              PIC 9(05)V99      VALUE ZEROS.
       77 WRK-QTDEDOLAR             PIC 9(05)V99      VALUE ZEROS.
       77 WRK-VLRREAL               PIC S9(05)V99     VALUE ZEROS.
       77 WRK-VLRREAL-ED            PIC -ZZZ9,99      VALUE ZEROS.


       PROCEDURE                         DIVISION.
       0100-RECEBE                       SECTION.
            DISPLAY "DIGITE O VALOR DO DOLAR"
               ACCEPT WRK-VLRDOLAR.
            DISPLAY "DIGITE A QTDE DE DOLAR"
               ACCEPT WRK-QTDEDOLAR.
            COMPUTE WRK-VLRREAL = WRK-QTDEDOLAR * WRK-VLRDOLAR.
            MOVE WRK-VLRREAL TO WRK-VLRREAL-ED.

       0200-MOSTRA                       SECTION.
            DISPLAY "VALOR EM REAL R$" WRK-VLRREAL-ED.

       0300-FINALIZAR                    SECTION.
                        STOP RUN.
