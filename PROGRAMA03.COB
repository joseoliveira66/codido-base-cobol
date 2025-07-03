       IDENTIFICATION              DIVISION.
       PROGRAM-ID. PROGRAMA03.
      *========================================
      *== AUTOR: JOSE OLIVEIRA    EMPRESA: CDB
      *== OBJETIVO: VARIAVEL COM SINAL E MASCARAS
      *== DATA: 04/02/2025
      *== OBSERVACOES: INFORMACOES RELEVANTES
      *========================================
       ENVIRONMENT                DIVISION.
       CONFIGURATION              SECTION.

       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.

       DATA                       DIVISION.
       WORKING-STORAGE             SECTION.

       77 WRK-VALOR1                PIC 9(05)V99   VALUE ZEROS.
       77 WRK-VALOR2                PIC 9(05)V99   VALUE ZEROS.
       77 WRK-RESULTADO             PIC S9(05)V99  VALUE ZEROS.
       77 WRK-RESULTADO-ED          PIC -ZZ.ZZ9,99 VALUE ZEROS.

       PROCEDURE                  DIVISION.
       0100-RECEBE                SECTION.
            DISPLAY "DIGITE O VALOR 1"
               ACCEPT WRK-VALOR1.
            DISPLAY "DIGITE O VALOR 2"
               ACCEPT WRK-VALOR2.
            COMPUTE WRK-RESULTADO = WRK-VALOR1 - WRK-VALOR2.
            MOVE WRK-RESULTADO TO WRK-RESULTADO-ED.

       0200-MOSTRA                SECTION.
            DISPLAY "RESULTADO: " WRK-RESULTADO-ED.

       0300-FINALIZAR             SECTION.
                        STOP RUN.
