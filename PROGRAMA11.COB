       IDENTIFICATION              DIVISION.
       PROGRAM-ID. PROGRAMA11.
      *========================================
      *== AUTOR: JOSE OLIVEIRA    EMPRESA: CDB
      *== OBJETIVO: OPERACOES DIVISAO (DIVIDE)
      *== DATA: 12/02/2025
      *== OBSERVACOES: INFORMACOES RELEVANTES
      *========================================
       ENVIRONMENT                DIVISION.
       CONFIGURATION              SECTION.

       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.

       DATA                       DIVISION.
       WORKING-STORAGE            SECTION.
       77 WRK-NUM1         PIC 9(04)          VALUE ZEROS.
       77 WRK-NUM2         PIC 9(04)          VALUE ZEROS.
       77 WRK-RESULTADO    PIC S9(08)V99      VALUE ZEROS.
       77 WRK-RESULTADO-ED PIC -ZZ.ZZZ.ZZ9,99 VALUE ZEROS.

       PROCEDURE                  DIVISION.
       0100-RECEBE                SECTION.
            DISPLAY "DIGITE O PRIMEIRO NUMERO"
            ACCEPT WRK-NUM1
            DISPLAY "DIGITE O SEGUNDO NUMERO"
            ACCEPT WRK-NUM2.
       0150-PROCESSA              SECTION.
      *     ADD 1 GIVING WRK-RESULTADO - ZERA O RESULTADO E ACRESCENTA O QUE ESTÁ ANTES
            DIVIDE WRK-NUM1 BY WRK-NUM2 GIVING WRK-RESULTADO
                   ON SIZE ERROR
                        DISPLAY "ERRO DIVISAO POR O.".

       0200-MOSTRA                SECTION.
            MOVE WRK-RESULTADO TO WRK-RESULTADO-ED
            DISPLAY "RESULTADO..." WRK-RESULTADO-ED.

       0300-FINALIZAR             SECTION.
                        STOP RUN.
