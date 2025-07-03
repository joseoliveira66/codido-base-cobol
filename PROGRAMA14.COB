       IDENTIFICATION              DIVISION.
       PROGRAM-ID. PROGRAMA14.
      *========================================
      *== AUTOR: JOSE OLIVEIRA    EMPRESA: CDB
      *== OBJETIVO: CALCULAR FRETE POR ESTADO
      *== DATA: 13/02/2025
      *== OBSERVACOES: INFORMACOES RELEVANTES
      *========================================
       ENVIRONMENT                DIVISION.
       CONFIGURATION              SECTION.

       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.

       DATA                       DIVISION.
       WORKING-STORAGE            SECTION.
      *==========VARIAVEIS DE APOIO========
       77 WRK-PRODUTO  PIC X(30)     VALUE SPACES.
       77 WRK-VALOR    PIC 9(08)V99  VALUE ZEROS.
       77 WRK-FRETE    PIC 9(08)V99  VALUE ZEROS.
       77 WRK-ESTADO   PIC X(02)     VALUE SPACES.

      *==========VARIAVEIS DE EDICAO========
       77 WRK-VALOR-ED PIC ZZ.ZZZ.ZZ9,99.
       77 WRK-FRETE-ED PIC ZZ.ZZZ.ZZ9,99.

       PROCEDURE                  DIVISION.
       0100-RECEBE                SECTION.
           DISPLAY "PRODUTO"
             ACCEPT WRK-PRODUTO
           DISPLAY "VALOR"
             ACCEPT WRK-VALOR
           DISPLAY "ESTADO (SP/RJ/ES)"
             ACCEPT WRK-ESTADO.

       0150-PROCESSA              SECTION.

           IF WRK-ESTADO EQUAL "SP"
               COMPUTE WRK-FRETE = WRK-VALOR * 0,10
           END-IF.
           IF WRK-ESTADO EQUAL "RJ"
               COMPUTE WRK-FRETE = WRK-VALOR * 0,15
           END-IF.
           IF WRK-ESTADO EQUAL "ES"
               COMPUTE WRK-FRETE = WRK-VALOR * 0,20
           END-IF.

           COMPUTE WRK-VALOR = WRK-VALOR + WRK-FRETE
           MOVE WRK-FRETE TO WRK-FRETE-ED.
           MOVE WRK-VALOR TO WRK-VALOR-ED.

       0200-MOSTRA                SECTION.

           DISPLAY "VALOR FINAL    R$.." WRK-VALOR-ED.

           IF WRK-ESTADO = "SP" OR WRK-ESTADO = "RJ" OR WRK-ESTADO= "ES"
               DISPLAY "ESTADO VALIDO"
           ELSE
               DISPLAY "NAO HE UM ESTADO VALIDO"
           END-IF.

           IF WRK-FRETE GREATER 0
               DISPLAY "FRETE      R$.." WRK-FRETE-ED
           ELSE
               DISPLAY "NAO ENTREGAMOS NO ESTADO..." WRK-ESTADO
           END-IF.


       0300-FINALIZAR             SECTION.
                        STOP RUN.
