       IDENTIFICATION              DIVISION.
       PROGRAM-ID. PROGRAMA15.
      *========================================
      *== AUTOR: JOSE OLIVEIRA    EMPRESA: CDB
      *== OBJETIVO: CALCULAR FRETE POR ESTADO USANDO EVALUATE
      *== DATA: 14/02/2025
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

           EVALUATE WRK-ESTADO
              WHEN "SP"
                COMPUTE WRK-FRETE = WRK-VALOR * 0,10
              WHEN "RJ"
               COMPUTE WRK-FRETE = WRK-VALOR * 0,15
              WHEN "ES"
               COMPUTE WRK-FRETE = WRK-VALOR * 0,20
              WHEN OTHER
               DISPLAY "NAO ENTREGAMOS NO ESTADO..." WRK-ESTADO
           END-EVALUATE.

           COMPUTE WRK-VALOR = WRK-VALOR + WRK-FRETE
           MOVE WRK-FRETE TO WRK-FRETE-ED.
           MOVE WRK-VALOR TO WRK-VALOR-ED.

       0200-MOSTRA                SECTION.

           DISPLAY "VALOR FINAL    R$.." WRK-VALOR-ED.

           IF WRK-FRETE GREATER 0
               DISPLAY "FRETE      R$.." WRK-FRETE-ED
           END-IF.

       0300-FINALIZAR             SECTION.
                        STOP RUN.
