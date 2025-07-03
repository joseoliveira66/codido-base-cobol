       IDENTIFICATION              DIVISION.
       PROGRAM-ID. EXE1SEM3.
      *========================================
      *== AUTOR: JOSE OLIVEIRA    EMPRESA: CDB
      *== OBJETIVO: CALCULO DE SALDO COM LOOP
      *== DATA: 20/02/2025
      *== OBSERVACOES: INFORMACOES RELEVANTES
      *========================================
       ENVIRONMENT                DIVISION.
       CONFIGURATION              SECTION.

       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.

       DATA                           DIVISION.
       WORKING-STORAGE                SECTION.
       77 WRK-SDINICIAL     PIC 9(04)  VALUE ZEROS.
       77 WRK-PAGAMENTO     PIC 9(04)  VALUE ZEROS.
       77 WRK-TOTPAGAMENTO  PIC 9(05)  VALUE ZEROS.
       77 WRK-SDFINAL       PIC S9(05)  VALUE ZEROS.

       PROCEDURE                       DIVISION.
       0001-PRINCIPAL                  SECTION.
           PERFORM 0100-INICIALIZAR
           PERFORM 0200-PROCESSAR UNTIL WRK-PAGAMENTO EQUAL 0
           PERFORM 0300-FINALIZAR
           STOP RUN.

       0100-INICIALIZAR                SECTION.
           DISPLAY "DIGITE SALDO INICIAL"
             ACCEPT WRK-SDINICIAL.
           DISPLAY "INFORME UM PAGAMENTO (0 PARA ENCERRAR):"
           ACCEPT WRK-PAGAMENTO.

       0200-PROCESSAR                   SECTION.
      *     PERFORM

                   ADD WRK-PAGAMENTO TO WRK-TOTPAGAMENTO

      *     END-PERFORM.
               DISPLAY "INFORME UM PAGAMENTO (0 PARA ENCERRAR):"
               ACCEPT WRK-PAGAMENTO.



       0300-FINALIZAR                   SECTION.
           COMPUTE WRK-SDFINAL = WRK-SDINICIAL - WRK-TOTPAGAMENTO.
           DISPLAY "SALDO INICIAL: " WRK-SDINICIAL.
           DISPLAY "TOTAL DE PAGAMENTOS: " WRK-TOTPAGAMENTO.
           DISPLAY "SALDO FINAL: " WRK-SDFINAL.

           DISPLAY "FIM DO PROGRAMA".
