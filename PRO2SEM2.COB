       IDENTIFICATION              DIVISION.
       PROGRAM-ID. PRG2SEM2.
      *========================================
      *== AUTOR: JOSE OLIVEIRA    EMPRESA: CDB
      *== OBJETIVO: CALCULO VALOR VIAGEM
      *== DATA: 16/02/2025
      *== OBSERVACOES: INFORMACOES RELEVANTES
      *========================================
       ENVIRONMENT                DIVISION.
       CONFIGURATION              SECTION.

       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.

       DATA                       DIVISION.
       WORKING-STORAGE            SECTION.
       01 WRK-PASSAGEIRO    PIC X(030)     VALUE SPACES.
       01 WRK-DESTINO       PIC X(030)     VALUE SPACES.
       01 WRK-LITRO         PIC 9(002)V99  VALUE ZEROS.
       01 WRK-KM            PIC 9(003)     VALUE ZEROS.
       01 WRK-CIDADE        PIC 9(001)     VALUE ZEROS.
       01 WRK-VFINAL        PIC 9(004)V99  VALUE ZEROS.

       01 WRK-MSGS.
          05 WRK-MSGS-DEST1 PIC X(030)     VALUE "SP-JUNDIAI".
          05 WRK-MSGS-DEST2 PIC X(030)     VALUE "SP-CAMPINAS".
          05 WRK-MSGS-DEST3 PIC X(030)     VALUE "SP-PIRACICABA".

       PROCEDURE                  DIVISION.
       0100-RECEBE                SECTION.
           DISPLAY "DIGITE O NOME DO PASSAGEIRO"
           ACCEPT WRK-PASSAGEIRO.

           DISPLAY "DIGITE O SEU DESTINO 1-PIRACIBABA 2-CAMPINAS"
                    "3-JUNDIAI".
           ACCEPT WRK-CIDADE.

       0150-PROCESSA              SECTION.
           EVALUATE WRK-CIDADE
              WHEN 1
              MOVE 220 TO WRK-KM
              MOVE WRK-MSGS-DEST3 TO WRK-DESTINO

              WHEN 2
              MOVE 180 TO WRK-KM
              MOVE WRK-MSGS-DEST2 TO WRK-DESTINO

              WHEN 3
              MOVE 120 TO WRK-KM
              MOVE WRK-MSGS-DEST1 TO WRK-DESTINO

              WHEN OTHER
              MOVE 0 TO WRK-KM

           END-EVALUATE.

       0200-MOSTRA                SECTION.
           IF WRK-KM EQUAL 0
               DISPLAY "SEM VIAGEM"
           ELSE
               DISPLAY "DIGITE O VALOR DO LITRO.."
               ACCEPT WRK-LITRO
           COMPUTE WRK-VFINAL = WRK-KM * (WRK-LITRO / 10)
           COMPUTE WRK-VFINAL = WRK-VFINAL * 1,30

           DISPLAY "NOME DO PASSAGEIRO..." WRK-PASSAGEIRO
           DISPLAY "DESTINO           ..." WRK-DESTINO
           DISPLAY "VALOR DA VIAGEM   ..." WRK-VFINAL
           END-IF.

       0300-FINALIZAR             SECTION.
                        STOP RUN.
