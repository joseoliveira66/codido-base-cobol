       IDENTIFICATION              DIVISION.
       PROGRAM-ID. ARQ006.
      *========================================
      *== AUTOR: JOSE OLIVEIRA    EMPRESA: CDB
      *== OBJETIVO: ESTATISTICA ARQ. LIDOS CLIENTE.DAT
      *== DATA: 04/03/2025
      *== OBSERVACOES: INFORMACOES RELEVANTES
      *========================================
       ENVIRONMENT                DIVISION.
       CONFIGURATION              SECTION.
       INPUT-OUTPUT               SECTION.
       FILE-CONTROL.
         SELECT CLIENTES ASSIGN TO "C:\COBOL\DADOS\CLIENTES.DAT"
         FILE STATUS IS FS-CLIENTES.

       DATA                       DIVISION.
       FILE                       SECTION.
       FD CLIENTES.
       01 RG-CLIENTES.
           05 REG-ID           PIC 9(04).
           05 REG-NOME         PIC X(20).
           05 REG-TELEFONE     PIC X(11).

       WORKING-STORAGE            SECTION.
       01 WRK-CLIENTES.
           05 WRK-ID           PIC 9(04).
           05 WRK-NOME         PIC X(20).
           05 WRK-TELEFONE     PIC X(11).

       77 WRK-ACUM-LIDOS       PIC 9(04)  VALUE ZEROS.

       77 FS-CLIENTES   PIC 9(02) VALUE ZEROS.
       77 WRK-MSG-ERRO  PIC X(30) VALUE SPACES.

       PROCEDURE                  DIVISION.
       0000-PRINCIPAL             SECTION.
           PERFORM 0100-INICIALIZAR.
           PERFORM 0200-PROCESSAR.
           PERFORM 0300-FINALIZAR.
           STOP RUN.

       0100-INICIALIZAR           SECTION.
           OPEN INPUT CLIENTES.
           IF FS-CLIENTES EQUAL 35
               MOVE "O ARQUIVO NAO PODE SER ABERTO" TO WRK-MSG-ERRO
               PERFORM 9000-TRATA-ERRO
           END-IF.

       0200-PROCESSAR             SECTION.
           READ CLIENTES
               IF FS-CLIENTES EQUAL 0
               PERFORM UNTIL FS-CLIENTES NOT EQUAL 0
                    ADD 1 TO WRK-ACUM-LIDOS
                    DISPLAY "ID........" REG-ID
                    DISPLAY "NOME......" REG-NOME
                    DISPLAY "TELEFONE.." REG-TELEFONE
                    DISPLAY"===================================="
                    READ CLIENTES
               END-PERFORM
               ELSE
               DISPLAY "ARQUIVO VAZIO"
               END-IF.

       0300-FINALIZAR             SECTION.

       CLOSE CLIENTES.
       PERFORM 0310-ESTATISTICA.
       DISPLAY "FIM DE PROGRAMA".

       0310-ESTATISTICA           SECTION.
            DISPLAY '============================='
            DISPLAY 'QTDE REGISTROS LIDOS...' WRK-ACUM-LIDOS.

       9000-TRATA-ERRO.
           DISPLAY WRK-MSG-ERRO.
           GOBACK.
