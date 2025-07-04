       IDENTIFICATION DIVISION.
       PROGRAM-ID. PROJETO.




       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CLIENTES ASSIGN TO 'DADOS\CLIENTES.DAT'
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               FILE STATUS IS CLIENTES-STATUS
               RECORD KEY IS CLIENTES-CHAVE.


               SELECT RELATO ASSIGN TO 'DADOS\CLIENTES.DAT'
               ORGANIZATION IS INDEXED
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS RELATO-STATUS
               RECORD KEY IS RELATO-DADOS.

       DATA DIVISION.
       FILE                               SECTION.
       FD CLIENTES.

       01 CLIENTES-REG.
           05 CLIENTES-CHAVE.
               10 CLIENTES-FONE   PIC 9(09) BLANK WHEN ZEROS.
           05 CLIENTES-NOME       PIC X(40).
           05 CLIENTES-EMAIL      PIC X(50).
           05 FILLER              PIC X(20).

       FD RELATO.
       01 RELATO-REG.
           05 RELATO-DADOS  PIC X(99).


       WORKING-STORAGE                    SECTION.
      *-------------- VARIAVEIS DE CONTROLE/APOIO
       01 WRK-OPCAO                     PIC X.
       01 WRK-TECLA                     PIC X.
       01 WRK-MSGERRO                   PIC X(30).
       01 WRK-LINHA                     PIC 9(02).
      *-------------- VARIAVEIS DE STATUS
       01 CLIENTES-STATUS              PIC 9(02).
       01 RELATO-STATUS                PIC 9(02).


      *-------------- VARIAVEIS DE TELA
       01 WRK-MODULO                   PIC X(30).
       01 WRK-CORFUNDO                 PIC 9 VALUE 1.




       SCREEN                             SECTION.
       01 TELA.
           05 LIMPA-TELA.
               10 BLANK SCREEN.
               10 LINE 01 COLUMN 01 ERASE EOL
                  BACKGROUND-COLOR  WRK-CORFUNDO.

           05 SS-CABECALHO.
               10 LINE 01 COLUMN 02    PIC X(30) FROM WRK-MODULO
                  FOREGROUND-COLOR 6
                  BACKGROUND-COLOR 1.
       01 MENU FOREGROUND-COLOR 6.
           05 LINE 07 COLUMN 15 VALUE "1 - INCLUIR".
           05 LINE 08 COLUMN 15 VALUE "2 - CONSULTAR".
           05 LINE 09 COLUMN 15 VALUE "3 - ALTERAR".
           05 LINE 10 COLUMN 15 VALUE "4 - EXCLUIR".
           05 LINE 11 COLUMN 15 VALUE "5 - RELATORIO".
           05 LINE 12 COLUMN 15 VALUE "X - ENCERRAR".
           05 LINE 13 COLUMN 15 VALUE "OPCAO: ".
           05 LINE 13 COLUMN 23 USING WRK-OPCAO.

       01 TELA-REGISTRO.
           05 CHAVE FOREGROUND-COLOR 2.
               10 LINE 10 COLUMN 10 VALUE "TELEFONE: ".
               10 COLUMN PLUS 2 PIC 9(09) USING CLIENTES-FONE
                  BLANK WHEN ZEROS.
           05 DADOS.
               10 LINE 11 COLUMN 10 VALUE "NOME....: ".
               10 COLUMN PLUS 2 PIC X(40) USING CLIENTES-NOME.
               10 LINE 12 COLUMN 10 VALUE "EMAIL...: ".
               10 COLUMN PLUS 2 PIC X(40) USING CLIENTES-EMAIL.
       01 MOSTRA-ERRO.
           02 MSG-ERRO.
               10 LINE 22 COLUMN 10 PIC X(45) FROM WRK-MSGERRO.
               10 LINE 22 COLUMN 55 USING WRK-TECLA.

       PROCEDURE DIVISION.

       PERFORM 9000-ABRIR-ARQUIVOS.

       000-INICIO                                     SECTION.
             MOVE 1 TO WRK-CORFUNDO.
              INITIALIZE CLIENTES-REG.

             MOVE 'MENU' TO WRK-MODULO.

               DISPLAY TELA.
               ACCEPT MENU.

               EVALUATE WRK-OPCAO
                   WHEN 1
                       PERFORM 0100-INCLUIR

                   WHEN 2
                       PERFORM 0200-CONSULTAR
                   WHEN 3
                       PERFORM 0300-ALTERACAO
                   WHEN 4
                       PERFORM 0400-EXCLUSAO
                   WHEN 5
                      PERFORM  0500-RELATORIO
               END-EVALUATE

               PERFORM 9100-FECHAR-ARQUIVOS.

              GOBACK.

       0100-INCLUIR                                       SECTION.
           MOVE 'INCLUSAO ' TO  WRK-MODULO.
            DISPLAY TELA.
            ACCEPT TELA-REGISTRO.
             WRITE CLIENTES-REG
             INVALID KEY
               MOVE "CLIENTE JA EXISTE" TO WRK-MSGERRO
               DISPLAY MOSTRA-ERRO
               ACCEPT WRK-TECLA
              END-WRITE.
            PERFORM 000-INICIO.


       0200-CONSULTAR                                     SECTION.
           MOVE 'CONSULTA ' TO  WRK-MODULO.
            DISPLAY TELA.
            DISPLAY TELA-REGISTRO.
            ACCEPT CHAVE.
               READ CLIENTES
               INVALID KEY
                       MOVE "CLIENTE N�O ENCONTRADO" TO WRK-MSGERRO
               NOT INVALID KEY

                       MOVE 'CLIENTE ENCONTRADO    ' TO WRK-MSGERRO
                       DISPLAY DADOS
               END-READ
                       DISPLAY MOSTRA-ERRO.
                       ACCEPT WRK-TECLA.
            PERFORM 000-INICIO.

       0300-ALTERACAO                                      SECTION.
           MOVE 'ALTERACAO ' TO  WRK-MODULO.
            DISPLAY TELA.
            DISPLAY TELA-REGISTRO.
            ACCEPT CHAVE.
               READ CLIENTES
                IF CLIENTES-STATUS EQUAL 0
                   ACCEPT DADOS
                     REWRITE CLIENTES-REG
                       IF CLIENTES-STATUS EQUAL 0
                         MOVE 'REGISTRO ALTERADO ' TO WRK-MSGERRO
                         ACCEPT MOSTRA-ERRO
                       ELSE
                         MOVE 'ERRO AO ALTERAR REGISTRO ' TO WRK-MSGERRO
                         ACCEPT MOSTRA-ERRO
                       END-IF
                ELSE
                    MOVE 'REGISTRO NAO ENCONTRADO' TO WRK-MSGERRO
                    ACCEPT MOSTRA-ERRO

                END-IF.

            PERFORM 000-INICIO.



       0400-EXCLUSAO                                      SECTION.
           MOVE ' ' TO WRK-TECLA.
           MOVE 'EXCLUSAO ' TO  WRK-MODULO.
            DISPLAY TELA.
            DISPLAY TELA-REGISTRO.
            ACCEPT CHAVE.
               READ CLIENTES
               INVALID KEY
                       MOVE "CLIENTE N�O ENCONTRADO" TO WRK-MSGERRO
               NOT INVALID KEY

                       MOVE 'ENCONTRADO - EXCLUI (S/N) ? '
                            TO WRK-MSGERRO
                       DISPLAY DADOS
               END-READ
                     ACCEPT MOSTRA-ERRO
                     IF WRK-TECLA = 'S' AND CLIENTES-STATUS = 00
                            DELETE CLIENTES
                            INVALID KEY
                            MOVE 'NAO EXCLUIDO ' TO WRK-MSGERRO
                            ACCEPT MOSTRA-ERRO
                            END-DELETE.

            PERFORM 000-INICIO.

       0500-RELATORIO                                         SECTION.
            MOVE ' ' TO WRK-TECLA.
            MOVE 'RELATORIO ' TO  WRK-MODULO.
            MOVE 'APERTE ENTER PARA CONTINUAR ' TO WRK-MSGERRO
            DISPLAY TELA.
             OPEN INPUT RELATO.
               READ RELATO.
                 MOVE 2 TO WRK-LINHA

               PERFORM UNTIL RELATO-STATUS NOT EQUAL 0
                 DISPLAY RELATO-REG LINE WRK-LINHA COLUMN 01
                 READ RELATO
                  ADD 1 TO WRK-LINHA
               END-PERFORM.

                  ACCEPT MOSTRA-ERRO.

             CLOSE RELATO.
              PERFORM 000-INICIO.

       9000-ABRIR-ARQUIVOS                                    SECTION.
            OPEN I-O CLIENTES
            IF CLIENTES-STATUS = 35  THEN
               OPEN OUTPUT CLIENTES
               CLOSE CLIENTES
               OPEN I-O CLIENTES
            END-IF.

       9100-FECHAR-ARQUIVOS                                   SECTION.
            CLOSE RELATO.
            CLOSE CLIENTES.
