      ******************************************************************
      * Author: OSCAR
      * Date: 11/05/2020
      * Purpose: TRABALHO DE LP
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. INTERNET-BANKING.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
       01  WS-SALD.
           05  WS-SALDO1 PIC 9(4)V9(2) VALUE 1025.45.
           05  WS-SALDO2 PIC 9(3)V9(2).
           05  WS-SALDO3 PIC 9(2)V9(2).
           05  WS-SALDO4 PIC 9(1)V9(2).
       01  WS-TRANSFER PIC 9(4)V9(2).
       01  WS-OPT PIC 9(2).
       01  WS-CONTA PIC 9(4)V9(2).
       01  WS-INV.
           05  WS-COUNT PIC 9(1) VALUE 2.
           05  WS-INVEST PIC 9(2).
           05  WS-SALDOIN PIC 9(4)V9(2) VALUE 0.
           05  WS-SALDOINF PIC 9(5)V9(2) VALUE 0.
           05  WS-SALDOPINF PIC 9(1)V9(2) VALUE 0.
           05  WS-GANHO PIC 9(2)V9(2) VALUE 0.
           05  WS-SELIC PIC 9(1)V9(4) VALUE 0.0038.
           05  WS-CDI PIC 9(1)V9(4) VALUE 0.0038.
           05  WS-POUP PIC 9(1)V9(4) VALUE 0.0050.
       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           A-PARA.
           DISPLAY "*************************".
           DISPLAY "*  INTERNET BANKING LP  *".
           DISPLAY "*************************".
           DISPLAY "*                       *".
           DISPLAY "*  1 -  CONSULTA SALDO  *".
           DISPLAY "*  2 -  TRANSFERIR      *".
           DISPLAY "*  3 -  PAGAMENTO       *".
           DISPLAY "*  4 -  INVESTIR        *".
           DISPLAY "*  0 -  EXIT            *".
           DISPLAY "*************************".
           DISPLAY " ".
           DISPLAY "QUAL OPCAO DESEJA OPERAR?".
           ACCEPT WS-OPT.
           IF WS-OPT<0 OR WS-OPT>4 THEN
               DISPLAY "OPCAO INVALIDA!"
               DISPLAY " "
               GO TO A-PARA
           END-IF
           GO TO B-PARA C-PARA D-PARA E-PARA DEPENDING ON WS-OPT.
           STOP RUN.

           B-PARA.
           IF WS-SALDO1>=1000 THEN
              DISPLAY "SEU SALDO: R$" WS-SALDO1
           END-IF
           IF WS-SALDO1<1000 AND WS-SALDO1>=100 THEN
              MOVE WS-SALDO1 TO WS-SALDO2
              DISPLAY "SEU SALDO: R$" WS-SALDO2
           END-IF
           IF WS-SALDO1<100 AND WS-SALDO1>=10 THEN
              MOVE WS-SALDO1 TO WS-SALDO3
              DISPLAY "SEU SALDO: R$" WS-SALDO3
           END-IF
           IF WS-SALDO1<10 THEN
              MOVE WS-SALDO1 TO WS-SALDO4
              DISPLAY "SEU SALDO: R$" WS-SALDO4
           END-IF
           DISPLAY " "
           GO TO A-PARA.

           C-PARA.
           DISPLAY "QUANTO DESEJA TRANFERIR? "
           ACCEPT WS-TRANSFER
           IF WS-SALDO1>WS-TRANSFER THEN
              SUBTRACT WS-TRANSFER FROM WS-SALDO1
              IF WS-SALDO1>=1000 THEN
                  DISPLAY "TRANSFERENCIA REALIZADA!"
                  DISPLAY "SEU NOVO SALDO: R$" WS-SALDO1
              END-IF
              IF WS-SALDO1<1000 AND WS-SALDO1>=100 THEN
                  MOVE WS-SALDO1 TO WS-SALDO2
                  DISPLAY "TRANSFERENCIA REALIZADA!"
                  DISPLAY "SEU NOVO SALDO: R$" WS-SALDO2
              END-IF
              IF WS-SALDO1<100 AND WS-SALDO1>=10 THEN
                  MOVE WS-SALDO1 TO WS-SALDO3
                  DISPLAY "TRANSFERENCIA REALIZADA!"
                  DISPLAY "SEU NOVO SALDO: R$" WS-SALDO3
              END-IF
              IF WS-SALDO1<10 THEN
                  MOVE WS-SALDO1 TO WS-SALDO4
                  DISPLAY "TRANSFERENCIA REALIZADA!"
                  DISPLAY "SEU NOVO SALDO: R$" WS-SALDO4
              END-IF
           ELSE
              DISPLAY "NAO EXISTE SALDO PARA ESTA TRANSFERENCIA!"
           END-IF
           DISPLAY " "
           GO TO A-PARA.

           D-PARA.
           DISPLAY "DIGITE O VALOR DA CONTA: "
           ACCEPT WS-CONTA
           IF WS-SALDO1>WS-CONTA THEN
               SUBTRACT WS-CONTA FROM WS-SALDO1
               IF WS-SALDO1>=1000 THEN
                  DISPLAY "PAGAMENTO REALIZADO!"
                  DISPLAY "SEU NOVO SALDO: R$" WS-SALDO1
               END-IF
               IF WS-SALDO1<1000 AND WS-SALDO1>=100 THEN
                  MOVE WS-SALDO1 TO WS-SALDO2
                  DISPLAY "PAGAMENTO REALIZADO!"
                  DISPLAY "SEU NOVO SALDO: R$" WS-SALDO2
               END-IF
               IF WS-SALDO1<100 AND WS-SALDO1>=10 THEN
                  MOVE WS-SALDO1 TO WS-SALDO3
                  DISPLAY "PAGAMENTO REALIZADO!"
                  DISPLAY "SEU NOVO SALDO: R$" WS-SALDO3
               END-IF
               IF WS-SALDO1<10 THEN
                  MOVE WS-SALDO1 TO WS-SALDO4
                  DISPLAY "PAGAMENTO REALIZADO!"
                  DISPLAY "SEU NOVO SALDO: R$" WS-SALDO4
               END-IF
           ELSE
               DISPLAY "NAO EXISTE SALDO PARA ESTE PAGAMENTO!"
           END-IF
           DISPLAY " "
           GO TO A-PARA.

           E-PARA.
           DISPLAY "QUAL INVESTIMENTO DESEJA REALIZAR?"
           DISPLAY "1 - SELIC (0.38%/M)"
           DISPLAY "2 - CDI (0.38%/M)"
           DISPLAY "3 - POUPANCA (0.50%/M)"
           ACCEPT WS-INVEST
           DISPLAY " "
           IF WS-INVEST=1 THEN
              MOVE WS-SALDO1 TO WS-SALDOIN
              MOVE WS-SALDOIN TO WS-SALDOINF
              DISPLAY "PROJECAO PARA OS PROXIMOS 6 MESES: "
              MULTIPLY WS-SELIC BY WS-SALDOINF
              MOVE WS-SALDOINF TO WS-SALDOPINF
              DISPLAY "MES 1: R$" WS-SALDOPINF
              PERFORM UNTIL WS-COUNT=7
                 ADD WS-SALDOINF TO WS-SALDOIN
                 MOVE WS-SALDOIN TO WS-SALDOINF
                 MULTIPLY WS-SELIC BY WS-SALDOINF
                 MOVE WS-SALDOINF TO WS-SALDOPINF
                 DISPLAY "MES " WS-COUNT ": R$" WS-SALDOPINF
                 ADD 1 TO WS-COUNT
              END-PERFORM
              DISPLAY "SALDO FINAL APOS 6 MESES: R$" WS-SALDOIN
              SUBTRACT WS-SALDO1 FROM WS-SALDOIN GIVING WS-GANHO
              DISPLAY "VALOR GANHO APOS 6 MESES: R$" WS-GANHO
              SUBTRACT 5 FROM WS-COUNT
              DISPLAY " "
           END-IF
           IF WS-INVEST=2 THEN
              MOVE WS-SALDO1 TO WS-SALDOIN
              MOVE WS-SALDOIN TO WS-SALDOINF
              DISPLAY "PROJECAO PARA OS PROXIMOS 6 MESES: "
              MULTIPLY WS-CDI BY WS-SALDOINF
              MOVE WS-SALDOINF TO WS-SALDOPINF
              DISPLAY "MES 1: R$" WS-SALDOPINF
              PERFORM UNTIL WS-COUNT=7
                 ADD WS-SALDOINF TO WS-SALDOIN
                 MOVE WS-SALDOIN TO WS-SALDOINF
                 MULTIPLY WS-CDI BY WS-SALDOINF
                 MOVE WS-SALDOINF TO WS-SALDOPINF
                 DISPLAY "MES " WS-COUNT ": R$" WS-SALDOPINF
                 ADD 1 TO WS-COUNT
              END-PERFORM
              DISPLAY "SALDO FINAL APOS 6 MESES: R$" WS-SALDOIN
              SUBTRACT WS-SALDO1 FROM WS-SALDOIN GIVING WS-GANHO
              DISPLAY "VALOR GANHO APOS 6 MESES: R$" WS-GANHO
              SUBTRACT 5 FROM WS-COUNT
              DISPLAY " "
           END-IF
           IF WS-INVEST=3 THEN
              MOVE WS-SALDO1 TO WS-SALDOIN
              MOVE WS-SALDOIN TO WS-SALDOINF
              DISPLAY "PROJECAO PARA OS PROXIMOS 6 MESES: "
              MULTIPLY WS-POUP BY WS-SALDOINF
              MOVE WS-SALDOINF TO WS-SALDOPINF
              DISPLAY "MES 1: R$" WS-SALDOPINF
              PERFORM UNTIL WS-COUNT=7
                 ADD WS-SALDOINF TO WS-SALDOIN
                 MOVE WS-SALDOIN TO WS-SALDOINF
                 MULTIPLY WS-POUP BY WS-SALDOINF
                 MOVE WS-SALDOINF TO WS-SALDOPINF
                 DISPLAY "MES " WS-COUNT ": R$" WS-SALDOPINF
                 ADD 1 TO WS-COUNT
              END-PERFORM
              DISPLAY "SALDO FINAL APOS 6 MESES: R$" WS-SALDOIN
              SUBTRACT WS-SALDO1 FROM WS-SALDOIN GIVING WS-GANHO
              DISPLAY "VALOR GANHO APOS 6 MESES: R$" WS-GANHO
              SUBTRACT 5 FROM WS-COUNT
              DISPLAY " "
           END-IF
           GO TO A-PARA.

       END PROGRAM INTERNET-BANKING.
