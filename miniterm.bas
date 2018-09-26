10    ! MiniTerm - A basic terminal client for the HP 9816
20    ! by  Davide Bucci (Twitter: @davbucci, Github: DarwinNE)
30    ! and Travis Rosenbaum (Twitter/GH: @Zagrophyte)
40    !
50    ! Repo: https://github.com/zagrophyte/hpbasic-miniterm
60    !
70    CLEAR SCREEN
80    CONTROL 9,12;128+32+16 ! Deactivate RS232 HW Control
90    Action=0
100   ON KBD GOSUB Keyhandler
110   Oa=Action
120   STATUS 1,0;Curx ! Capture current X
130   STATUS 1,1;Cury ! Capture current Y
140   DIM Disp$[80] ! Status Message
150   Addcr=1 ! Received CRLFs are processed
160   Echo=0 ! Sent chars are not echoed
170   GOSUB Disptermline
180   Choice$=""
190   Warning=0
200   ASSIGN @Bufin TO BUFFER [1000]
210   ASSIGN @Rs232in TO 9
220   ASSIGN @Rs232out TO 9
230   TRANSFER @Rs232in TO @Bufin;CONT
240   ASSIGN @Bufout TO BUFFER [1000]
250   TRANSFER @Bufout TO @Rs232out;CONT
260   OUTPUT @Bufout;" "
270 Termloop:   ! Main terminal loop
280   IF Oa<>Action THEN
290     GOTO Exittermloop
300   END IF
310   IF LEN(Choice$)>0 THEN
320     SELECT Choice$
330     CASE CHR$(255)&"E" ! Enter Key
340       Choice$=CHR$(13)
350     CASE CHR$(255)&"B" ! Backspace
360       Choice$=CHR$(8)
370     CASE CHR$(17) ! CTRL+Q
380       DISP ""
390       GOTO Exittermloop
400     CASE CHR$(4) ! CTRL+D
410       IF Addcr THEN
420         Addcr=0
430       ELSE
440         Addcr=1
450       END IF
460       GOSUB Disptermline
470     CASE CHR$(5) ! CTRL+E
480       IF Echo THEN
490         Echo=0
500       ELSE
510         Echo=1
520       END IF
530     CASE CHR$(255)&"U" ! Caps lock
540       STATUS 2,0;I
550       IF I THEN
560         CONTROL 2,0;0
570       ELSE
580         CONTROL 2,0;1
590       END IF
600       GOTO Termloop
610     CASE ELSE
620       IF Choice$[1,1]=CHR$(255) THEN
630         Choice$="?"
640         GOTO Termloop
650       END IF
660     END SELECT
670     IF Echo THEN
680       IF Choice$=CHR$(13) THEN
690         A$=CHR$(13)
700         GOSUB Printchar
710         A$=CHR$(10)
720         GOSUB Printchar
730       ELSE
740         A$=Choice$
750         GOSUB Printchar
760       END IF
770       A$=""
780     END IF
790     OUTPUT @Bufout;Choice$;
800     Choice$=""
810   END IF
820   STATUS @Bufin,4;I
830   IF I>0 THEN
840     IF I>900 THEN
850       DISP "WARNING: Receive buffer is more than 90% full!"
860       Warning=1
870     ELSE
880       IF Warning THEN
890         Warning=0
900         DISP ""
910       END IF
920     END IF
930     ENTER @Bufin USING "#,A";A$
940     GOSUB Printchar
950   END IF
960   GOTO Termloop
970 Printchar:  !
980   IF A$<>"" THEN
990     STATUS 1,0;Curx   ! We use STATUS/CONTROL to avoid extra CRLFs
1000    STATUS 1,1;Cury
1010    IF Curx>=80 THEN   ! Handle edge-of-term
1020      Curx=1
1030    END IF
1040    IF A$=CHR$(13) OR A$=CHR$(10) THEN   ! CRLF Handler
1050      IF A$=CHR$(13) AND Addcr THEN
1060        Cury=MIN(Cury+1,18)   ! Simulate LF
1070        IF Cury=18 THEN
1080          CONTROL 1,1;19   ! Set out of bounds to force LF scroll
1090          PRINT ! Force LF Scroll
1100        END IF
1110      ELSE
1120        IF A$=CHR$(10) THEN
1130          Curx=1  ! Simulate CR
1140          CONTROL 1,0;Curx ! Simulate CR
1150        END IF
1160      END IF
1170      A$=""
1180    ELSE
1190      Curx=Curx+LEN(A$)
1200      PRINT A$;
1210    END IF
1220    CONTROL 1,0;Curx   ! Ensure Cursor X in place
1230    CONTROL 1,1;Cury   ! Ensure Cursor Y in place
1240  END IF
1250 Disptermline:    !
1260  Disp$="MiniTerm: "
1270  IF Addcr THEN
1280    Disp$=Disp$&"^D: CR+LF ON"
1290  ELSE
1300    Disp$=Disp$&"^D: CR+LF OFF"
1310  END IF
1320  IF Echo THEN
1330    Disp$=Disp$&", ^E: Echo ON"
1340  ELSE
1350    Disp$=Disp$&", ^E: Echo OFF"
1360  END IF
1370  Disp$=Disp$&" [^Q to Quit]"
1380  DISP Disp$
1390  RETURN
1400 Exittermloop:  !
1410  ABORTIO @Rs232in
1420  ABORTIO @Rs232out
1430  ASSIGN @Rs232 TO *
1440  ASSIGN @Bufin TO *
1450  ASSIGN @Bufout TO *
1460  PRINT "Exited."
1470  STOP
1480 Keyhandler:  !
1490  Choice$=KBD$
1500  RETURN
1510  END
