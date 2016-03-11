!Alex Lapena
!0844071
! A PROGRAM TO PLAY TIC-TAC-TOE
      PROGRAM TICTACTOE
      
      CHARACTER * 1 TICTAC(3,3), WINNER
      LOGICAL::OVER 
      LOGICAL::CHKPLAY
      INTEGER::MOVE, TURN, I

      WRITE(*,*) "PLAY TIC-TAC-TOE. ENTER 1-9 TO PLAY"
      WRITE(*,*) " "
      WRITE(*,*) "        1 | 2 | 3 "
      WRITE(*,*) "       ---+---+---"
      WRITE(*,*) "        4 | 5 | 6 "
      WRITE(*,*) "       ---+---+---"
      WRITE(*,*) "        7 | 8 | 9 "
      WRITE(*,*) " "
            
      CALL BOARDSETUP(TICTAC)
      
      OVER = .FALSE.
      
      !Loops for each turn and the AI's turn
       DO WHILE(OVER .EQV. .FALSE.)
		  TURN = 0
		  WRITE(*,*) "Your move? "
		  READ(*,*) MOVE
		  IF (MOVE < 0 .OR. MOVE > 9)THEN
			WRITE(*,*) "Invalid input."
		  ELSE
			!Checks for the available move, then adds in the user's input into 
			!the tic tac toe board when it is printed.
			 IF (CHKPLAY(TICTAC,MOVE)) THEN
                IF (MOVE == 1)THEN
                   TICTAC(1,1) = "X"
                ELSE IF (MOVE == 2)THEN
                   TICTAC(1,2) = "X"
                ELSE IF (MOVE == 3)THEN
                   TICTAC(1,3) = "X"
                ELSE IF (MOVE == 4)THEN
                   TICTAC(2,1) = "X"
                ELSE IF (MOVE == 5)THEN
                   TICTAC(2,2) = "X"
                ELSE IF (MOVE == 6)THEN
                   TICTAC(2,3) = "X"
                ELSE IF (MOVE == 7)THEN
                   TICTAC(3,1) = "X"
                ELSE IF (MOVE == 8)THEN
                   TICTAC(3,2) = "X"
                ELSE IF (MOVE == 9)THEN
                   TICTAC(3,3) = "X"
                END IF
              ELSE
				WRITE(*,*) "Invalid move, box already occupied."
			 END IF
			
			!Winner check
			CALL CHKOVR(TICTAC,OVER,WINNER)
			IF (OVER)THEN
				WRITE(*,*) "The game is over!"
				IF (WINNER .EQ. "D") THEN
					WRITE(*,*) "The game is a draw. "
				ELSE
					WRITE(*,*) "The winner is: ", WINNER
				END IF
			END IF
			
			!AI call
			WRITE(*,*) "After my move..."
			CALL COMPMOVE(TICTAC)
			CALL CHKOVR(TICTAC,OVER,WINNER)
			DO I = 1 , 3
				WRITE(*,2) (TICTAC(I,J), J=1,3)
				2 FORMAT(2X,A1,1X,"|",1X,A1,1X,"|",1X,A1,1X)
				IF (I /= 3)THEN
					WRITE(*,*) "---+---+---"
				END IF
			END DO
			!win check
			IF (OVER)THEN
				WRITE(*,*) "The game is over!"
				IF (WINNER .EQ. "D") THEN
					WRITE(*,*) "The game is a draw. "
				ELSE
					WRITE(*,*) "The winner is: ", WINNER
				END IF
			END IF
		END IF
	  END DO
 
END
      
! SUBROUTINE TO CHECK TO SEE IF THE GAME IS OVER      
! =========================================
      SUBROUTINE CHKOVR(TICTAC,OVER,WINNER)
      CHARACTER * 1 TICTAC(3,3), WINNER
      LOGICAL OVER
      
      CHARACTER * 1 BLANK, DRAW
      PARAMETER (BLANK = ' ', DRAW = 'D')

      LOGICAL SAME
      LOGICAL DSAME
      INTEGER IR, IC

! ASSUME GAME IS OVER AT START
      OVER = .TRUE.
!
! CHECK FOR A WINNER
! CHECK ROWS FOR A WINNER
      DO IR = 1, 3
		IF (SAME(TICTAC(IR,1),TICTAC(IR,2),TICTAC(IR,3))) THEN
		WINNER = TICTAC(IR,1)
		RETURN
		END IF
  END DO
! NO WINNER BY ROWS, CHECK COLUMNS FOR A WINNER
      DO IC = 1, 3
		IF (SAME(TICTAC(1,IC),TICTAC(2,IC),TICTAC(3,IC))) THEN
			WINNER = TICTAC(1,IC)
			RETURN
		END IF
  END DO 
! NO WINNER BY ROWS OR COLUMNS, CHECK DIAGONALS
      DSAME = SAME(TICTAC(1,1),TICTAC(2,2),TICTAC(3,3)) .OR. SAME(TICTAC(1,3),TICTAC(2,2),TICTAC(3,1)) 
      IF (DSAME) THEN
		WINNER = TICTAC(2,2)
		RETURN
      END IF
! NO WINNER AT ALL. SEE IF GAME IS A DRAW
! CHECK EACH ROW FOR AN EMPTY SPACE
      DO  IR = 1,3
		DO IC = 1,3
			IF (TICTAC(IR,IC) .EQ. BLANK) THEN
				OVER = .FALSE.
				RETURN
			END IF
		END DO
    END DO
! 
! NO BLANK FOUND, GAME IS A DRAW
      WINNER = DRAW

      RETURN    
      END
      
! SUBROUTINE TO PLAY FOR THE COMPUTER  
! =========================================
      SUBROUTINE COMPMOVE(TICTAC)
      CHARACTER * 1 TICTAC(3,3)
      INTEGER PATHS(3,8), PATHSUM(8)
      DATA PATHS/1,2,3,4,5,6,7,8,9,1,4,7,2,5,8,3,6,9,1,5,9,3,5,7/
      INTEGER:: BOARD(9,2), K, X, Y, RANDPOS
      DATA BOARD/1,1,1,2,2,2,3,3,3,1,2,3,1,2,3,1,2,3/
      
      DO I = 1,8
		PATHSUM(I) = 0
		DO J = 1,3
			X = BOARD(PATHS(J,I),1)
			Y = BOARD(PATHS(J,I),2)
		  IF (TICTAC(X,Y) == " ")THEN
			K = 0
		  ELSE IF (TICTAC(X,Y) == "X")THEN
			K = 1
		  ELSE IF (TICTAC(X,Y) == "O")THEN
			K = 4
		  END IF 
		  PATHSUM(I) = PATHSUM(I) + K     
		  END DO
	  END DO

!     OFFENSIVE CODE TO DEAL WITH SCENARIOS WHERE THE
!     COMPUTER HAS TWO IN A PATH
      DO I = 1,8
		IF (PATHSUM(I) .EQ. 8) THEN
			DO J = 1,3
				X = BOARD(PATHS(J,I),1)
				Y = BOARD(PATHS(J,I),2)
				IF (TICTAC(X,Y) == " ") THEN
					TICTAC(X,Y) = "O"
					RETURN
				END IF
    		END DO
		END IF
      END DO
  
!     DEFENSIVE CODE TO DEAL WITH SCENARIOS WHERE THE
!     OPPONENT HAS TWO IN A PATH
      DO I = 1,8
		IF (PATHSUM(I) .EQ. 2) THEN
		DO J = 1,3
			X = BOARD(PATHS(J,I),1)
			Y = BOARD(PATHS(J,I),2)
		IF (TICTAC(X,Y) == " ") THEN
			TICTAC(X,Y) = "O"
			RETURN
		END IF
		END DO
      END IF
     END DO
  
  DO I = 1,8
	RANDPOS = INT(RAND(0)*9)+1
		X = BOARD(RANDPOS,1)
		Y = BOARD(RANDPOS,2)
      IF (TICTAC(X,Y) == " ") THEN
          TICTAC(X,Y) = "O"
          RETURN
      END IF
  END DO
  
  RETURN    
END

! FUNCTION TO CHECK TO SEE IF THREE ELEMENTS IN A ROW, COLUMN OR DIAGONAL
! ARE THE SAME           
! =========================================
      LOGICAL FUNCTION SAME(T1,T2,T3)
      CHARACTER:: T1,T2,T3
      
      IF (T1 == "X" .AND. T2 == "X" .AND. T3 == "X")THEN
       SAME = .TRUE.     
      ELSE IF(T1 == "O" .AND. T2 == "O" .AND. T3 == "O")THEN  
		SAME = .TRUE.
	  ELSE
		SAME = .FALSE.
	  END IF
	  
      END
  
! SUBROUTINE TO SET UP THE TIC-TAC-TOE BOARD  
! =========================================  
      SUBROUTINE BOARDSETUP(TICTAC)
      CHARACTER * 1 TICTAC(3,3)

      DO I = 1,3
		DO J = 1,3
			TICTAC(I,J) = " "
		 END DO
	    END DO
      RETURN
      END

! SUBROUTINE TO CHECK HUMAN PLAY  
! ========================================= 
      LOGICAL FUNCTION CHKPLAY(TICTAC,MOVE) 
      CHARACTER * 1 TICTAC(3,3)
      INTEGER::MOVE
                
      IF(MOVE == 1 .AND. (TICTAC(1,1) == " "))then
		CHKPLAY = .TRUE.
	ELSE IF(MOVE == 2 .AND. (TICTAC(1,2) == " "))then		
		CHKPLAY = .TRUE.
	ELSE IF(MOVE == 3 .AND. (TICTAC(1,3) == " "))then
		CHKPLAY = .TRUE.
	ELSE IF(MOVE == 4 .AND. (TICTAC(2,1) == " "))then
		CHKPLAY = .TRUE.
	ELSE IF(MOVE == 5 .AND. (TICTAC(2,2) == " "))then
		CHKPLAY = .TRUE.
	ELSE IF(MOVE == 6 .AND. (TICTAC(2,3) == " "))then
		CHKPLAY = .TRUE.
	ELSE IF(MOVE == 7 .AND. (TICTAC(3,1) == " "))then
		CHKPLAY = .TRUE.
	ELSE IF(MOVE == 8 .AND. (TICTAC(3,2) == " "))then
		CHKPLAY = .TRUE.
	ELSE IF(MOVE == 9 .AND. (TICTAC(3,3) == " "))then
		CHKPLAY = .TRUE.
	ELSE
		CHKPLAY = .FALSE.
	END IF          
                
END
