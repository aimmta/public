      PROGRAM BLACKJACK
      INTEGER DECK(52), PLHAND(12), DLHAND(12)
      INTEGER PLSCORE, DLSCORE, DCARD, PCARD, DECKPOS
      LOGICAL PLBUST, DLBUST
      CHARACTER CHOICE

C     Initialize deck (1-13 repeated 4 times)
      DO 10 I = 1, 52
        DECK(I) = MOD(I-1,13) + 1
10    CONTINUE

C     Shuffle deck using Fisher-Yates algorithm
      CALL SRAND(TIME())
      DO 20 I = 52, 1, -1
        J = INT(RAND() * I) + 1
        TEMP = DECK(I)
        DECK(I) = DECK(J)
        DECK(J) = TEMP
20    CONTINUE

C     Initialize hands and scores
      DECKPOS = 1
      PLSCORE = 0
      DLSCORE = 0
      PLBUST = .FALSE.
      DLBUST = .FALSE.

C     Deal initial cards
      PLHAND(1) = DECK(DECKPOS)
      DECKPOS = DECKPOS + 1
      DLHAND(1) = DECK(DECKPOS)
      DECKPOS = DECKPOS + 1
      PLHAND(2) = DECK(DECKPOS)
      DECKPOS = DECKPOS + 1
      DLHAND(2) = DECK(DECKPOS)
      DECKPOS = DECKPOS + 1
      PCARD = 2
      DCARD = 2

C     Player's turn
      PRINT *, 'Player''s Hand: ', PLHAND(1), PLHAND(2)
      PLSCORE = CALCSCORE(PLHAND, PCARD)
      PRINT *, 'Current Score: ', PLSCORE

30    CONTINUE
      PRINT *, 'Hit (H) or Stand (S)?'
      READ(*, '(A)') CHOICE

      IF (CHOICE .EQ. 'H') THEN
        PCARD = PCARD + 1
        PLHAND(PCARD) = DECK(DECKPOS)
        DECKPOS = DECKPOS + 1
        PLSCORE = CALCSCORE(PLHAND, PCARD)
        PRINT *, 'New Card: ', PLHAND(PCARD)
        PRINT *, 'Total Score: ', PLSCORE

        IF (PLSCORE .GT. 21) THEN
          PRINT *, 'Bust! You lose.'
          PLBUST = .TRUE.
          GOTO 40
        ENDIF
        GOTO 30
      ENDIF

C     Dealer's turn
40    IF (.NOT. PLBUST) THEN
        PRINT *, 'Dealer''s Hand: ', DLHAND(1), DLHAND(2)
        DLSCORE = CALCSCORE(DLHAND, DCARD)
        DO WHILE (DLSCORE .LT. 17)
          DCARD = DCARD + 1
          DLHAND(DCARD) = DECK(DECKPOS)
          DECKPOS = DECKPOS + 1
          DLSCORE = CALCSCORE(DLHAND, DCARD)
          PRINT *, 'Dealer draws: ', DLHAND(DCARD)
        ENDDO

        IF (DLSCORE .GT. 21) THEN
          PRINT *, 'Dealer busts! You win.'
          DLBUST = .TRUE.
        ELSE
          PRINT *, 'Dealer''s Score: ', DLSCORE
        ENDIF
      ENDIF

C     Determine winner
      IF (PLBUST) THEN
        PRINT *, 'Player busts - Dealer wins!'
      ELSE IF (DLBUST) THEN
        PRINT *, 'Dealer busts - Player wins!'
      ELSE IF (PLSCORE .GT. DLSCORE) THEN
        PRINT *, 'Player wins!'
      ELSE IF (DLSCORE .GT. PLSCORE) THEN
        PRINT *, 'Dealer wins!'
      ELSE
        PRINT *, 'Push (Tie)!'
      ENDIF

      END

      INTEGER FUNCTION CALCSCORE(HAND, NCARD)
      INTEGER HAND(NCARD), NCARD, I, VALUE, ACES
      CALCSCORE = 0
      ACES = 0

      DO 50 I = 1, NCARD
        VALUE = HAND(I)
        IF (VALUE .EQ. 1) THEN
          ACES = ACES + 1
          CALCSCORE = CALCSCORE + 11
        ELSE IF (VALUE .GE. 10) THEN
          CALCSCORE = CALCSCORE + 10
        ELSE
          CALCSCORE = CALCSCORE + VALUE
        ENDIF
50    CONTINUE

C     Handle aces
      DO WHILE (CALCSCORE .GT. 21 .AND. ACES .GT. 0)
        CALCSCORE = CALCSCORE - 10
        ACES = ACES - 1
      ENDDO

      RETURN
      END
