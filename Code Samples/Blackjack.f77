      PROGRAM BlackjackGame
      IMPLICIT NONE

      ! Declare all variables and function types
      INTEGER :: deck(52), i, j, temp, cardIndex
      REAL :: randomNum
      INTEGER :: playerTotal, dealerTotal
      INTEGER :: playerHand(11), dealerHand(11), cardValue
      CHARACTER*8 cardValues(13), cardSuits(4)
      CHARACTER*20 shuffledDeck(52)
      CHARACTER*1 choice, playAgain
      LOGICAL :: aceInHand
      REAL :: bankroll, bet

      ! Explicitly declare the GetCardValue function as returning an INTEGER
      INTEGER :: GetCardValue

      ! Initialize card values (A, 2, ..., K)
      DATA cardValues / 'A', '2', '3', '4', '5', '6', '7', '8', '9', '10', 'J', 'Q', 'K' /
      
      ! Initialize card suits (Clubs, Diamonds, Hearts, Spades)
      DATA cardSuits / 'Clubs', 'Diamonds', 'Hearts', 'Spades' /

      ! Start game with an initial bankroll
      bankroll = 100.0
      PRINT '(A, F10.2)', 'Welcome to Blackjack! You start with $', bankroll

      ! Outer DO-WHILE loop to play again
      playAgain = 'y'
      DO WHILE (playAgain .EQ. 'y' .AND. bankroll > 0)

         ! Player places a bet
         DO
            PRINT '(A, F10.2)', 'You have $', bankroll, '. How much would you like to bet?'
            READ *, bet
            IF (bet <= bankroll .AND. bet > 0) EXIT
            PRINT *, 'Invalid bet. Please enter a valid amount.'
         END DO

         ! Initialize the deck (1 to 52)
         DO i = 1, 52
            deck(i) = i
         END DO

         ! Shuffle the deck
         CALL ShuffleDeck(deck)

         ! Start the Blackjack game
         PRINT *, 'Dealing cards...'

         ! Deal two cards to the player
         cardIndex = 1
         playerTotal = 0
         aceInHand = .FALSE.
         PRINT *, 'Your cards:'
         DO i = 1, 2
            CALL DrawCard(deck, cardIndex, playerHand(i), aceInHand, cardValues, cardSuits)
            playerTotal = playerTotal + GetCardValue(playerHand(i))
            cardIndex = cardIndex + 1
         END DO
         CALL AdjustForAce(playerTotal, aceInHand)
         PRINT *, 'Your total is ', playerTotal

         ! Player's turn: Hit or Stand
         DO
            PRINT *, 'Do you want to hit or stand? (h/s)'
            READ *, choice
            IF (choice .EQ. 's') EXIT

            ! Draw a card if the player hits
            CALL DrawCard(deck, cardIndex, playerHand(i), aceInHand, cardValues, cardSuits)
            playerTotal = playerTotal + GetCardValue(playerHand(i))
            cardIndex = cardIndex + 1
            CALL AdjustForAce(playerTotal, aceInHand)
            PRINT *, 'Your total is ', playerTotal

            IF (playerTotal > 21) THEN
               PRINT *, 'You bust! Dealer wins.'
               bankroll = bankroll - bet
               EXIT
            END IF
         END DO

         ! Dealer's turn if the player did not bust
         IF (playerTotal <= 21) THEN
            dealerTotal = 0
            aceInHand = .FALSE.
            PRINT *, 'Dealer''s cards:'
            DO i = 1, 2
               CALL DrawCard(deck, cardIndex, dealerHand(i), aceInHand, cardValues, cardSuits)
               dealerTotal = dealerTotal + GetCardValue(dealerHand(i))
               cardIndex = cardIndex + 1
            END DO
            CALL AdjustForAce(dealerTotal, aceInHand)
            PRINT *, 'Dealer''s total is ', dealerTotal

            ! Dealer must hit until 17 or more
            DO WHILE (dealerTotal < 17)
               CALL DrawCard(deck, cardIndex, dealerHand(i), aceInHand, cardValues, cardSuits)
               dealerTotal = dealerTotal + GetCardValue(dealerHand(i))
               cardIndex = cardIndex + 1
               CALL AdjustForAce(dealerTotal, aceInHand)
               PRINT *, 'Dealer''s total is ', dealerTotal
            END DO

            ! Determine the winner
            IF (dealerTotal > 21) THEN
               PRINT *, 'Dealer busts! You win.'
               bankroll = bankroll + bet
            ELSE IF (playerTotal > dealerTotal) THEN
               PRINT *, 'You win with ', playerTotal, ' points!'
               bankroll = bankroll + bet
            ELSE IF (playerTotal .EQ. dealerTotal) THEN
               PRINT *, 'It''s a tie! Your bet is returned.'
            ELSE
               PRINT *, 'Dealer wins with ', dealerTotal, ' points.'
               bankroll = bankroll - bet
            END IF
         END IF

         ! Ask if the player wants to play again, provided they still have money
         IF (bankroll > 0) THEN
            PRINT '(A, F10.2)', 'You now have $', bankroll, '. Do you want to play again? (y/n)'
            READ *, playAgain
         ELSE
            PRINT *, 'You''re out of money! Game over.'
            playAgain = 'n'
         END IF

      END DO

      PRINT '(A, F10.2)', 'Thanks for playing! You ended with $', bankroll, '. Goodbye.'

      END

      SUBROUTINE ShuffleDeck(deck)
      INTEGER :: deck(52), i, j, temp
      REAL :: randomNum

      ! Fisher-Yates shuffle algorithm
      DO i = 52, 2, -1
         CALL RANDOM_NUMBER(randomNum)
         j = INT(randomNum * i) + 1  ! Generate random index between 1 and i

         ! Swap deck(i) with deck(j)
         temp = deck(i)
         deck(i) = deck(j)
         deck(j) = temp
      END DO
      END

      SUBROUTINE DrawCard(deck, cardIndex, card, aceInHand, cardValues, cardSuits)
      INTEGER :: deck(52), cardIndex, card
      LOGICAL :: aceInHand
      INTEGER :: j, temp
      CHARACTER*8 cardValues(13), cardSuits(4)

      card = deck(cardIndex)
      j = MOD(card - 1, 13) + 1   ! Card value index (1 to 13)
      temp = (card - 1) / 13 + 1  ! Suit index (1 to 4)
      PRINT *, cardValues(j), ' of ', cardSuits(temp)
      IF (j == 1) THEN
         aceInHand = .TRUE.
      END IF
      END

      INTEGER FUNCTION GetCardValue(card)
      INTEGER :: card, value  ! Declare integers at the start of the function

      value = MOD(card - 1, 13) + 1
      IF (value > 10) THEN
         GetCardValue = 10
      ELSE IF (value == 1) THEN
         GetCardValue = 11  ! Initially count Ace as 11
      ELSE
         GetCardValue = value
      END IF
      END

      SUBROUTINE AdjustForAce(total, aceInHand)
      INTEGER :: total
      LOGICAL :: aceInHand

      IF (aceInHand .AND. total > 21) THEN
         total = total - 10
         aceInHand = .FALSE.  ! Only adjust for one ace
      END IF
      END
