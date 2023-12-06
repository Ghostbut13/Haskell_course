module BlackJack where
import Cards
import RunGame
import Test.QuickCheck
import System.Random
--set -Wincomplete-patterns  

--A0
hand2 :: Hand
hand2 = Add (Card (Numeric 2) Hearts)
            (Add (Card Jack Spades) Empty)

sizeSteps :: [Integer]
sizeSteps = [ size hand2
            , size (Add (Card (Numeric 2) Hearts)
                        (Add (Card Jack Spades) Empty))
            -- ... add the remaining steps here
            , 1 + size (Add (Card Jack Spades) Empty)
            , 1 + 1 + size Empty
            , 1 + 1 + 0
            ,2]


--A1
-- | show the one card's figure(1,2,..,10,J,Q,K) 
--   and pattern (♥ ♠ ♦ ♣) 
displayCard :: Card -> String
displayCard card = show (rank card) ++ " of " 
                   ++ show (suit card) ++ "\n"

-- | show all cards figures and pattern in hand (recursion way)
display :: Hand -> String
display Empty = []
display (Add card hand) = displayCard card
                           ++ display hand



--A2
-- | this fuction is first time computing : Ace = 11         
valueRank11 :: Rank -> Integer
valueRank11 (Numeric int) = int
valueRank11 Ace = 11
valueRank11 _ = 10
-- | this fuction is second time computing : Ace = 1 
valueRank1 :: Rank -> Integer
valueRank1 (Numeric int) = int
valueRank1 Ace = 1
valueRank1 _ = 10

-- | we use "initialValue" function to calculate the 
--   initial values of all cards: that is , Ace = 11
initialValue :: Hand -> Integer
initialValue Empty = 0
initialValue (Add card hand) = valueRank11 (rank card) +
                               initialValue hand

-- | at same time, we also calculate the values of all cards 
--  when : Ace = 1  by "numberofAces" function
numberofAces :: Hand -> Integer
numberofAces Empty = 0
numberofAces (Add card hand) = valueRank1 (rank card) +
                               numberofAces hand


-- | finally, we pick one result out of "initialValue" and "numberofAces"
--   as the final result, accroding to different condition
-- if initialValue's output is larger than 21, 
--      we pick numberofAces's value
-- else, we choose initialValue as the final values of all cards in hand
value :: Hand -> Integer
value Empty = 0
value (Add card hand) 
  | initialValue (Add card hand) <= 21  = initialValue (Add card hand)
  | otherwise                           = numberofAces (Add card hand)


--A3
-- if the final values of hand is larger than 21, 
-- according to rule, it will "gameover", so we will output "True"
gameOver :: Hand -> Bool
gameOver Empty = False
gameOver (Add card hand) 
  | value (Add card hand) > 21 = True
  | otherwise = False


--A4
winner :: Hand -> Hand -> Player
winner guest bank 
 | not (gameOver guest) && not (gameOver bank) && --if neither is "gameover"
  (value guest > value bank) = Guest    --guest > bank => guest win!
 | not (gameOver guest) &&   -- if guest not gameover but bank gameover
   gameOver bank = Guest   -- guest win
 | otherwise = Bank     -- other cases: bank win


------------test function
--test function for A1
-- 'lines' function can split the string by \n
-- calculate the elements (cards) number
-- compared with the countCards
prop_displayLength :: Hand -> Bool
prop_displayLength hand = length (lines (display hand)) == countCards hand
  where
    countCards Empty = 0
    countCards (Add _ rest) = 1 + countCards rest


--test function for A2
-- check where the final value source comes from
prop_valueCheck :: Hand -> Bool
prop_valueCheck hand = (initialValue hand <= 21 && 
                        value hand == initialValue hand) 
                    || (initialValue hand > 21 && 
                        value hand == numberofAces hand)

--test function for A3
prop_gameOver :: Hand -> Bool
prop_gameOver hand = (gameOver hand && value hand > 21) 
                    || (not (gameOver hand) && value hand <= 21)

--test function for A4
prop_winner :: Hand -> Hand -> Bool
prop_winner guest bank
  | gameOver guest           = winner guest bank == Bank
  | gameOver bank            = winner guest bank == Guest
  | value guest > value bank = winner guest bank == Guest
  | otherwise                = winner guest bank == Bank














-------------------------------------------
-------------------------------------------
-------------------------------------------
-- B1
(<+) :: Hand -> Hand -> Hand
(<+) handA Empty = handA
(<+) Empty handB = handB
(<+) (Add cardA handA) (Add cardB handB) = 
  Add cardA $ (<+) handA (Add cardB handB)

prop_onTopOf_assoc :: Hand -> Hand -> Hand -> Bool 
prop_onTopOf_assoc p1 p2 p3 = p1<+(p2<+p3) == (p1<+p2)<+p3


-- B2
-- | the local function f can genereate all combinations of 
--   ranks and suits into cards
--   The funcion full is for adding all cards into deck by foldr,
--   the base case is Empty, and construction fuction is Add
fullDeck :: Hand
fullDeck = full $ f rankFull suitFull
  where 
    full          = foldr Add Empty 
    f ranks suits = [Card a b | b <- suits, a <- ranks]
    rankFull      = [Numeric a | a <- [2..10]] ++ [Jack,Queen,King,Ace]
    suitFull      = [Hearts,Spades,Diamonds,Clubs]    


--B3
-- | draw function will draw the card on top of deck
-- and return the (new deck, new hand with drawn card)
draw :: Hand -> Hand -> (Hand,Hand)
draw Empty _   = error "draw: The deck is empty." 
draw (Add card deck) hand = (deck, Add card hand)


--B4
-- playBank :: Hand -> Hand
-- when value > 16, return the all value of cards
-- otherwise , keep playing
playBank :: Hand -> Hand
playBank deck = f deck Empty
  where 
    f (Add card deck) hand 
     | value hand <16 = f deck $ snd $ draw (Add card deck) hand
     | otherwise = hand
    f Empty hand 
     | value hand <16 = error "draw: The deck is empty."
     | otherwise = hand




--B5
-- | in B5, we implement 3 functions to construct the shuffleDeck function
-- they are "drawAny", "handUpDrawn", "shuffleHelp"
--
-- "drawAny" is for randomly drawing one card from deck.
-- the argument "a" is the index from 0 to size of deck, starting from top
-- of card. That is, to draw the first card (top) of deck --> drawAny 0 deck
--                   to draw the second card of deck --> drawAny 1 deck
--                   ....
--                   to draw the nth card of deck --> drawAny (nth-1) deck
-- also, "drawAny" can document the deck beneath the card drawn
-- which means, if we drew the 8th card from deck whose size is 10,
-- the "drawAny" would document the deck composed with 9th and 10th card

drawAny :: (Show t, Eq t, Ord t, Num t) => t -> Hand -> (Card, Hand)
drawAny a Empty = error "You can not draw card from zero"
drawAny a (Add card hand) 
 | a > size hand = error(show a ++ " " ++ "You can not draw out of range")
 | a > 0         = drawAny (a-1) hand
 | otherwise     = (card,hand)    -- until the a == 0, 
                                  -- output the (card_drawn, deck_beneath)

-- "handUpDrawn" function is for documenting the deck on the card drawn
-- that is, if we drew the 8th card from deck whose size is 10,
-- it would document the deck composed with 1st, 2nd ,...,7th card
handUpDrawn :: (Eq t, Ord t, Num t) => t -> Hand -> Hand
handUpDrawn a Empty = Empty 
handUpDrawn a (Add card hand) 
 | a==0      = Empty
 | otherwise = Add card (handUpDrawn (a-1) hand)

-- "shuffleHelp" is an important recursive function.
-- input : (Int,Gen), deck, hand; output : hand
-- it is for :
-- 1.drawing a card arbitarily from deck
-- 2.and putting this card on top of a new deck = "newDeck"
-- 3.updating the old deck, and the random seed
-- repeat this process until the old deck is Empty 
shuffleHelp :: (Int,StdGen) -> Hand -> Hand -> Hand
shuffleHelp (r,g) Empty newDeck = newDeck
shuffleHelp (r,g) deck newDeck  = shuffleHelp (r',g') deck' newDeck'
  where newDeck'= Add (fst $ drawAny r deck) newDeck  -- add card
        deck'   = (<+) (handUpDrawn r deck) (snd $ drawAny r deck) --update
        (r',g') = randomR (0,size deck-2) g    -- update random
        

-- the shuffleDeck is constructed by shufflehelp function
shuffleDeck :: StdGen -> Hand -> Hand
shuffleDeck g deck = shuffleHelp (randomR (0,size deck-1) g) deck Empty


-- B6
implementation :: Interface
implementation = Interface
  { iFullDeck = fullDeck
  , iValue    = value
  , iDisplay  = display
  , iGameOver = gameOver
  , iWinner   = winner 
  , iDraw     = draw
  , iPlayBank = playBank
  , iShuffle  = shuffleDeck
  }
main :: IO () 
main = runGame implementation









--------------test function 
-- for B1
prop_size_onTopOf :: Hand -> Hand -> Bool
prop_size_onTopOf handA handB = 
  size (handA <+ handB) == size handA + size handB


-- for B2
-- prop_fullDeck checking the total cards
-- also checking the suit and rank through the "all" high order function
prop_fullDeck :: Bool
prop_fullDeck = size fullDeck == 52 && allCardsPresent fullDeck
  where
    allCardsPresent deck = 
      all (`belongsTo` deck) [Card rank suit | rank <- ranks, suit <- suits]
    ranks = [Numeric n | n <- [2..10]] ++ [Jack, Queen, King, Ace]
    suits = [Hearts, Spades, Diamonds, Clubs]


-- for B3
-- we dont care the case when deck is Empty  (see otherwise)
-- if deck is not Emoty, the size of old deck minus 1
-- should equal size of new deck
-- and, the size of cards in hand should one larger than
-- the size of cards in old hand
prop_draw :: Hand -> Hand -> Bool
prop_draw deck hand 
  | deck/=Empty = size (fst $ draw deck hand) == size deck - 1
                  && size (snd $ draw deck hand) == size hand + 1
  | otherwise   = True


--test function for B4
-- we dont care case when value<16 in hand but empty deck at same time
-- that is : the total value of deck is less than 16
prop_playBank :: Hand -> Bool
prop_playBank deck
    | deck       ==Empty = True
    | value deck <16     = True -- we dont care
    | otherwise =
        value (playBank deck) >= 16 && value (playBank deck) <= 21 
        || gameOver (playBank deck)
        




-------------test function for B5
prop_shuffle_sameCards :: StdGen -> Card -> Hand -> Bool 
prop_shuffle_sameCards g c h = 
  c `belongsTo` h == c `belongsTo` shuffleDeck g h

belongsTo :: Card -> Hand -> Bool 
c `belongsTo` Empty = False 
c `belongsTo` (Add c' h) = c == c' || c `belongsTo` h
-- The above property does not guarantee that the size of the deck is 
--- preserved by shuffle; 
-- all cards could be duplicated, for instance. 

-------so....
-- we put cards in hand into list by function "extractCards"
extractCards :: Hand -> [Card]
extractCards Empty         = []
extractCards (Add card h)  = card : extractCards h

-- checking size equal between deck and shuffledDeck
-- also checking : whether all cards in old deck are in new deck
-- if both condition is satisified , we are sure
-- all cards are in new deck, and only one time, no copies
prop_size_shuffle :: StdGen -> Hand -> Bool
prop_size_shuffle gen deck = 
  size deck == size shuffledDecks && 
               all (`belongsTo` shuffledDecks) cardsInDeck
  where
    shuffledDecks = shuffleDeck gen deck
    cardsInDeck   = extractCards deck 































-------------------
-- | test handAce (only for simple test by ourself)
-- | handAce21 : 11+2+8=21
handAce21 :: Hand
handAce21 = Add (Card (Numeric 2) Spades) 
              (Add (Card (Numeric 8) Spades)
                  (Add (Card Ace Hearts) Empty))
-- | handAce22 : 11+2+9=22>21,so it ( should) becomes to 1+2+9=12             
handAce22 :: Hand
handAce22 = Add (Card (Numeric 2) Spades) 
              (Add (Card (Numeric 9) Spades)
                  (Add (Card Ace Hearts) Empty))
-- | handAce25 : 11+11+3=25 > 21 , so it shold become to 1+1+3=5
handAce25 :: Hand
handAce25 = Add (Card (Numeric 3) Spades) 
              (Add (Card Ace Spades)
                  (Add (Card Ace Hearts) Empty))
-- | handAce23 : 10+11+11+11+10=53>>21, and it will becomes to 
-- 10+1+1+1+10= 23, althoough still larger than 21
handAce23 :: Hand
handAce23 = Add (Card Queen Spades) 
              (Add (Card Ace Spades)
                  (Add (Card Ace Hearts) 
                    (Add (Card Ace Diamonds) 
                      (Add (Card Queen Diamonds) Empty))))

deck1 :: Hand
deck1 = Add (Card (Numeric 10) Hearts) Empty

deck5 :: Hand
deck5 = Add (Card (Numeric 2) Spades) 
              (Add (Card (Numeric 3) Spades)
                  (Add (Card (Numeric 3) Hearts) 
                    (Add (Card Jack Diamonds) 
                      (Add (Card Queen Diamonds) Empty))))