{- Code for Auto-Play Eight Off Solitaire by
    Name: Shei Pern Chua
    Student Number: 200188591
    Date Modified: 2021-12-08
-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Solitaire where
    import Prelude
    import Data.List
    import System.Random
    import Data.Maybe
    import Foreign.C (CLLong(CLLong))
    
    --Datatypes declaration
    data Suit = Hearts | Clubs | Spades | Diamonds deriving (Eq,Ord,Enum,Show)
    data Pip = Ace | Two | Three | Four | Five | Six
        | Seven | Eight | Nine | Ten | Jack | Queen | King deriving (Eq,Ord,Enum,Show)
    data Card = Empty | Unknown (Pip ,Suit) | Card' (Pip ,Suit) deriving (Eq,Ord)
    type Deck = [Card]
    type Foundation = [[Card]]
    type Column = [[Card]]
    type Reserve = [Card]
    type Stock = [[Card]]
    data Board = EOBoard Foundation Reserve Column | SBoard Foundation Stock Column

    --Displaying board
    instance Show Board where
        show b = boardShow "" b
            where
                boardShow pref (EOBoard foundation reserve column)
                    = "\nEOBoard\nFoundations  " ++ show (map head foundation)
                    ++ "\nColumns"
                    ++ columnsToString column ++ "\nReserve    "
                    ++ show reserve ++ "\n"

                boardShow pref (SBoard foundation stock column)
                    = "SBoard\nFoundations " ++ show (map head foundation)
                    ++ "\nColumns"
                    ++ columnsToString column ++ "\nStock    "
                    ++ show (length stock) ++ " Deals remaining"

    --Displaying card
    instance Show Card where
        show c = cardShow "" c
            where
                cardShow pref (Card' (pip,suit))
                    = show (pip,suit)

                cardShow pref (Unknown (pip,suit))
                    = show "<unknown>"

                cardShow pref Empty
                    = show "Empty"

    testEOBoard = EOBoard foundation reserve column
        where
        foundation = []
        reserve = [Card'(Two,Hearts),Card'(Six,Clubs),Card'(Five,Clubs),Card'(Jack,Diamonds)]
        column = [[Card'(Ace,Clubs),Card'(Seven,Diamonds),Card'(Ace,Hearts),Card'(Queen,Hearts),Card'(King,Clubs),Card'(Four,Spades)],
                    [Card'(Five,Diamonds),Card'(Queen,Spades),Card'(Three,Diamonds),Card'(Five,Spades),Card'(Six,Spades),Card'(Seven,Hearts)],
                    [Card'(King,Hearts),Card'(Ten,Diamonds),Card'(Seven,Spades),Card'(Queen,Diamonds),Card'(Five,Hearts),Card'(Eight,Diamonds)],
                    [Card'(Jack,Spades),Card'(Six,Hearts),Card'(Seven,Clubs),Card'(Eight,Spades),Card'(Ten,Clubs),Card'(Queen,Clubs)],
                    [Card' (Ace,Spades),Card'(Eight,Clubs),Card'(Ace,Diamonds),Card'(King,Diamonds),Card'(Jack,Hearts),Card'(Four,Clubs)],
                    [Card'(Two,Diamonds),Card'(Three,Hearts),Card'(Three,Clubs),Card'(Ten,Hearts),Card'(Six,Diamonds),Card'(Jack,Clubs)],
                    [Card'(Nine,Spades),Card'(Four,Diamonds),Card'(Nine,Clubs),Card'(Nine,Hearts),Card'(Three,Spades),Card'(Ten,Spades)],
                    [Card'(Two,Spades),Card'(Two,Clubs),Card'(Four,Hearts),Card'(Nine,Diamonds),Card'(King,Spades),Card'(Eight,Hearts)]]

    testFSBoard = SBoard foundation stack column
        where
            foundation = [[Card'(King,Hearts),Card'(Queen,Hearts),Card'(Jack,Hearts),Card'(Ten,Hearts),Card'(Nine,Hearts),Card'(Eight,Hearts),
                Card'(Seven,Hearts),Card'(Six,Hearts),Card'(Five,Hearts),Card'(Four,Hearts),Card'(Three,Hearts),Card'(Two,Hearts),Card'(Ace,Hearts)]]
            stack = [[Card'(Seven,Diamonds),Card'(Ace,Spades),Card'(King,Spades),Card'(Five,Clubs),Card'(Eight,Hearts),Card'(Four,Spades),
                    Card'(King,Clubs),Card'(Six,Hearts),Card'(Five,Clubs),Card'(Ten,Clubs)],
                    [Card'(Six,Spades),Card'(Six,Spades),Card'(Four,Clubs),Card'(Three,Clubs),Card'(Seven,Spades),Card'(Eight,Spades),
                    Card'(Six,Clubs),Card'(Ten,Spades),Card'(Jack,Spades),Card'(Queen,Spades)]]
            column = [[Card'(Eight,Diamonds),Card'(Nine,Hearts)],
                        [Card'(Two,Diamonds)],
                        [Card'(Ace,Spades),Card'(Two,Spades),Card'(Three,Spades),Card'(Four,Spades),Card'(Five,Spades),Card'(Six,Clubs),Card'(Seven,Clubs),
                        Card'(Eight,Clubs),Card'(Nine,Clubs),Card'(Ten,Diamonds),Card'(Jack,Diamonds),Card'(Queen,Diamonds),Card'(King,Diamonds),
                        Unknown(Ace,Diamonds),Unknown(Three,Diamonds)],
                        [Card'(Seven,Clubs),Card'(Eight,Diamonds),Card'(Nine,Diamonds),Card'(Ten,Diamonds),Card'(Jack,Diamonds),Card'(Queen,Diamonds),
                        Card'(King,Diamonds),Card'(Nine,Clubs),Card'(Ten,Diamonds),Card'(Jack,Clubs)],
                        [Card'(Ace,Hearts),Card'(Two,Hearts),Card'(Three,Hearts),Card'(Four,Hearts),Card'(Five,Hearts),Card'(Six,Diamonds),Card'(Seven,Diamonds),
                        Card'(Queen,Clubs),Card'(King,Hearts)],
                        [Card'(Two,Diamonds),Card'(Three,Diamonds),Card'(Four,Diamonds)],
                        [Card'(Jack,Clubs),Card'(Queen,Clubs),Card'(King,Clubs),Card'(Two,Spades),Card'(Three,Spades),Card'(Four,Diamonds),Card'(Five,Diamonds),
                        Card'(Six,Diamonds),Card'(Seven,Hearts),Card'(Eight,Clubs),Card'(Nine,Spades),Card'(Ten,Clubs),Card'(Ace,Clubs),Card'(Two,Clubs),
                        Card'(Three,Clubs),Card'(Four,Clubs),Card'(Five,Spades)],
                        [Card'(Seven,Spades),Card'(Eight,Spades),Card'(Nine,Spades),Card'(Ten,Spades),Card'(Jack,Spades),Card'(Queen,Spades),Card'(King,Spades),
                        Unknown(Nine,Diamonds),Unknown(Ace,Diamonds),Unknown(Five,Diamonds)],
                        [Card'(Jack,Hearts),Card'(Queen,Hearts)],
                        [Card'(Ace,Clubs),Card'(Two,Clubs)]]

    -- Print columns on separate lines
    columnsToString :: Column->String
    columnsToString column = concat ["\n " ++ show col |col<-column]

    --Card pack for eight-off board
    ePack :: Deck
    ePack = [Card' (p,s)| p<-[Ace .. King], s<-[Hearts .. Diamonds]]

    --Card pack for spider board
    sPack :: Deck
    sPack = [Unknown (p,s)| p<-[Ace .. King],  s<-[Hearts .. Diamonds]]

    --Returns a shuffled deck
    shuffle :: Int -> Deck -> Deck
    shuffle seed xs = [x | (x,n) <- sortBy cmp (zip xs (randoms (mkStdGen seed) :: [Int]))]
        where cmp (x1,y1) (x2,y2) = compare y1 y2

    --Splits a deck into groups of n
    splitDeck :: Int -> [Card] -> [[Card]]
    splitDeck _ [] = []
    splitDeck n xs = p1 : splitDeck n p2
        where (p1,p2) = splitAt n xs

    --Returns true if provided card is Ace
    isAce :: Card -> Bool
    isAce (Card' (p, _)) = p == Ace

    --Returns true if provided card is King
    isKing :: Card -> Bool
    isKing (Card' (p, _)) = p == King

    --Returns the card's successor card
    sCard :: Card -> Card
    sCard (Card' (p, s))
        | p == King = Card' (Ace,s)
        | otherwise = Card' ([Ace .. King]!!x, s)
                    where
                        x = fromJust q + 1
                        q = elemIndex p [Ace .. King]

    --Returns the card's predecessor card
    pCard :: Card -> Card
    pCard (Card' (p, s))
        | p == Ace = Card' (King,s)
        | otherwise = Card' ([Ace .. King]!!x, s)
                    where
                        x = fromJust q - 1
                        q = elemIndex p [Ace .. King]

    --Returns true if both cards have the same suit
    sameSuit :: Card -> Card -> Bool
    sameSuit (Card' (_,s1)) (Card'(_,s2)) = s1 == s2

    --Returns the previous card in provided list
    prevCard :: Card -> [Card] -> Card
    prevCard _ [] = Empty
    prevCard _ [x] = Empty
    prevCard card (x:y:rest)
        | y == card = x
        | otherwise = prevCard card (y:rest)

    --Returns the following card in provided list
    nextCard _ [] = Empty
    nextCard _ [x] = Empty
    nextCard card (x:y:rest)
        | x == card = y
        | otherwise = nextCard card (y:rest)

    --Helper function. Remove duplicates in list
    remDup :: Eq a => [a] -> [a]
    remDup [] = []
    remDup (x:xs) = x : remDup (remove x xs)
        where
            remove x [] = []
            remove x (y:ys)
                | x == y = remove x ys
                | otherwise = y : remove x ys

    --Returns true if reserve is not full
    canMoveToReserve :: Reserve -> Bool
    canMoveToReserve reserve = length reserve < 8

    --Returns true if column is not full
    canMoveToColumn :: Column -> Bool
    canMoveToColumn column = length column < 8

    --Deals an 8-Off Board
    eoDeal :: Int -> Board
    eoDeal seed = EOBoard [] reserve column
        where
            shuffledDeck = shuffle seed ePack
            reserve = p1
            column = splitDeck 6 p2
            (p1,p2) = splitAt 4 shuffledDeck

    --Swap unknown cards to visible cards
    unknownToCard :: Card -> Card
    unknownToCard (Unknown (p,s)) = Card' (p,s)

    --Swaps first unknown card to visible cards in each column
    firstUnknownToCard :: Column -> Column
    firstUnknownToCard [] = []
    firstUnknownToCard ((x:y):xs) = (unknownToCard x:y) : firstUnknownToCard xs

    --Deals a Four Suit Spider Board
    sDeal :: Int -> Board
    sDeal seed = SBoard [] stock column
        where
            shuffledDeck = shuffle seed (sPack ++ sPack)
            stock = map (map unknownToCard) (splitDeck 10 p1)
            column = firstUnknownToCard (splitDeck 6 c1 ++ splitDeck 5 c2)
            (p1,p2) = splitAt 50 shuffledDeck
            (c1,c2) = splitAt 24 p2

    --Returns Ace found in each column's head
    findAcesInColumns :: Column -> [Card]
    findAcesInColumns column = filter isAce (map head (filter (not.null) column))

    --Returns Ace in reserves
    findAcesInReserves :: Reserve -> [Card]
    findAcesInReserves = filter isAce

    --Returns Ace that can be moved from reserve or column
    findMoveableAces :: Board -> [Card]
    findMoveableAces (EOBoard _ reserve column) = findAcesInColumns column ++ findAcesInReserves reserve

    --Returns successor cards that can be moved from column to foundation
    findSuccessorsCF :: Board -> [Card]
    findSuccessorsCF (EOBoard foundation _ column) = filter condition list
        where
            --Checks if successor's head card in foundation is found at column's head card
            condition = \card -> card `elem` map head (filter (not.null) column)

            --Returns a list of successor cards of each head card in foundation
            list = map (\card -> if isKing (head card) then head card else sCard (head card)) foundation

    --Returns successor cards that can be moved from reserve to foundation
    findSuccessorsRF :: Board -> [Card]
    findSuccessorsRF (EOBoard foundation reserve column) = filter condition list
        where
            --Checks if successor's head card in foundation is found at column's head card
            condition = (`elem` reserve)

            --Returns a list of successor cards of each head card in foundation
            list = map (\card -> if isKing (head card) then head card else sCard (head card)) foundation

    --Returns list of successor cards that can be moved from reserve or column
    findMoveableSuccessors :: Board -> [Card]
    findMoveableSuccessors board = findSuccessorsCF board ++ findSuccessorsRF board

    --Move the card to foundation by either adding a new Ace card or
    --successor card with the same suit
    addCardToFoundations :: Card -> Foundation -> Foundation
    addCardToFoundations card foundation
        | [card] `elem` foundation = foundation
        | isAce card = [card] : foundation
        | otherwise --Move successor card to its matching foundation
            = map (\foundation -> if sameSuit card (head foundation) && sCard (head foundation) == card
                then card : foundation else foundation) foundation

    --Updates the board by adding successor cards to foundation and 
    --removing them from reserve or column
    updateFoundation :: Card -> Board -> Board
    updateFoundation card (EOBoard foundation reserve column) = EOBoard new_foundation new_reserve new_column
        where
            new_foundation = addCardToFoundations card foundation
            new_reserve = delete card reserve
            new_column = filter (not.null) (map (delete card) column)

    --Updates the board by providing playable ace and successor cards
    moveToFoundations :: Board -> Board
    moveToFoundations board = EOBoard new_foundation new_reserve new_column
            where
                EOBoard new_foundation new_reserve new_column = foldr updateFoundation board moveableCards
                moveableCards = findMoveableAces board ++ findMoveableSuccessors board

    --Return cards to foundations if there are available plays
    toFoundations :: Board -> Board
    toFoundations board
        | availableMoves = toFoundations(moveToFoundations board)
        | otherwise = board
        where
            availableMoves = (not.null)(findMoveableAces board) || (not.null)(findMoveableSuccessors board)
    
    --PART 2
    --Add successor cards to column
    addCardsToColumns :: [Card] -> Column -> Column
    addCardsToColumns card xs
        | isKing (head card)  || isKing (last card) = card : xs
        | length card == 1 = map (\f -> if pCard (head f) == head card then card ++ f else f) xs
        | otherwise = map (\f -> if pCard (head f) == last card then card ++ f else f) xs

    --Delete cards from column
    deleteCardsFromColumns :: [Card] -> Column -> Column
    deleteCardsFromColumns card column
        | length card == 1 = map (delete (head card)) column
        | otherwise = map (filter(`notElem` card)) column

    --Returns a list of consecutive cards e.g [(Two,Spades),(Three,Spades),(Four,Spades)]
    filterConsecutiveCards :: [Card] -> [Card]
    filterConsecutiveCards [] = []
    filterConsecutiveCards [x] = []
    filterConsecutiveCards (x:y:xs)
        | isKing x && isAce y = []
        | sCard x == y = remDup (x : y : filterConsecutiveCards (y:xs))
        | otherwise = []
 
    --Checks if the list contains a list of consecutive predecessor cards and ends with a King
    checkKingInLastElement :: [Card] -> Bool
    checkKingInLastElement list | length list == 1 && (isKing.last) list = True
                               | length list == 2 && (isKing.last) list = (sCard.head) list == last list
                               | (isKing.last) list = any (\card -> sCard card == nextCard card list
                                                    && pCard card == prevCard card list) list
                               | otherwise = False

    --Filter king and its predecessor cards found in column
    --e.g. [[(Queen,Hearts),(King,Hearts),(Two,Spades)],[(Two,Diamonds),(King,Diamonds)]] -> [[(Queen,Hearts),(King,Hearts)],[]]
    filterKingConsecutive :: Column -> [[Card]]
    filterKingConsecutive column
        = filter (any isKing)
            (filter (not.null) (map filterConsecutiveCards (filter (any isKing) (filter (not.checkKingInLastElement) column))))

    --Return king cards in columns that can be played to empty column
    findMoveableKingToColumn :: Column -> [[Card]]
    findMoveableKingToColumn column
        | canMoveToColumn column = filterKingConsecutive column
        | otherwise = []

    --Return cards that can be played from reserve to column
    findMoveableReserve :: Board -> [[Card]]
    findMoveableReserve (EOBoard _ reserve column)
        | canMoveToColumn column = map (: []) (filter isKing reserve)
        | otherwise = map (: []) (filter condition list)
        where
            condition = (`elem` reserve)
            list = map (pCard.head) column

    --Return playable cards that can be played from column to column
    findMoveableColumn :: Column -> [[Card]]
    findMoveableColumn column = filter condition list
        where
            condition = \card -> (sCard . last) card `elem` map head (filter (not.null) column)
            list = filter (not.null) (map filterConsecutiveCards column)

    --Return cards that can be played column to column and reserve to column
    findMoveableColumnAndReserve :: Board -> [[Card]]
    findMoveableColumnAndReserve board@(EOBoard foundation reserve column)
        = filter (not.null) (findMoveableKingToColumn column ++ findMoveableReserve board ++ findMoveableColumn column)

    --Return cards found in front of Ace Cards in a list 
    quarryAce :: [Card] -> [Card]
    quarryAce [] = []
    quarryAce (x:xs) | any isAce (x:xs) && not (isAce x) = x : quarryAce xs
                    | otherwise = []

    --Return cards found in front of successor cards of foundation in a list
    quarrySuccessor :: Card -> [Card] -> [Card]
    quarrySuccessor Empty _ = []
    quarrySuccessor _ [] = []
    quarrySuccessor card (x:xs) | card `elem` (x:xs) && card /= x = x : quarrySuccessor card xs
                                           | card == x = []
                                           | otherwise = []

    --Returns King if the first card in the list is King, otherwise returns the list
    checkKing :: [Card] -> [Card]
    checkKing [] = []
    checkKing xs | (isKing.head) xs = [head xs]
            | otherwise = xs

    --Returns head card or consecutive cards in a list
    filterHeadOrConsecutiveCards :: [Card] -> [Card]
    filterHeadOrConsecutiveCards (x:xs) = checkKing filteredList
        where filteredList = x : takeWhile (\card -> sCard card == nextCard card (x:xs) || pCard card == prevCard card (x:xs)) xs
        
    --Return cards that can be played from column to reserve
    findColumnToReserve :: Board -> [[Card]]
    findColumnToReserve board@(EOBoard _ reserve column)
        | canMoveToReserve reserve = filter (not.null) (map filterHeadOrConsecutiveCards
                                        (filter (not.checkKingInLastElement) column))
        | otherwise = []

    --Returns the number of cards has to be played to reach Ace
    aceSteps :: [Card] -> Int
    aceSteps (x:xs) | isAce x = 0
                    | any isAce (x:xs) = 1 + aceSteps xs
                    | otherwise = -1
    
    --Returns the column that requires to move least steps to reserve in order to reach Ace
    leastAceSteps :: Column -> [Card]
    leastAceSteps column | all (<0) steps = []
                         | otherwise = column!!fromJust(elemIndex (minimum modifiedSteps) steps)
        where steps = map aceSteps column
              modifiedSteps = [x | x <- steps, x>=0]

    --Returns the number of cards has to be played to reach successor card in foundation
    successorSteps :: Card -> [Card] -> Int
    successorSteps card (x:xs) | x == card = 0
                               | card `elem` (x:xs) = 1 + successorSteps card xs
                               | otherwise = -1
    
    --Returns a list of number of cards that has to be played to reach each successor cards in foundation
    allSuccessorSteps :: [Card] -> Column -> [Int]
    allSuccessorSteps [] [] = []
    allSuccessorSteps [] _ = []
    allSuccessorSteps _ [] = []
    allSuccessorSteps (x:xs) (y:ys) = successorSteps x y : allSuccessorSteps xs ys

    --Returns the column that requires to move least cards to reserve in order to reach the successor card
    leastSuccessorSteps :: [Card] -> Column -> [Card]
    leastSuccessorSteps cards column
        | all (<0) steps = []
        | otherwise =column!!fromJust(elemIndex (minimum steps) steps)
        where steps = allSuccessorSteps cards column
        
    --Returns a list of numbers of steps needed to reach Ace or successor card in column
    countSteps :: Card -> [[Card]] -> Board -> [Int]
    countSteps card [] _ = []
    countSteps card (x:xs) board@(EOBoard foundation reserve column)
        | any isAce x = aceSteps x : countSteps card xs board
        | otherwise = successorSteps card x : countSteps card xs board

    --Returns the successor card found in the column
    findSuccessorCard :: [Card] -> [Card] -> Card
    findSuccessorCard [] column = Empty
    findSuccessorCard (x:xs) column | x `elem` column = x
                                    | otherwise = findSuccessorCard xs column

    --Returns the column that the successor card is in 
    findSuccessorsColumn :: [Card]-> Board -> Column
    findSuccessorsColumn [] _ = []
    findSuccessorsColumn (x:xs) board@(EOBoard foundation _ column)= filter (elem x) column ++ findSuccessorsColumn xs board

    --Returns least cards play to reach ace and successor
    allSteps :: Board -> [[Card]]
    allSteps board@(EOBoard foundation _ column) = filter (not.null) (aces : [successor])
        where
            aces = leastAceSteps column
            successor = leastSuccessorSteps successorCards successorColumns
            successorCards = map (sCard.head) foundation
            successorColumns = findSuccessorsColumn successorCards board

    --Returns the column that requires to move least cards in order to reach Ace or successor cards
    leastAllSteps :: Board -> [Card]
    leastAllSteps board@(EOBoard foundation reserve column)
        | all (<0) steps = []
        | otherwise = list!!fromJust(elemIndex (minimum steps) steps)
        where steps = countSteps card list board
              card = findSuccessorCard cards (leastSuccessorSteps cards (findSuccessorsColumn cards board))
              cards = map (sCard.head) foundation
              list = allSteps board
              
    --Checks if the cards in column can be moved to reserve 
    findMoveableColumnToReserve :: Board -> [[Card]]
    findMoveableColumnToReserve board@(EOBoard foundation reserve column)
        = filter (not.null) (filter (\x-> length x + length reserve <= 8) list)
            where
                list = quarryAce xs : quarrySuccessor card xs : findColumnToReserve board
                xs = leastAllSteps board
                card = findSuccessorCard cards col
                cards = map (sCard.head) foundation
                col = leastSuccessorSteps cards (findSuccessorsColumn cards board)

    --Update corresponding column list by deleting and adding cards
    updateColumnP1 :: [Card] -> Board -> Board
    updateColumnP1 list board@(EOBoard foundation reserve column)
        | (not.null) list = EOBoard foundation new_reserve new_column
        | otherwise = board
            where
                new_reserve = delete (head list) reserve
                --Deletes the card from its original column first and then adding them
                --to the new column
                new_column' = filter (not.null) (deleteCardsFromColumns list column)
                new_column = addCardsToColumns list new_column'
                
    --Find possible moves from column to column and reserve to column
    findMovesP1 :: [[Card]] -> Board -> [Board]
    findMovesP1 [] _ = []
    findMovesP1 (x:xs) board
        = toFoundations (updateColumnP1 x board) : findMovesP1 xs board

    --Update corresponding column list by deleting and adding cards
    updateColumnP2 :: [Card] -> Board -> Board
    updateColumnP2 list board@(EOBoard foundation reserve column)
        | (not.null) list = EOBoard foundation new_reserve new_column
        | otherwise = board
            where
                new_reserve = reserve ++ list
                new_column = filter (not.null) (deleteCardsFromColumns list column)
                
    --Find possible moves from column to reserve
    findMovesP2 :: [[Card]] -> Board -> [Board]
    findMovesP2 [] _ = []
    findMovesP2 (x:xs) board
        = toFoundations (updateColumnP2 x board) : findMovesP2 xs board

    --Returns a list of all possible board states after playing each 
    --possible move based on initial board state
    findMoves :: Board -> [Board]
    findMoves board = findMovesP1 (findMoveableColumnAndReserve board) board
                        ++ findMovesP2 (findMoveableColumnToReserve board) board

    --Choose the initial move found in findMoves
    chooseMove :: Board -> Maybe Board
    chooseMove board@(EOBoard foundation reserve column)
        | null (findMoves board) = Nothing
        | otherwise = Just (head(findMoves board))

    --Returns true if there are no more cards in reserve and column
    haveWon :: Board -> Bool
    haveWon board@(EOBoard foundation reserve column) = null reserve && null column

    --Calculates score of the board by finding how many cards are found in foundation
    scoreBoard :: Board -> Int
    scoreBoard (EOBoard foundation _ _) = length (concat foundation)

    --Returns the score of the board
    playSolitaire :: Board -> Int
    playSolitaire board@(EOBoard foundation reserve column)
        | isJust (chooseMove board) = playSolitaire (fromJust (chooseMove board))
        | otherwise = scoreBoard board

    --Returns a list of score after playing a number of eight off solitaire games with a provided seed
    playSolitaires :: Int -> Int -> [Int]
    playSolitaires seed num = map (playSolitaire.eoDeal) [seed..(seed+num-1)]

    --Returns number of winning boards and average score after playing a number of board
    analyseEO :: Int -> Int -> (Int,Float)
    analyseEO seed num = (winningBoard,avgScore)
                    where
                        winningBoard = length (filter (==52) results)
                        avgScore = realToFrac (sum results) / genericLength results
                        results = playSolitaires seed num
                        
    {- Paste the contents of this file, including this comment, into your source file, below all
        of your code. You can change the indentation to align with your own, but other than this,
        ONLY make changes as instructed in the comments.
    -}
    -- Constants that YOU must set:
    studentName = "Shei Pern Chua"
    studentNumber = "200188591"
    studentUsername = "aca20spc"

    initialBoardDefined = testEOBoard 
    secondBoardDefined = testFSBoard

    {- Beyond this point, the ONLY change you should make is to change the comments so that the
        work you have completed is tested. DO NOT change anything other than comments (and indentation
        if needed). The comments in the template file are set up so that only the constant eight-off
        board from part 1 and the toFoundations function from part 1 are tested. You will probably
        want more than this tested.

        CHECK with Emma or one of the demonstrators if you are unsure how to change this.

        If you mess this up, your code will not compile, which will lead to being awarded 0 marks
        for functionality and style.
    -}

    main :: IO()
    main =
        do
        putStrLn $ "Output for " ++ studentName ++ " (" ++ studentNumber ++ ", " ++ studentUsername ++ ")"

        putStrLn "***The eight-off initial board constant from part 1:"
        print initialBoardDefined

        let board = toFoundations initialBoardDefined
        putStrLn "***The result of calling toFoundations on that board:"
        print board

        {- Move the start comment marker below to the appropriate position.
            If you have completed ALL the tasks for the assignment, you can
            remove the comments from the main function entirely.
            DO NOT try to submit/run non-functional code - you will receive 0 marks
            for ALL your code if you do, even if *some* of your code is correct.
        -}

        let boards = findMoves board      -- show that findMoves is working
        putStrLn "***The possible next moves after that:"
        print boards

        let chosen = chooseMove board     -- show that chooseMove is working
        putStrLn "***The chosen move from that set:"
        print chosen

        putStrLn "***Now showing a full game"     -- display a full game
        score <- displayGame initialBoardDefined 0
        putStrLn $ "Score: " ++ score
        putStrLn $ "and if I'd used playSolitaire, I would get score: " ++ show (playSolitaire initialBoardDefined)

        putStrLn "\n\n\n************\nNow looking at the alternative game:"

        putStrLn "***The spider initial board constant from part 1 (or equivalent if playing a different game of solitaire):"
        print secondBoardDefined          -- show the suitable constant. For spider solitaire this
                                            -- is not an initial game, but a point from which the game
                                            -- can be won

        {- start comment marker - move this if appropriate
        putStrLn "***Now showing a full game for alternative solitaire"
        score <- displayGame secondBoardDefined 0 -- see what happens when we play that game (assumes chooseMove
                                                    -- works correctly)
        putStrLn $ "Score: " ++ score
        putStrLn $ "and if I'd used playSolitaire, I would get score: " ++ show (playSolitaire secondBoardDefined)
        -}

    {- displayGame takes a Board and move number (should initially be 0) and
        displays the game step-by-step (board-by-board). The result *should* be
        the same as performing playSolitaire on the initial board, if it has been
        implemented correctly.
        DO NOT CHANGE THIS CODE other than aligning indentation with your own.
    -}
    displayGame :: Board -> Int ->IO String
    displayGame board n =
        if haveWon board
        then return "A WIN"
        else
            do
            putStr ("Move " ++ show n ++ ": " ++ show board)
            let maybeBoard = chooseMove board
            if isJust maybeBoard then
                do
                let (Just newBoard) = maybeBoard
                displayGame newBoard (n+1)
            else
                do
                let score = show (playSolitaire board)
                return score
