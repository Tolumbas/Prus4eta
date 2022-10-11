data Turn = FirstPlayer | SecondPlayer deriving (Eq, Show);

data GameState = GameState {
    firstPlayer::(Int,Int),
    secondPlayer::(Int,Int),
    turn::Turn
} deriving (Show,Eq)

data SmallHand = S Int deriving (Show)

instance Eq (S Int) where
    (S x) == (S y) = x `mod` 4 == y `mod` 4

instance Num (S Int) where
    (S x) + (S y) = (x + y) `mod` 4

instance Ord (S Int) where
    (S x) <= (S y) = (x `mod` 4) <= (y `mod` 4)

type AllFingers = Int
type Player = (AllFingers,SmallHand)

data GameStateOptimal = GameStateO {
    currentPlayer :: Player
    nextPlayer :: Player
}  deriving (Eq,Show)



equalHands :: Player -> Bool
equalHands (all,sh@(S shi)) = (S (all - shi)) == sh

splitHand :: GameStateO -> Maybe GameStateO
splitHand (GameStateO {currentPlayer =(cpall,(S cpsh)), nextPlayer = np})
    | cpsh == S 0 , even cpall = Just GameStateO {currentPlayer = (half,(S half)), nextPlayer = np}
    | otherwise                = Nothing
        where
            half = cpall `div` 2  

smallsmallHand :: GameStateO -> Maybe GameStateO
smallsmallHand (GameStateO {currentPlayer = (_,(S 0)), nextPlayer = _}) = Nothing
smallsmallHand (GameStateO {currentPlayer = cp@(cpall,(S cpsh)), nextPlayer = np@(npall,(S npsh))}) = Just $ GameStateO {currentPlayer =(newall,newsmallhand) , nextPlayer = cp}
    where
        newsmallhand = 


generateturns :: GameStateOptimal -> [GameStateOptimal]
generateturns (GameStateO {currentPlayer = cp@(cpall,(S cpsh)), nextPlayer = np@(npall,(S npsh))}) = allturns
    where
        splitHand = [ GameStateO {currentPlayer = cp@(half,(S half)), nextPlayer = np@(npall,npsh)} | let half = cpall `div` 2, cpsh == S 0,even cpall]
        eqHands = [ GameStateO {currentPlayer = np@(npall,npsh) , nextPlayer = cp@(half,(S half))} | equalHands cp, equalHands np, let finalFingers = cpsh + npsh]
        touchLeftWithLeft = [ GameState {firstPlayer = fp, secondPlayer = (changed,srightHand), turn = SecondPlayer} | let changed = (sleftHand + fleftHand) `mod` 5, fleftHand > 0,sleftHand > 0]
        touchRightWithLeft = [ GameState {firstPlayer = fp, secondPlayer = (sleftHand,changed), turn = SecondPlayer} | let changed = (srightHand + fleftHand) `mod` 5, fleftHand > 0, sleftHand > 0]
        touchLeftWithRight = [ GameState {firstPlayer = fp, secondPlayer = (changed,srightHand), turn = SecondPlayer} | let changed = (sleftHand + frightHand) `mod` 5, frightHand > 0, sleftHand > 0]
        touchRightWithRight = [ GameState {firstPlayer = fp, secondPlayer = (sleftHand,changed), turn = SecondPlayer} | let changed = (srightHand + frightHand) `mod` 5, frightHand > 0, sleftHand > 0]
        allturns = concat [splitHand,touchLeftWithLeft,touchLeftWithRight,touchRightWithLeft,touchRightWithRight]















(~~) :: GameState -> GameState -> Bool
a ~~ b = 
    a == b ||
    reverseState a == b ||
    reverseHandFp a == b ||
    reverseHandSp a == b ||
    (reverseState . reverseHandFp) a == b ||
    (reverseHandFp . reverseState) a == b ||
    (reverseState . reverseHandSp) a == b ||
    (reverseHandSp . reverseState) a == b


-- Symetries
reverseState :: GameState -> GameState
reverseState (GameState {firstPlayer = fp, secondPlayer = sp, turn = FirstPlayer}) = GameState {firstPlayer = sp, secondPlayer = fp,turn = SecondPlayer}
reverseState (GameState {firstPlayer = sp, secondPlayer = fp,turn = SecondPlayer}) = GameState {firstPlayer = fp, secondPlayer = sp, turn = FirstPlayer}

fullHand :: GameState -> GameState
fullHand (GameState {firstPlayer = (5,a), secondPlayer = sp, turn = t}) = GameState {firstPlayer = (0,a), secondPlayer = sp,turn = t}

reverseHandFp :: GameState -> GameState
reverseHandFp (GameState {firstPlayer = (leftHand,rightHand), secondPlayer = sp, turn = b}) = GameState {firstPlayer = (rightHand, leftHand), secondPlayer = sp ,turn = b}

reverseHandSp :: GameState -> GameState
reverseHandSp (GameState {firstPlayer = fp, secondPlayer = (leftHand,rightHand), turn = b}) = GameState {firstPlayer = fp, secondPlayer = (rightHand, leftHand),turn = b}



countAllFingers :: (Int,Int) -> Int
countAllFingers (a,b) = a+b

startState :: GameState
startState = GameState {firstPlayer = (1,1), secondPlayer = (1,1), turn = FirstPlayer}

fpWin::GameState -> Bool
fpWin (GameState {firstPlayer = _, secondPlayer = (0,0), turn = SecondPlayer}) = True
fpWin _ = False

spWin::GameState -> Bool
spWin (GameState {firstPlayer = (0,0), secondPlayer =_, turn = FirstPlayer}) = True
spWin _ = False


-- possibleState :: GameState -> Bool
-- possibleState (GameState {firstPlayer = (fpl,fpr), secondPlayer = (spl,spr), turn = _}) = fpl <= 5 && fpr <=5 && spl <=5 && spr <= 5

generateturns :: GameState -> [GameState]
generateturns (GameState {firstPlayer = fp@(fleftHand,frightHand), secondPlayer = sp@(sleftHand,srightHand), turn = FirstPlayer}) = allturns
    where
        splitHand = [ GameState {firstPlayer = (half,half), secondPlayer = sp , turn = FirstPlayer} | let half = (countAllFingers fp) `div` 2, even (countAllFingers fp), fleftHand == 0 || frightHand == 0]
        touchLeftWithLeft = [ GameState {firstPlayer = fp, secondPlayer = (changed,srightHand), turn = SecondPlayer} | let changed = (sleftHand + fleftHand) `mod` 5, fleftHand > 0,sleftHand > 0]
        touchRightWithLeft = [ GameState {firstPlayer = fp, secondPlayer = (sleftHand,changed), turn = SecondPlayer} | let changed = (srightHand + fleftHand) `mod` 5, fleftHand > 0, sleftHand > 0]
        touchLeftWithRight = [ GameState {firstPlayer = fp, secondPlayer = (changed,srightHand), turn = SecondPlayer} | let changed = (sleftHand + frightHand) `mod` 5, frightHand > 0, sleftHand > 0]
        touchRightWithRight = [ GameState {firstPlayer = fp, secondPlayer = (sleftHand,changed), turn = SecondPlayer} | let changed = (srightHand + frightHand) `mod` 5, frightHand > 0, sleftHand > 0]
        allturns = concat [splitHand,touchLeftWithLeft,touchLeftWithRight,touchRightWithLeft,touchRightWithRight]
generateturns sp = map reverseState $ generateturns $ reverseState sp

minmax :: [GameState] -> (GameState,Float)
minmax (state:path) = 
    if fpWin state 
        then 1.0 
    else if spWin state 
        then -1.0 
    else if  eqWin state
        then 0.0 
    else if turn state == FirstPlayer then maxscore else minscore
        where
            nextturns = generateturns state
            onlynewstates = filter 
            scores = map minmax nextturns
            nextrunscored = zip nextturns scores
            maxscore = maximum scores
            minscore = minimum scores