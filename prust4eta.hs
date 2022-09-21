data Turn = FirstPlayer | SecondPlayer deriving (Eq, Show);

data GameState = GameState {
    firstPlayer::(Int,Int),
    secondPlayer::(Int,Int),
    turn::Turn
} deriving (Show,Eq)


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

fullHand :: GameState -> GameState
fullHand (GameState {firstPlayer = (5,a), secondPlayer = sp, turn = t}) = GameState {firstPlayer = (0,a), secondPlayer = sp,turn = t}

reverseHandFp :: GameState -> GameState
reverseHandFp (GameState {firstPlayer = (leftHand,rightHand), secondPlayer = sp, turn = b}) = GameState {firstPlayer = (rightHand, leftHand), secondPlayer = sp ,turn = b}

reverseHandSp :: GameState -> GameState
reverseHandSp (GameState {firstPlayer = fp, secondPlayer = (leftHand,rightHand), turn = b}) = GameState {firstPlayer = fp, secondPlayer = (rightHand, leftHand),turn = b}



countAllFingers :: (Int,Int) -> Int
countAllFingers (a,b) = a+b

fpWin::GameState -> Bool
fpWin (GameState {firstPlayer = fp, secondPlayer = (0,0), turn = SecondPlayer}) = (countAllFingers fp) > 0
fpWin _ = False

spWin::GameState -> Bool
spWin = reverseState fpWin

possibleState :: GameState -> Bool
possibleState (GameState {firstPlayer = (fpl,fpr), secondPlayer = (spl,spr), turn = _}) = fpl <= 5 && fpr <=5 && spl <=5 && spr <= 5

generateturns :: GameState -> [GameState]
generateturns (GameState {firstPlayer = fp@(fleftHand,frightHand), secondPlayer = sp@(sleftHand,srightHand), turn = FirstPlayer}) = filter possibleState allturns
    where
        splitHand = [ GameState {firstPlayer = (nfleftHand,nfrightHand) secondPlayer = _ , SecondPlayer} | nfleftHand = nfrightHand = (countAllFingers sp) / 2, even (countAllFingers sp) ]
        
