module Chess where


import CommonTypes
import qualified DocTypes as Prox
import XprezLib
import PresTypes

import DocumentEdit
import Char

import Maybe

piece pc color sqColor rowNr colNr moves focus path =
       overlay [ pieceXp pc sqColor (focus==Prox.PathD path)
                   `withbgColor` backgroundColor sqColor (focus==Prox.PathD path)
              , rect 80 84 
                `withfColor` backgroundColor sqColor (focus==Prox.PathD path)
                `withColor` backgroundColor sqColor (focus==Prox.PathD path)
              ]
 where isReachable = (colNr, rowNr) `elem` moves
       pieceXp Prox.Empty          sqc f  = if isReachable 
                                       then markReachable `withMouseDown` moveHere path focus 
                                       else empty
       pieceXp (Prox.King _ c)     sqc _  = txtPiece  $ (if c then toUpper else id) 'k'
       pieceXp (Prox.Queen _ c)    sqc _  = txtPiece  $ (if c then toUpper else id) 'q'
       pieceXp (Prox.Bishop _ c)   sqc _  = txtPiece  $ (if c then toUpper else id) 'b'
       pieceXp (Prox.Knight _ c)   sqc _  = txtPiece  $ (if c then toUpper else id) 'n'
       pieceXp (Prox.Rook _ c)     sqc _  = txtPiece  $ (if c then toUpper else id) 'r'
       pieceXp (Prox.Pawn _ c)     sqc _  = txtPiece  $ (if c then toUpper else id) 'p'
       pieceXp (Prox.King _ True)  sqc True  = img "img/Chess/pieceWF.bmp" `withSize` (80,84)
       pieceXp (Prox.King _ False) sqc True  = img "img/Chess/pieceBF.bmp" `withSize` (80,84)
       pieceXp (Prox.King _ True)  True _  = img "img/Chess/pieceWW.bmp" `withSize` (80,84)
       pieceXp (Prox.King _ False) True _  = img "img/Chess/pieceBW.bmp" `withSize` (80,84)
       pieceXp (Prox.King _ True)  False _  =  img "img/Chess/pieceWB.bmp" `withSize` (80,84)
       pieceXp (Prox.King _ False) False _  = img "img/Chess/pieceBB.bmp" `withSize` (80,84)
       
       txtPiece  ch = (if isReachable then (\x -> withMouseDown x (moveHere path focus)) else id) 
                       $ overlay $
                           [   text [ch] `withFont` Font "Traveller Standard" 30 False False False False
                                            `withColor` black  `withRef` (-16,-16)
                           ]
                        ++ if isReachable
                           then [markReachable]
                           else []
       markReachable = overlay 
                        [ rect 56 60 `withfColor` backgroundColor sqColor (focus==Prox.PathD path)
                            `withRef` (-12,-12)
                        , rect 60 64 `withfColor` black
                            `withRef` (-10,-10)
                        ] -- moveHere cannot be added to markReachable, because it is not always the object in front.
                          -- should add it to the square itself
       backgroundColor True  False = (251,251,0)
       backgroundColor True  True  = blue
       backgroundColor False False = (160,80,0)--(117,58,0)
       backgroundColor False True  = blue



moveHere tPath focus (Prox.DocumentLevel d path cl) =
  let (Prox.DocumentLevel d' path' pCl ) = editCutD (Prox.DocumentLevel d focus cl)
      (Prox.DocumentLevel d'' path'' cl'') = editPasteD (Prox.DocumentLevel d' (Prox.PathD tPath) pCl)
  in  (Prox.DocumentLevel d'' path'' cl)
               

-- talking to Prometheus

computeMoves :: Prox.Board -> (Int,Int) -> [(Int,Int)]
computeMoves board lt = map tupleFromMove 
                          $ moves (locationFromTuple lt) (listFromBoard board)
  where tupleFromMove     (NormalMove l (Location c r)) = (c, r)
        locationFromTuple (c,r) = Location c r
    
               
listFromBoard :: Prox.Board -> [[Field]]
listFromBoard (Prox.Board _ x1 x2 x3 x4 x5 x6 x7 x8)    = map listFromRow [x1,x2,x3,x4,x5,x6,x7,x8]
listFromBoard _                                         = replicate 8 (replicate 8 Empty)
listFromRow   (Prox.BoardRow _ x1 x2 x3 x4 x5 x6 x7 x8) = map pieceFromSquare [x1,x2,x3,x4,x5,x6,x7,x8]
listFromRow   _                                        = replicate 8 Empty

pieceFromSquare (Prox.Queen _ True)   = Piece White Queen
pieceFromSquare (Prox.Queen _ False)  = Piece Black Queen
pieceFromSquare (Prox.King _ True)    = Piece White King
pieceFromSquare (Prox.King _ False)   = Piece Black King
pieceFromSquare (Prox.Rook _ True)    = Piece White Rook
pieceFromSquare (Prox.Rook _ False)   = Piece Black Rook
pieceFromSquare (Prox.Knight _ True)  = Piece White Knight
pieceFromSquare (Prox.Knight _ False) = Piece Black Knight
pieceFromSquare (Prox.Bishop _ True)  = Piece White Bishop
pieceFromSquare (Prox.Bishop _ False) = Piece Black Bishop
pieceFromSquare (Prox.Pawn _ True)    = Piece White Pawn
pieceFromSquare (Prox.Pawn _ False)   = Piece Black Pawn
pieceFromSquare Prox.Empty            = Empty



{-

Inlined Prometheus modules

-}

-- Prometheus


type Direction = (Int, Int)

moves :: Location -> Board -> [Move]
moves loc board =
    let field = getLocation loc board in
    case field of
        Empty -> []
        Piece colour kind ->
            case kind of 
                Pawn   -> pawnMoves   loc colour board
                Rook   -> rookMoves   loc colour board
                Knight -> knightMoves loc colour board
                Queen  -> queenMoves  loc colour board
                King   -> kingMoves   loc colour board
                Bishop -> bishopMoves loc colour board
        
knightMoves :: Location -> Colour -> Board -> [Move]
knightMoves loc colour board = 
    singleStep loc colour knightDirections board

kingMoves :: Location -> Colour -> Board -> [Move]
kingMoves loc colour board = 
    singleStep loc colour kingAndQueenDirections board
    
knightDirections :: [Direction]
knightDirections = [(1, 2), (2, 1), (2, -1), (1, -2), (-1, -2), (-2, -1), (-2, 1), (-1, 2)]

kingAndQueenDirections :: [Direction]
kingAndQueenDirections = rookDirections ++ bishopDirections

rookDirections :: [Direction]
rookDirections = [(0, 1), (1, 0), (0, -1), (-1, 0)]

bishopDirections :: [Direction]
bishopDirections = [(1, 1), (1, -1), (-1, -1), (-1, 1)]

rookMoves :: Location -> Colour -> Board -> [Move]
rookMoves loc colour board = 
    multipleSteps loc colour rookDirections board

bishopMoves :: Location -> Colour -> Board -> [Move]
bishopMoves loc colour board = 
    multipleSteps loc colour bishopDirections board

queenMoves :: Location -> Colour -> Board -> [Move]
queenMoves loc colour board =
    multipleSteps loc colour kingAndQueenDirections board
    
singleStep :: Location -> Colour -> [Direction] -> Board -> [Move]
singleStep source myColour directions board =
    [ NormalMove source destination
    | destination <- mapMaybe (addLocation source) directions
    , case getLocation destination board of
        Empty -> True
        Piece colour _ -> colour /= myColour
    ]

multipleSteps :: Location -> Colour -> [Direction] -> Board -> [Move]
multipleSteps source myColour directions board =
    concatMap (tryDirection source) directions
    where
        tryDirection :: Location -> Direction -> [Move]
        tryDirection loc direction =             
            case addLocation loc direction of
                Nothing -> [ ]
                Just destination ->
                    case getLocation destination board of
                        Empty -> 
                            NormalMove source destination : 
                            tryDirection destination direction
                        Piece colour _ -> 
                            if colour /= myColour
                                then [ NormalMove source destination ]
                                else [ ]
                
pawnMoves :: Location -> Colour -> Board -> [Move]
pawnMoves source colour board =
    if oneStepEmpty -- maybe promotion 
        then NormalMove source oneStep : 
                if getRow source == initialRow && twoStepEmpty then
                    [ NormalMove source twoStep ]
                else
                    []
    else
        [] -- hit left, hit right
    where
        direction, initialRow :: Int
        (direction, initialRow) = 
            if colour == White then (1, 1) else (-1, 6)
        
        step :: Location -> Location
        step loc = fromJust (addLocation loc (0, direction))
        
        oneStep = step source
        oneStepEmpty = isEmpty (getLocation oneStep board)
        
        twoStep = step oneStep
        twoStepEmpty = isEmpty (getLocation twoStep board)
        
        
        
        
-- Location

data Location =
    Location Int Int

instance Show Location where
    show (Location x y) = [ chr (x + ord 'a'), chr (y + ord '1') ]

addLocation :: Location -> (Int, Int) -> Maybe Location
addLocation (Location x y) (dx, dy) = 
    let x', y' :: Int
        x' = x + dx
        y' = y + dy
    in if x' >= 0 && x' <= 7 && y' >= 0 && y' <= 7 
        then Just (Location x' y') 
        else Nothing

location :: String -> Location
location [column, row] 
    | ord column >= ord 'A' && ord column <= ord 'H' && ord row >= ord '1' && ord row <= ord '8'
        = Location (ord column - ord 'A') (ord row - ord '1')
    | ord column >= ord 'a' && ord column <= ord 'h' && ord row >= ord '1' && ord row <= ord '8'
        = Location (ord column - ord 'a') (ord row - ord '1')
location text 
    = error ("Location.location: incorrect location '" ++ text ++ "'") 
    
getRow :: Location -> Int
getRow (Location _ row) = row

getColumn :: Location -> Int
getColumn (Location col _) = col



-- Move

data Move
    = NormalMove Location Location
    
instance Show Move where
    show (NormalMove source destination) = 
        show source ++ "-" ++ show destination

makeMove :: Move -> Board -> Board
makeMove (NormalMove fromLoc toLoc) board = 
    let fromField = getLocation fromLoc board 
    in setLocation toLoc fromField (setLocation fromLoc Empty board)




-- Field

data Field
    = Empty
    | Piece Colour Kind

data Colour
    = White
    | Black
    deriving Eq

data Kind
    = Pawn | Rook | Knight | Bishop | Queen | King
    
instance Show Colour where
    show White = "W"
    show Black = "B"

instance Show Kind where
    show Pawn = "P"
    show Rook = "R"
    show Knight = "K"
    show Bishop = "B"
    show Queen = "Q"
    show King = "*"

instance Show Field where
    show Empty = ".."
    show (Piece colour kind) = show colour ++ show kind

isEmpty :: Field -> Bool
isEmpty Empty = True
isEmpty _ = False




-- Board

type Board = [[Field]]

prettyBoard :: Board -> String
prettyBoard rows = concatMap ((++ "\n") . prettyRow) (reverse rows)
    where
        prettyRow :: [Field] -> [Char]
        prettyRow row = concatMap ((++ " ") . show) row

emptyBoard :: Board
emptyBoard = 
    replicate 8 (replicate 8 Empty)

initialBoard :: Board
initialBoard = 
    [ let pieces :: [Field]
          pieces = [ Piece White Rook, Piece White Knight, Piece White Bishop ]
      in pieces ++ [ Piece White Queen, Piece White King ] ++ reverse pieces
    , replicate 8 (Piece White Pawn)
    ] 
    ++ 
    replicate 4 (replicate 8 Empty)
    ++
    [ replicate 8 (Piece Black Pawn)
    , let pieces :: [Field]
          pieces = [ Piece Black Rook, Piece Black Knight, Piece Black Bishop ]
      in pieces ++ [ Piece Black Queen, Piece Black King ] ++ reverse pieces
    ]
    
getLocation :: Location -> Board -> Field
getLocation (Location x y) board = 
    (board !! y) !! x

setLocation :: Location -> Field -> Board -> Board
setLocation loc field board = 
    let row = getRow loc
        col = getColumn loc
        rowContents = board !! row
        modifiedRow = take col rowContents ++ [ field ] ++ drop (col + 1) rowContents
    in take row board ++ [ modifiedRow ] ++ drop (row+1) board
   