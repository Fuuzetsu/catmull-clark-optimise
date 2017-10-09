{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -O2 -ddump-simpl -ddump-stg -ddump-to-file #-}
module Mesh.V3 (main) where

import Control.Monad (liftM2)
import Data.Vector (Vector, (!))
import qualified Data.Vector as V
import Data.Foldable as F (length, sum)
import Data.Maybe (fromMaybe)
import Prelude hiding (length, concat, sum)
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Data.Double.Conversion.Text as T

{-
A SimpleMesh consists of only vertices and faces that refer to them.
A Mesh extends the SimpleMesh to contain edges as well as references to
adjoining mesh components for each other component, such as a vertex
also contains what faces it belongs to.
An isolated edge can be represented as a degenerate face with 2 vertices.
Faces with 0 or 1 vertices can be thrown out, as they do not contribute to
the result (they can also propagate NaNs).
-}

newtype VertexId = VertexId { getVertexId :: Int } deriving (Ord, Eq, Show)
newtype EdgeId = EdgeId { getEdgeId :: Int } deriving (Ord, Eq, Show)
newtype FaceId = FaceId { getFaceId :: Int } deriving (Ord, Eq, Show)

data Vertex a = Vertex
  { vertexPoint :: a
  , vertexEdges :: Vector EdgeId
  , vertexFaces :: Vector FaceId
  } deriving Show

data Edge = Edge
  { _edgeVertexA :: VertexId
  , _edgeVertexB :: VertexId
  , edgeFaces :: Vector FaceId
  } deriving Show

data Face = Face
  { faceVertices :: Vector VertexId
  , _faceEdges :: Vector EdgeId
  } deriving Show

type VertexArray a = Vector (Vertex a)
type EdgeArray = Vector Edge
type FaceArray = Vector Face

data Mesh a = Mesh
  { meshVertices :: (VertexArray a)
  , meshEdges :: EdgeArray
  , meshFaces :: FaceArray
  } deriving Show

newtype SimpleVertex a = SimpleVertex { sVertexPoint :: a } deriving Show
newtype SimpleFace = SimpleFace { sFaceVertices :: Vector VertexId } deriving Show

type SimpleVertexArray a = Vector (SimpleVertex a)
type SimpleFaceArray = Vector SimpleFace

data SimpleMesh a = SimpleMesh
  { sMeshVertices :: (SimpleVertexArray a)
  , sMeshFaces :: SimpleFaceArray
  } deriving Show

average :: (Foldable f, Fractional a) => f a -> a
average xs = (sum xs) / (fromIntegral $ length xs)

-- Intermediary point types for ultimately converting into a point `a`.
newtype FacePoint a = FacePoint { getFacePoint :: a } deriving Show
newtype EdgeCenterPoint a = EdgeCenterPoint { getEdgeCenterPoint :: a } deriving Show
newtype EdgePoint a = EdgePoint { getEdgePoint :: a } deriving Show
newtype VertexPoint a = VertexPoint { getVertexPoint :: a } deriving Show

type FacePointArray a = Vector (FacePoint a)
type EdgePointArray a = Vector (EdgePoint a)
type EdgeCenterPointArray a = Vector (EdgeCenterPoint a)
type IsEdgeHoleArray = Vector Bool
type VertexPointArray a = Vector (VertexPoint a)

-- Subdivision helpers.
facePoint :: Fractional a => Mesh a -> Face -> FacePoint a
facePoint mesh = FacePoint . average . (fmap $ vertexPointById mesh) . faceVertices

allFacePoints :: Fractional a => Mesh a -> FacePointArray a
allFacePoints = liftM2 fmap facePoint meshFaces

vertexPointById :: Mesh a -> VertexId -> a
vertexPointById mesh = vertexPoint . (meshVertices mesh !) . getVertexId

edgeCenterPoint :: Fractional a => Mesh a -> Edge -> EdgeCenterPoint a
edgeCenterPoint mesh (Edge ea eb _)
  = EdgeCenterPoint . average $ fmap (vertexPointById mesh) [ea, eb]

allEdgeCenterPoints :: Fractional a => Mesh a -> EdgeCenterPointArray a
allEdgeCenterPoints = liftM2 fmap edgeCenterPoint meshEdges

allIsEdgeHoles :: Mesh a -> IsEdgeHoleArray
allIsEdgeHoles = fmap ((< 2) . length . edgeFaces) . meshEdges

edgePoint :: Fractional a => Edge -> FacePointArray a -> EdgeCenterPoint a -> EdgePoint a
edgePoint (Edge _ _ faceIds) _ (EdgeCenterPoint ecp) | V.length faceIds == 1 = EdgePoint ecp
edgePoint (Edge _ _ faceIds) facePoints (EdgeCenterPoint ecp)
  = EdgePoint $ average [ecp, average $ fmap (getFacePoint . (facePoints !) . getFaceId) faceIds]

allEdgePoints :: Fractional a => Mesh a -> FacePointArray a -> EdgeCenterPointArray a -> EdgePointArray a
allEdgePoints mesh fps ecps = V.zipWith (\e ecp -> edgePoint e fps ecp) (meshEdges mesh) ecps

vertexPoint' :: Fractional a => Vertex a -> FacePointArray a -> EdgeCenterPointArray a -> IsEdgeHoleArray -> VertexPoint a
vertexPoint' vertex facePoints ecps iehs
  | V.length faceIds == V.length edgeIds = VertexPoint newCoords
  | otherwise = VertexPoint avgHoleEcps
  where
    newCoords = (oldCoords * m1) + (avgFacePoints * m2) + (avgMidEdges * m3)
    oldCoords = vertexPoint vertex
    avgFacePoints = average $ fmap (getFacePoint . (facePoints !) . getFaceId) faceIds
    avgMidEdges = average $ fmap (getEdgeCenterPoint . (ecps !) . getEdgeId) edgeIds
    m1 = (n - 3) / n
    m2 = 1 / n
    m3 = 2 / n
    n = fromIntegral $ V.length faceIds
    faceIds = vertexFaces vertex
    edgeIds = vertexEdges vertex
    avgHoleEcps = average . V.cons oldCoords . V.map (getEdgeCenterPoint . (ecps !) . getEdgeId) $ V.filter ((iehs !) . getEdgeId) edgeIds

allVertexPoints :: Fractional a => Mesh a -> FacePointArray a -> EdgeCenterPointArray a -> IsEdgeHoleArray -> VertexPointArray a
allVertexPoints mesh fps ecps iehs = fmap (\v -> vertexPoint' v fps ecps iehs) (meshVertices mesh)

-- For each vertex in a face, generate a set of new faces from it with its vertex point,
-- neighbor edge points, and face point. The new faces will refer to vertices in the
-- combined vertex array.
newFaces :: Face -> FaceId -> Int -> Int -> Vector SimpleFace
newFaces (Face vertexIds edgeIds) faceId epOffset vpOffset =
  -- Check thet we have at least 2 edge ids: if not then we can't
  -- feed subsequented edge ID into newFace and we can just return
  -- the result straight away.
  if V.length edgeIds > 1
  then V.unfoldrN (V.length vertexIds) go (0, 0, 1)
  else V.empty
  where
    atEnd = V.length edgeIds - 1
    go (ix, eIx, deIx) =
      Just $! ( newFace (vertexIds V.! ix) (edgeIds V.! eIx) (edgeIds V.! deIx)
              , ( ix + 1
                  -- Jump back to start if we're at the end.
                , if eIx == atEnd then 0 else eIx + 1
                , if deIx == atEnd then 1 else deIx + 1
                )
              )
    f = VertexId . (+ epOffset) . getEdgeId
    newFace vid epA epB = SimpleFace $ V.fromList
      [ VertexId . (+ vpOffset) $ getVertexId vid
      , f epA
      , VertexId $ getFaceId faceId
      , f epB]

subdivide :: Fractional a => SimpleMesh a -> SimpleMesh a
subdivide simpleMesh
  = SimpleMesh combinedVertices faces
  where
    mesh = makeComplexMesh simpleMesh
    fps = allFacePoints mesh
    ecps = allEdgeCenterPoints mesh
    eps = allEdgePoints mesh fps ecps
    iehs = allIsEdgeHoles mesh
    vps = allVertexPoints mesh fps ecps iehs
    edgePointOffset = length fps
    vertexPointOffset = edgePointOffset + length eps
    combinedVertices
      = V.map SimpleVertex
      $ V.concat [ V.map getFacePoint fps
                 , V.map getEdgePoint eps
                 , V.map getVertexPoint vps ]

    faces = V.concat
          $ zipWith (\i face -> newFaces face (FaceId i) edgePointOffset vertexPointOffset)
                    [0 ..] (V.toList $ meshFaces mesh)

-- Transform to a Mesh by filling in the missing references and generating edges.
-- Faces can be updated with their edges, but must be ordered.
-- Edge and face order does not matter for vertices.
-- TODO: Discard degenerate faces (ones with 0 to 2 vertices/edges),
-- or we could transform these into single edges or vertices.
makeComplexMesh :: forall a. SimpleMesh a -> Mesh a
makeComplexMesh (SimpleMesh sVertices sFaces) = Mesh vertices edges faces
  where
    makeEdgesFromFace :: SimpleFace -> FaceId -> Vector Edge
    makeEdgesFromFace (SimpleFace vertexIds) fid = case vertexIds V.!? 0 of
      Nothing -> V.empty
      -- zip vertices with their tail
      Just h -> V.imap
        (\i a -> Edge a (fromMaybe h (vertexIds V.!? (i + 1))) (V.singleton fid))
        vertexIds

    edgeKey :: VertexId -> VertexId -> (VertexId, VertexId)
    edgeKey a b = (min a b, max a b)

    faceEdges :: Vector (Vector Edge)
    faceEdges = V.imap (\i face -> makeEdgesFromFace face (FaceId i)) sFaces

    edgeMap :: Map.Map (VertexId, VertexId) Edge
    edgeMap
      = let joinEdge (Edge a b fidsA) (Edge _ _ fidsB) = Edge a b (fidsA V.++ fidsB)
        in V.foldl' (V.foldl' (\m edge@(Edge a b _) -> Map.insertWith joinEdge (edgeKey a b) edge m))
             Map.empty faceEdges

    edges :: EdgeArray
    edges = V.fromList $ Map.elems edgeMap

    edgeIdMap :: Map.Map (VertexId, VertexId) EdgeId
    edgeIdMap = Map.fromList
              . V.toList
              $ V.imap (\i (Edge a b _) -> ((edgeKey a b), EdgeId i)) edges

    faceEdgeIds :: Vector (Vector EdgeId)
    faceEdgeIds = V.map (V.mapMaybe (\(Edge a b _) -> Map.lookup (edgeKey a b) edgeIdMap)) faceEdges

    faces :: FaceArray
    faces
      = V.zipWith (\(SimpleFace verts) edgeIds -> Face verts edgeIds) sFaces faceEdgeIds

    vidsToFids :: Map.Map VertexId (Vector FaceId)
    vidsToFids
      = let af e Nothing = Just (V.singleton e)
            af e (Just es) = Just (es `V.snoc` e)
        in V.ifoldl' (\n i (SimpleFace vertexIds) ->
                        V.foldl' (\m vid -> Map.alter (af (FaceId i)) vid m) n vertexIds)
           Map.empty sFaces

    vidsToEids :: Map.Map VertexId (Vector EdgeId)
    vidsToEids
      = let af e Nothing = Just (V.singleton e)
            af e (Just es) = Just (es `V.snoc` e)
        in V.foldl' (\m (va, vb, fc) -> Map.alter (af fc) vb (Map.alter (af fc) va m)) Map.empty
           $ V.imap (\i (Edge a b _) -> (a, b, EdgeId i)) edges


    simpleToComplexVert :: SimpleVertex a -> VertexId -> Vertex a
    simpleToComplexVert (SimpleVertex point) vid
      = Vertex point
      (Map.findWithDefault V.empty vid vidsToEids)
      (Map.findWithDefault V.empty vid vidsToFids)

    vertices :: VertexArray a
    vertices
      = V.imap (\i e -> simpleToComplexVert e (VertexId i)) sVertices

pShowSimpleMesh :: Show a => SimpleMesh a -> String
pShowSimpleMesh (SimpleMesh vertices faces)
  = "Vertices:\n" ++ arrShow vertices sVertexPoint
  ++ "Faces:\n" ++ arrShow faces (fmap getVertexId . sFaceVertices)
  where
    arrShow a f = Prelude.unlines
                . V.toList
                $ V.imap (\i e -> show (i, f e)) a

-- Testing types.
data Point = Point {-# UNPACK #-} !Double
                   {-# UNPACK #-} !Double
                   {-# UNPACK #-} !Double
--  deriving (Show)

instance Show Point where
  show (Point x y z) = T.unpack $! T.unwords
    [ "Point"
    , T.toShortest x
    , T.toShortest y
    , T.toShortest z
    ]

zipPoint :: (Double -> Double -> Double) -> Point -> Point -> Point
zipPoint f (Point x y z) (Point x' y' z') = Point (f x x') (f y y') (f z z')
{-# INLINE zipPoint #-}

instance Num Point where
  (+) = zipPoint (+)
  (-) = zipPoint (-)
  (*) = zipPoint (*)
  negate (Point x y z) = Point (negate x) (negate y) (negate z)
  abs (Point x y z) = Point (abs x) (abs y) (abs z)
  signum (Point x y z) = Point (signum x) (signum y) (signum z)
  fromInteger i =
    let i' = fromInteger i
    in Point i' i' i'

instance Fractional Point where
  recip (Point x y z) = Point (recip x) (recip y) (recip z)
  fromRational r = let r' = fromRational r in Point r' r' r'

testCube :: SimpleMesh Point
testCube = SimpleMesh vertices faces
  where
    vertices = V.map SimpleVertex
      $ V.fromList
      [ Point (-1) (-1) (-1)
      , Point (-1) (-1) 1
      , Point (-1) 1 (-1)
      , Point (-1) 1 1
      , Point 1 (-1) (-1)
      , Point 1 (-1) 1
      , Point 1 1 (-1)
      , Point 1 1 1]
    faces = V.map (SimpleFace . V.map VertexId . V.fromList)
      $ V.fromList
      [ [0, 4, 5, 1]
      , [4, 6, 7, 5]
      , [6, 2, 3, 7]
      , [2, 0, 1, 3]
      , [1, 5, 7, 3]
      , [0, 2, 6, 4]]

testCubeWithHole :: SimpleMesh Point
testCubeWithHole
  = SimpleMesh (sMeshVertices testCube) (V.take 5 (sMeshFaces testCube))

testTriangle :: SimpleMesh Point
testTriangle = SimpleMesh vertices faces
  where
    vertices = V.map SimpleVertex
      $ V.fromList
      [ Point 0 0 0
      , Point 0 0 1
      , Point 0 1 0]
    faces = V.map (SimpleFace . V.map VertexId . V.fromList)
      $ V.fromList
      [ [0, 1, 2] ]

main :: Int -> String
main i = pShowSimpleMesh $ go i testCube
  where
    go 0 c = c
    go n c = go (n - 1) (subdivide c)
