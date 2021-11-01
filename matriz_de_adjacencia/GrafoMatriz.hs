{-

Teoria dos Grafos - Representação de Grafos em Matriz
Felipe Daniel Dias dos Santos - 11711ECP004
Graduação em Engenharia de Computação - Faculdade de Engenharia Elétrica - Universidade Federal de Uberlândia

-}

module GrafoMatriz(

    Grafo,
    novoGrafo,
    vertices, 
    novoVertice,
    removeVertice,
    adjacente,    
    grau,         
    vizinhos,     
    pertence,     
    arestas,      
    matrizAdj,   
    novaAresta,   
    removeAresta  

)where

import Control.Monad
import Control.Monad.ST
import Data.Array.ST
import Data.Array.Unboxed
import Data.List

type Matriz e = UArray (Int, Int) e

data Grafo = GrAdj [Int] (Matriz Int)
   deriving Show

novoGrafo ::  Int -> [(Int, Int)] -> Grafo
novoGrafo n as = GrAdj [1 .. n] matriz where
    matriz = runSTUArray $ do
       m <- newArray ((1, 1), (n, n)) 0
       forM_ as $ \(i,j) -> do
          writeArray m (i,j) 1
          writeArray m (j,i) 1
       return m

vertices (GrAdj vs as) = vs

novoVertice (GrAdj vs as) v | v <= n = GrAdj (adiciona v vs) as
                            | v == n+1 = GrAdj (adiciona v vs) as'
                            | otherwise = error "O vértice é maior que o número de vértices atual mais um" where
      n = (fst . snd . bounds) as
      as' = runSTUArray $ do
         m <- newArray ((1, 1), (v, v)) 0
         forM_ [1..n] $ \i -> do
            forM_ [i..n] $ \j -> do
               let e = as ! (i,j)
               writeArray m (i,j) e
               writeArray m (j,i) e
         return m

removeVertice (GrAdj vs as) v = GrAdj vs' as' where
      n = (fst . snd . bounds) as
      vs' = filter (/= v) vs
      as' = runSTUArray $ do
         stM <- thaw as
         forM_ [1..n] $ \i -> do
            writeArray stM (v,i) 0
            writeArray stM (i,v) 0
         return stM

adjacente (GrAdj vs as) u v = as!(u,v) /= 0

grau (GrAdj vs as) v = sum [as ! (v,j) | j <- [1 .. n] ] where
      n = (fst . snd . bounds) as

vizinhos (GrAdj vs as) v = [ j | j <- [1 .. n],  as ! (v,j) /= 0] where
      n = (fst . snd . bounds) as

pertence (GrAdj vs as) v = elem v vs

matrizAdj (GrAdj vs as) = as

arestas (GrAdj vs as) = arestas 1 1 [] where
      n = (fst . snd . bounds) as
      arestas i j acc | i > n = acc
                      | j > n = arestas (i+1) (i+1) acc
                      | otherwise = let paralelas = replicate (as!(i,j)) (i,j)
                                    in arestas i (j+1) (paralelas ++ acc)

novaAresta (GrAdj vs as) (u,v) = GrAdj vs as' where
      as' = runSTUArray $ do
         stM <- thaw as
         writeArray stM (u,v) 1
         writeArray stM (v,u) 1
         return stM

removeAresta (GrAdj vs as) (u,v) = GrAdj vs as' where
      as' = runSTUArray $ do
         stM <- thaw as
         writeArray stM (u,v) 0
         writeArray stM (v,u) 0
         return stM

adiciona x xs = if elem x xs then xs else sort (x:xs)
