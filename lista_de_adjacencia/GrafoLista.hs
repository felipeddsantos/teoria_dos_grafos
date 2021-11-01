{-

Teoria dos Grafos - Representação de Grafos em Lista
Felipe Daniel Dias dos Santos - 11711ECP004
Graduação em Engenharia de Computação - Faculdade de Engenharia Elétrica - Universidade Federal de Uberlândia

-}

module GrafoLista(
    
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
    listaAdj,  
    novaAresta, 
    removeAresta
   
)where

import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
                                             
import Data.List

data Grafo = LsAdj (IntMap [Int]) deriving (Show,Eq)

novoGrafo :: Int -> [(Int,Int)] -> Grafo
novoGrafo n as = LsAdj (insere vazio as) where
      vazio = IntMap.fromList [(i,[]) | i <- [1..n]]
      insere mapa [] = mapa
      insere mapa ((u,v):as)= insere (IntMap.insertWith (++) u [v] novoMapa) as where
            novoMapa = IntMap.insertWith (++) v [u] mapa

vertices (LsAdj mapa) = IntMap.keys mapa

novoVertice (LsAdj mapa) v | IntMap.member v mapa = LsAdj mapa
                           | otherwise = LsAdj (IntMap.insert v [] mapa)

removeVertice (LsAdj mapa) v = LsAdj novoMapa where
      f mapa k = IntMap.adjust (filter (/= v)) k mapa
      novoMapa = case IntMap.lookup v mapa of
                    Nothing -> mapa
                    Just vizinhos -> IntMap.delete v (foldl f mapa vizinhos)

adjacente (LsAdj mapa) u v =
   case IntMap.lookup u mapa of
      Nothing -> False
      Just vz -> elem v vz

grau (LsAdj mapa) v =
   case IntMap.lookup v mapa of
      Nothing -> -1
      Just vz -> length vz

vizinhos (LsAdj mapa) v = sort (IntMap.findWithDefault [] v mapa)

pertence (LsAdj mapa) v = IntMap.member v mapa

listaAdj (LsAdj mapa) = IntMap.assocs mapa

arestas (LsAdj mapa) = procuraArestas lista [] where
      lista = IntMap.assocs mapa
      geraArestas u as [] = as
      geraArestas u as (v:vs) | elem (u,v) as || elem (v,u) as = geraArestas u as vs
                              | otherwise = geraArestas u ((u,v):as) vs
      procuraArestas [] as = as
      procuraArestas ((v,vz):ls) as =
         procuraArestas ls (geraArestas v as vz)

novaAresta (LsAdj mapa) (u,v) = LsAdj (IntMap.insertWith (++) u [v] novoMapa) where
      novoMapa = IntMap.insertWith (++) v [u] mapa

removeAresta (LsAdj mapa) (u,v) = LsAdj novoMapa where
      mapa' = IntMap.adjust (filter (/= v)) u mapa
      novoMapa = IntMap.adjust (filter (/= u)) v mapa'
