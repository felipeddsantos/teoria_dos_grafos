{-

Teoria dos Grafos - Testes de Operações e Algoritmos de Grafos
Felipe Daniel Dias dos Santos - 11711ECP004
Graduação em Engenharia de Computação - Faculdade de Engenharia Elétrica - Universidade Federal de Uberlândia

-}

import Grafo
import GrafoLista
import qualified GrafoMatriz as M (novoGrafo)

g = novoGrafo 8 [(1, 2), (3, 1), (4, 1), (4, 5), (4, 7), (7, 6), (7, 8)]

g1 = novoGrafo 7 [(1, 2), (1, 3), (2, 3), (2, 4), (2, 5), (3, 4), (3, 6), (4, 5), (4, 6), (5, 6), (5, 7), (6, 7)]

g1Matriz = M.novoGrafo 7 [(1, 2), (1, 3), (2, 3), (2, 4), (2, 5), (3, 4), (3, 6), (4, 5), (4, 6), (5, 6), (5, 7), (6, 7)]

g2 = novoGrafo 6 [(1, 2), (1, 3), (2, 3), (2, 4), (2, 5), (3, 4), (3, 6), (4, 5), (4, 6), (5, 6)]

g3 = novoGrafo 5 [(1, 2), (1, 3), (1, 4), (1, 5), (2, 3), (2, 4), (3, 5), (4, 5)]

g4 = M.novoGrafo 13 [(5, 4), (4, 6), (4, 7), (4, 3), (6, 8), (7, 9), (8, 10), (9, 11), (3, 8), (3, 9), (3, 10), (3, 11), (3, 2),(10, 2), (2, 11), (2, 1), (12, 1), (1, 13)]

g5 = novoGrafo 0 []

c3 = novoGrafo 3 [(1, 2), (2, 3), (3, 1)]

p2 = novoGrafo 2 [(1, 2)]

b33 = novoGrafo 6 [(1, 4), (1, 5), (1, 6), (2, 4), (2, 5), (2, 6), (3, 4), (3, 5), (3, 6)]

b33Matriz = M.novoGrafo 6 [(1, 4), (1, 5), (1, 6), (2, 4), (2, 5), (2, 6), (3, 4), (3, 5), (3, 6)]

arvore = novoGrafo 12 [(1, 2), (1, 7), (1, 8), (2, 3), (2, 6), (3, 4), (3, 5), (2, 6), (8, 9), (8, 12), (9, 10), (9, 11)]

arvoreMatriz = M.novoGrafo 12 [(1, 2), (1, 7), (1, 8), (2, 3), (2, 6), (3, 4), (3, 5), (2, 6), (8, 9), (8, 12), (9, 10), (9, 11)]

estrela7 = novoGrafo 6 [(1, 2), (1, 3), (1, 4), (1, 5), (1, 6)]

arvoreBinaria = novoGrafo 9 [(1, 2), (1, 3), (2, 4), (4, 7), (3, 5), (3, 6), (5, 8), (5, 9)]

arvoreBinariaEstrita = novoGrafo 7 [(1, 2), (1, 3), (3, 4), (3, 5), (4, 6), (4, 7)]

arvore3aria = novoGrafo 10 [(1, 2), (1, 3), (1, 4), (3, 5), (3, 6), (3, 7), (4, 8), (4, 9), (5, 10)]

main :: IO()
main = do

    putStrLn "\nÉ Simples"
    print $ eSimples g1
    print $ eSimples g2
    print $ eSimples g3
    
    putStrLn "\nÉ Trivial"
    print $ eTrivial g1
    print $ eTrivial g2
    print $ eTrivial g3

    putStrLn "\nÉ Isolado"
    print $ eIsolado g1 1
    print $ eIsolado g2 1
    print $ eIsolado g3 1 
   
    putStrLn "\nÉ Terminal" 
    print $ eTerminal g1 1
    print $ eTerminal g2 1
    print $ eTerminal g3 1 
    
    putStrLn "\nÉ Par"
    print $ ePar g1 1
    print $ ePar g2 1
    print $ ePar g3 1 

    putStrLn "\nÉ Ímpar"
    print $ eImpar g1 1
    print $ eImpar g2 1
    print $ eImpar g3 1 
    
    putStrLn "\nSequência de Graus"
    print $ seqGraus g1
    print $ seqGraus g2
    print $ seqGraus g3

    putStrLn "\nGrau Máximo"    
    print $ grauMax g1
    print $ grauMax g2
    print $ grauMax g3

    putStrLn "\nGrau Mínimo"
    print $ grauMin g1
    print $ grauMin g2
    print $ grauMin g3

    putStrLn "\nÉ Regular"    
    print $ eRegular g1
    print $ eRegular g2
    print $ eRegular g3

    putStrLn "\nÉ K-Regular"    
    print $ eKRegular g1 0
    print $ eKRegular g2 2
    print $ eKRegular g3 4

    putStrLn "\nÉ Vazio"    
    print $ eVazio g1
    print $ eVazio g2
    print $ eVazio g3

    putStrLn "\nÉ Nulo"    
    print $ eNulo g1
    print $ eNulo g2
    print $ eNulo g5

    putStrLn "\nÉ Completo"    
    print $ eCompleto g1
    print $ eCompleto g2
    print $ eCompleto g3

    putStrLn "\nÉ Kn"    
    print $ eKn g1 5
    print $ eKn g2 5
    print $ eKn g3 5

    putStrLn "\nGrafo Completo"   
    print $ grafoCompleto 2
    print $ grafoCompleto 3
    print $ grafoCompleto 7
   
    putStrLn "\nGrafo Complemento"    
    print $ grafoComplemento g1
    print $ grafoComplemento g2
    print $ grafoComplemento g3

    putStrLn "\nÉ Subgrafo"    
    print $ eSubgrafo g1 g3
    print $ eSubgrafo g2 g3
    print $ eSubgrafo g3 g2

    putStrLn "\nÉ Subgrafo Próprio"    
    print $ eSubgrafoProprio g1 g3
    print $ eSubgrafoProprio g2 g3
    print $ eSubgrafoProprio g3 g3

    putStrLn "\nÉ Subgrafo Induzido de Vértices"
    print $ eSubgrafoInduzidoVertices g1 g3 [1]
    print $ eSubgrafoInduzidoVertices g2 g3 [1, 2, 3, 4]
    print $ eSubgrafoInduzidoVertices g3 g2 [1, 2]

    putStrLn "\nÉ Subgrafo Induzido de Arestas"
    print $ eSubgrafoInduzidoArestas g1 g3 [(1, 2)]
    print $ eSubgrafoInduzidoArestas g2 g3 [(1, 2), (2, 3), (3, 4)]
    print $ eSubgrafoInduzidoArestas g3 g2 [(7, 8)]

    putStrLn "\nÉ Clique"
    print $ eClique g1 g1
    print $ eClique g3 g2
    print $ eClique arvore g3

    putStrLn "\nÉ Conjunto Independente de Vértices"
    print $ eCjIndependenteVertices g1 g1
    print $ eCjIndependenteVertices g2 arvore
    print $ eCjIndependenteVertices g5 g1

    putStrLn "\nUnião"
    print $ uniao g1 g3
    print $ uniao g2 g3
    print $ uniao arvore g1

    putStrLn "\nInterseção"
    print $ intersecao g1 g3
    print $ intersecao g2 g3
    print $ intersecao arvore g3

    putStrLn "\nSoma"
    print $ soma g1 g3
    print $ soma g2 g3
    print $ soma g2 arvore
    
    putStrLn "\nFunde Vértices"
    print $ fundeVertices g1 1 2
    print $ fundeVertices g2 3 4
    print $ fundeVertices g3 5 1

    putStrLn "\nContrai Vértices"
    print $ contraiVertices g1 1 2
    print $ contraiVertices g2 3 4
    print $ contraiVertices g3 5 1

    putStrLn "\nContrai Aresta"
    print $ contraiAresta g1 (1, 2)
    print $ contraiAresta g2 (3, 4)
    print $ contraiAresta g3 (5, 1)
    
    putStrLn "\nÉ Passeio"
    print $ ePasseio g1 [1, 2, 3, 4, 5, 6, 7]
    print $ ePasseio g2 [1, 2, 3, 4, 8]
    print $ ePasseio g3 [1, 2, 5]

    putStrLn "\nÉ Passeio Aberto"
    print $ ePasseioAberto g1 [1, 2, 3, 4, 5, 6, 7]
    print $ ePasseioAberto g2 [1, 2, 3, 4, 8]
    print $ ePasseioAberto g3 [1, 2, 3, 1]

    putStrLn "\nÉ Passeio Fechado"
    print $ ePasseioFechado g1 [1, 2, 3, 4, 5, 6, 7]
    print $ ePasseioFechado g2 [1, 2, 3, 4, 8]
    print $ ePasseioFechado g3 [1, 2, 3, 2, 1] 

    putStrLn "\nÉ Trilha"
    print $ eTrilha g1 [1, 2, 3, 4, 5, 6, 7, 6]
    print $ eTrilha g2 [1, 2, 3]
    print $ eTrilha g3 [1, 2, 3, 1] 

    putStrLn "\nÉ Caminho"
    print $ eCaminho g1 [1, 2, 3, 4, 5, 6, 7]
    print $ eCaminho g2 [1, 2, 5]
    print $ eCaminho g3 [1, 2, 3, 1]

    putStrLn "\nÉ Ciclo"
    print $eCiclo g1 [1, 2, 3, 4, 5, 6, 7]
    print $eCiclo g2 [1, 2, 3, 2, 1]
    print $eCiclo g3 [1, 2, 3, 1]

    putStrLn "\nÉ Grafo Cíclico"
    print $ eGrafoCiclico g1
    print $ eGrafoCiclico g2
    print $ eGrafoCiclico c3
    
    putStrLn "\nÉ Cn"
    print $ eCn g1 3
    print $ eCn c3 2
    print $ eCn c3 3 

    putStrLn "\nÉ Grafo Caminho"
    print $ eGrafoCaminho g1
    print $ eGrafoCaminho c3
    print $ eGrafoCaminho p2
    
    putStrLn "\nÉ Pn"
    print $ ePn g1 3
    print $ ePn c3 3
    print $ ePn p2 2 

    putStrLn "\nÉ Grafo Bipartido"
    print $ eGrafoBipartido g1 [4, 7] [5]
    print $ eGrafoBipartido g2 [1] [2, 3]
    print $ eGrafoBipartido b33 [1, 2, 3] [4, 5, 6]

    putStrLn "\nÉ Trilha Euleriana"
    print $ eTrilhaEuleriana g1 [1, 2, 5, 7, 6, 3, 2, 4, 5, 6, 4, 3, 1] 
    print $ eTrilhaEuleriana g2 [1, 2, 5]
    print $ eTrilhaEuleriana p2 [1, 2] 

    putStrLn "\nÉ Grafo Euleriano"
    print $ eGrafoEuleriano g1 
    print $ eGrafoEuleriano g2
    print $ eGrafoEuleriano g3 

    putStrLn "\nÉ Semi-Euleriano"
    print $ eSemiEuleriano g1
    print $ eSemiEuleriano g2
    print $ eSemiEuleriano g3

    putStrLn "\nÉ Hamiltoniano Ore"
    print $ eHamiltonianoOre g1
    print $ eHamiltonianoOre g2
    print $ eHamiltonianoOre g3 
    
    putStrLn "\nÉ Hamiltoniano Dirac"
    print $ eHamiltonianoDirac g1
    print $ eHamiltonianoDirac g2
    print $ eHamiltonianoDirac g3  

    putStrLn "\nFecho"
    print $ fecho g1
    print $ fecho g2
    print $ fecho g3
    
    putStrLn "\nBusca Genérica"
    print $ buscaGenerica g1
    print $ buscaGenerica b33
    print $ buscaGenerica arvore  

    putStrLn "\nBusca em Largura"
    print $ buscaLargura g1
    print $ buscaLargura b33
    print $ buscaLargura arvore 
    
    putStrLn "\nBusca em Profundidade"
    print $ buscaProfundidade g1
    print $ buscaProfundidade b33
    print $ buscaProfundidade arvore  

    putStrLn "\nMenor Caminho"
    print $ menorCaminho arvore 4 12
    print $ menorCaminho g1 1 7
    print $ menorCaminho b33 2 2  

    putStrLn "\nDijkstra"
    print $ dijkstra arvoreMatriz 1
    print $ dijkstra g1Matriz 1
    print $ dijkstra b33Matriz 1
    
    putStrLn "\nDistância"
    print $ distancia arvoreMatriz 4 12
    print $ distancia g1Matriz 1 7
    print $ distancia b33Matriz 2 2 
    
    putStrLn "\nExcentricidade"
    print $ excentricidade arvoreMatriz 4
    print $ excentricidade g1Matriz 7
    print $ excentricidade g4 10
    
    putStrLn "\nRaio"
    print $ raio arvoreMatriz 
    print $ raio g1Matriz 
    print $ raio g4 
    
    putStrLn "\nDiâmetro"
    print $ diametro arvoreMatriz 
    print $ diametro g1Matriz 
    print $ diametro g4 
    
    putStrLn "\nCentro"
    print $ centro arvoreMatriz 
    print $ centro g1Matriz 
    print $ centro g4

    putStrLn "\nÉ Disjunto Vértices" 
    print $ eDisjuntoVertices estrela7 [2, 1, 3] [5, 1, 4]
    print $ eDisjuntoVertices arvoreBinaria [1, 2, 4, 7] [1, 3, 5]
    print $ eDisjuntoVertices g [2, 1, 4, 5] [8, 7, 6]
    
    putStrLn "\nÉ Estrela"
    print $ eEstrela arvoreBinaria
    print $ eEstrela g
    print $ eEstrela estrela7 

    putStrLn "\nÉ Folha"
    print $ eFolha arvoreBinaria 9
    print $ eFolha g 2
    print $ eFolha estrela7 1

    putStrLn "\nÉ Binária"
    print $ eBinaria arvoreBinaria
    print $ eBinaria arvoreBinariaEstrita
    print $ eBinaria arvore3aria 

    putStrLn "\nÉ Binária Estrita"
    print $ eBinariaEstrita arvoreBinaria
    print $ eBinariaEstrita arvoreBinariaEstrita
    print $ eBinariaEstrita arvore3aria 

    putStrLn "\nÉ Árvore M-ária"
    print $ eArvMaria estrela7 4
    print $ eArvMaria arvoreBinariaEstrita 5
    print $ eArvMaria arvore3aria 3

    putStrLn "\nCodificação de Prufer"
    print $ codPrufer g
    print $ codPrufer estrela7
    print $ codPrufer arvoreBinaria
    
    putStrLn "\nDecodificação de Prufer"
    print $ decodPrufer [1, 1, 4, 4, 7, 7]
    print $ decodPrufer [1, 1, 1, 1]
    print $ decodPrufer [3, 4, 2, 1, 3, 5, 5]

    putStrLn "\nÉ Sequência Válida"
    print $ eSeqValida [1, 1, 1, 2]
    print $ eSeqValida [1, 2, 2, 5]
    print $ eSeqValida [1, 1, 2]

    putStrLn "\nNúmero de Árvores Rotuladas"
    print $ numArvoresRot [1, 1, 2, 2, 2]
    print $ numArvoresRot [1, 1, 1, 1, 1, 1, 1, 2, 2, 7]
    print $ numArvoresRot [1, 1, 2]

    putStrLn "\nCobertura de Vértices"
    print $ coberturaVertices g
    print $ coberturaVertices estrela7
    print $ coberturaVertices arvoreBinaria
