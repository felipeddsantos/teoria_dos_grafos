{-

Teoria dos Grafos - Testes de Representação de Grafos em Matriz
Felipe Daniel Dias dos Santos - 11711ECP004
Graduação em Engenharia de Computação - Faculdade de Engenharia Elétrica - Universidade Federal de Uberlândia

-}

import GrafoMatriz

g1 = novoGrafo 6 [(1,4), (2,3), (2,4), (3,4), (4,5), (4,6), (5,6)]

g2 = novoGrafo 4 [(1,1), (1,2), (2,3), (2,3)]

-- Exemplos de uso

-- > vertices g1
-- [1,2,3,4,5,6]

-- > arestas g1
-- [(5,6),(4,6),(4,5),(3,4),(2,4),(2,3),(1,4)]

-- > adjacente g1 1 4
-- True

-- > adjacente g1 1 6
-- False

-- > grau g1 4
-- 5

-- > grau g1 1
-- 1

-- > vizinhos g1 4
-- [1,2,3,5,6]

-- > vizinhos g1 2
-- [3,4]

-- > pertence g1 3
-- True

-- > pertence g1 7
-- False

-- > matrizAdj g2
-- array ((1,1),(4,4)) [((1,1),1),((1,2),1),((1,3),0),((1,4),0),
--                      ((2,1),1),((2,2),0),((2,3),1),((2,4),0),
--                      ((3,1),0),((3,2),1),((3,3),0),((3,4),0),
--                      ((4,1),0),((4,2),0),((4,3),0),((4,4),0)]

-- > g3 = removeVertice g1 1

-- > vertices g3
-- [2,3,4,5,6]

-- > arestas g3
-- [(5,6),(4,6),(4,5),(3,4),(2,4),(2,3)]

-- > g4 = novaAresta g1 (3,6)

-- > arestas g4
-- [(5,6),(4,6),(4,5),(3,6),(3,4),(2,4),(2,3),(1,4)]

-- > g5 = novoVertice g1 7

-- > vertices g5
-- [1,2,3,4,5,6,7]

-- > g6 = removeAresta g1 (3,4)

-- > arestas g6
-- [(5,6),(4,6),(4,5),(2,4),(2,3),(1,4)]
