{-

Teoria dos Grafos - Operações e Algoritmos de Grafos
Felipe Daniel Dias dos Santos - 11711ECP004
Graduação em Engenharia de Computação - Faculdade de Engenharia Elétrica - Universidade Federal de Uberlândia

-}

module Grafo(
    
    eSimples,
    eTrivial,
    eIsolado,
    eTerminal,
    ePar,
    eImpar,
    seqGraus,
    grauMax,
    grauMin,
    eRegular,
    eKRegular,
    eVazio,
    eNulo,
    eCompleto,
    eKn,
    grafoCompleto,
    grafoComplemento,
    eSubgrafo,
    eSubgrafoProprio,
    eSubgrafoInduzidoVertices,
    eSubgrafoInduzidoArestas,
    eClique,
    eCjIndependenteVertices,
    uniao,
    intersecao,
    soma,
    fundeVertices,
    contraiVertices,
    contraiAresta,
    ePasseio,
    ePasseioAberto,
    ePasseioFechado,
    eTrilha,
    eCaminho,
    eCiclo,
    eGrafoCiclico,
    eCn,
    eGrafoCaminho,
    ePn,
    eGrafoBipartido,
    eTrilhaEuleriana,
    eGrafoEuleriano,
    eSemiEuleriano,
    eHamiltonianoOre,
    eHamiltonianoDirac,
    fecho,
    buscaGenerica,
    buscaLargura,
    buscaProfundidade,
    menorCaminho,
    dijkstra,
    distancia,
    excentricidade,
    raio,
    diametro,
    centro,
    eDisjuntoVertices,
    eEstrela,
    eFolha,
    eBinaria,
    eBinariaEstrita,
    eArvMaria,
    codPrufer,
    decodPrufer,
    eSeqValida,
    numArvoresRot,
    coberturaVertices
    
)where

import qualified GrafoMatriz as M (Grafo, vertices, vizinhos, matrizAdj)
import GrafoLista
import Data.List
import Data.Array.ST
import Data.Array.Unboxed

--Seção de funções auxiliares

--Função que retorna o fatorial de um número n
fatorial :: Int -> Int
fatorial 0 = 1
fatorial n = product [1..n]

--Função que verifica se todos os elementos de uma lista xs são iguais
iguais :: [Int] -> Bool
iguais [] = False
iguais xs = filter (== head xs) xs == xs

--Função que retorna o índice de um elemento y em uma lista xs
procuraIndice :: [Int] -> Int -> Int
procuraIndice (x:xs) y | y == x = 0
                       | otherwise = 1 + procuraIndice xs y
                       
--Função que realiza uma atribuição de um valor y na posição ind em uma lista xs
atribuicao :: [Int] -> Int -> Int -> [Int]
atribuicao xs ind y = before ++ [y] ++ after where
    (before, _:after) = splitAt ind xs

--Função que retorna a diferença entre dois conjuntos a e b
diferenca :: [Int] -> [Int] -> [Int]
diferenca a b = [x | x <- a, elem x a, notElem x b]

--Função que retorna o elemento de uma lista c que é igual ao índice do elemento de menor valor de uma lista d
menor :: [Int] -> [Int] -> Int
menor d c | elem v c = v
          | otherwise = menor (atribuicao d ind 9999) c where
            v = ind + 1
            ind = procuraIndice d (minimum d)

--Função que verifica se um conjunto de vértices vh pertence a um conjunto de vértices vg
pertenceVertices :: [Int] -> [Int] -> Bool
pertenceVertices [] _ = True
pertenceVertices vh [] = False
pertenceVertices (v:vs) vg = elem v vg && pertenceVertices vs vg

--Função que verifica se um conjunto de arestas ah pertence a um conjunto de arestas ag
pertenceArestas :: [(Int, Int)] -> [(Int, Int)] -> Bool
pertenceArestas [] _ = True
pertenceArestas ah [] = False
pertenceArestas (a:as) ag = (elem a ag || elem y ag) && pertenceArestas as ag where
    y = (snd a, fst a)

--Função que verifica se um conjunto de vértices vs é formado por elementos distintos
verticesDistintos :: [Int] -> Bool
verticesDistintos [] = True
verticesDistintos (v:vs) = notElem v vs && verticesDistintos vs

--Função que verifica se um conjunto de aresta as é formado por elementos distintos
arestasDistintas :: [(Int, Int)] -> Bool
arestasDistintas [] = True
arestasDistintas (a:as) = notElem a as && notElem y as && arestasDistintas as where 
    y = (snd a, fst a)

--Função que remove um conjunto de vértices vs de um grafo g
removeVertices :: Grafo -> [Int] -> Grafo
removeVertices g [] = g
removeVertices g [v] = removeVertice g v
removeVertices g (v:vs) = removeVertices (removeVertice g v) vs

--Função que remove um conjunto de arestas as de um grafo g
removeArestas :: Grafo -> [(Int, Int)] -> Grafo
removeArestas g [] = g
removeArestas g [a] = removeAresta g a
removeArestas g (a:as) = removeArestas (removeAresta g a) as

--Função que adiciona um conjunto de vértices vs em um grafo g
adicionaVertices :: Grafo -> [Int] -> Grafo
adicionaVertices g [] = g
adicionaVertices g [v] = novoVertice g v
adicionaVertices g (v:vs) = adicionaVertices (novoVertice g v) vs

--Função que adiciona um conjunto de arestas as de um grafo g
adicionaArestas :: Grafo -> [(Int, Int)] -> Grafo
adicionaArestas g [] = g
adicionaArestas g [a] = novaAresta g a
adicionaArestas g (a:as) = adicionaArestas (novaAresta g a) as

--Função que retorna a sequência de arestas de uma sequência de vértices vs
seqArestas :: [Int] -> [(Int, Int)]
seqArestas [] = []
seqArestas [v] = []
seqArestas (v:vs) = (v, head vs):seqArestas vs

--Seção de funções principais

--Função que verifica se um grafo g é simples
eSimples :: Grafo -> Bool
eSimples g = sort vg == sort v where
    vg = vertices g
    v = [u | u <- vg, verticesDistintos $ vizinhos g u]

--Função que verifica se um grafo g é nulo
eNulo :: Grafo -> Bool
eNulo g = length (vertices g) == 0

--Função que verifica se um grafo g é vazio
eVazio :: Grafo -> Bool
eVazio g = length (arestas g) == 0

--Função que verifica se um grafo g é trivial
eTrivial :: Grafo -> Bool
eTrivial g = length (vertices g) == 1

--Função que verifica se um vértice v de um grafo g é isolado
eIsolado :: Grafo -> Int -> Bool
eIsolado g v = grau g v == 0

--Função que verifica se um vértice v de um grafo g é terminal
eTerminal :: Grafo -> Int -> Bool
eTerminal g v = grau g v == 1

--Função que verifica se um vértice v de um grafo g é par
ePar :: Grafo -> Int -> Bool
ePar g v = even $ grau g v

--Função que verifica se um vértice v de um grafo g é ímpar
eImpar :: Grafo -> Int -> Bool
eImpar g v = odd $ grau g v

--Função que retorna a sequência de graus de um grafo g
seqGraus :: Grafo -> [Int]
seqGraus g = sort [grau g v | v <- vertices g]

--Função que retorna o grau máximo de um grafo g
grauMax :: Grafo -> Int
grauMax g | eNulo g = error "O grau máximo de um grafo nulo não é definido." 
          | otherwise = last $ seqGraus g

--Função que retorna o grau mínimo de um grafo g
grauMin :: Grafo -> Int
grauMin g | eNulo g = error "O grau mínimo de um grafo nulo não é definido."
          | otherwise = head $ seqGraus g

--Função que verifica se um grafo g é regular
eRegular :: Grafo -> Bool
eRegular g = iguais $ seqGraus g

--Função que verifica se um grafo g é k-regular
eKRegular :: Grafo -> Int -> Bool
eKRegular g k | eNulo g = False
              | otherwise = eRegular g && head (seqGraus g) == k

--Função que verifica se um grafo g é completo
eCompleto :: Grafo -> Bool
eCompleto g = eKRegular g n where
    n = length (vertices g) - 1

--Função que verifica se um grafo g é Kn
eKn :: Grafo -> Int -> Bool
eKn g n = eKRegular g (n - 1)

--Função que retorna um grafo Kn
grafoCompleto :: Int -> Grafo
grafoCompleto n = novoGrafo n a where
    a = [(u, v) | u <- [1..n], v <- [u + 1..n]]

--Função que retorna o complemento de um grafo g
grafoComplemento :: Grafo -> Grafo
grafoComplemento g = removeArestas kn (arestas g) where
    kn = grafoCompleto (length $ vertices g)

--Função que verifica se um grafo h é subgrafo de um grafo g
eSubgrafo :: Grafo -> Grafo -> Bool
eSubgrafo h g = pertenceVertices (vertices h) (vertices g) && pertenceArestas (arestas h) (arestas g)

--Função que verifica se um subgrafo h é subgrafo próprio de um grafo g
eSubgrafoProprio :: Grafo -> Grafo -> Bool
eSubgrafoProprio h g = eSubgrafo h g && h /= g

--Função que verifica se um grafo h é subgrafo induzido por um conjunto de vértices vs de um grafo g
eSubgrafoInduzidoVertices :: Grafo -> Grafo -> [Int] -> Bool
eSubgrafoInduzidoVertices h g vs = removeVertices g (diferenca vg vh) == h && sort vh == sort vs where
    vg = vertices g
    vh = vertices h 

--Função que verifica se um grafo h é subgrafo induzido por um conjunto de arestas as de um grafo g
eSubgrafoInduzidoArestas :: Grafo -> Grafo -> [(Int, Int)] -> Bool
eSubgrafoInduzidoArestas h g as = eSubgrafo h g && pertenceArestas as ah && length as == length ah where
    ah = arestas h
    
--Função que verifica se um grafo h é clique de um grafo g
eClique :: Grafo -> Grafo -> Bool
eClique h g = eSubgrafo h g && eCompleto h 

--Função que verifica se um grafo h é um conjunto independente de vértices de um grafo g
eCjIndependenteVertices :: Grafo -> Grafo -> Bool
eCjIndependenteVertices h g = eVazio h && pertenceVertices (vertices h) (vertices g)

--Função que retorna o grafo união dos grafos g e h
uniao :: Grafo -> Grafo -> Grafo
uniao g h = adicionaArestas (adicionaVertices g vh) ah where
    vh = vertices h
    ah = arestas h

--Função que retorna o grafo interseção dos grafos g e h
intersecao :: Grafo -> Grafo -> Grafo
intersecao g h = removeArestas (removeVertices g v) a where
    v = [u | u <- vertices g, notElem u (vertices h)]
    a = [b | b <- arestas g, not $ pertenceArestas [b] (arestas h)]

--Função que retorna o grafo soma dos grafos g e h
soma :: Grafo -> Grafo -> Grafo
soma g h = adicionaArestas (uniao g h) a where
    a = [(u, v) | u <- vertices g, v <- vertices h, u /= v]

--Função que retorna um grafo com a fusão de um par de vértices v e w em um grafo g
fundeVertices :: Grafo -> Int -> Int -> Grafo
fundeVertices g v w = adicionaArestas (removeVertice g u) a where
    u = max v w
    a = [(min v w, y) | y <- vizinhos g u]
    
--Função que retorna um grafo com a contração de um par de vértices v e w em um grafo g
contraiVertices :: Grafo -> Int -> Int -> Grafo
contraiVertices g v w = adicionaArestas h a where
    h = removeVertice g u
    u = max v w
    x = min v w
    a = [(x, y) | y <- vizinhos g u, x /= y, not $ pertenceArestas [(x, y)] (arestas h)]

--Função que retorna um grafo com a contração de uma aresta (v, w) em um grafo g
contraiAresta :: Grafo -> (Int, Int) -> Grafo
contraiAresta g (v, w) = fundeVertices h v w where
    h = removeAresta g (v, w)

--Função que verifica se uma lista de vértices vs é um passeio em um grafo g
ePasseio :: Grafo -> [Int] -> Bool
ePasseio _ [] = False
ePasseio g vs = pertenceVertices vs (vertices g) && pertenceArestas (seqArestas vs) (arestas g)

--Função que verifica se uma lista de vértices vs é um passeio aberto em um grafo g
ePasseioAberto :: Grafo -> [Int] -> Bool
ePasseioAberto _ [] = False
ePasseioAberto g vs = ePasseio g vs && head vs /= last vs

--Função que verifica se uma lista de vértices vs é um passeio fechado em um grafo g
ePasseioFechado :: Grafo -> [Int] -> Bool
ePasseioFechado _ [] = False
ePasseioFechado g vs = ePasseio g vs && head vs == last vs

--Função que verifica se uma lista de vértices vs é uma trilha em um grafo g
eTrilha :: Grafo -> [Int] -> Bool
eTrilha g vs = ePasseio g vs && arestasDistintas (seqArestas vs)

--Função que verifica se uma lista de vértices vs é um caminho em um grafo g
eCaminho :: Grafo -> [Int] -> Bool
eCaminho g vs = eTrilha g vs && verticesDistintos vs

--Função que verifica se uma lista de vértices vs é um ciclo em um grafo g
eCiclo :: Grafo -> [Int] -> Bool
eCiclo _ [] = False
eCiclo g vs = eCaminho g (tail vs) && head vs == last vs

--Função que verifica se um grafo c é cíclico
eGrafoCiclico :: Grafo -> Bool
eGrafoCiclico c = eKRegular c 2 

--Função que verifica se um grafo g é cíclico com n vértices
eCn :: Grafo -> Int -> Bool
eCn c n = eGrafoCiclico c && length (vertices c) == n

--Função que verifica se um grafo g é um grafo caminho
eGrafoCaminho :: Grafo -> Bool
eGrafoCaminho g = eCaminho g vg && length (seqArestas vg) == length (arestas g) where
    vg = vertices g

--Função que verifica se um grafo g é um grafo caminho com n vértices
ePn :: Grafo -> Int -> Bool
ePn p n = eGrafoCaminho p && length (vertices p) == n

--Função que verifica se um grafo g é bipartido com a partição dos vértices dada por dois conjuntos de vértices v1 e v2
eGrafoBipartido :: Grafo -> [Int] -> [Int] -> Bool
eGrafoBipartido g v1 v2 = length ag == length a where
    ag = arestas g
    a = [(u, v) | (u, v) <- ag, (elem u v1 && elem v v2) || (elem v v1 && elem u v2)]

--Função que verifica se uma trilha t é euleriana em um grafo g
eTrilhaEuleriana :: Grafo -> [Int] -> Bool
eTrilhaEuleriana g t = eTrilha g t && length (arestas g) == length (seqArestas t)

--Função que verifica se um grafo g é euleriano
eGrafoEuleriano :: Grafo -> Bool
eGrafoEuleriano g = length par == length seq where
    par = [d | d <- seq, even d]
    seq = seqGraus g

--Função que verifica se um grafo g é semi-euleriano
eSemiEuleriano :: Grafo -> Bool
eSemiEuleriano g = length ímpar == 2 where
    ímpar = [d | d <- seqGraus g, odd d]

--Função que verfica se um grafo g é hamiltoniano usando o teorema de Ore
eHamiltonianoOre :: Grafo -> Bool
eHamiltonianoOre g = n >= 3 && minimum a >= n where
    n = length vg
    vg = vertices g
    a = [grau g v + grau g w | v <- vg, w <- (diferenca vg (vizinhos g v))]

--Função que verfica se um grafo g é hamiltoniano usando o teorema de Dirac
eHamiltonianoDirac :: Grafo -> Bool
eHamiltonianoDirac g = n >= 3 && n == length a where
    n = length seq
    seq = seqGraus g
    a = [d | d <- seq, d >= div (n + 1) 2]

--Função que retorna um grafo que é o fecho de um grafo g
fecho :: Grafo -> Grafo
fecho g = adicionaArestas g a where
    a = [(v, w) | v <- vg, w <- (diferenca vg (vizinhos g v)), grau g v + grau g w >= length vg, v < w]
    vg = vertices g

--Função que retorna uma lista de vértices na sequência em que são primeiramente visitados em uma busca genérica em um grafo g
buscaGenerica :: Grafo -> [Int]
buscaGenerica g = buscaGenericaAux [head $ vertices g] (arestas g)

--Função auxiliar da busca genérica que explora todas as arestas grafo
buscaGenericaAux :: [Int] -> [(Int, Int)] -> [Int]
buscaGenericaAux vis [] = vis
buscaGenericaAux (v:vs) ag | a == [] = v:(buscaGenericaAux vs ag)
                           | otherwise = buscaGenericaAux ((v:vs) ++ y) (delete (a1, a2) ag) where
                            a = [(u, w) | (u, w) <- ag, (v == u) || (v == w), u /= w]
                            y = if elem x (v:vs) then [] else [x]
                            x = if a1 == v then a2 else a1
                            (a1, a2) = head a                            

--Função que retorna uma lista de vértices na sequência em que são primeiramente visitados em uma busca em largura em um grafo g
buscaLargura :: Grafo -> [Int]
buscaLargura g = buscaLarguraAux g [v] [v] where
    v = head $ vertices g

--Função auxiliar da busca em largura que explora todos os vértices do grafo
buscaLarguraAux :: Grafo -> [Int] -> [Int] -> [Int]
buscaLarguraAux _ vis [] = vis
buscaLarguraAux g vis (f:fs) = buscaLarguraAux g (vis ++ adj) (fs ++ adj) where
    adj = map head (group [u | u <- vizinhos g f, notElem u vis, u /= f])

--Função que retorna uma lista de vértices na sequência em que são primeiramente visitados em uma busca em profundidade em um grafo g
buscaProfundidade :: Grafo -> [Int]
buscaProfundidade g = buscaProfundidadeAux g v [v] where
    v = head $ vertices g

--Função auxiliar da busca em profundidade que explora todas os vértices grafo
buscaProfundidadeAux :: Grafo -> Int -> [Int] -> [Int]
buscaProfundidadeAux g v vis | adj == [] = vis
                             | otherwise = buscaProfundidadeAux g v (buscaProfundidadeAux g adj1 visNovo) where 
                              adj = [w | w <- vizinhos g v, notElem w vis, w /= v]
                              adj1 = head adj
                              visNovo = vis ++ [adj1]

--Função que retorna uma lista com os vértices no menor caminho entre dois vértices u e v em um grafo g
menorCaminho :: Grafo -> Int -> Int -> [Int]
menorCaminho g u v = obterCaminho (menorCaminhoAux g [u] [u] p v) u v where
    p = [-1 | x <- [1..n]]
    n = length $ vertices g

--Função auxiliar do menor caminho que explora todos os vértices do grafo até encontrar o destino v
menorCaminhoAux :: Grafo -> [Int] -> [Int] -> [Int] -> Int -> [Int]
menorCaminhoAux _ _ [] p _ = p
menorCaminhoAux g vis (f:fs) p v | f == v = p
                                 | otherwise = menorCaminhoAux g (vis ++ adj) (fs ++ adj) (atualizaCaminho p f adj) v where
                                  adj = [u | u <- vizinhos g f, notElem u vis]

--Função auxiliar do menor caminho que atualiza o vetor de predecessores p
atualizaCaminho :: [Int] -> Int -> [Int] -> [Int]
atualizaCaminho p _ [] = p
atualizaCaminho p v (u:us) = atualizaCaminho pNovo v us where
    pNovo = atribuicao p (u - 1) v

--Função auxiliar do menor caminho que obtêm o caminho entre dois vértices u e v a partir do vetor de predecessores p
obterCaminho :: [Int] -> Int -> Int -> [Int]
obterCaminho p u v | pred == -1 = [u]
                   | otherwise = (obterCaminho p u pred) ++ [v] where
                    pred = p !! (v - 1)

--Função que retorna um par (d, p) de vetores contendo em d as menores distâncias de um vértice v até qualquer outro vértice de um grafo g e em p os predecessores com a utilização do algoritmo de Dijkstra
dijkstra :: M.Grafo -> Int -> ([Int], [Int])
dijkstra g v = percorreVértices g (M.matrizAdj g) c (d, p) where
    c = M.vertices g
    n = length $ c
    d = atribuicao ([9999 | x <- [1..n]]) (v - 1) 0
    p = [-1 | x <- [1..n]]

--Função auxiliar do algoritmo de Dijkstra que percorre todos os vértices do grafo
percorreVértices :: M.Grafo -> UArray (Int, Int) Int -> [Int] -> ([Int], [Int]) -> ([Int], [Int])
percorreVértices _ _ [] (d, p) = (d, p)
percorreVértices g m c (d, p) = percorreVértices g m (delete u c) dpNovo where
    u = menor d c 
    dpNovo = percorreVizinhos m (M.vizinhos g u) (d, p) u

--Função auxiliar do algoritmo de Dijkstra que percorre todos os vizinhos do vértice corrente
percorreVizinhos :: UArray (Int, Int) Int -> [Int] -> ([Int], [Int]) -> Int -> ([Int], [Int])  
percorreVizinhos _ [] (d, p) _ = (d, p)
percorreVizinhos m (w:ws) (d, p) u | d !! (w - 1) > dist = percorreVizinhos m ws (dNovo, pNovo) u 
                                   | otherwise = percorreVizinhos m ws (d, p) u where
                                    dist = d !! (u - 1) + m ! (u, w)
                                    dNovo = atribuicao d (w - 1) dist
                                    pNovo = atribuicao p (w - 1) u

--Função que retorna a distância entre os vértices u e v em um grafo g
distancia :: M.Grafo -> Int -> Int -> Int
distancia g u v = (fst $ dijkstra g u) !! (v - 1)

--Função que retorna a excentricidade de um vértice v em um grafo g
excentricidade :: M.Grafo -> Int -> Int
excentricidade g v = maximum (fst $ dijkstra g v)

--Função que retorna o raio de um grafo g
raio :: M.Grafo -> Int
raio g = minimum exc where
    exc = [excentricidade g v | v <- M.vertices g]

--Função que retorna o diâmetro de um grafo g
diametro :: M.Grafo -> Int
diametro g = maximum exc where
    exc = [excentricidade g v | v <- M.vertices g] 

--Função que retorna o centro de um grafo g
centro :: M.Grafo -> [Int]
centro g = c where
    c = [vc | vc <- vg, exc !! (vc - 1) == raio g]
    exc = [excentricidade g v | v <- vg] 
    vg = M.vertices g

--Função que verifica se dois caminhos c1 e c2 são internamente disjuntos em vértíces em um grafo g
eDisjuntoVertices :: Grafo -> [Int] -> [Int] -> Bool
eDisjuntoVertices g c1 c2 = eCaminho g c1 && eCaminho g c2 && diferenca c1 c2 == c1

--Função que verifica se um grafo g é um grafo estrela
eEstrela :: Grafo -> Bool
eEstrela g = eSimples g && centro == 1 && pontas == n where
    pontas = length [u | u <- vg, grau g u == 1] 
    centro = length [u | u <- vg, grau g u == n] 
    n = (length vg) - 1
    vg = vertices g

--Função que verifica se um vértice v é uma folha em uma árvore t
eFolha :: Grafo -> Int -> Bool
eFolha t v = elem v (vertices t) && grau t v <= 1

--Função que verifica se uma árvore t é binária
eBinaria :: Grafo -> Bool
eBinaria t = grau t v <= 2 && maximum graus <= 3 where
    (v:vt) = vertices t
    graus = [grau t u | u <- vt]

--Função que verifica se uma árvore t é estritamente binária
eBinariaEstrita :: Grafo -> Bool
eBinariaEstrita t = grau t v == 2 && length graus == length vt where
    (v:vt) = vertices t
    graus = [u | u <- vt, grau t u == 3 || grau t u == 1]

--Função que verifica se uma árvore t é m-ária
eArvMaria :: Grafo -> Int -> Bool
eArvMaria t m = grau t v <= m && maximum graus <= m + 1 where
    (v:vt) = vertices t
    graus = [grau t u | u <- vt]

--Função que retorna o código de Prufer de uma árvore t
codPrufer :: Grafo -> [Int]
codPrufer t = codPruferAux t f [] where
    f = [u | u <- vertices t, grau t u == 1]

--Função auxiliar da codificação de Prufer que percorre o conjunto de folhas
codPruferAux :: Grafo -> [Int] -> [Int] -> [Int]
codPruferAux t f c | length (vertices t) == 2 = c
                   | otherwise = codPruferAux tNovo fNovo (c ++ [u]) where
                       tNovo = removeVertice t v
                       v = minimum f
                       fNovo = (delete v f) ++ w
                       w = if grau tNovo u == 1 then [u] else []
                       u = head $ vizinhos t v

--Função que retorna a árvore de um código de Prufer c
decodPrufer :: [Int] -> Grafo
decodPrufer c = decodPruferAux c f vt [] where  
    f = filter (\x -> notElem x c) vt
    vt = [1..n]
    n = 2 + length c   

--Função auxiliar da decodificação de Prufer que percorre o código
decodPruferAux :: [Int] -> [Int] -> [Int] -> [(Int, Int)] -> Grafo
decodPruferAux [] (u:w) vt at = novoGrafo (length vt) (at ++ [(u, head w)])
decodPruferAux c f vt at = decodPruferAux cNovo fNovo vt atNovo where
    cNovo = delete u c
    u = head c
    fNovo = (delete v f) ++ w
    v = minimum f
    w = if elem u cNovo then [] else [u]
    atNovo = at ++ [(u, v)]

--Função que verifica se uma sequência de graus s para uma árvore é válida
eSeqValida :: [Int] -> Bool
eSeqValida s = sum s == 2 * (length s) - 2

--Função que retorna a quantidade de árvores rotuladas que possuem uma sequência de graus válida s
numArvoresRot :: [Int] -> Int
numArvoresRot s = fatorial (length s - 2) `div` den where
    den = product [fatorial (d - 1) | d <- s]
                              
--Função que retorna uma cobertura de vértices para um grafo g
coberturaVertices :: Grafo -> [Int]
coberturaVertices g = coberturaVerticesAux (arestas g) []

--Função auxiliar da cobertura de vértices que percorre o conjunto de arestas do grafo
coberturaVerticesAux :: [(Int, Int)] -> [Int] -> [Int]
coberturaVerticesAux [] k = k
coberturaVerticesAux (a:as) k = coberturaVerticesAux as (v ++ k) where
    v = [u | notElem u k, notElem w k]
    (u, w) = a
