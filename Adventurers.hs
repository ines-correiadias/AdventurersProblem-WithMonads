-- Group project developed for the Cyber-Physical course, whose goal was to model and analyse a cyber-physical system using monads in Haskell.
-- Note that the project consisted in completing a code previously provided to us by the Professor of the course. Said code
-- corresponds to file StartingPoint_Adventurers.hs, which you can find in this same repository.

{-# LANGUAGE FlexibleInstances #-}

module Adventurers where

import DurationMonad -- this module was provided to us by the Professor of the course and can be found in DurationMonad.hs, in this same repository.


-- The list of adventurers
data Adventurer = P1 | P2 | P5 | P10 deriving (Show,Eq)
-- Adventurers(=Left P_) + the lantern(=Right ())
type Objects = Either Adventurer ()


-- The time that each adventurer needs to cross the bridge
-- To implement 
getTimeAdv :: Adventurer -> Int
getTimeAdv adv = if (adv == (P1)) then 1 else 
                 if (adv == (P2)) then 2 else 
                 if (adv == (P5)) then 5 else 10


-- Remove o ultimo elemento de uma lista                
-- Se fornecermos a lista de objectos, remove a lampada(=lanterna) dos objetos
removeUlt :: [a] -> [a]
removeUlt [x] = []
removeUlt (h:t) = h:(removeUlt (t))

-- Dado um objeto Left Px, pega apenas no Px(aventureiro)
retiraLeft :: Objects -> Adventurer
retiraLeft (Left x) = x


--Retira a lampada
retLamp :: [Objects] -> [Objects]
retLamp [] = []
retLamp ((Left x): t) = (Left x): retLamp t
retLamp ((Right ()): t) = retLamp t

-- Pega na lista do objectos e fornece a lista dos aventureiros correspondente(sem a lampada porque a lampada não é um aventureiro)
-- [Left P1, Left P2, Left P5, Right()] -> dá-nos [P1,P2,P5]
fromObjectsToAdv :: [Objects] -> [Adventurer]
fromObjectsToAdv [] = []
fromObjectsToAdv l = map retiraLeft (retLamp l)

-- Dada uma lista com os aventureiros [P1,P2,P10], devolve o tempo do aventureiro que demora mais tempo. Neste caso, P10 é o aventureiro que demora mais tempo, logo é retornado o valor 10
getAdvMax :: [Adventurer] -> Int
getAdvMax [] = error "Erro: Forneceu uma lista de aventureiros vazia"
getAdvMax l = maximum (map getTimeAdv l)


-- Dada uma lista de objectos dá-nos o tempo do aventureiro mais lento
getObjMax :: [Objects] -> Int
getObjMax [] = error "Erro: Forneceu uma lista de objectos vazia"
getObjMax l = getAdvMax (fromObjectsToAdv l)




{-- The state of the game, i.e. the current position of each adventurer
+ the lantern. The function (const False) represents the initial state
of the game, with all adventurers and the lantern on the left side of
the bridge. Similarly, the function (const True) represents the end
state of the game, with all adventurers and the lantern on the right
side of the bridge.  --}
type State = Objects -> Bool

instance Show State where
  show s = (show . (fmap show)) [s (Left P1),
                                 s (Left P2),
                                 s (Left P5),
                                 s (Left P10),
                                 s (Right ())]


instance Eq State where
  (==) s1 s2 = and [s1 (Left P1) == s2 (Left P1),
                    s1 (Left P2) == s2 (Left P2),
                    s1 (Left P5) == s2 (Left P5),
                    s1 (Left P10) == s2 (Left P10),
                    s1 (Right ()) == s2 (Right ())]



-- The initial state of the game
-- const False é uma função
-- const False :: b -> Bool(que será sempre False independentemente do valor que fornecermos à função)
gInit :: State
gInit = const False

--Quando todos os objectos estiverem no lado direito, temos o estado gFinal
gFinal :: State
gFinal = const True


-- Changes the state of the game for a given object
changeState :: Objects -> State -> State
changeState a s = let v = s a in (\x -> if x == a then not v else s x)



-- Changes the state of the game of a list of objects 
mChangeState :: [Objects] -> State -> State
mChangeState os s = foldr changeState s os


objectos = [Left P1, Left P2, Left P5, Left P10, Right()]


-- Pega num estado e devolve uma lista de Bool's
stateToList :: State -> [Bool]
stateToList s = map s objectos 

--Exemplo:
--Adventurers> newState = mChangeState [(Left P1), (Left P5), (Right())] gInit
--Adventurers> stateToList newState
--[True,False,True,False,True]

-- Pegar no ultimo valor de uma lista
ult :: [a] -> a
ult [x] = x
ult (h:t) = ult t


-- A função recebe um Bool1 e uma lista de pares (Bool2,Objects) e devolve os objectos que têm Bool2 = Bool1
aux2 :: Bool ->[(Bool,Objects)] -> [Objects]
aux2 b [] = [] 
aux2 b ((bx,obj):t) = if (b==bx) then obj:(aux2 b t) else (aux2 b t)

-- dá-nos os jogadores que têm o mesmo nivel logico que a lampada
-- temos de nos certificar que a lista de Bools e a de objectos vem ordenada, ou seja, o primeiro elemento da lista de Bools tem o booleano correspondente do primeiro elemento da lista de Objects, ...
fromlistBoolToPlayers :: [Bool] -> [Objects] -> [Objects]
fromlistBoolToPlayers lb obj = if (len(lb) /= len(obj)) then error "Erro: As listas tem diferentes comprimentos" else aux2 (ult(lb)) (zip (removeUlt(lb)) (removeUlt(obj)))


-- Pega num estado e fornece os objectos(Left P1,...) que têm o mesmo nivel logico que a lampada 
fromStateToPlayers :: State -> [Objects]
fromStateToPlayers s = let lb = (stateToList s) in (fromlistBoolToPlayers lb objectos)




-- calcular o comprimento de uma lista
len :: [a] -> Int
len [] = 0
len (h:t) = 1 + len (t)



-- recebe uma lista de pares (Int,State) e devolve uma lista de Durations
transfDur :: [(Int,State)] -> [Duration State]
transfDur [] = []
transfDur [(x,y)] = [Duration (x,y)]
transfDur ((x,y):t) = (Duration (x,y)) : (transfDur t)



--Adiciona a lampada à lista de objectos
addLamp :: [Objects] -> [Objects]
addLamp l = l ++ [Right ()]



-- faz o zip da lista Int com uma lista de States
--nossoZip :: [Int] -> [State] -> [(Int,State)]
nossoZip :: [a] -> [b] -> [(a,b)]
nossoZip [i] [s] = [(i,s)]
nossoZip (i:t) (s1:s) = (i,s1) : (nossoZip t s)



--- Calcula uma lista de todos os pares que um objeto x pode fazer com os elementos da lista de Objects
pair:: a -> [a] -> [[a]]
pair x [y] = [[x,y]]
pair x (h:t) = [x,h] : (pair x t)

-- combinations n l dá uma lista de combinaçoes possiveis de n elementos formadas a partir de l, com n=1 ou 2
combinations :: Int -> [a] -> [[a]]
combinations 1 [h] = [[h]]
combinations 1 (h:t)= [h] : (combinations 1 t)
combinations 2 [x,y]= [[x,y]]
combinations 2 (h:t) = (pair h t) ++ (combinations 2 t)


--A partir de uma lista de Objects (neste caso Left Px), cria uma lista com todas as combinações possíveis de adventurers que podem atravessar a ponte
-- Apenas podemos ter combinações de 1 ou 2 elementos
todasCombinacoes :: [a] -> [[a]]
todasCombinacoes [x] = combinations 1 [x]
todasCombinacoes(h:t) = (combinations 1 (h:t)) ++ (combinations 2 (h:t))



-- Pega numa lista de duracoes e fornece LD [Duration State]
addLD :: [Duration State] -> ListDur State
addLD x = LD x



{-- For a given state of the game, the function presents all the
possible moves that the adventurers can make.  --}
-- To implement
allValidPlays :: State -> ListDur State
allValidPlays s = let listaSemLampada = fromStateToPlayers s -- Dado um estado, obtemos a lista de objectos sem a lampada que estao do mesmo lado que a lampada
                      lista_os = todasCombinacoes(listaSemLampada) -- cria uma lista com todas as combinações possíveis de adventurers que podem atravessar a ponte
                      lista_os_ComLamp = map addLamp lista_os -- adiciona a lampada a todas as combinacoes porque os aventureiros apenas podem atravessar a ponte com a lampada
                      newstates = map (\x -> (mChangeState x s)) lista_os_ComLamp --obter os novos estados possiveis resultantes da movimentacao dos objectos
                      times = map (getObjMax) (lista_os) -- Obter os tempos de cada combinacao, por exemplo , lista_os = [[Left P1, Left P2], [Left P1, Left P10],...] vai dar times = [2,10,....]           
                  in addLD (transfDur (nossoZip times newstates)) -- Com o zip obtemos lista = [(time1,state1),(time2,state2),...]. Ao fazer transfDur sobre isto obtemos [Duration (time1,state1),Duration (time2,state2),...]. No fim adicionamos LD, ou seja, LD [Duration (time1,state1),Duration (time2,state2),...].



-- Funcao identidade
identidade :: a -> a
identidade x = x


{-- For a given number n and initial state, the function calculates
all possible n-sequences of moves that the adventures can make --}
exec :: Int -> State -> ListDur State
exec 0 s = return s -- = LD [Duration (0,s)]
exec n s = LD $ do x <- remLD (exec (n-1) s)
                   g x where
                     g (Duration (t,s)) = let listaDur = (remLD (allValidPlays (s))) -- listaDur = [Duration (...), Duration(...),...]
                                              listaDur_nova = l1 <*> l2 where l1 = LD [Duration (t,identidade)]
                                                                              l2 = LD (listaDur)
                                           in remLD (listaDur_nova)  




-- Verifica se todos os objectos do estado "s" estão no lado direito
allObjectsNaDireita :: State -> Bool
allObjectsNaDireita s = if ((stateToList s) == [True,True,True,True,True]) then True else False


-- Se um Duration(t,s) tem um tempo<=17 e no estado "s" todos os objectos estão na direita retorna True
filtra17NaDireita :: Duration State -> Bool
filtra17NaDireita (Duration (t,s)) = if (t<=17 && (allObjectsNaDireita s)) then True else False


-- Se um Duration(t,s) tem um tempo<17 e no estado "s" todos os objectos estão na direita retorna True
filtraMenor17NaDireita :: Duration State -> Bool
filtraMenor17NaDireita (Duration (t,s)) = if (t<17 && (allObjectsNaDireita s)) then True else False


{-- Is it possible for all adventurers to be on the other side
in <=17 min and not exceeding 5 moves ? --}
leq17Aux :: Int -> Bool
leq17Aux 0 = False
leq17Aux n = let listaDur = remLD (exec n gInit)
                 lista17 = filter filtra17NaDireita listaDur
              in ((if (len (lista17) == 0) then False else True) || leq17Aux (n-1))


-- Esta funcao diz-nos se é possivel todos os objectos estarem no lado direito num tempo <=17 com um numero de passos máximo = 5.
-- Se colocarmos numero de passos = 6, vai continuar a dar True porque é possível estarem todos os objectos no lado direito em 5 passos.
leq17 :: Bool
leq17 = leq17Aux 5



--Apenas guarda os casos em que o t<17
listaComMenor17 :: [Duration State] -> [Duration State]
listaComMenor17 [] = []
listaComMenor17 (Duration (t,s):l) = if (t<17) then (Duration (t,s) : (listaComMenor17 l)) else (listaComMenor17 l)



-- diz se há algum caso em que todos os objectos estão no lado direito em menos de 17 min
allRightLess17 :: [Duration State] -> Bool
allRightLess17 [] = False
allRightLess17 (Duration (t,s):l) = if (allObjectsNaDireita s == True && t<17) then True else (allRightLess17 l)




execAux :: State -> ListDur State
execAux s = exec 1 s



--l17Aux l retorna True se algum dos próximos movimentos (com 1 passo) possíveis (a partir da lista l) resultar num Duration State para o qual o tempo é <17 e o os objects estão todos à direita. Se não houver, vai ver se nos próximos movimentos possíveis há algum Duration State que cumpra esses requisitos, e dá False quando todos os próximos movimentos corresponderem a uma Duration com tempo >=17 
l17Aux :: [Duration State] -> Bool
l17Aux [] = False
l17Aux l = let lista_dur = remLD ((LD l) >>= execAux)
               lista_dur_menores17 = listaComMenor17 lista_dur -- pegar apenas nos Durations States que têm um tempo menor que 17 
            in if (allRightLess17(lista_dur_menores17)) then True else (l17Aux lista_dur_menores17) -- Primeiro observamos se há algum caso com o tempo <17 e com todos os objectos no lado direito . 
                                                                                                    --Se existir retornamos True porque é possivel estarem todos os elementos na direita com t<17. 
                                                                                                    --Se nao for possivel e a lista não for vazia, vamos observar se é possível com mais um passo(daí termos "exec 1 s"). Se a lista for vazia significa que não é possível e é retornado Falso.
               


l17 :: Bool
l17 = l17Aux (remLD (return gInit)) -- remLD (return gInit) = [Duration(0,gInit)]



--------------------------------- Fazer para l18 ------------------------
--Observação: Estas funções são exatamente iguais às anteriores e apenas serviram para verificar se é possível estarem todos os objectos no lado direito em menos de 18min

listaComMenor18 :: [Duration State] -> [Duration State]
listaComMenor18 [] = []
listaComMenor18 (Duration (t,s):l) = if (t<18) then (Duration (t,s) : (listaComMenor18 l)) else (listaComMenor18 l)


allRightLess18 :: [Duration State] -> Bool
allRightLess18 [] = False
allRightLess18 (Duration (t,s):l) = if (allObjectsNaDireita s == True && t<18) then True else (allRightLess18 l)


l18Aux :: [Duration State] -> Bool
l18Aux [] = False
l18Aux l = let lista_dur = remLD ((LD l) >>= execAux)
               lista_dur_menores18 = listaComMenor18 lista_dur -- pegar apenas nos casos que têm um tempo menor que 18
            in if (allRightLess18(lista_dur_menores18)) then True else (l18Aux lista_dur_menores18)  

l18 :: Bool
l18 = l18Aux (remLD (return gInit))



--------------------------------------------------------------------------

data ListDur a = LD [Duration a] deriving Show


remLD :: ListDur a -> [Duration a]
remLD (LD x) = x


instance Functor ListDur where
   fmap f = let f' = \(Duration (t,x)) -> (Duration (t, f x)) in
                    LD . (map f'). remLD


instance Applicative ListDur where
   pure x = LD [Duration (0,x)]
   l1 <*> l2 = LD $ do x <- remLD l1
                       y <- remLD l2
                       g(x,y) where
                         g((Duration (t1,f)), (Duration (t2,s))) = return (Duration (t1+t2, (f s)))


instance Monad ListDur where
   return = pure 
   l >>= k = LD $ do x <- remLD l
                     g x where
                      g (Duration (t,s)) = let u = (remLD (k s)) in map (\(Duration (t',s')) ->  (Duration (t+t',s'))) u



manyChoice :: [ListDur a] -> ListDur a
manyChoice = LD . concat . (map remLD)


