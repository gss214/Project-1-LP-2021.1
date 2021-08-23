module UnBCare where

import ModeloDados

{-

██╗░░░██╗███╗░░██╗██████╗░  ░█████╗░░█████╗░██████╗░██████╗
██║░░░██║████╗░██║██╔══██╗  ██╔══██╗██╔══██╗██╔══██╗██╔════╝
██║░░░██║██╔██╗██║██████╦╝  ██║░░╚═╝███████║██████╔╝█████╗░░
██║░░░██║██║╚████║██╔══██╗  ██║░░██╗██╔══██║██╔══██╗██╔══╝░░
╚██████╔╝██║░╚███║██████╦╝  ╚█████╔╝██║░░██║██║░░██║███████╗
░╚═════╝░╚═╝░░╚══╝╚═════╝░  ░╚════╝░╚═╝░░╚═╝╚═╝░░╚═╝╚══════╝

 
 
O objetivo desse trabalho é fornecer apoio ao gerenciamento de cuidados a serem prestados a um paciente.
O paciente tem um receituario médico, que indica os medicamentos a serem tomados com seus respectivos horários durante um dia.
Esse receituário é organizado em um plano de medicamentos que estabelece, por horário, quais são os remédios a serem
tomados. Cada medicamento tem um nome e uma quantidade de comprimidos que deve ser ministrada.
Um cuidador de plantão é responsável por ministrar os cuidados ao paciente, seja ministrar medicamento, seja comprar medicamento.
Eventualmente, o cuidador precisará comprar medicamentos para cumprir o plano.
O modelo de dados do problema (definições de tipo) está disponível no arquivo ModeloDados.hs
Defina funções que simulem o comportamento descrito acima e que estejam de acordo com o referido
modelo de dados.

-}


{-

   QUESTÃO 1, VALOR: 1,0 ponto

Defina a função "comprarMedicamento", cujo tipo é dado abaixo e que, a partir de um medicamento, uma quantidade e um
estoque inicial de medicamentos, retorne um novo estoque de medicamentos contendo o medicamento adicionado da referida
quantidade. Se o medicamento já existir na lista de medicamentos, então a sua quantidade deve ser atualizada no novo estoque.
Caso o remédio ainda não exista no estoque, o novo estoque a ser retornado deve ter o remédio e sua quantidade como cabeça.

-}

{-
Função checkMedicine responsavel por verificar se ha medicamento, se nao ha adicionar no inicio
-}

checkMedicine :: Medicamento -> EstoqueMedicamentos -> Bool 
checkMedicine _ [] = False  
checkMedicine med ((m,q):as)
      | med == m = True 
      | otherwise = checkMedicine med as 

comprarMedicamento :: Medicamento -> Quantidade -> EstoqueMedicamentos -> EstoqueMedicamentos
comprarMedicamento med qnt [] = [(med,qnt)] 
comprarMedicamento med qnt ((m,q):as) 
                  | not (checkMedicine med ((m,q):as)) = (med,qnt) : (m,q):as
                  | m == med = (med, q + qnt) : as
                  | otherwise = (m,q) : comprarMedicamento med qnt as

{-
   QUESTÃO 2, VALOR: 1,0 ponto

Defina a função "tomarMedicamento", cujo tipo é dado abaixo e que, a partir de um medicamento e de um estoque de medicamentos,
retorna um novo estoque de medicamentos, resultante de 1 comprimido do medicamento ser ministrado ao paciente.
Se o medicamento não existir no estoque, Nothing deve ser retornado. Caso contrário, deve se retornar Just v,
onde v é o novo estoque.

-}

tomarMedicamento :: Medicamento -> EstoqueMedicamentos -> Maybe EstoqueMedicamentos
tomarMedicamento med [] = Nothing 
tomarMedicamento med ((m,q):as)
               | m == med && q-1 >= 0 = Just ((m, q - 1) : as)
               | otherwise = tomarMedicamento med as


{-
   QUESTÃO 3  VALOR: 1,0 ponto

Defina a função "consultarMedicamento", cujo tipo é dado abaixo e que, a partir de um medicamento e de um estoque de
medicamentos, retorne a quantidade desse medicamento no estoque.
Se o medicamento não existir, retorne 0.

-}

consultarMedicamento :: Medicamento -> EstoqueMedicamentos -> Quantidade
consultarMedicamento _ [] = 0
consultarMedicamento med ((m,q):as)
                  | med == m = q
                  | otherwise = consultarMedicamento med as


{-
   QUESTÃO 4  VALOR: 1,0 ponto

  Defina a função "demandaMedicamentos", cujo tipo é dado abaixo e que computa a demanda de todos os medicamentos
  por um dia a partir do receituario. O retorno é do tipo EstoqueMedicamentos e deve ser ordenado lexicograficamente
  pelo nome do medicamento.

  Dica: Observe que o receituario lista cada remédio e os horários em que ele deve ser tomado no dia.
  Assim, a demanda de cada remédio já está latente no receituario, bastando contar a quantidade de vezes que cada remédio
  é tomado.

-}

demandaMedicamentos :: Receituario -> EstoqueMedicamentos
demandaMedicamentos [] = []
demandaMedicamentos ((m,h):as) = (m, length h) : demandaMedicamentos as

{-
   QUESTÃO 5  VALOR: 1,0 ponto, sendo 0,5 para cada função.

 Um receituário é válido se, e somente se, todo os medicamentos são distintos e estão ordenados lexicograficamente e,
 para cada medicamento, seus horários também estão ordenados e são distintos.

 Inversamente, um plano de medicamentos é válido se, e somente se, todos seus horários também estão ordenados e são distintos,
 e para cada horário, os medicamentos são distintos e são ordenados lexicograficamente.

 Defina as funções "receituarioValido" e "planoValido" que verifiquem as propriedades acima e cujos tipos são dados abaixo:

 -}

{-
A função isSorted verifica se uma lista está ordenada e tem elementos distintos
-}

isSorted :: (Ord a) => [a] -> Bool
isSorted [] = True
isSorted [x] = True
isSorted (x:y:xs) =  (x < y) && isSorted (y:xs)

{-
A função AllDiferent verifica se os medicamentos num receituario são ordenados e dinstintos
-}

allDifferent :: Receituario -> Bool 
allDifferent [] = True 
allDifferent ((med, horarios):tail)
            | not (isEqual (med, horarios) tail) = False 
            | otherwise = allDifferent tail

{-
A função isEqual verifica se tem alguma ocorrência duplicada de um medicamento em um receituario
-}

isEqual :: (String, [Int]) -> Receituario  -> Bool 
isEqual _ [] = True 
isEqual (a,b) ((c,d):as)
            | a == c = False 
            | otherwise = isEqual (a,b) as

receituarioValido :: Receituario -> Bool
receituarioValido n 
            | isSorted n && allDifferent n && length (filter isSorted [horarios | (_,horarios) <- n]) == length [horarios | (_,horarios) <- n] = True 
            | otherwise = False 

planoValido :: PlanoMedicamento -> Bool
planoValido n 
         | isSorted n && length (filter isSorted [med | (_, med) <- n]) == length [med | (_, med) <- n] = True 
         | otherwise = False 

{-

   QUESTÃO 6  VALOR: 1,0 ponto,

 Um plantão é válido se, e somente se, todas as seguintes condições são satisfeitas:

 1. Os horários da lista são distintos e estão em ordem crescente;
 2. Não há, em um mesmo horário, ocorrência de compra e medicagem de um mesmo medicamento (e.g. `[Comprar m1, Medicar m1 x]`);
 3. Para cada horário, as ocorrências de Medicar estão ordenadas lexicograficamente.

 Defina a função "plantaoValido" que verifica as propriedades acima e cujo tipo é dado abaixo:

 -}

{-
Função checkMedication responsavel por chegar se há ocorrência de compra e medicagem de um mesmo medicamento 
-}

checkMedication :: Medicamento -> [Cuidado] -> Bool 
checkMedication _ [] = False 
checkMedication med (Medicar m:xs)
               | m == med = True 
               | otherwise = checkMedication med xs
checkMedication med (Comprar m q:xs)
               | m == med = True 
               | otherwise = checkMedication med xs

{-
Função validMedication responsavel por validar a lista de cuidados, 
para que não tenha um medicamento com ocorrência de compra e medicagem
-}

validMedication :: [Cuidado] -> Bool 
validMedication [] = True  
validMedication (Comprar m q:xs) 
               | checkMedication m xs = False 
               | otherwise = validMedication xs
validMedication (Medicar m:xs)
               | checkMedication m xs = False 
               | otherwise = validMedication xs

{-
Função getMedication responsavel por criar uma lista de medicamentos
dada uma lista de cuidado
-}

getMedication :: [Cuidado] -> [Medicamento]
getMedication [] = []
getMedication (Medicar m:xs) = m : getMedication xs
getMedication (Comprar m q:xs) = getMedication xs

{-
Função validMedicationLexi valida se para cada horário, as ocorrências
de Medicar estão ordenadas lexicograficamente
-}

validMedicationLexi :: [[Cuidado]] -> Bool 
validMedicationLexi [] = True 
validMedicationLexi (x:xs)
                  | isSorted (getMedication x) = validMedicationLexi xs 
                  | otherwise = False 

plantaoValido :: Plantao -> Bool
plantaoValido n 
            | isSorted [horarios | (horarios, _) <- n] && length (filter validMedication ([cuidados | (_, cuidados) <- n])) == length [cuidados | (_, cuidados) <- n] && validMedicationLexi [m | (_,m) <- n] = True 
            | otherwise = False 


{-
   QUESTÃO 7  VALOR: 1,0 ponto

  Defina a função "geraPlanoReceituario", cujo tipo é dado abaixo e que, a partir de um receituario válido,
  retorne um plano de medicamento válido.

  Dica: enquanto o receituário lista os horários que cada remédio deve ser tomado, o plano de medicamentos  é uma
  disposição ordenada por horário de todos os remédios que devem ser tomados pelo paciente em um certo horário.

-}

{-
Função quick sort para ordernar a lista de horários 
-}
quickSort :: [Horario] -> [Horario]
quickSort [] = []
quickSort (x:xs) = quickSort [e | e <- xs, e < x] ++ [x] ++ quickSort [e | e <- xs, e > x]


{-
Função getSchedules responsavel por pegar a lista de horarios dado um 
receituario
-}

getSchedules :: Receituario -> [Horario]
getSchedules [] = []
getSchedules ((_, horarios):xs) = [h | h <- horarios] ++ getSchedules xs 

{-
Função checkSchedule responsavel por checar se um horario esta em uma lista de horarios
-}

checkSchedule :: Horario -> [Horario] -> Bool 
checkSchedule _ [] = False 
checkSchedule h (x:xs)
   | h == x = True 
   | otherwise = checkSchedule h xs

{-
Função assembleMedicine responsavel por juntar os medicamentos em uma lista dado o horario e o receituario, ela verifica se o horario está na lista de horarios de cada medicamento do receituario
-}

assembleMedicine :: Horario -> Receituario -> [Medicamento]
assembleMedicine _ [] = []
assembleMedicine h ((m,ho):ys) 
   | checkSchedule h ho = m : assembleMedicine h ys
   | otherwise = assembleMedicine h ys

{-
Função assemblePlan responsavel por criar o plano medico dado um receituario e uma lista de horarios
-}

assemblePlan :: Receituario -> [Horario] -> PlanoMedicamento 
assemblePlan _ [] = []
assemblePlan ((m,ho):ys) (h:hs) = (h, assembleMedicine h ((m,ho):ys)) : assemblePlan ((m,ho):ys) hs 

geraPlanoReceituario :: Receituario -> PlanoMedicamento
geraPlanoReceituario n = assemblePlan n (quickSort (getSchedules n))

{- QUESTÃO 8  VALOR: 1,0 ponto

 Defina a função "geraReceituarioPlano", cujo tipo é dado abaixo e que retorna um receituário válido a partir de um
 plano de medicamentos válido.
 Dica: Existe alguma relação de simetria entre o receituário e o plano de medicamentos? Caso exista, essa simetria permite
 compararmos a função geraReceituarioPlano com a função geraPlanoReceituario ? Em outras palavras, podemos definir
 geraReceituarioPlano com base em geraPlanoReceituario ?

-}

geraReceituarioPlano :: PlanoMedicamento -> Receituario
geraReceituarioPlano = undefined


{-  QUESTÃO 9 VALOR: 1,0 ponto

Defina a função "executaPlantao", cujo tipo é dado abaixo e que executa um plantão válido a partir de um estoque de medicamentos,
resultando em novo estoque. A execução consiste em desempenhar, sequencialmente, todos os cuidados para cada horário do plantão.
Caso o estoque acabe antes de terminar a execução do plantão, o resultado da função deve ser Nothing. Caso contrário, o resultado 
deve ser Just v, onde v é o valor final do estoque de medicamentos

-}

executaPlantao :: Plantao -> EstoqueMedicamentos -> Maybe EstoqueMedicamentos
executaPlantao = undefined


{-
QUESTÃO 10 VALOR: 1,0 ponto

Defina uma função "satisfaz", cujo tipo é dado abaixo e que verifica se um plantão válido satisfaz um plano 
de medicamento válido para um certo estoque, ou seja, a função "satisfaz" deve verificar se a execução do plantão 
implica terminar com estoque diferente de Nothing e administrar os medicamentos prescritos no plano.
Dica: fazer correspondencia entre os remédios previstos no plano e os ministrados pela execução do plantão.
Note que alguns cuidados podem ser comprar medicamento e que eles podem ocorrer sozinhos em certo horário ou
juntamente com ministrar medicamento.

-}

satisfaz :: Plantao -> PlanoMedicamento -> EstoqueMedicamentos  -> Bool
satisfaz = undefined


{-

QUESTÃO 11 (EXTRA) VALOR: 1,0 ponto

 Defina a função "plantaoCorreto", cujo tipo é dado abaixo e que gera um plantão válido que satisfaz um plano de
 medicamentos válido e um estoque de medicamentos.
 Dica: a execução do plantão deve atender ao plano de medicamentos e ao estoque.

-}

plantaoCorreto :: PlanoMedicamento ->  EstoqueMedicamentos  -> Plantao
plantaoCorreto = undefined

