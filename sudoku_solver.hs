-- get functions		
getEl row col prob = prob !! (row * 9 + col)
getRow row prob = take 9 $ drop (row * 9) prob
getCol col prob = [ getEl i col prob | i <- [0..8] ]
getBox box prob = [ getEl (3 * (box `div` 3) + i) (3 * (box `mod` 3) + j) prob | i <- [0..2], j <- [0..2]]

getRowIndexes row = [ row * 9 + i | i<-[0..8]]
getColIndexes col = [ i * 9 + col | i<-[0..8]]
getBoxIndexes box = [ (3 * (box `div` 3) + i) * 9 + (3 * (box `mod` 3) + j) | i<-[0..2], j<-[0..2]]

-- test functions									 
testSet set = foldr (&&) True [ (0 == set !! i) || not (elem (set !! i) (drop (i+1) set)) | i <- [0..8] ]
testRows prob = foldr (&&) True [ testSet $ getRow i prob | i <- [0..8]]
testCols prob = foldr (&&) True [ testSet $ getCol i prob | i <- [0..8]]
testBoxs prob = foldr (&&) True [ testSet $ getBox i prob | i <- [0..8]]
testAll prob = testRows prob && testCols prob && testBoxs prob
testZeros prob = foldr (&&) True $ map (/=0) prob
testSolved prob = testAll prob && testZeros prob

-- candidate extension
extend prob = if elem 0 prob 
			  then filter (testAll) ([takeWhile (/=0) prob ++ i:tail (dropWhile(/=0) prob) | i<-[1..9]])
			  else []

-- DFS algorithm
search [] = []
search (x:xs) = if testSolved x then x else search $ extend x ++ xs

-- DFS variations for debugging purposes
-- searchT ((x:xs),t) = if testSolved x then t ++ [x] else searchT (filter testAll (extend x) ++ xs, t ++ [x])
searchN ([],n) = ([],n)
searchN ((x:xs),n) = if testSolved x then (x,n) else searchN (extend x ++ xs, n+1)

--main = print $ search [prob]
--main = print $ searchN ([stest1],0)

-- rule checking
checkNoOtherValueAllowed index prob
		| (index == 81) = prob
		| (length (allowedValues index prob) == 1) = checkNoOtherValueAllowed (index+1) (insert index value prob)
		| otherwise = checkNoOtherValueAllowed (index+1) prob
		where value = head $ allowedValues index prob
		
checkValueAllowedNoOtherSquare box iter prob
 		| (box == 9 && iter == 10) = prob
		| (iter == 10) = checkValueAllowedNoOtherSquare (box+1) 0 prob
		| ((length noOtherSetValues) == 1) = checkValueAllowedNoOtherSquare box (iter+1) (insert ((getBoxIndexes box) !! iter) (head noOtherSetValues) prob)
			where noOtherSetValues = (valuesAllowedNoOtherSet (getBoxIndexes box) iter prob)
		| otherwise = checkValueAllowedNoOtherSquare box  (iter+1) prob
		
		

diff prob0 prob1 = zipWith (-) prob1 prob0

valuesAllowedNoOtherSet indexes iter prob = notAllowedValuesIndexes (remove iter indexes) prob

notAllowedValuesIndexes indexes prob = commonElements [ notAllowedValues i prob | i<-indexes]

allowedValuesIndexes indexes prob = commonElements [ allowedValues i prob | i<-indexes]

commonElements sets = foldr1 (\set1 set2 -> [ e | e<-set1, elem e set2]) sets 

notAllowedValues index prob = [ val | val<-[1..9], notElem val $ allowedValues index prob]

allowedValues index prob = if (prob !! index) == 0
							then [ val | val<-[1..9], testAll (insert index val prob)]
			  				else []	
		  
extendIndex index prob = if (prob !! index) == 0
			  then filter (testAll) ([insert index val prob | val<-[1..9]])
			  else []

insert i v prob = take i prob ++ v:drop (i+1) prob
remove i set = take i set ++ drop (i+1) set

-- sample problems
prob = [0,0,0,3,0,0,0,7,0,
		7,0,0,4,0,5,0,3,8,
		0,0,8,0,1,0,0,0,0,
		0,0,6,2,0,0,0,0,3,
		9,0,0,0,0,0,0,0,2,
		5,0,0,0,0,1,9,0,0,
		0,0,0,0,5,0,4,0,0,
		3,2,0,7,0,8,0,0,9,
		0,1,0,0,0,2,0,0,0]
		
ptest1 = [0,0,0,3,0,0,0,7,0,
		  7,0,0,4,2,5,6,3,8,
		  0,0,8,6,1,7,5,9,4,
		  0,8,6,2,9,4,7,5,3,
		  9,4,3,5,7,6,8,1,2,
		  5,7,2,8,3,1,9,4,6,
		  0,6,9,1,5,3,4,2,7,
		  3,2,5,7,4,8,1,6,9,
		  0,1,7,9,6,2,3,8,5]
		
ptest2 = [0,0,0,3,0,0,0,7,0,
		  7,0,0,4,0,5,0,3,8,
		  0,0,8,0,1,0,0,0,0,
		  1,8,6,2,9,4,7,5,3,
		  9,4,3,5,7,6,8,1,2,
		  5,7,2,8,3,1,9,4,6,
		  8,6,9,1,5,3,4,2,7,
		  3,2,5,7,4,8,1,6,9,
		  4,1,7,9,6,2,3,8,5]	
		
sol =  [6,5,4,3,8,9,2,7,1,
		7,9,1,4,2,5,6,3,8,
		2,3,8,6,1,7,5,9,4,
		1,8,6,2,9,4,7,5,3,
		9,4,3,5,7,6,8,1,2,
		5,7,2,8,3,1,9,4,6,
		8,6,9,1,5,3,4,2,7,
		3,2,5,7,4,8,1,6,9,
		4,1,7,9,6,2,3,8,5]
		
stest1 =   [8,0,0, 0,0,0, 0,0,0,
			0,0,3, 6,0,0, 0,0,0,
			0,7,0, 0,9,0, 2,0,0,

			0,5,0, 0,0,7, 0,0,0,
			0,0,0, 0,4,5, 7,0,0,
			0,0,0, 1,0,0, 0,3,0,

			0,0,1, 0,0,0, 0,6,8,
			0,0,8, 5,0,0, 0,1,0,
			0,9,0, 0,0,0, 4,0,0]
		
stest2 =   [5,0,0, 3,0,0, 0,0,0,
			8,0,0, 0,1,0, 0,9,0,
			0,6,0, 5,0,4, 0,0,0,

			0,0,0, 0,0,8, 0,0,7,
			7,0,0, 0,0,0, 0,0,6,
			1,0,0, 9,0,0, 0,0,0,

			0,0,0, 4,0,6, 0,7,0,
			0,2,0, 0,3,0, 0,0,4,
			0,0,0, 0,0,2, 0,0,8]
			
blank = [0,0,0, 0,0,0, 0,0,0,
		 0,0,0, 0,0,0, 0,0,0,
		 0,0,0, 0,0,0, 0,0,0,
		 
		 0,0,0, 0,0,0, 0,0,0,
		 0,0,0, 0,0,0, 0,0,0,
		 0,0,0, 0,0,0, 0,0,0,
		 
		 0,0,0, 0,0,0, 0,0,0,
		 0,0,0, 0,0,0, 0,0,0,
		 0,0,0, 0,0,0, 0,0,0]