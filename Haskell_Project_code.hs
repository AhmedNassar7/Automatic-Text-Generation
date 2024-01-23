import DataFile
import Data.Set 


wordToken:: String -> [String]
wordTokenList :: [String] -> [String]
uniqueBigrams :: [String] -> [(String,String)]
uniqueTrigrams :: [String] -> [(String,String,String)]
bigramsFreq :: Num a => [String] -> [((String,String),a)]
trigramsFreq:: Num a => [String] -> [((String,String,String),a)]
getFreq :: (Eq a, Num b) => a -> [(a,b)] -> b
generateOneProb :: Fractional a => ((String,String,String),a) ->[((String,String),a)] -> a
genProbPairs :: Fractional a => [((String,String,String),a)] ->[((String,String),a)] -> [((String,String,String),a)]
generateNextWord :: (Ord a, Fractional a) => [String] ->[((String,String,String),a)] -> String
generateText :: String -> Int -> String

---------
wordToken x = words x
----------
wordTokenList []=[]
wordTokenList (x:y) = words x ++ wordTokenList y
--------
---take a list and return a list of unique elemnts either single element or pair ot tubles
mkUniq :: Ord a => [a] -> [a]
mkUniq = toList  . fromList
--------
uniqueBigrams l = mkUniq  (helper l)
helper [x]=[]
helper (x:y:z)= [(x,y)] ++ helper (y:z) 

--------------
uniqueTrigrams l = mkUniq  (helper2 l)
helper2 [x,y]=[]
helper2 (x:y:z:w)= [(x,y,z)] ++ helper2 (y:z:w) 

------------------

bigramsFreq x = bigramsFreqh x []
bigramsFreqh [] res=[]
bigramsFreqh (x:[]) res=[]
bigramsFreqh (x:y:m) res= helpbig (x,y) res ++ bigramsFreqh (y:m) res
helpbig (x,y) []=[((x,y),1)]
helpbig (x,y) (((a,b),d):e)=if(x==a && y==b) then [((a,b),(d+1))]
								else helpbig (x,y) e
								
-----------------	
trigramsFreq x = trigramsFreqh x []
trigramsFreqh [] res=[]
trigramsFreqh (x:[]) res=[]
trigramsFreqh (x:y:[]) res=[]
trigramsFreqh (x:y:z:m) res= helptrig (x,y,z) res ++ trigramsFreqh (y:z:m) res
helptrig (x,y,z) []=[((x,y,z),1)]
helptrig (x,y,z) (((a,b,c),d):e)=if(x==a && y==b && z==c) then [((a,b,c),(d+1))]
								else helptrig (x,y,z) e

----------------------
getFreq x [] =0
getFreq x ((a,n):b) = if x==a then n
					  else getFreq x b
---------------------		
generateOneProb x [] =0			  
generateOneProb ((x,y,z),n) (((a,b),num):xs)= if (x==a && y==b) then (n/num)
											  else generateOneProb ((x,y,z),n) xs
---------------------
genProbPairs [] x=[]
genProbPairs (((x,y,z),n):xs) a	= if ((generateOneProb ((x,y,z),n) a) ==0) then genProbPairs xs a
								 else [((x,y,z),(generateOneProb ((x,y,z),n) a))] ++ genProbPairs xs a										  
---------------------
generateNextWord x a=chooserand f ((randomZeroToX ((length (f))-1))) where f = (helpgen x a (length a))
chooserand [] a= error "Sorry, it is not possible to infer from current database"
chooserand (x:y) n = if (n==0) then x
					else chooserand (y++[x]) (n-1)
helpgen a x 0 =[]
helpgen (x:y:d) (((a,b,c),n):bs) len = if (x==a && y==b && n>0.03 ) then [c]++helpgen (x:y:[]) (bs++[((a,b,c),n)]) (len-1)
								else helpgen (x:y:[]) (bs++[((a,b,c),n)]) (len-1)
---------------------
generateText x n = helpgenT (wordToken x) n
helpgenT x 0= unwords x
helpgenT x n= helpgenT (x ++ [(generateNextWord (last2 x) (genProbPairs (trigramsFreq (wordTokenList docs) ) (bigramsFreq x)))]) (n-1)	
last2 (x:y:[])=[x,y]
last2 (x:y:z)=last2 (y:z)							
