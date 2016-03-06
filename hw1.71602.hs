main = do
  print (mybc 5 3)  --10
  print (mybc 49 6)   --13989816
  print (myheron 5 3 4)   --6
  print (mysumevens 1 6)  --12
  print (myreverse 345)  --543
  print (myreverse 7)   --7
  print (mypalindrome 222322)   --false
  print (mypalindrome 2123232)  --false
  print (myprime 2) -- true
  print (myprime 12)  --false
  print (mymaxdivisor 10) -- 5
  print (mysumprimes 1 10) --17
  print (mysumprimes 2 3) --5
  print (mysumprimes 90 96) --0
  
--zadacha_0
mymaxdivisor x =  if(x==2) then 2
    else if(mod x 2 == 0) then (div x 2) 
    else (mymaxdivisor1 x 3)
mymaxdivisor1 x y = if(mod x (y-1) == 0) then (y-1)  else (mymaxdivisor1 x (y-1))

--zadacha_1
factorial n =  if(n==0) then 1 
    else if (n==1) then 1
    else (n*(factorial (n-1)))
mybc n k =  div (factorial n) ((factorial k) * (factorial (n-k)) )

--zadacha_2 
myheron a b c = 1/4*(sqrt((2*((a^2)*(b^2)+(a^2)*(c^2)+(b^2)*(c^2)))-(a^4+b^4+c^4)))

--zadacha_3
mysumevens a b = if (a>b) then 0
   else ( if (mod b 2 == 0) then (b + (mysumevens a (b-2))) else (mysumevens a (b-1)))

--zadacha_4
count x = if (x==0) then 0 else (1 + (count(div x 10))) 
myreverse x = if (x==0)then 0 else ((10^(count x - 1)) * (mod x 10) + myreverse (div x 10))
    
--zadacha_5
--izpolzvam funkciite ot zadacha_4
mypalindrome n = if(n==(myreverse n)) then "true" else "false"

--zadacha_6
myprime n = if(n==1) then "nito prosto, nito sustavno"
    else if(n==2 || n==3 || n==5 || n==7) then "true"
    else if(mod n 2 == 0) then "false"
    else ( myprime1 n 3 )
myprime1 n y = if (y==n) then "true" else (if (mod n y == 0) then "false" else (myprime1 n (y+1)))
    
--zadacha_7
mysumprimes a b = if (a>b) then 0 
   else (if (myprime b == "true") then (b + (mysumprimes a (b-1))) 
   else (mysumprimes a (b-1)))
 
   
 