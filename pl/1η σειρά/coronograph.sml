fun coronograph file =
let
 fun next_int input =
   Option.valOf (TextIO.scanStream (Int.scan StringCvt.DEC) input)
 val stream=TextIO.openIn file
 val N=next_int stream
 val _ = TextIO.inputLine stream

fun corona1 file =
let
 fun input file=
 let
  fun parse file =
     let
 	  fun next_int input =
      Option.valOf (TextIO.scanStream (Int.scan StringCvt.DEC) input)
 	   (*val stream = TextIO.openIn file*)
     val n = next_int stream
     val k = next_int stream
     val _ = TextIO.inputLine stream
     fun scanall 0 acc  = acc
      | scanall k acc =
      let
       val n1=next_int stream
       val k1=next_int stream
       val _ = TextIO.inputLine stream
      in
       scanall (k-1) (k1::n1::acc)
     end
 	 in
     (n,k,rev(scanall k []))
   end
  val (n,k,list_a)= parse file
 in
  (n,k,list_a)
   (*(n,k) *)
end (*TELIWNEI TO INPUT*)
val (N,K,list_a)=(input file)

fun printlist  xs =
  print(String.concatWith " " (map Int.toString xs));

(*MATRIX IN ARRAY arr*)
fun listFromArray arr = List.tabulate(Array.length(arr), fn i => Array.sub(arr, i))
fun partialsol N K list_a =
 let
  val counter=Array.array(1,0)
  val arr=Array.array(N+1,[])
  val color=Array.array(N+1,0)
  val par=Array.array(N+1,0)
  val cyclenum=Array.array(1,0)
  val mark=Array.array(N+1,0)
  val flag=Array.array(1,0)
  val flagucurr=Array.array(1,0)
  val cycles=Array.array(N+1,[])
  fun puttomatrix [] = 0
    | puttomatrix (n1::n2::xs) =
    let
      val w=Array.sub(arr,n1)
      val z=Array.sub(arr,n2)
      val x=Array.update(arr,n1,n2::w)
      val y=Array.update(arr,n2,n1::z)
      val w=Array.sub(arr,n1)
      val z=Array.sub(arr,n2)
    in
      puttomatrix xs
    end
  val w=puttomatrix list_a
  val stack=Array.array(1,Fifo.empty)
  val stack2=Array.array(1,Fifo.empty)

  fun updates u z parent w=
   let
      val x=Array.update(par,u,parent)
      val x=Array.update(stack,0,Fifo.enqueue(z,u))
      val t=Array.sub(counter,0)
      val x=Array.update(stack2,0,w)
      val w=Array.update(color,u,t)
    in
      x
    end
  fun checkifelement u [] = false
    | checkifelement u (x::xs) =
      if u=x
      then true
      else checkifelement u xs
  fun pushifelementnotexists u l =
    let
      val l1=Fifo.contents(l)
      val f=checkifelement u l1
    in
      if f=false
      then Array.update(stack2,0,Fifo.enqueue(l,u))
      else Array.update(stack2,0,l)
    end
  fun iterate u parent w=
    let
      val parq=Array.sub(par,parent)
    in
      if  parq=u
      then  Array.update(stack2,0,w)
      else  pushifelementnotexists u w
    end

  fun checkcolor u par=
    let
      val x=Array.sub(color,u)
      val z=Array.sub(stack,0)
      val w=Array.sub(stack2,0)
    in
      if x=0
      then updates u z par w
      else iterate u par w
    end
  fun firstelement [] par= checkcolor 1 1
    | firstelement (x::xs) par =
      let
        val q=checkcolor x par
      in
        firstelement xs par
      end
  fun checkstack [] = false
    | checkstack (x::xs) = true
  fun bfs random =
    let
      val x=Array.sub(stack,0) (*GETS THE STACK*)
      val node=Fifo.head x  (*POPS THE ITEM TO CHECK*)
      val (dq,hed)=Fifo.dequeue(x)
      val q=Array.update(stack,0,dq) (*POPS ITEM FROM STACK*)
      val z=Array.sub(arr,node) (*Adjacency list of item*)
  (*val parent=Array.sub(par,z) *)
    val check=firstelement z node (*Find adjacency list pushes into stack and gives them number*)
    val t=Array.sub(counter,0)  (*INCREASES THE COUNTER FOR NEXT-LEVEL*)
    val k=Array.update(counter,0,t+1)
  in
    if Fifo.isEmpty(Array.sub(stack,0))=false
    then bfs 0
    else 1
  end

  val x=Array.sub(stack,0)
  val u=Array.update(stack,0,Fifo.enqueue(x,1))
  val q=Array.update(color,1,1)
  val w=Array.update(counter,0,2)
  val k=bfs 0

  fun checkifzero [] = true
   | checkifzero  (x::xs) =
    if x=0
    then false
    else checkifzero xs

  fun nocorona K N flage=
   if N<K orelse flage=false
   then false
   else true

 fun checkcycles x =
  let
  (*stack2 has the last two elements of the circle trace back using  par*)
  (*now i need to find the remaining parts of the cycle*)
   val cyclelist=Array.array(1,Fifo.empty)
   val st=Array.sub(cyclelist,0)
   val x=Array.sub(stack2,0)
   val (dq,hed)=Fifo.dequeue(x)
   val y=Array.update(stack2,0,dq)
   val y=Array.sub(stack2,0)
   (*val z=Fifo.head y*)
   val (dq,z)=Fifo.dequeue(y)
   val y=Array.update(stack2,0,dq)
   val x=Array.sub(stack2,0)
   val y=Array.update(stack2,0,Fifo.enqueue(x,z))
   val x=Array.update(cyclelist,0,Fifo.enqueue(st,z))
  (*cyclelist has the two nodes*)
  (*find first common parent*)
  fun findparents u  =
    let
      val q=Array.sub(par,u)
      val x=Array.sub(cyclelist,0)
      val w=Array.update(cyclelist,0,Fifo.enqueue(x,q))
    in
      if q=0
      then 0
      else findparents q
    end
  val cyclelist1=Array.array(1,Fifo.empty)
  val x=Array.sub(stack2,0)
  val w=Fifo.head x
  val st=Array.sub(cyclelist1,0)
  val x=Array.update(cyclelist1,0,Fifo.enqueue(st,w))
  fun findparents1 u  =
    let
      val q=Array.sub(par,u)
      val x=Array.sub(cyclelist1,0)
      val w=Array.update(cyclelist1,0,Fifo.enqueue(x,q))
    in
      if q=0
      then 0
      else findparents1 q
    end
  val x=Array.sub(cyclelist,0)
  val y=Fifo.head x
  val z=findparents y
  val x=Array.sub(cyclelist1,0)
  val y=Fifo.head x
  val z=findparents1 y
  (*find if elements in cyclelist1 belong in cyclelist*)






  val hashcycl=Array.array(N+1,0)
  val hashcycl1=Array.array(N+1,0)

  fun updatehashcycl u =Array.update(hashcycl,u,1)
  fun updatehashcyclv2 [] = Array.update(hashcycl,0,1)
  | updatehashcyclv2 (x::xs) =
   let
    val q=updatehashcycl x
   in
    updatehashcyclv2 xs
   end

   val cycl=Array.sub(cyclelist,0)
   val cyc=Fifo.contents(cycl)
   val random=updatehashcyclv2 cyc

   fun updatehashcycl1 u =Array.update(hashcycl1,u,1)
   fun updatehashcyclv21 [] = Array.update(hashcycl1,0,1)
   | updatehashcyclv21 (x::xs) =
    let
     val q=updatehashcycl1 x
    in
     updatehashcyclv21 xs
    end

    val cycl=Array.sub(cyclelist1,0)
    val cyc=Fifo.contents(cycl)
    val random=updatehashcyclv21 cyc

    fun belongsto u =
     let
      val k=Array.sub(hashcycl,u)
      val k1=Array.sub(hashcycl1,u)
     in
      if k=k1
      then true
      else false
    end






(*  fun belongsto u [] = false
    | belongsto u (x::xs) =
      if x=u
      then true
      else belongsto u xs *)

  fun pushelement y flag =
    let
      val k=Array.sub(cyclelist,0)
    in
      if flag=false
      then Array.update(cyclelist,0,Fifo.enqueue(k,y))
      else Array.update(cyclelist,0,k)
    end

  fun pushelement1 y flag cyc=
        if flag=false
        then  y::cyc
        else cyc
  fun pushelements cyc1 =
    let
      (*val SHIIIIT=print("CAME HERE 1---- \n") *)
      val x=Array.sub(cyclelist1,0)
      (*val y=Fifo.head x*)
      val (dq,y)=Fifo.dequeue(x)
      val z=Array.update(cyclelist1,0,dq)
      val cycl=Array.sub(cyclelist,0)
      (*val cyc=Fifo.contents(cycl) *)
      (*val flag=belongsto y cyc*)
      val flag=belongsto y
      (*val k=pushelement y flag*)
      val extracycles=pushelement1 y flag cyc1
    in
      if flag=false
      then pushelements extracycles
      else extracycles
    end

    val extraelements= pushelements []

  (*Now i need to remove the common elements of the lists*)
  (*find the first common element*)
  val x= Array.sub(cyclelist1,0)
  val y= Array.sub(cyclelist,0)

  fun belonganddelete u [] l = Array.update(cyclelist1,0,l)
    | belonganddelete u (x::xs) l =
      if u=x
      then Array.update(cyclelist,0,l)
      else belonganddelete u xs (Fifo.enqueue(l,x))

  fun deletecommon q=
    let
      val x= Array.sub(cyclelist1,0)
      val y= Fifo.head(x)
      val zt= Array.sub(cyclelist,0)
      val z=Fifo.contents(zt)
      val (dq,hed)=Fifo.dequeue(x)
      val w=Array.update(cyclelist,0,dq)
      val flag=belonganddelete y z Fifo.empty
      val garbage=1
    in
      garbage
    end

  val k=deletecommon 1
  fun extraels [] = Array.update(cyclelist,0,Array.sub(cyclelist,0))
  | extraels (x::xs) =
  let
   val temp=Array.sub(cyclelist,0)
   val tempo=Array.update(cyclelist,0,Fifo.enqueue(temp,x))
  in
   extraels xs
  end

  val garbage=extraels extraelements
  val lis=listFromArray color
  val lis1=listFromArray cyclelist1
  val lis2=listFromArray cyclelist
  val lis3=listFromArray stack2
 in
  (stack2,cyclelist,arr,color,true)
 end

 val lis=listFromArray color
 val colorupd=tl lis
 val flage= checkifzero colorupd
 val f=nocorona K N flage
  val x=Array.sub(stack,0)
  val list3=Array.array(1,Fifo.empty)
  val list2=Array.array(1,Fifo.empty)
 (*  val q=checkcycles 0 *)
 in
   if f=true
  then checkcycles 0
  else (list3,list2,arr,color,f)
 end

fun checker N K =
 if K<=N-1
 then false
 else true

(*(stack2,cyclelist,arr,color,f)=partialsol N K list_a *)
fun callpartialsol f N K list_a =
 let
  val stack2 =Array.array(1,Fifo.empty)
  val cyclelist =Array.array(1,Fifo.empty)
  val arr=Array.array(N+1,[])
  val color=Array.array(N+1,0)
  val q=false
 in
  if f=true
  then partialsol N K list_a
  else (stack2,cyclelist,arr,color,q)
 end

val fl= checker N K
val (stack2,cyclelist,arr,color,f)=callpartialsol fl N K list_a

fun secondsolution stack2 cyclelist arr color  =
let
val x=Array.sub(stack2,0)
val (dq,hed)=Fifo.dequeue(x)
val stack2=Array.update(stack2,0,dq)

val hash=Array.array(N+1,0)

fun updatehash [] = Array.update(hash,0,0)
 | updatehash (x::xs) =
  let
   val z=Array.sub(hash,x)
   val y=Array.update(hash,x,z+1)
  in
   updatehash xs
  end

val x=Array.sub(cyclelist,0)
val z=Fifo.contents(x)
val k=updatehash z
val stack3=Array.array(1,Fifo.empty)

fun checktoupdate x g =
 let
  val q=Array.sub(stack3,0)
 in
  if g=0
  then Array.update(stack3,0,Fifo.enqueue(q,x))
  else Array.update(stack3,0,q)
 end

fun secondcheck x =
 if Array.sub(hash,x)=0
 then Array.update(hash,x,1)
 else Array.update(hash,x,Array.sub(hash,x))
fun checktoupdate1 x u g =
 let
  val q=Array.sub(hash,u)
  val f=secondcheck x
 in
  if g=0
  then Array.update(hash,u,q+1)
  else Array.update(hash,u,q)
 end

fun pushgeitones u [] = 0
 | pushgeitones u (x::xs) =
  let
   val k=Array.sub(hash,x)
   val w=checktoupdate x k
   val z=checktoupdate1 x u k
  in
   pushgeitones u xs
  end

fun pushola u =
 let
  val x=Array.sub(stack3,0)
  val (dq,first)=Fifo.dequeue(x)
  val k=Array.update(stack3,0,dq)
  val q=Array.sub(arr,first)
  val omg=pushgeitones u q
  val ch=Array.sub(stack3,0)
  val f=Fifo.isEmpty(ch)
 in
  if f=true
  then 0
  else pushola u
end
fun updatehashtable [] = Array.update(hash,0,1)
 | updatehashtable (x::xs) =
  let
  val k=Array.sub(stack3,0)
  val w=Fifo.enqueue(k,x)
  val p=Array.update(stack3,0,w)
  val test=pushola x
  in
  updatehashtable xs
  end
val x=Array.sub(cyclelist,0)
val cyclelis = Fifo.contents(x)
val fin=updatehashtable cyclelis
in
 hash
end
val a=Array.array(N+1,0)
fun checktosolve stack2 cyclelist arr color f a=
 if f=false
 then a
 else secondsolution stack2 cyclelist arr color

fun finalsolution  stack2 cyclelist arr color f a=
let
val hasht=checktosolve stack2 cyclelist arr color f a
fun createhashlist acc u=
 let
  val w=Array.sub(hasht,u)
 in
  w::acc
end
fun createhashlist1 [] acc = acc
 | createhashlist1 (x::xs) acc =
  let
   val lista=createhashlist acc x
  in
   createhashlist1 xs lista
 end

val cyclelis=Fifo.contents(Array.sub(cyclelist,0))
val hashlist=createhashlist1 cyclelis []


(*fun insert x [] = [x]
  | insert x (y::ys) =
    if x < y then x :: y :: ys
    else y :: (insert x ys)
fun isort_aux [] acc = acc
  | isort_aux (x::xs) acc =
    isort_aux xs (insert x acc)
fun isort_2 xs = isort_aux xs []
val result=isort_aux hashlist [] *)
(*fun selectionsort [] = []
|   selectionsort (first::last) =
    let
      fun select_r small ([], output) = small::(selectionsort output)
      |   select_r small (x::xs, output) =
            if (x< small) then
              select_r x (xs, small::output)
            else
              select_r small (xs, x::output)
    in
      select_r first (last, [])
    end *)
    fun merge([], ys) = ys
    |	merge(xs, []) = xs
    |	merge(x::xs, y::ys) =
    	if x < y then
    		x::merge(xs, y::ys)
    	else
    		y::merge(x::xs, ys);

    fun split [] = ([],[])
    |	split [a] = ([a],[])
    |	split (a::b::cs) =
    		let val (M,N) =
    			split cs in (a::M, b::N)
    		end
        
    fun mergesort [] = []
|	mergesort [a] = [a]
|   mergesort [a,b] =	if a <= b then
                            [a,b]
                        else [b,a]
|   mergesort L =
        let val (M,N) = split L
        in
          merge (mergesort M, mergesort N)
        end

val result= mergesort hashlist

in
result
end

fun finalcheck stack2 cyclelist arr color f a =
 if f=true
 then finalsolution stack2 cyclelist arr color f a
 else []

fun printsuccess l =
 let
  val x= print("CORONA ")
  val k= length l
  val w =print( (Int.toString (k)))
  val ffs=print("\n")
  val y=printlist l
  val z=print("\n")
 in
  x
 end
fun solution stack2 cyclelist arr color f a =
 let
  val x=finalcheck stack2 cyclelist arr color f a
 in
  if x=[]
  then print("NO CORONA\n")
  else printsuccess x
end

val final =solution stack2 cyclelist arr color f a
in
final
end

fun executeNtimes N file=
 let
  val x=corona1 file
  val y=N-1
 in
  if N=1
  then x
  else executeNtimes y file
 end

val run=executeNtimes N file
in
N
end