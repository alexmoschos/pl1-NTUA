
val arr = Array.array(100000,Int.toLarge 0);
val M = ref 0;
val N = ref 0;
val i = ref 0;


fun findhighaux(arr,i,n,sum) = 
		if (i = n) then sum
		else findhighaux(arr,i+1,n,sum + Array.sub(arr,i))


fun findhigh (arr,n) =
	findhighaux(arr,0,n,Int.toLarge 0)





fun printresult(arr,boo,i,m) = 
	if (i = 0) then(print (IntInf.toString(Array.sub(arr,i)));
			if(Array.sub(boo,i)) then(
						print " |";
						printresult(arr,boo,i+1,m))
			else printresult(arr,boo,i+1,m))

	else if(i = m) then ()
	else(
		print (" " ^ IntInf.toString(Array.sub(arr,i)));
		if (Array.sub(boo,i)) then (
			print " |";
			printresult(arr,boo,i+1,m))
		else
			printresult(arr,boo,i+1,m)
		)




fun build (arr,sum,i,min,boo,count) = 
	if i = ~1 then 
		(boo,count)
	else if (sum + Array.sub(arr,i)>min) then( 
		Array.update(boo,i,true);
		build(arr,Array.sub(arr,i),i-1,min,boo,count+1))
	else 
		build(arr,sum+Array.sub(arr,i),i-1,min,boo,count)

fun fixbooaux(boo,count,n,m,i)=
	if i=m then
		boo
	else if(count=n-1) then boo
	else if(Array.sub(boo,i)) then 
		fixbooaux(boo,count,n,m,i+1)
	else (
		Array.update(boo,i,true);
		fixbooaux(boo,count+1,n,m,i+1))
		

	

fun fixboo (boo,count,n,m) =
	if count < n then
		fixbooaux(boo,count,n,m,0)
	else boo








fun checkaux (arr,mid,m,n,currentsum,i,count)=
	if (i = m) then if currentsum > mid then false
			else (count < n)
	else if (Array.sub(arr,i)>mid) then
		false
	else if (currentsum + Array.sub(arr, i)>mid) then
		checkaux(arr,mid,m,n,Array.sub(arr, i),i+1,count+1)
	else
		checkaux(arr,mid,m,n,currentsum + Array.sub (arr, i),i+1,count)

fun check (arr,mid,m,n) =
	checkaux (arr,mid,m,n,0,0,0)



fun binarysearch (arr,high,low,min,m,n) =
	let
		val mid = (high+low) div 2
	in
		if  high-low <= 1  then min
		else if check(arr,mid,m,n) then binarysearch (arr,mid,low,mid,m,n)
		else binarysearch(arr,high,mid,min,m,n)
	end


fun read file = 
	let fun int_from_stream stream =
  			Option.valOf (TextIO.scanStream (Int.scan StringCvt.DEC) stream)
  		val fstream = TextIO.openIn file
  		val i = ref 0;
  	in 
  		M := int_from_stream fstream;
  		N := int_from_stream fstream;
  		while (!i < !M) do (
  			Array.update(arr,!i,Int.toLarge (int_from_stream fstream));
  			i := !i +1);
  		TextIO.closeIn fstream
  	end


fun printN (N,a,b) = 
	if a=b then Int.toString (Array.sub(N,a))
	else (Int.toString (Array.sub(N,a)) )^" " ^ printN (N,a+1,b)





fun result (arr,m,n) = 
	let

		val high = findhigh (arr,m);
		val min = binarysearch(arr,high,0,high,m,n);
		val (boo,count) = build(arr,0,m-1,min,Array.array(m,false),0);
		val boo = fixboo(boo,count,n,m);
	in
		boo
end

fun fairparts file = 
	let
		val a =	read file;
	in
		let
			val boo = result(arr,!M,!N);
		in
			printresult(arr,boo,0,!M)
		end
	end



val args = CommandLine.arguments();
val (x::y) = args;
val _ = fairparts x;
