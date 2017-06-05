val N = ref 0;
val b = Array.array(420000,0.0);
val h = Array.array(420000,0.0);
val w = Array.array(420000,0.0);
val l = Array.array(420000,0.0);
val Vol = ref (IntInf.fromInt 0); 
fun read file = 
    let fun int_from_stream stream =
  			Option.valOf (TextIO.scanStream (Int.scan StringCvt.DEC) stream)
  		fun long_from_stream stream =
 			Option.valOf (TextIO.scanStream (IntInf.scan StringCvt.DEC) stream)
  		val fstream = TextIO.openIn file
  		val i = ref 0;
  	in 
  		N := int_from_stream fstream;
  		while (!i < !N) do (
  			Array.update(b,!i, Real.fromInt(int_from_stream fstream));
  			Array.update(h,!i, Real.fromInt(int_from_stream fstream));
  			Array.update(w,!i, Real.fromInt(int_from_stream fstream));
  			Array.update(l,!i, Real.fromInt(int_from_stream fstream));
  			i := !i +1);
      	Vol := long_from_stream fstream;
  		TextIO.closeIn fstream
  	end


fun check (b,h,w,l,mid,N,volume,i,current : real) =
    if(current >= volume) then true
    else if(i=N) then false
    else if Array.sub(b,i) < mid then 
        if Array.sub(h,i) + Array.sub(b,i) < mid then
            check(b,h,w,l,mid,N,volume,i+1,current+Array.sub(h,i)*Array.sub(w,i)*Array.sub(l,i)) 
        else 
            check(b,h,w,l,mid,N,volume,i+1,current+(mid-Array.sub(b,i))*Array.sub(w,i)*Array.sub(l,i)) 

    else 
        check(b,h,w,l,mid,N,volume,i+1,current) 



fun binarysearch (low : real,high : real ,b,h,w,l,N,volume) =
    let
        val mid = 1.0*(1.0*high+1.0*low) / 2.0
    in
        if (high - low < 0.01 ) then mid
        else if(not(check(b,h,w,l,mid,N,volume,0,0.0))) then binarysearch(mid,high,b,h,w,l,N,volume)
        else binarysearch(low,mid,b,h,w,l,N,volume)
    end


fun maxHeight (h,b,maxH : real,i,N) =
    if i = N then maxH
    else if (Array.sub(h,i) + Array.sub(b,i) > maxH) then maxHeight(h,b,Array.sub(h,i) + Array.sub(b,i),i+1,N)
    else maxHeight(h,b,maxH,i+1,N)


fun checkOverflow(b,h,w,l,i,N,volume :real,current : real) =
    if i = N then current < volume
    else checkOverflow(b,h,w,l,i+1,N,volume,current + Array.sub(w,i)*Array.sub(h,i)*Array.sub(l,i))





fun dek (b,h,w,l,N,vol) = 
    let
        val volume = Real.fromLargeInt(vol)
    in
            if checkOverflow(b,h,w,l,0,N,volume,0.0) then ~1.0
        else let
            val maxheight = maxHeight(h,b,0.0,0,N)
        in
            Real.realRound (binarysearch(0.0,maxheight,b,h,w,l,N,volume)*100.0) / 100.0
        end
    end



fun deksamenes file = 
    let
        val f = read file
    in
        dek(b,h,w,l,!N,!Vol)
    end        

val args = CommandLine.arguments();
val (x::y) = args;
val _ = print ((Real.fmt (StringCvt.FIX (SOME 2)) (deksamenes x))^"\n")
