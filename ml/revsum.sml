val a = Array.array(100000,0); 
val N = Array.array(100000,0);
val c = ref "";
val i = ref 0 and j = ref 0;
val M = ref 0;
val iasks = ref 0 and  jasks = ref 0 and igives = ref 0 and jgives = ref 0 and oldjasks = ref 0 and oldigives = ref 0;
val k = ref 0;
val flag1 = ref false;
(*fun cat s =
  let
    val f = TextIO.openIn s 
    and c = ref ""
  in
    while (c := TextIO.inputN (f, 1); !c <> "") do
   ( Array.update(a,!i,!c);
     TextIO.output (TextIO.stdOut, !c);
     i := (!i)+1  );
    TextIO.closeIn f
  end;
*)
fun printN (N,a,b) = 
	let 
		fun printhelp (N,a,b,t) = 
			if a=b then t 
			else printhelp (N,a+1,b,t^(Int.toString (Array.sub(N,a))))
	in printhelp(N,a,b,"")
	end

fun printfile (file,N,a,b) = 
	let 
		val stream = TextIO.openOut file;
		fun printhelp (N,a,b) = 
			if a=b then (
				TextIO.output(stream,Int.toString (Array.sub(N,a)));
				TextIO.closeOut stream
				)
			else (
				TextIO.output(stream,Int.toString (Array.sub(N,a)));
				printhelp (N,a+1,b)
				)
	in 
		printhelp (N,a,b)
	end

fun readfile file = 
	let
		val f = TextIO.openIn file
    	and c = ref ""
	in
		while (c := TextIO.inputN (f, 1); !c <> "") do 
				(
					Array.update (a,!M, ord (String.sub(!c,0)) - ord (#"0")); 
					M:= !M+1 );
				TextIO.closeIn f
	end;

 fun ass [] x = ()
 	| ass y [] = ()
  	| ass (x::xs) (y::ys) = (x:=y;ass xs ys);

fun abs a = if a>=0 then a else ~a 
fun min (a, b) = if (a<b) then a else b
fun construct (i,j,iasks,jasks,igives,jgives,oldigives,oldjasks,M,N,flag1) = 
	if abs(!i - !j)>1 andalso not (!flag1) then 
		(
			iasks := !oldigives;
			jgives := !oldjasks;
			if abs (Array.sub(a,!i)- Array.sub(a,!j)) <=1 then 
				(
					igives:= !jgives;
					k := min (Array.sub (a,!i), Array.sub(a,!j)) ;
					if Array.sub(a,!i) = Array.sub(a,!j) then jasks := !iasks else (jasks := 1 - !iasks ; if (Array.sub(a,!i)> Array.sub(a,!j) andalso !jasks =1) orelse (Array.sub(a,!i)<Array.sub(a,!j) andalso !iasks = 1) then flag1 := true else ());
					if !igives = 1 then 
						(if !iasks = 1 andalso !jasks =1 then (Array.update(a,!i,9);Array.update(a,!j,!k))
						 else (
						 	if (Array.sub(a,!i)=9 andalso Array.sub(a,!j)=9 andalso (!iasks + !jasks) = 0 ) then flag1 := true else ();
						 	Array.update(N,!i,9);
						 	Array.update(N,!j,!k + 1)
						 	)
						 ) 
					else 
						if (!iasks + !jasks = 2) then (
							if Array.sub(a,!i) + Array.sub(a,!j) = 0 then flag1 := true else ();
							Array.update(N,!i,!k - 1);
							Array.update(N,!j,0)
							)
						else (Array.update(N,!i,!k);Array.update(N,!j,0));
					oldjasks := !jasks;
					oldigives := !igives
				)
			else if (Array.sub(a,!i)=0 andalso Array.sub(a,!j) = 9 andalso !iasks = 1 andalso !jgives = 0) then
				(
					igives := 1;
					jasks := 0;
					Array.update(N,!i,9);
					Array.update(N,!j,0);
					oldjasks := !jasks;
					oldigives := !igives)
			else if (Array.sub(a,!i)=9 andalso Array.sub(a,!j) = 0 andalso !iasks = 0 andalso !jgives = 1) then 
				(
					jasks := 1;
					igives := 0;
					Array.update(N,!i,9);
					Array.update(N,!j,0);
					oldjasks := !jasks;
					oldigives := !igives)
			else flag1 := true ;
			i := !i - 1;
			j := !j + 1;
			construct (i,j,iasks,jasks,igives,jgives,oldigives,oldjasks,M,N,flag1)
		)
	else if not (!flag1) then
		(
			if (!i - !j = 1) then (
				iasks := !oldigives;
				jgives := !oldjasks;
				if abs (Array.sub(a,!i)- Array.sub(a,!j)) <=1 then (
					igives:= !jgives;
					k := min (Array.sub (a,!i), Array.sub(a,!j)) ;
					if Array.sub(a,!i) = Array.sub(a,!j) then jasks := !iasks else (jasks := 1 - !iasks ; if (Array.sub(a,!i)> Array.sub(a,!j) andalso !jasks =1) orelse (Array.sub(a,!i)<Array.sub(a,!j) andalso !iasks = 1) then flag1 := true else ());
					if !jasks = !igives then (
						if !igives = 1 then 
						(if !iasks = 1 andalso !jasks =1 then (Array.update(a,!i,9);Array.update(a,!j,!k))
						 else (
						 	if (Array.sub(a,!i)=9 andalso Array.sub(a,!j)=9 andalso (!iasks + !jasks) = 0 ) then flag1 := true else ();
						 	Array.update(N,!i,9);
						 	Array.update(N,!j,!k + 1)
						 	)
						 ) 
					else 
						if (!iasks + !jasks = 2) then (
							if Array.sub(a,!i) + Array.sub(a,!j) = 0 then flag1 := true else ();
							Array.update(N,!i,!k - 1);
							Array.update(N,!j,0)
							)
						else (Array.update(N,!i,!k);Array.update(N,!j,0))
					)
				else flag1:= true
				)
				else if (Array.sub(a,!i)=0 andalso Array.sub(a,!j) = 9 andalso !iasks = 1 andalso !jgives = 0) then(
					igives := 1;
					jasks := 0;
					if !jasks = !igives then (
						Array.update(N,!i,9);
						Array.update(N,!j,0))
				else flag1:= true
				)
				else if (Array.sub(a,!i)=9 andalso Array.sub(a,!j) = 0 andalso !iasks = 0 andalso !jgives = 1) then(
					jasks := 1;
					igives := 0;
					if !jasks = !igives then (
						Array.update(N,!i,9);
						Array.update(N,!j,0)
					)
					else flag1 := true
				)
				else flag1 := true
			)
			else if !i = !j then
				(
					iasks := !oldigives;
					jgives := !oldjasks;
					if (!iasks =1 andalso Array.sub(a,!i) mod 2 = 0) orelse (!iasks = 0 andalso Array.sub(a,!i) mod 2 = 1) then flag1 := true else ();
					Array.update(N,!i, 5 * !jgives + (Array.sub(a,!i) - !iasks) div 2)
				)
			else ()

		)
	else ()

fun revsum file = 
	(
		M :=0;
		readfile file;
		(*M := !M -1;*)
		if !M = 1 then 
			if Array.sub(a,0) mod 2 = 0 then Int.toString (Array.sub(a,0) div 2 )else "0"
		else if !M = 2 then
			if Array.sub(a,0) = 1 then
				if (10 + Array.sub(a,1)) mod 2 = 0 then Int.toString ((10 + Array.sub(a,1)) div 2)
				else if Array.sub(a,0) = 1 andalso Array.sub(a,1) = 1 then "10"
				else "0"
			else
				if Array.sub(a,0) <> Array.sub(a,1) then "0"
				else Int.toString (Array.sub(a,1) - 1) ^ "1"
		else if Array.sub(a,0) <> 1 then 
		(
			i := !M - 1;
			j:= 0;
			iasks := 0;
			igives := 0;
			jgives := 0;
			if abs(Array.sub(a,!i) - Array.sub(a,!j)) > 1 then "0"
			else if Array.sub(a,!i)>Array.sub(a,!j) then "0"
				else (
					oldigives := !igives;
					if Array.sub(a,!i) = Array.sub(a,!j) then jasks := 0 else jasks := 1;
					Array.update(N,!j,1);
					Array.update(N,!i,Array.sub(a,!i) - 1);
					oldjasks := !jasks;
					i := !i - 1;
					j := !j + 1;
					construct (i,j,iasks,jasks,igives,jgives,oldigives,oldjasks,M,N,flag1);
					if !flag1 then "0" else printN (N, 0, !M -1)
					)
		)
		else (
			j := 1;
			i := !M -1;
			jgives := 1;
			iasks := 0;
			if abs(Array.sub(a,!i) - Array.sub(a,!j)) <= 1 then 
				if Array.sub(a,!i) = 9 andalso Array.sub(a,!j) = 9 then flag1 := true
				else (
					k := min (Array.sub (a,!i), Array.sub(a,!j)) ;
					igives := !jgives;
					if Array.sub(a,!i) > Array.sub(a,!j) then flag1 := true
					else (
						if Array.sub(a,!i) = Array.sub(a,!j) then jasks := 0 else jasks := 1;
						Array.update(N,!j,9);
						Array.update(N,!i,!k +1 );
						oldjasks := !jasks;
						oldigives := !igives
					)
				)
			else if Array.sub(a,!i) = 9 andalso Array.sub(a,!j) = 0 then (
				jasks := 1;
				igives := 0;
				oldjasks := !jasks;
				oldigives := !igives;
				Array.update(N,!j,9);
				Array.update(N,!i,0)
			)
			else flag1 := true;
			i := !i - 1;
			j := !j + 1;
			construct (i,j,iasks,jasks,igives,jgives,oldigives,oldjasks,M,N,flag1);
			if !flag1 then (
				i := !M - 1;
				j:= 0;
				iasks := 0;
				igives := 0;
				jgives := 0;
				flag1 := false;
				if abs(Array.sub(a,!i) - Array.sub(a,!j)) > 1 then "0"
				else if Array.sub(a,!i)>Array.sub(a,!j) then "0"
					else (
						oldigives := !igives;
						if Array.sub(a,!i) = Array.sub(a,!j) then jasks := 0 else jasks := 1;
						if Array.sub(a,!i)>0 then Array.update(N,!j,1) else Array.update(N,!j,0);
						Array.update(N,!i,0);
						oldjasks := !jasks;
						i := !i - 1;
						j := !j + 1;
						construct (i,j,iasks,jasks,igives,jgives,oldigives,oldjasks,M,N,flag1);
						if !flag1 then "0" else printN (N ,0,!M - 1)
						)
			)
			else   printN (N ,1 ,!M -1) 
		)
	)
		