fun rep 0 = []
	| rep m = (m mod 3)::rep(m div 3);

fun expint (a,0) = 1
	| expint(a,b) =
		let
			val x = expint(a,b div 2)
		in
			if b mod 2 =0 then x*x
			else x*x*a
		end
fun construct [] _ = ([],[])
	| construct (x1::y1::xs) i =
		if (x1 = 0) then construct (y1::xs) (i+1)
		else if (x1=1) then
			let
				val (x,y) = construct (y1::xs) (i+1)
			in
				(x,i::y)
			end
		else if x1=2 then
			let
				val (x,y) = construct ((y1+1)::xs) (i+1)
			in
				(i::x,y)
			end
		else if x1=3 then
			let
				val (x,y) = construct ((y1+1::xs)) (i+1)
			in
				(x,y)
			end
		else ([],[])
	| construct [m] i =
		if  m=1 then ([],[i])
		else if m=2 then ([i],[i+1])
		else if m=3 then ([],[i+1])
		else ([],[])
fun sum 0 = 1
        | sum n = expint(3,n) + sum (n-1)
fun balance n w =
	if sum (n-1) > w then construct (rep w) 1
	else ([],[])
