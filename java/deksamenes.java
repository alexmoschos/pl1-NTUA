import java.util.Scanner;
import java.io.File;



class deksamenes {
	public static void main(String[] args) throws Throwable {
		File file = new File(args[0]);
			
		Scanner input = new Scanner( file );
		int N = input.nextInt();
		int b,w,h,l;
		dek[] arr = new dek[N];
		double maxvolume = 0;
		double maxHeight = 0;
		for (int i = 0; i < N; ++i) {
			b = input.nextInt();
			w = input.nextInt();
			h = input.nextInt();
			l = input.nextInt();
			maxvolume += w*h*l;
			arr[i] = new dek();
			arr[i].set(b,w,h,l);
			if (h+b>maxHeight){
				maxHeight = h+b;
			}
		}
		int volume = input.nextInt();
		if(volume > maxvolume){
			System.out.println("Overflow");	
			return;	
		}
		double low = 0;
		double high = maxHeight;
		double mid;
		mid = 0;

		if ( !true ) return ;
		while(high - low > 0.01){
			mid = (high + low) / 2;
			//mid = Math.round(mid * 100.0) / 100.0;
			//System.out.println(low);
			//System.out.println(high);
			//System.out.println("next");
			if(!check(arr,mid,N,volume)){
				low = mid;
			}
			else{
				high = mid;

			}
		}
		mid = (high + low) / 2;
		mid = Math.round(mid * 100.0) / 100.0;
		System.out.println( mid);
	}

	public static boolean check(dek[] arr, double mid,int N,int volume){
		double current=0;
		for(int i = 0; i < N ; ++i){
			//System.out.println(current);
			if(arr[i].b  < mid){
				if(arr[i].h + arr[i].b  < mid)
					current += arr[i].h*arr[i].w*arr[i].l;
				else
					current += (mid-arr[i].b)*arr[i].w*arr[i].l;			
			}
			//System.out.println(current);
			if(current >= volume) return true;
		}
		//System.out.println(current);
		return false;	
	}
}
