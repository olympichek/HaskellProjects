import java.util.Scanner;
import java.util.Arrays;
import java.util.LinkedList;

class Main {
    public static LinkedList<Integer> getPrimes(int m, int n) {
        boolean[] boolArr = new boolean[n-1];
        Arrays.fill(boolArr, true);
        for(int i = 2; i*i <= n; i++) {
            for(int j = i*i; j <= n; j += i)
                boolArr[j-2] = false;
        }
        LinkedList<Integer> primes = new LinkedList<>();
        for(int i = Math.max(0, m-2); i < n-1; i++) {
            if(boolArr[i])
                primes.add(i+2);
        }
        return primes;
    }

    public static void main(String[] args) {
        Scanner sc = new Scanner(System.in);
        int m = sc.nextInt();
        int n = sc.nextInt();
        LinkedList<Integer> primes = getPrimes(m, n);
        if(primes.size() == 0) {
            System.out.println("Absent");
        }
        else {
            for(int i : primes)
                System.out.println(i);
        }
    }
}