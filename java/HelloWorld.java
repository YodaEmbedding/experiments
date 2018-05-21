
import java.lang.String;
import java.util.Scanner;

public class HelloWorld {
    public static void main(String[] args) {
        Scanner sc = new Scanner(System.in);

        System.out.println("Who is a scrub?");
        String scrub = sc.next();

        System.out.println(scrub + " is a scrub.");
    }
}


