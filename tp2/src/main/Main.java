package main;

import se.sics.jasper.Query;
import se.sics.jasper.SICStus;

import java.util.HashMap;
import java.util.Scanner;

/**
 * Created by frm on 22/04/15.
 */
public class Main {
    private static SICStus sp;


    public static void runJasper(String line) throws Exception {
        HashMap map = new HashMap();
        Query q = sp.openPrologQuery(line, map);
        if(q.nextSolution()) {
            if (map.size() == 0)
                System.out.println("sucesso");
            else
                System.out.println(map.toString());
        }
        else
            System.out.println("insucesso");
    }

    public static void main(String[] args) {
        try {
            sp = new SICStus();
            sp.load("tp2.pl");
            Scanner in = new Scanner(System.in);
            String line;
            System.out.print("> ");
            while(in.hasNextLine()){
                line = in.nextLine();
                runJasper(line);
                System.out.print("> ");
            }
        } catch (Exception e) {
            System.out.println("Error sampling Jasper");
            e.printStackTrace();
        }
    }
}
