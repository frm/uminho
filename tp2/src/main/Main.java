package main;

import se.sics.jasper.Query;
import se.sics.jasper.SICStus;

import java.util.HashMap;

/**
 * Created by frm on 22/04/15.
 */
public class Main {
    public static void sampleJasper() throws Exception {
        SICStus sp = new SICStus();
        sp.load("prolog/sample.pl");

        HashMap map = new HashMap();
        String p = "succ(2, R).";
        Query q = sp.openPrologQuery(p, map);

        while(q.nextSolution()) {
            System.out.println("Query: " + p + "\nAnswer: " + map.toString());
        }

        q.close();
    }

    public static void main(String[] args) {
        System.out.println("Hello, world!");
        try {
            sampleJasper();
        } catch (Exception e) {
            System.out.println("Error sampling Jasper");
            e.printStackTrace();
        }
    }
}
