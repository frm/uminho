/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package habitat;

import java.sql.Date;

/**
 *
 * @author mendes
 */
public class Util {
    public static String strToDate(String date) {
        return reformatDate(date, "/", "-");
    }
    
    public static String dateToStr(String date) {
        return reformatDate(date, "-", "/");
    }
    
    private static String reformatDate(String date, String currentDelimiter, String newDelimiter) {
        String[] s = date.split(currentDelimiter);
        int first = Integer.parseInt(s[0]);
        int month = Integer.parseInt(s[1]);
        int last = Integer.parseInt(s[2]);
        return new StringBuilder()
                    .append(last)
                    .append(newDelimiter)
                    .append(month)
                    .append(newDelimiter)
                    .append(first)
                    .toString();
    }
}
