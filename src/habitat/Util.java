/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package habitat;

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
    
    public static String dateToStr(java.sql.Date date) {
        return reformatDate(date.toString(), "-", "/");
    }
    
    private static String reformatDate(String date, String currentDelimiter, String newDelimiter) {
        if(date == null || date.trim().length() == 0)
            return null;
        String[] s = date.split(currentDelimiter);
        String first = s[0];
        String month = s[1];
        String last = s[2];
        return new StringBuilder()
                    .append(last)
                    .append(newDelimiter)
                    .append(month)
                    .append(newDelimiter)
                    .append(first)
                    .toString();
    }
}
