package autores;

/**
 * Crono = mede um tempo entre start() e stop()
 * O tempo é medido em nanosegundos e convertido para 
 *  um double que representa os segs na sua parte inteira.
 * 
 * @author FMM 
 * @version (a version number or a date)
 */
public class Crono {

  private static long inicio = 0L;
  private static long fim = 0L;
  
  public static void start() { 
      fim = 0L; inicio = System.nanoTime();  
  }
  
  public static double stop() { 
      fim = System.nanoTime();
      long elapsedTime = fim - inicio;
      double segs = elapsedTime / 1.0E09;
      return segs;
  }
  
  public static String print() {
      return "" + stop();
  }
}
