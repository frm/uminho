package autores;

@SuppressWarnings("serial")
public class NoAuthorsInIntervalException extends Exception {
	
	public NoAuthorsInIntervalException() {
		super();
	}
	
	public NoAuthorsInIntervalException(String msg) {
		super(msg);
	}

}
