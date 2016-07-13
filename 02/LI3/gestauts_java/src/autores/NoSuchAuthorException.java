package autores;

@SuppressWarnings("serial")
public class NoSuchAuthorException extends Exception {
	
	public NoSuchAuthorException() {
		super();
	}
	
	public NoSuchAuthorException(String msg) {
		super(msg);
	}
}
