package autores;

public class Tuple<F, S> {
	private F first;
	private S second;
	
	public Tuple() {
		this.first = null;
		this.second = null;
	}
	
	public Tuple(F first, S second) {
		this.first = first;
		this.second = second;
	}
	
	public F getFirst() {
		return this.first;
	}
	
	public S getSecond() {
		return this.second;
	}
	
	public void setFirst(F first) {
		this.first = first;
	}
	
	public void setSecond(S second) {
		this.second = second;
	}
}
