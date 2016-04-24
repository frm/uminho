package autores;

import java.util.List;
import java.util.ArrayList;
import java.util.Set;

public class Navigator<T> {
	private List<T> items;
	int current;
	
	public Navigator(List<T> l) {
		this.items = new ArrayList<T>(l);
		this.current = 0;
	}
	
	public Navigator(Set<T> s) {
		this.items = new ArrayList<T>(s);
		this.current = 0;
	}
	
	public T getNext() throws NoMoreItemsException {
		if (this.current >= this.items.size()) {
			throw new NoMoreItemsException();
		}
		else {
			this.current++;
			return this.items.get(this.current - 1);
		}
	}
	
	public List<T> getNext(int n) throws NoMoreItemsException {
		if (this.current >= this.items.size()) {
			throw new NoMoreItemsException();
		}
		else {
			int curr = this.current;
			this.current += n;
			
			if (this.current > this.items.size()) {
				this.current = this.items.size();
			}
			
			return this.items.subList(curr, this.current);
		}
	}
	
	public void back(int n) throws NoMoreItemsException {
		if (this.current <= 0) {
			throw new NoMoreItemsException();
		}
		else {
			this.current -= n;
			
			if (this.current < 0) {
				this.current = 0;
			}
		}
	}
	
	public void back() throws NoMoreItemsException {
		if (this.current <= 0) {
			throw new NoMoreItemsException();
		}
		else {
			this.current--;
		}
	}
	
	public int size() {
		return this.items.size();
	}
	
	public int current() {
		return this.current;
	}
	
	public int itemsLeft() {
		return this.items.size() - this.current;
	}
}
